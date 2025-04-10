library(dplyr)
library(optparse)


option_list <- list(
  make_option("--n_spat", type = "integer", default = 10,
              help = "Number of spatial locations at each time point"),
  make_option("--n_time", type = "integer", default = 10,
              help = "number of time points, indexing starts from 0"),
  make_option("--m", type = "integer", default = 1,
              help = "Number of repetitions per scenario"),
  make_option("--x_blocks", type = "character", default = "33:33:34",
              help = "Segmentation of x coordinate, string gives proportions of the segment lengths"),
  make_option("--y_blocks", type = "character", default = "33:33:34",
              help = "Segmentation of y coordinate, string gives proportions of the segment lengths"),
  make_option("--time_blocks", type = "character", default = "33:33:34",
              help = "Segmentation of time, string gives proportions of the segment lengths"),
  make_option("--setting", type = "integer", default = 1,
              help = "Setting number, see README for description of different settings"),
  make_option("--file", default = "results/test.txt", help = "Output file name")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)


#' Compute negative square root of a positive definite matrix
#'
#' More precisely, function computes matrix lambda
#' s.t. sigma^{-1} = lambda %*% lambda.
#'
#' @param sigma Double matrix, positive definite matrix.
#'
#' @return Double matrix, square root of the matrix.
sqrtmat_inv <- function(sigma) {
  eigenval <- eigen(sigma)$values
  if (any(eigenval <= 0) || any(sigma != t(sigma))) {
    rlang::abort("`sigma` must be a symmetric positive definite matrix.")
  }
  eigenvec <- eigen(sigma)$vectors
  eigenvec %*% diag(eigenval^(-0.5)) %*% t(eigenvec)
}


#' Generate uniform sample of spatial locations from a box [0, 1] x [0, 1]
#'
#' @param n_spat Number of spatial locations
#'
#' @returns sf object with coordinated but with 0 features
gen_unif_coords_box <- function(n_spat) {
  x <- stats::runif(n_spat, 0, 1)
  y <- stats::runif(n_spat, 0, 1)
  tibble::tibble(x = x, y = y) %>%
    sf::st_as_sf(coords = c("x", "y"))
}


#' Generate white noise with possible trend depending on location and time
#'
#' @param coords Object of class sftime including geometry and time
#' @param stationary If TRUE simulate random noise, else add trend
#' @param theta1 Trend with respect to the first spatial coordinate
#' @param theta2 Trend with respect to the second spatial coordinate
#' @param theta3 Trend with respect to time
#'
#' @returns vector representing the realized field at the corresponding
#'   spatio-temporal locations
gen_field_smooth_trend <- function(coords, stationary = TRUE, theta1 = 1,
                                   theta2 = -1, theta3 = 3) {
  white_noise <- stats::rnorm(nrow(coords), mean = 0, sd = 1)
  if (stationary) {
    white_noise
  } else {
    do.call(rbind, sf::st_geometry(coords)) %>%
      tibble::as_tibble() %>%
      mutate(time = sftime::st_time(coords), e = white_noise) %>%
      purrr::pmap_dbl(~ theta1 * ..1 + theta2 * ..2 + theta3 * ..3 + ..4)
  }
}


# Compute spatial locations
coords <- gen_unif_coords_box(opt$n_spat)

# Add temporal locations
time <- 0:(opt$n_time - 1)
coords <- coords %>%
  replicate(opt$n_time, ., simplify = FALSE) %>%
  bind_rows() %>%
  mutate(time = rep(time, each = opt$n_spat)) %>%
  sftime::st_sftime(sf_column_name = "geometry", time_column_name = "time")


simulate <- function(i, coords, a) {
  # Compute latent components
  if (opt$setting == 1) {
    a <- matrix(runif(8 * 8, 1, 100), ncol = 8)
    n_nonstationary <- 3
    n_comp <- 8
    latent <- coords %>%
      mutate(s1 = gen_field_smooth_trend(., stationary = FALSE, 0.5, -2, 3),
             s2 = gen_field_smooth_trend(., stationary = FALSE, -2, 3, 0.5),
             s3 = gen_field_smooth_trend(., stationary = FALSE, 3, 0.5, -2),
             s4 = gen_field_smooth_trend(., stationary = TRUE),
             s5 = gen_field_smooth_trend(., stationary = TRUE),
             s6 = gen_field_smooth_trend(., stationary = TRUE),
             s7 = gen_field_smooth_trend(., stationary = TRUE),
             s8 = gen_field_smooth_trend(., stationary = TRUE)) %>%
      sftime::st_drop_time() %>%
      sf::st_drop_geometry()
    colnames(latent) <- paste0("f", 1:n_comp)
  }
  
  set.seed(123)
  # Compute observed field
  observed <- latent %>%
    apply(1, function(x) a %*% matrix(x, ncol = 1)) %>%
    t() %>%
    as_tibble()
  colnames(observed) <- paste0("f", 1:n_comp)
  
  # Compute whitened field
  mean_p <- colMeans(observed)
  cov_p_inv_sqrt <- sqrtmat_inv(cov(observed))
  
  whitened <- observed %>%
    sweep(2, mean_p, "-") %>%
    apply(1, function(x) cov_p_inv_sqrt %*% matrix(x, ncol = 1)) %>%
    t() %>%
    as_tibble()
  colnames(whitened) <- paste0("f", 1:n_comp)
  
  # Parse cut points
  x_prop <- stringr::str_split(opt$x_blocks, ":", simplify = TRUE) %>%
    as.integer()
  y_prop <- stringr::str_split(opt$y_blocks, ":", simplify = TRUE) %>%
    as.integer()
  time_prop <- stringr::str_split(opt$time_blocks, ":", simplify = TRUE) %>%
    as.integer()
  if (sum(x_prop) != 100 | sum(y_prop) != 100 | sum(time_prop) != 100) {
    rlang::abort("Sum of proportions must be 100.")
  }
  
  time_cuts <- cumsum(c(0, time_prop / 100 * opt$n_time))
  x_cuts <- cumsum(c(0, x_prop / 100))
  y_cuts <- cumsum(c(0, y_prop / 100))
  
  # Add segments to whitened field (NOTICE GROUPING)
  whitened_with_seg <- cbind(coords, whitened, sf_column_name = "geometry",
                             tc_column_name = "time") %>%
    mutate(x_segment = cut(sf::st_coordinates(.)[, "X"], x_cuts,
                           include.lowest = TRUE),
           y_segment = cut(sf::st_coordinates(.)[, "Y"], y_cuts,
                           include.lowest = TRUE),
           time_segment = cut(time, time_cuts, right = FALSE)) %>%
    sftime::st_drop_time() %>%
    sf::st_drop_geometry() %>%
    group_by(x_segment, y_segment, time_segment)
  
  # Compute means and proportional sample sizes in segments
  means_segment <- whitened_with_seg %>%
    summarise(across(starts_with("f"), ~ mean(.x)), .groups = "drop") %>%
    mutate(n_prop = group_size(whitened_with_seg) / (opt$n_spat * opt$n_time))
  
  # Compute variance of the segment means
  outer_prods <- means_segment %>%
    select(starts_with("f")) %>%
    as.matrix() %>%
    apply(1, function(x) x %o% x, simplify = FALSE)
  
  mean_var <- purrr::pmap(list(a = means_segment$n_prop, b = outer_prods),
                          \(a, b) a * b) %>%
    purrr::reduce(`+`)
  
  # Compute unmixing matrix
  w <- t(eigen(mean_var)$vectors) %*% cov_p_inv_sqrt
  w_nonstationary <- w[1:n_nonstationary, ]
  w_stationary <- w[(n_nonstationary + 1):n_comp, ]
  
  # Compute performance indices
  a_inv <- solve(a)
  a_inv_proj_nonstat <- LDRTools::B2P(t(a_inv[1:n_nonstationary, ]))
  a_inv_proj_stat <- LDRTools::B2P(t(a_inv[(n_nonstationary + 1):n_comp, ]))
  w_proj_nonstat <- LDRTools::B2P(t(w_nonstationary))
  w_proj_stat <- LDRTools::B2P(t(w_stationary))
  ind_stat <- LDRTools::Pdist(list(a_inv_proj_stat, w_proj_stat),
                              weights = "constant")
  ind_nonstat <- LDRTools::Pdist(list(a_inv_proj_nonstat, w_proj_nonstat),
                                 weights = "constant")
  
  ret <- list(w, c(stationary = ind_stat, nonstationary = ind_nonstat),
              eigen_val = eigen(mean_var)$values)
  names(ret) <- c("unmixing", "performance", "mean_var_eigen_values")
  ret
}

file_con <- file(opt$file)
writeLines(capture.output(simulate(1, coords, a)), file_con)
close(file_con)
