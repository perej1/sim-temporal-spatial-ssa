# Perform m rounds of simulations for a particular setting. On each
# round i = 1, ..., m, the mixing matrix is different. More precisely, elements
# of the mixing matrix are generated from uniform distribution on [-1, 1].

library(optparse)
source("functions.R")


option_list <- list(
  make_option("--latent", type = "character", default = "spacetime",
              help = "For different options the latent field is different."),
  make_option("--n_spatial", type = "integer", default = 100,
              help = "Number of spatial locations at each time point"),
  make_option("--n_time", type = "integer", default = 100,
              help = "number of time points, indexing starts from 0"),
  make_option("--m", type = "integer", default = 100,
              help = "Number of repetitions per scenario"),
  make_option("--x_blocks", type = "character", default = "33:33:34",
              help = stringr::str_c("Segmentation of x coord, string gives ",
                                    "proportions of the segment lengths")),
  make_option("--y_blocks", type = "character", default = "33:33:34",
              help = stringr::str_c("Segmentation of y coord, string gives ",
                                    "proportions of the segment lengths")),
  make_option("--time_blocks", type = "character", default = "33:33:34",
              help = stringr::str_c("Segmentation of time, string gives ",
                                    "proportions of the segment lengths")),
  make_option("--seed_spatial", type = "integer", default = 123,
              help = "Seed for generating spatial locations"),
  make_option("--seed_sim", type = "integer", default = 321,
              help = "Seed for for the function 'simulate'")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# We always have four components, 2 nonstationary ones
dim <- 4
dim_nonstationary <- 2

#' Perform ith repetition of the setting
#'
#' @param i Repetition i of the simulation setting
#' @param coords Spatio-temporal coordinates
#'
#' @returns A list of length 2: eigenvalues and performance indices for
#'   stationary and nonstationary subspaces
simulate <- function(i, coords) {

  # Set mixing matrix
  repeat {
    a <- matrix(runif(dim^2, min = -1, max = 1), ncol = dim)
    if (is.matrix(try(solve(a), silent = TRUE))) {
      break
    }
  }

  # Set latent components
  if (opt$latent == "spacetime") {
    f1 <- gen_field_cluster(coords, seq(-20, 30, 10), rep(1, 6), 100, c(50, 50),
                            c(33, 33, 34))
    f2 <- gen_field_cluster(coords, seq(-40, 30, 10), rep(1, 8), c(50, 50),
                            c(50, 50), c(50, 50))
  } else if (opt$latent == "oscillating") {
    f1 <- gen_field_cluster(coords, c(5, 5, -5, -5, 5, 5, -5, -5), rep(1, 8),
                            c(50, 50), c(50, 50), c(50, 50))
    f2 <- gen_field_cluster(coords, c(5, -5, 5, -5, 5, -5, 5, -5), rep(1, 8),
                            c(50, 50), c(50, 50), c(50, 50))
  } else {
    rlang::abort("Invalid option for the argument --latent.")
  }

  latent <- coords %>%
    mutate(
      f1 = f1,
      f2 = f2,
      f4 = gen_field_cluster(., 0, 1, 100, 100, 100),
      f5 = gen_field_cluster(., 0, 1, 100, 100, 100)
    ) %>%
    sftime::st_drop_time() %>%
    sf::st_drop_geometry()

  # Compute observed field
  observed <- latent %>%
    apply(1, function(x) a %*% matrix(x, ncol = 1)) %>%
    t()

  # Compute whitened field
  mean_p <- colMeans(observed)
  cov_p_inv_sqrt <- sqrtmat_inv(cov(observed))

  whitened <- observed %>%
    sweep(2, mean_p, "-") %>%
    apply(1, function(x) cov_p_inv_sqrt %*% matrix(x, ncol = 1)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal")
  colnames(whitened) <- stringr::str_c("f", 1:dim)

  # Parse cut points
  x_prop <- stringr::str_split(opt$x_blocks, ":", simplify = TRUE) %>%
    as.integer()
  y_prop <- stringr::str_split(opt$y_blocks, ":", simplify = TRUE) %>%
    as.integer()
  time_prop <- stringr::str_split(opt$time_blocks, ":", simplify = TRUE) %>%
    as.integer()

  whitened_with_seg <- compute_segments(coords, x_prop, y_prop, time_prop) %>%
    cbind(whitened) %>%
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

  mean_var <- purrr::pmap(list(arg1 = means_segment$n_prop, arg2 = outer_prods),
                          \(arg1, arg2) arg1 * arg2) %>%
    purrr::reduce(`+`)

  # Compute unmixing matrix
  w <- t(eigen(mean_var)$vectors) %*% cov_p_inv_sqrt
  w_nonstationary <- w[1:dim_nonstationary, , drop = FALSE]
  w_stationary <- w[(dim_nonstationary + 1):dim, , drop = FALSE]

  # Compute performance indices
  a_inv <- solve(a)
  a_inv_proj_nonstat <- LDRTools::B2P(t(a_inv[1:dim_nonstationary, ,
                                              drop = FALSE]))
  a_inv_proj_stat <- LDRTools::B2P(t(a_inv[(dim_nonstationary + 1):dim, ,
                                           drop = FALSE]))
  w_proj_nonstat <- LDRTools::B2P(t(w_nonstationary))
  w_proj_stat <- LDRTools::B2P(t(w_stationary))
  ind_stat <- LDRTools::Pdist(list(a_inv_proj_stat, w_proj_stat),
                              weights = "constant")
  ind_nonstat <- LDRTools::Pdist(list(a_inv_proj_nonstat, w_proj_nonstat),
                                 weights = "constant")

  res <- list(c(stationary = ind_stat, nonstationary = ind_nonstat),
              eigen_val = eigen(mean_var)$values)
  names(res) <- c("performance", "mean_var_eigen_values")
  res
}

# Set spatial locations
coords <- gen_coords(opt$n_spatial, opt$n_time, opt$seed_spatial)

# Perform m repetitions fo the scenario
RNGkind("L'Ecuyer-CMRG")
set.seed(opt$seed_sim)
res <- parallel::mclapply(1:opt$m, simulate, coords = coords,
                          mc.set.seed = TRUE,
                          mc.cores = parallel::detectCores()) %>%
  purrr::transpose()

# Collect results in a matrix (one row corresponds to one repetition)
performance <- do.call(rbind, res$performance)
eigenval <- do.call(rbind, res$mean_var_eigen_values)
colnames(eigenval) <- stringr::str_c("f", 1:dim)

# Save results
filename <- stringr::str_c("latent_", opt$latent,
                           "_n_spatial_", opt$n_spatial,
                           "_n_time_", opt$n_time,
                           "_m_", opt$m,
                           "_x_blocks_", opt$x_blocks,
                           "_y_blocks_", opt$y_blocks,
                           "_time_blocks_", opt$time_blocks,
                           "_seed_spatial_", opt$seed_spatial,
                           "_seed_sim_", opt$seed_sim, ".csv")

tibble::as_tibble(performance) %>%
  readr::write_csv(stringr::str_c("results/perf/", filename))

tibble::as_tibble(eigenval) %>%
  readr::write_csv(stringr::str_c("results/eigen/", filename))
