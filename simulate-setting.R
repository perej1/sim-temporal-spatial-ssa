library(optparse)
source("functions.R")


option_list <- list(
  make_option("--n_spatial", type = "integer", default = 100,
              help = "Number of spatial locations at each time point"),
  make_option("--n_time", type = "integer", default = 100,
              help = "number of time points, indexing starts from 0"),
  make_option("--m", type = "integer", default = 100,
              help = "Number of repetitions per scenario"),
  make_option("--x_blocks", type = "character", default = "100",
              help = "Segmentation of x coordinate, string gives proportions of the segment lengths"),
  make_option("--y_blocks", type = "character", default = "50:50",
              help = "Segmentation of y coordinate, string gives proportions of the segment lengths"),
  make_option("--time_blocks", type = "character", default = "50:50",
              help = "Segmentation of time, string gives proportions of the segment lengths"),
  make_option("--include_var_nonstationary", type = "logical", default = FALSE,
              help = "If true then variance of the fourth component is nonstationary"),
  make_option("--dim", type = "integer", default = 5,
              help = "Number of components"),
  make_option("--dim_nonstationary", type = "integer", default = 3,
              help = "Dimension of the nonstationary subspace"),
  make_option("--seed_spatial", type = "integer", default = 123,
              help = "Seed for generating spatial locations"),
  make_option("--seed_sim", type = "integer", default = 321,
              help = "Seed for for the function 'simulate'")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)


#' Perform ith repetition of the setting
#'
#' @param i Repetition i of the simulation setting
#' @param coords Spatio-temporal coordinates
#' @param a The mixing matrix
#'
#' @returns A list of length 2: eigenvalues and performance indices for
#'   stationary and nonstationary subspaces
simulate <- function(i, coords, a) {
  sigma <- 10
  
  # Nonstationary latent component 1
  # - Nonstationary in time and space
  # - Oscillating mean
  x_prop1 <- c(50, 50)
  y_prop1 <- c(50, 50)
  time_prop1 <- c(50, 50)
  mu1 <- c(5, 5, -5, -5, 5, 5, -5, -5)
  sigma1 <- rep(sigma, 8)

  # Nonstationary latent component 2
  # - Only nonstationary in time wrt mean
  x_prop2 <- 100
  y_prop2 <- 100
  time_prop2 <- c(10, 20, 30, 40)
  mu2 <- c(-10, -200, 500, 100)
  sigma2 <- rep(sigma, 4)

  # Nonstationary latent component 3
  # - Only nonstationary in space wrt mean
  x_prop3 <- c(10, 90)
  y_prop3 <- c(10, 30, 60)
  time_prop3 <- 100
  mu3 <- c(-10, -200, -500, 500, 100, 200)
  sigma3 <- rep(sigma, 6)

  # Nonstationary latent component 4
  # - Nonstationary in time and space wrt variance but not wrt mean
  x_prop4 <- c(50, 50)
  y_prop4 <- c(50, 50)
  time_prop4 <- c(50, 50)
  mu4 <- rep(0, 8)
  sigma4 <- if (opt$include_var_nonstationary) 2^(0:7) else rep(1, 8)

  # Stationary component 5
  # - Mean and variance do not change
  x_prop5 <- 100
  y_prop5 <- 100
  time_prop5 <- 100
  mu5 <- 0
  sigma5 <- 1

  # Compute latent components
  latent <- coords %>%
    mutate(
      f1 = gen_field_cluster(., mu1, sigma1, x_prop1, y_prop1, time_prop1),
      f2 = gen_field_cluster(., mu2, sigma2, x_prop2, y_prop2, time_prop2),
      f3 = gen_field_cluster(., mu3, sigma3, x_prop3, y_prop3, time_prop3),
      f4 = gen_field_cluster(., mu4, sigma4, x_prop4, y_prop4, time_prop4),
      f5 = gen_field_cluster(., mu5, sigma5, x_prop5, y_prop5, time_prop5)
    ) %>%
    sftime::st_drop_time() %>%
    sf::st_drop_geometry()

  n_nonstationary <- opt$dim_nonstationary
  n_comp <- ncol(latent)

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
  colnames(whitened) <- paste0("f", 1:n_comp)

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
  w_nonstationary <- w[1:n_nonstationary, , drop = FALSE]
  w_stationary <- w[(n_nonstationary + 1):n_comp, , drop = FALSE]

  # Compute performance indices
  a_inv <- solve(a)
  a_inv_proj <- LDRTools::B2P(t(a_inv))
  a_inv_proj_nonstat <- LDRTools::B2P(t(a_inv[1:n_nonstationary, ,
                                              drop = FALSE]))
  a_inv_proj_stat <- LDRTools::B2P(t(a_inv[(n_nonstationary + 1):n_comp, ,
                                           drop = FALSE]))
  w_proj <- LDRTools::B2P(t(w))
  w_proj_nonstat <- LDRTools::B2P(t(w_nonstationary))
  w_proj_stat <- LDRTools::B2P(t(w_stationary))
  ind <- LDRTools::Pdist(list(a_inv_proj, w_proj), weights = "constant")
  ind_stat <- LDRTools::Pdist(list(a_inv_proj_stat, w_proj_stat),
                              weights = "constant")
  ind_nonstat <- LDRTools::Pdist(list(a_inv_proj_nonstat, w_proj_nonstat),
                                 weights = "constant")

  res <- list(c(stationary = ind_stat, nonstationary = ind_nonstat, total = ind),
              eigen_val = eigen(mean_var)$values)
  names(res) <- c("performance", "mean_var_eigen_values")
  res
}

a <- diag(opt$dim)
  
coords <- gen_coords(opt$n_spatial, opt$n_time, opt$seed_spatial)

# Perform m repetitions fo the scenario
RNGkind("L'Ecuyer-CMRG")
set.seed(opt$seed_sim)
res <- parallel::mclapply(1:opt$m, simulate, coords = coords, a = a,
                          mc.set.seed = TRUE,
                          mc.cores = parallel::detectCores()) %>%
  purrr::transpose()

# Collect results in a matrix (one row corresponds to one repetition)
performance <- do.call(rbind, res$performance)
eigenval <- do.call(rbind, res$mean_var_eigen_values)
colnames(eigenval) <- paste0("f", 1:opt$dim)

# Save results
filename <- paste0("n_spatial_", opt$n_spatial,
                   "_n_time_", opt$n_time,
                   "_m_", opt$m,
                   "_x_blocks_", opt$x_blocks,
                   "_y_blocks_", opt$y_blocks,
                   "_time_blocks_", opt$time_blocks,
                   "_include_var_nonstationary_", opt$include_var_nonstationary,
                   "_dim_", opt$dim,
                   "_dim_nonstationary_", opt$dim_nonstationary,
                   "_seed_spatial_", opt$seed_spatial,
                   "_seed_sim_", opt$seed_sim, ".csv")

tibble::as_tibble(performance) %>%
  readr::write_csv(paste0("results/perf/", filename))

tibble::as_tibble(eigenval) %>%
  readr::write_csv(paste0("results/eigen/", filename))
