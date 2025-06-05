# Perform m rounds of simulations for a particular setting. On each
# round i, the mixing matrix is different. More precisely, elements
# of the mixing matrix are generated from uniform distribution on [-1, 1].
library(optparse)
source("functions.R")


option_list <- list(
  make_option("--mu", type = "character", default = "xyt4",
              help = stringr::str_c("For different options the latent field ",
                                    "has different mean.")),
  make_option("--epsilon", type = "character", default = "sephigh",
              help = stringr::str_c("For different options the latent field ",
                                    "has different dependence structure")),
  make_option("--ns", type = "integer", default = 50,
              help = "Number of spatial locations at each time point"),
  make_option("--nt", type = "integer", default = 100,
              help = "number of time points, indexing starts from 0"),
  make_option("--m", type = "integer", default = 100,
              help = "Number of repetitions per scenario"),
  make_option("--xblocks", type = "character", default = "100",
              help = stringr::str_c("Segmentation of x coord, string gives ",
                                    "proportions of the segment lengths")),
  make_option("--yblocks", type = "character", default = "100",
              help = stringr::str_c("Segmentation of y coord, string gives ",
                                    "proportions of the segment lengths")),
  make_option("--tblocks", type = "character", default = "100",
              help = stringr::str_c("Segmentation of time, string gives ",
                                    "proportions of the segment lengths")),
  make_option("--random_eigen", type = "logical", default = FALSE,
              help = stringr::str_c("If TRUE random eigenvectors are used in ",
                                    "the computation of the unmixing matrix")),
  make_option("--seed_spatial", type = "integer", default = 123,
              help = "Seed for generating spatial locations"),
  make_option("--seed_sim", type = "integer", default = 321,
              help = "Seed for the function 'simulate'")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Set number of components, and parameters for covariance functions
if (opt$mu %in% c("osc", "xyt2")) {
  dim <- 4
  dim_nonstationary <- 2
} else if (opt$mu == "xyt3") {
  dim <- 6
  dim_nonstationary <- 3
} else if (opt$mu == "xyt4") {
  dim <- 8
  dim_nonstationary <- 4
} else {
  rlang::abort("Invalid option for the argument --mu.")
}

if (opt$epsilon == "seplow") {
  aRange <- 0.5
  range <- 0.1
  smoothness <- 0.5
} else if (opt$epsilon == "sephigh") {
  aRange <- 2
  range <- 1
  smoothness <- 0.5
}

#' Perform ith repetition for a particular setting
#'
#' @param i The simulation round.
#' @param coords An sftime object with locations and time but no fields.
#'
#' @returns A list of length 2: (i) eigenvalues and (ii) performance indices for
#'   stationary and nonstationary subspaces.
simulate <- function(i, coords) {

  # Set mixing matrix
  repeat {
    a <- matrix(runif(dim^2, min = -1, max = 1), ncol = dim)
    if (is.matrix(try(solve(a), silent = TRUE))) {
      break
    }
  }

  # Set latent components
  if (opt$epsilon == "noise") {
    epsilon <- 1:dim %>%
      purrr::map(\(i) stats::rnorm(opt$ns * opt$nt))
  } else if (opt$epsilon %in% c("seplow", "sephigh")) {
    epsilon <- 1:dim %>%
      purrr::map(\(i) gen_separable(coords, range, smoothness, aRange))
  } else {
    rlang::abort("Invalid option for the argument --epsilon.")
  }

  if (opt$mu == "xyt2") {
    f1 <- gen_field_cluster(coords, epsilon[[1]], c(0, 0.2), 100, c(50, 50),
                            100)
    f2 <- gen_field_cluster(coords, epsilon[[2]], c(-1, -1.2), 100, 100,
                            c(50, 50))
    latent <- coords %>%
      mutate(
        f1 = f1,
        f2 = f2,
        f3 = epsilon[[3]],
        f4 = epsilon[[4]]
      ) %>%
      sftime::st_drop_time() %>%
      sf::st_drop_geometry()
  } else if (opt$mu == "xyt3") {
    f1 <- gen_field_cluster(coords, epsilon[[1]], c(0, 0.2), 100, c(50, 50),
                            100)
    f2 <- gen_field_cluster(coords, epsilon[[2]], c(-1, -1.2), 100, 100,
                            c(50, 50))
    f3 <- gen_field_cluster(coords, epsilon[[3]], c(2, 2.2), c(50, 50), 100,
                            100)
    latent <- coords %>%
      mutate(
        f1 = f1,
        f2 = f2,
        f3 = f3,
        f4 = epsilon[[4]],
        f5 = epsilon[[5]],
        f6 = epsilon[[6]]
      ) %>%
      sftime::st_drop_time() %>%
      sf::st_drop_geometry()
  } else if (opt$mu == "xyt4") {
    f1 <- gen_field_cluster(coords, epsilon[[1]], c(0, 0.2), 100, c(50, 50),
                            100)
    f2 <- gen_field_cluster(coords, epsilon[[2]], c(-1, -1.2), 100, 100,
                            c(50, 50))
    f3 <- gen_field_cluster(coords, epsilon[[3]], c(2, 2.2), c(50, 50), 100,
                            100)
    f4 <- gen_field_cluster(coords, epsilon[[4]], seq(-2, -2.2, length.out = 8),
                            c(50, 50), c(50, 50), c(50, 50))
    latent <- coords %>%
      mutate(
        f1 = f1,
        f2 = f2,
        f3 = f3,
        f4 = f4,
        f5 = epsilon[[5]],
        f6 = epsilon[[6]],
        f7 = epsilon[[7]],
        f8 = epsilon[[8]]
      ) %>%
      sftime::st_drop_time() %>%
      sf::st_drop_geometry()
  } else if (opt$mu == "osc") {
    f1 <- gen_field_cluster(coords, epsilon[[1]], c(5, 5, -5, -5, 5, 5, -5, -5),
                            c(50, 50), c(50, 50), c(50, 50))
    f2 <- gen_field_cluster(coords, epsilon[[2]], c(5, -5, 5, -5, 5, -5, 5, -5),
                            c(50, 50), c(50, 50), c(50, 50))
    latent <- coords %>%
      mutate(
        f1 = f1,
        f2 = f2,
        f3 = epsilon[[3]],
        f4 = epsilon[[4]]
      ) %>%
      sftime::st_drop_time() %>%
      sf::st_drop_geometry()
  } else {
    rlang::abort("Invalid option for the argument --mu.")
  }

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
  x_prop <- stringr::str_split(opt$xblocks, ":", simplify = TRUE) %>%
    as.integer()
  y_prop <- stringr::str_split(opt$yblocks, ":", simplify = TRUE) %>%
    as.integer()
  time_prop <- stringr::str_split(opt$tblocks, ":", simplify = TRUE) %>%
    as.integer()

  whitened_with_seg <- compute_segments(coords, x_prop, y_prop, time_prop) %>%
    cbind(whitened) %>%
    group_by(x_segment, y_segment, time_segment)

  # Compute means and proportional sample sizes in segments
  means_segment <- whitened_with_seg %>%
    summarise(across(starts_with("f"), ~ mean(.x)), .groups = "drop") %>%
    mutate(n_prop = group_size(whitened_with_seg) / (opt$ns * opt$nt))

  # Compute variance of the segment means
  outer_prods <- means_segment %>%
    select(starts_with("f")) %>%
    as.matrix() %>%
    apply(1, function(x) x %o% x, simplify = FALSE)

  mean_var <- purrr::pmap(list(arg1 = means_segment$n_prop, arg2 = outer_prods),
                          \(arg1, arg2) arg1 * arg2) %>%
    purrr::reduce(`+`)

  # Compute unmixing matrix
  if (opt$random_eigen) {
    v_transpose <- pracma::randortho(dim, type = "orthonormal")
  } else {
    v_transpose <- t(eigen(mean_var)$vectors)
  }

  w <- v_transpose %*% cov_p_inv_sqrt
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

  # Collect results
  res <- list(c(stationary = ind_stat, nonstationary = ind_nonstat),
              eigen_val = eigen(mean_var)$values)
  names(res) <- c("performance", "mean_var_eigen_values")
  res
}

# Set spatiotemporal coordinates
coords <- gen_coords(opt$ns, opt$nt, opt$seed_spatial)

# Perform m repetitions for the scenario
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
filename <- stringr::str_c("mu_", opt$mu,
                           "_epsilon_", opt$epsilon,
                           "_ns_", opt$ns,
                           "_nt_", opt$nt,
                           "_m_", opt$m,
                           "_xblocks_", opt$xblocks,
                           "_yblocks_", opt$yblocks,
                           "_tblocks_", opt$tblocks,
                           "_random_eigen_", opt$random_eigen, ".csv")

tibble::as_tibble(performance) %>%
  readr::write_csv(stringr::str_c("results/perf/", filename))

tibble::as_tibble(eigenval) %>%
  readr::write_csv(stringr::str_c("results/eigen/", filename))
