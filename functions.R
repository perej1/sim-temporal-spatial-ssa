# Functions for the simulations
suppressMessages(library(dplyr))


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
#' @param n_spatial Number of spatial locations
#' @param seed Seed for generating spatial locations
#'
#' @returns sf object with coordinated but with 0 features
gen_unif_coords_box <- function(n_spatial, seed) {
  set.seed(seed)
  x <- stats::runif(n_spatial, 0, 1)
  y <- stats::runif(n_spatial, 0, 1)
  tibble::tibble(x = x, y = y) %>%
    sf::st_as_sf(coords = c("x", "y"))
}


#' Generate spatio-temporal locations on which the field is observed
#'
#' Locations are uniformly distributed but they are the same for each time
#' point. Field is observed at each time point 0, 1, ..., n_time - 1.
#'
#' @param n_spatial Number of spatial locations
#' @param n_time Number of time points
#' @param seed Seed for generating spatial locations
#'
#' @returns sftime object with locations and time but no fields
gen_coords <- function(n_spatial, n_time, seed) {
  set.seed(seed)

  # Compute spatial locations
  coords <- gen_unif_coords_box(n_spatial, seed)

  # Add temporal locations
  time <- 0:(n_time - 1)
  coords %>%
    replicate(n_time, ., simplify = FALSE) %>%
    bind_rows() %>%
    mutate(time = rep(time, each = n_spatial)) %>%
    sftime::st_sftime(sf_column_name = "geometry", time_column_name = "time")
}


#' Create a covariance matrix corresponding to the spatiotemporal locations
#'
#' The chosen covariance function is separable. For spatial dependence structure
#' we use the Matern covariance function. For the temporal dependence structure
#' we use the exponential covariance function.
#'
#' @param coords sftime object with locations and time but no fields
#'
#' @returns Covariance matrix corresponding to spatiotemporal locations
gen_cov_separable <- function(coords) {
  coords_space <- unique(coords$geometry)
  coords_time <- unique(coords$time)
  n_space <- length(coords_space)
  n_time <- length(coords_time)

  covmat_space <- matrix(rep(0, n_space^2), nrow = n_space)
  for (i in 1:n_space) {
    for (j in i:n_space) {
      dist_space <- as.double(sf::st_distance(coords_space[[i]],
                                              coords_space[[j]]))
      covmat_space[i, j] <- fields::Matern(dist_space, range = 1,
                                           smoothness = 1)
    }
  }
  covmat_space <- covmat_space + t(covmat_space) - diag(n_space)

  covmat_time <- matrix(rep(0, n_time^2), nrow = n_time)
  for (i in 1:n_time) {
    for (j in i:n_time) {
      dist_time <- abs(coords_time[i] - coords_time[j])
      covmat_time[i, j] <- fields::Exponential(dist_time, aRange = 1, theta = 1)
    }
  }
  covmat_time <- covmat_time + t(covmat_time) - diag(n_time)

  covmat_time %x% covmat_space
}


#' Compute segments for spatio-temporal coordinates
#'
#' @param coords sftime object with locations and time but no fields
#' @param x_prop Division of x-axis of the [0,1] x [0,1] box in terms of %
#' @param y_prop Division of y-axis of the [0,1] x [0,1] box in terms of %
#' @param time_prop Division of time in terms of %
#'
#' @returns Tibble of segments for different axes (3 columns)
compute_segments <- function(coords, x_prop, y_prop, time_prop) {
  if (sum(x_prop) != 100 || sum(y_prop) != 100 || sum(time_prop) != 100) {
    rlang::abort("Sum of proportions must be 100.")
  }

  n_time <- sftime::st_time(coords) %>%
    unique() %>%
    max() + 1

  x_cuts <- c(0, cumsum(x_prop / 100))
  y_cuts <- c(0, cumsum(y_prop / 100))
  time_cuts <- quantile(0:(n_time - 1), c(0, cumsum(time_prop / 100)))

  coords %>%
    mutate(x_segment = cut(sf::st_coordinates(.)[, "X"], x_cuts,
                           include.lowest = TRUE),
           y_segment = cut(sf::st_coordinates(.)[, "Y"], y_cuts,
                           include.lowest = TRUE),
           time_segment = cut(time, time_cuts, include.lowest = TRUE)) %>%
    sftime::st_drop_time() %>%
    sf::st_drop_geometry()
}


#' Simulate field that is normally distributed in each segment
#'
#' @param coords sftime object with locations and time but no fields
#' @param mu Expected value for each segment
#' @param covmat Covariance matrix corresponding to spatiotemporal locations
#' @param x_prop Division of x-axis of the [0,1] x [0,1] box in terms of %
#' @param y_prop Division of y-axis of the [0,1] x [0,1] box in terms of %
#' @param time_prop Division of time in terms of %
#'
#' @returns Vector giving value of the field at each spatio-temporal coordinate
gen_field_cluster <- function(coords, mu, covmat, x_prop, y_prop, time_prop) {
  if (length(mu) != length(x_prop) * length(y_prop) * length(time_prop)) {
    rlang::abort("'mu' has wrong length.")
  }
  segments <- compute_segments(coords, x_prop, y_prop, time_prop) %>%
    mutate(comb = interaction(x_segment, y_segment, time_segment)) %>%
    pull(comb)

  indices <- match(segments, levels(segments))
  mu_for_segment <- mu[indices]

  MASS::mvrnorm(n = 1, mu_for_segment, covmat)
}
