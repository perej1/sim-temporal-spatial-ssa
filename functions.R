# Functions for the simulations
suppressMessages(library(dplyr))


#' Compute negative square root of a positive definite matrix
#'
#' More precisely, function computes matrix lambda
#' s.t. sigma^{-1} = lambda %*% lambda.
#'
#' @param sigma A Double matrix, positive definite matrix.
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
#' @param n_spatial The number of spatial locations.
#' @param seed The seed for generating spatial locations.
#'
#' @returns sf object with coordinates but with 0 features.
gen_unif_coords_box <- function(n_spatial, seed) {
  set.seed(seed)
  x <- stats::runif(n_spatial, 0, 1)
  y <- stats::runif(n_spatial, 0, 1)
  tibble::tibble(x = x, y = y) %>%
    sf::st_as_sf(coords = c("x", "y"))
}


#' Generate spatiotemporal locations on which the field is observed
#'
#' Locations are uniformly distributed but they are the same for each time
#' point. Field is observed at each time point 0, 1, ..., n_time - 1.
#'
#' @param n_spatial The number of spatial locations.
#' @param n_time The number of time points.
#' @param seed The seed for generating spatial locations.
#'
#' @returns An sftime object with locations and time but no fields.
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


#' Generate realization of a stationary field having independent spatial and
#' time components
#'
#' @param coords An sftime object with locations and time but no fields.
#' @param range,smoothness,aRange,theta The parameters for Matern and
#'   Exponential covariance functions.
#'
#' @returns A Vector giving value of the field at each spatiotemporal
#'   coordinate.
gen_independent_loc_time <- function(coords, range, smoothness, aRange, theta) {
  coords_space <- unique(coords$geometry)
  coords_time <- unique(coords$time)
  n_space <- length(coords_space)
  n_time <- length(coords_time)

  covmat_space <- matrix(rep(0, n_space^2), nrow = n_space)
  for (i in 1:n_space) {
    for (j in i:n_space) {
      dist_space <- as.double(sf::st_distance(coords_space[[i]],
                                              coords_space[[j]]))
      covmat_space[i, j] <- fields::Matern(dist_space, range = range,
                                           smoothness = smoothness)
    }
  }
  covmat_space <- covmat_space + t(covmat_space) - diag(n_space)

  covmat_time <- matrix(rep(0, n_time^2), nrow = n_time)
  for (i in 1:n_time) {
    for (j in i:n_time) {
      dist_time <- abs(coords_time[i] - coords_time[j])
      covmat_time[i, j] <- fields::Exponential(dist_time,
                                               aRange = aRange,
                                               theta = theta)
    }
  }
  covmat_time <- covmat_time + t(covmat_time) - diag(n_time)

  data_spatial <- as.vector(mvtnorm::rmvnorm(n = 1, sigma = covmat_space))
  data_time <- as.vector(mvtnorm::rmvnorm(n = 1, sigma = covmat_time))
  rep(data_spatial, times = n_time) + rep(data_time, each = n_space)
}


#' Create a covariance matrix corresponding to the spatiotemporal locations
#'
#' The chosen covariance function is separable. For spatial dependence structure
#' we use the Matern covariance function. For the temporal dependence structure
#' we use the exponential covariance function.
#'
#' @param coords An sftime object with locations and time but no fields.
#' @param range,smoothness,aRange,theta Parameters for Matern and Exponential
#'   covariance functions.
#'
#' @returns A Covariance matrix corresponding to spatiotemporal locations.
gen_cov_separable <- function(coords, range, smoothness, aRange, theta) {
  coords_space <- unique(coords$geometry)
  coords_time <- unique(coords$time)
  n_space <- length(coords_space)
  n_time <- length(coords_time)

  covmat_space <- matrix(rep(0, n_space^2), nrow = n_space)
  for (i in 1:n_space) {
    for (j in i:n_space) {
      dist_space <- as.double(sf::st_distance(coords_space[[i]],
                                              coords_space[[j]]))
      covmat_space[i, j] <- fields::Matern(dist_space, range = range,
                                           smoothness = smoothness)
    }
  }
  covmat_space <- covmat_space + t(covmat_space) - diag(n_space)

  covmat_time <- matrix(rep(0, n_time^2), nrow = n_time)
  for (i in 1:n_time) {
    for (j in i:n_time) {
      dist_time <- abs(coords_time[i] - coords_time[j])
      covmat_time[i, j] <- fields::Exponential(dist_time,
                                               aRange = aRange,
                                               theta = theta)
    }
  }
  covmat_time <- covmat_time + t(covmat_time) - diag(n_time)

  covmat_time %x% covmat_space
}


#' Compute segments for spatio-temporal coordinates
#'
#' @param coords An sftime object with locations and time but no fields
#' @param x_prop,y_prop,time_prop Division of the corresponding axis in terms of
#'   percentages. Percentages are given as a double vector.
#'
#' @returns Tibble of segments for different axes (3 columns).
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


#' Simulate field with separable covariance structure in segments
#'
#' @param coords An sftime object with locations and time but no fields.
#' @param x_prop,y_prop,time_prop Division of the corresponding axis in terms of
#'   percentages. Percentages are given as a double vector.
#' @param range,smoothness,Arange,theta Each parameter is a vector has length
#'   equal to the number of segments. These are parameters for Matern and
#'   Exponential covariance functions for each segment.
#'
#' @returns A Vector giving value of the field at each spatiotemporal
#'   coordinate.
gen_separable_blocks <- function(coords, x_prop, y_prop, time_prop, range,
                                 smoothness, aRange, theta) {
  segments <- compute_segments(coords, x_prop, y_prop, time_prop) %>%
    mutate(comb = interaction(x_segment, y_segment, time_segment)) %>%
    pull(comb)

  indices <- match(segments, levels(segments))

  coords_filter <- coords %>%
    mutate(segment = segments, index = indices, value = NA)

  for (i in 1:(length(x_prop) * length(y_prop) * length(time_prop))) {
    coords_i <- coords_filter %>%
      filter(index == i) %>%
      select(geometry, time)
    covmat <- gen_cov_separable(coords_i, range[i], smoothness[i], aRange[i],
                                theta[i])
    coords_filter$value[coords_filter$index == i] <-
      mvtnorm::rmvnorm(n = 1, sigma = covmat)
  }
  coords_filter$value
}


#' Simulate field that is nonstationary with respect to location.
#'
#' The location is different in each segment in spacetime. The field is
#' generatedby adding location to the zero mean field epsilon.
#'
#' @param coords An sftime object with locations and time but no fields.
#' @param epsilon A Vector giving value of a zero mean field at each
#' spatiotemporal coordinate.
#' @param mu The expected value for each segment.
#' @param x_prop,y_prop,time_prop Division of the corresponding axis in terms of
#'   percentages. Percentages are given as a double vector.
#'
#' @returns A Vector giving value of the field at each spatiotemporal
#'   coordinate.
gen_field_cluster <- function(coords, epsilon, mu, x_prop, y_prop, time_prop) {
  if (length(mu) != length(x_prop) * length(y_prop) * length(time_prop)) {
    rlang::abort("'mu' has wrong length.")
  }
  segments <- compute_segments(coords, x_prop, y_prop, time_prop) %>%
    mutate(comb = interaction(x_segment, y_segment, time_segment)) %>%
    pull(comb)

  indices <- match(segments, levels(segments))
  mu[indices] + epsilon
}
