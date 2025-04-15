# Functions for the simulations
library(dplyr)

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


compute_segments <- function(coords,
                             x_prop = c(33,33,34),
                             y_prop = c(33,33,34),
                             time_prop = c(33,33,34)) {
  if (sum(x_prop) != 100 | sum(y_prop) != 100 | sum(time_prop) != 100) {
    rlang::abort("Sum of proportions must be 100.")
  }
  
  n_time <- sftime::st_time(coords) %>%
    unique() %>%
    max() + 1
  
  x_cuts <- c(0, cumsum(x_prop / 100))
  y_cuts <- c(0, cumsum(y_prop / 100))
  time_cuts <- c(1, cumsum(time_prop / 100 * n_time)) - 1
  
  coords %>%
    mutate(x_segment = cut(sf::st_coordinates(.)[, "X"], x_cuts,
                           include.lowest = TRUE),
           y_segment = cut(sf::st_coordinates(.)[, "Y"], y_cuts,
                           include.lowest = TRUE),
           time_segment = cut(time, time_cuts, include.lowest = TRUE)) %>%
    sftime::st_drop_time() %>%
    sf::st_drop_geometry()
}


gen_field_cluster <- function(coords,
                              mu = 1:27,
                              sigma = rep(0.01, 27),
                              x_prop = c(33,33,34),
                              y_prop = c(33,33,34),
                              time_prop = c(33,33,34)) {
  mu_sigma_len <- length(x_prop) * length(y_prop) * length(time_prop)
  if(length(mu) != mu_sigma_len | length(sigma) != mu_sigma_len) {
    rlang::abort("'mu' or 'sigma' has wrong length.")
  }
  segments <- compute_segments(coords, x_prop, y_prop, time_prop) %>%
    mutate(comb = interaction(x_segment, y_segment, time_segment)) %>%
    pull(comb)
  
  indices <- match(segments, levels(segments))
  mu_for_segment <- mu[indices]
  sigma_for_segment <- sigma[indices]
  
  stats::rnorm(nrow(coords), mu_for_segment, sigma_for_segment)
}
