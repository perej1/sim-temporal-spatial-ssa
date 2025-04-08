library(tmap)
library(dplyr)
library(optparse)
library(ggplot2)


option_list <- list(
  make_option("--n_spat", type = "integer", default = 10,
              help = "Number of spatial locations at each time point"),
  make_option("--n_time", type = "integer", default = 10,
              help = "number of time points, indexing starts from 0"),
  make_option("--m", type = "integer", default = 2,
              help = "Number of repetitions per scenario"),
  make_option("--x_blocks", type = "character", default = "20:80",
              help = "Segmentation of x coordinate, string gives proportions of the segment lengths"),
  make_option("--y_blocks", type = "character", default = "33:33:34",
              help = "Segmentation of y coordinate, string gives proportions of the segment lengths"),
  make_option("--time_blocks", type = "character", default = "20:60:20",
              help = "Segmentation of time, string gives proportions of the segment lengths"),
  make_option("--area", type = "character", default = "box",
              help = "map type, must belong to c('italy', 'finland', 'box')"),
  make_option("--filename", type = "character", default = "example.pdf",
              help = "File name for the test figure")
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


#' Get country bounding box and polygon
#'
#' @param country Name of the country in lower case
#'
#' @returns List of the bounding box and polygon corresponding to the country
get_country <- function(country) {
  if (country == "finland") {
    name_polygon <- "Finland"
    name_box <- "FI"
  } else if (country == "italy") {
    name_polygon <- "Italy"
    name_box <- "IT"
  } else {
    rlang::abort("Invalid country. Country must be in c('finland', 'italy')")
  }

  sf_country <- rnaturalearth::ne_countries(returnclass = "sf",
                                            scale = "medium",
                                            country = name_polygon) %>%
    sf::st_transform(crs = 4326)

  bounds <- rjson::fromJSON(file = "data/bounding-boxes.json")[[name_box]][[2]]
  names(bounds) <- c("lon_min", "lat_min", "lon_max", "lat_max")
  list(bounding_box = bounds, sf_country = sf_country)
}


#' Generate uniform sample of spatial locations from a country
#'
#' Points that do not lie on the country polygon are filtered out. Thus, the
#'  effective sample size is <= n.
#'
#' @param n_spat Number of spatial locations
#' @param sf_country Country polygon
#' @param bounds Country bounding box
#'
#' @returns sf object with coordinates and 0 features
gen_unif_coords_country <- function(n_spat, sf_country, bounds) {

  lat <- stats::runif(n_spat, bounds["lat_min"], bounds["lat_max"])
  lon <- stats::runif(n_spat, bounds["lon_min"], bounds["lon_max"])

  tibble::tibble(lat = lat, lon = lon) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_filter(sf_country, .predicate = sf::st_within)
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


#' Generate sample of spatial locations from a distribution on a country polygon
#'
#' @param n_spat Number of spatial locations
#' @param gen_style_coords Function for generating samples
#' @param ... Optional parameters for the function gen_style_coords
#'
#' @returns sf object with coordinated but with 0 features
gen_coords <- function(n_spat = 100, gen_style_coords, ...) {
  coords <- gen_style_coords(n_spat, ...)
  while (nrow(coords) < n_spat) {
    coords <- rbind(coords, gen_style_coords(n_spat, ...))
  }
  coords %>%
    dplyr::slice(1:n_spat)
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
if (opt$area == "box") {
  coords <- gen_coords(opt$n_spat, gen_unif_coords_box)
} else if (opt$area %in% c("finland", "italy")) {
  country <- get_country(opt$area)
  coords <- gen_coords(opt$n_spat, gen_unif_coords_country, country$sf_country,
                       country$bounding_box)
} else {
  rlang::abort("Invalid area. Area must be in c('finland', 'italy', 'box')")
}

# Add temporal locations
time <- 0:(opt$n_time - 1)
coords <- coords %>%
  replicate(opt$n_time, ., simplify = FALSE) %>%
  bind_rows() %>%
  dplyr::mutate(time = rep(time, each = opt$n_spat)) %>%
  sftime::st_sftime(sf_column_name = "geometry", time_column_name = "time")

gen_mixing_matrix <- function(n_comp = 8) {
  u <- pracma::randortho(n_comp, type = "orthonormal")
  lambda <- diag(stats::runif(n_comp, -1, 1))
  u %*% lambda %*% t(u)
}


simulate <- function(i, coords, a) {
  # Compute latent components
  latent <- coords %>%
    mutate(s1 = gen_field_smooth_trend(., stationary = TRUE),
           s2 = gen_field_smooth_trend(., stationary = TRUE),
           s3 = gen_field_smooth_trend(., stationary = TRUE),
           s4 = gen_field_smooth_trend(., stationary = TRUE),
           s5 = gen_field_smooth_trend(., stationary = TRUE),
           s6 = gen_field_smooth_trend(., stationary = FALSE, 0.5, -2, 3),
           s7 = gen_field_smooth_trend(., stationary = FALSE, -2, 3, 0.5),
           s8 = gen_field_smooth_trend(., stationary = FALSE, 3, 0.5, -2)) %>%
    sftime::st_drop_time() %>%
    sf::st_drop_geometry()
  colnames(latent) <- paste0("f", 1:ncol(latent), "m", i)
  
  # Compute observed field
  observed <- latent %>%
    apply(1, function(x) a %*% matrix(x, ncol = 1)) %>%
    t() %>%
    as_tibble()
  colnames(observed) <- paste0("f", 1:ncol(observed), "m", i)
  
  # Compute whitened field
  mean_p <- colMeans(observed)
  cov_p <- cov(observed)
  
  whitened <- observed %>%
    sweep(2, mean_p, "-") %>%
    apply(1, function(x) sqrtmat_inv(cov_p) %*% matrix(x, ncol = 1)) %>%
    t() %>%
    as_tibble()
  colnames(whitened) <- paste0("f", 1:ncol(whitened), "m", i)
  
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
  if (opt$area == "box") {
    x_cuts <- cumsum(c(0, x_prop / 100))
    y_cuts <- cumsum(c(0, y_prop / 100))
  } else {
    x_len <- country$bounding_box["lat_max"] - country$bounding_box["lat_min"]
    y_len <- country$bounding_box["lon_max"] - country$bounding_box["lon_min"]
    x_min <- country$bounding_box["lat_min"]
    y_min <- country$bounding_box["lon_min"]
    
    x_cuts <- cumsum(c(x_min, x_prop / 100 * x_len))
    y_cuts <- cumsum(c(y_min, y_prop / 100 * y_len))
  }
  
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
  
  # Compute means and sample sizes in segments
  means_segment <- whitened_with_seg %>%
    summarise(across(starts_with("f"), ~ mean(.x)), .groups = "drop") %>%
    mutate(n = group_size(whitened_with_seg))

  list(latent = latent, observed = observed)
}


a <- gen_mixing_matrix(8)
res <- simulate(1, coords, a)
