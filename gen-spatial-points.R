library(tmap)
library(dplyr)

# Global parameters
n_spat <- 5L
n_time <- 3L
country_name <- "italy"

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
    rlang::abort("Invalid country. Country has to be in c('finland', 'italy')")
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

  data <- tibble::tibble(lat = lat, lon = lon) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) # Original was sf

  data %>%
    sf::st_filter(sf_country, .predicate = sf::st_within)
}


#' Generate uniform sample of spatial locations from a rectangle
#'
#' @param n_spat Number of spatial locations
#' @param left Left border
#' @param right Right border
#' @param low Lower border
#' @param up Upper border
#'
#' @returns sf object with coordinated but with 0 features
gen_unif_coords_box <- function(n_spat, left = 0, right = 1, low = 0, up = 1) {
  x <- stats::runif(n_spat, left, right)
  y <- stats::runif(n_spat, low, up)
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


# Example with a country
country <- get_country(country_name)

t <- 0:(n_time - 1)

coords <- gen_coords(n_spat, gen_unif_coords_country, country$sf_country,
                     country$bounding_box) %>%
  replicate(n_time, ., simplify = FALSE) %>%
  bind_rows() %>%
  dplyr::mutate(time = rep(t, each = n_spat), a = seq_len(nrow(.))) %>%
  sftime::st_sftime(sf_column_name = "geometry", time_column_name = "time")

tm_shape(country$sf_country) +
  tm_polygons() +
  tm_shape(coords) +
  tm_facets(by = "time") +
  tm_bubbles(col = "black", fill = "a", size = 0.5)
