library(tmap)
library(dplyr)


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
                                            scale = "medium") %>%
    dplyr::filter(name == name_polygon) %>%
    sf::st_transform(crs = 4326)

  bounds <- rjson::fromJSON(file = "data/bounding-boxes.json")[[name_box]][[2]]
  names(bounds) <- c("lon_min", "lat_min", "lon_max", "lat_max")
  list(bounding_box = bounds, sf_country = sf_country)
}


#' Generate uniform sample of points from a bounding box
#'
#' Points that do not lie on the country polygon are filtered out. Thus, the
#'  effective sample size is <= n.
#'
#' @param n Sample size
#' @param sf_country Country polygon
#' @param bounds Country bounding box
#'
#' @returns sf object with coordinated but with 0 features
gen_unif_coords_country <- function(n, sf_country, bounds) {

  lat <- stats::runif(n, bounds["lat_min"], bounds["lat_max"])
  lon <- stats::runif(n, bounds["lon_min"], bounds["lon_max"])

  data <- tibble::tibble(lat = lat, lon = lon) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  data %>%
    sf::st_filter(sf_country, .predicate = sf::st_within)
}


#' Generate uniform sample from a box
#'
#' @param n Sample size
#' @param left Left border
#' @param right Right border
#' @param low Lower border
#' @param up Upper border
#'
#' @returns sf object with coordinated but with 0 features
gen_unif_coords_box <- function(n, left = 0, right = 1, low = 0, up = 1) {
  x <- stats::runif(n, left, right)
  y <- stats::runif(n, low, up)
  tibble::tibble(x = x, y = y) %>%
    sf::st_as_sf(coords = c("x", "y"))
}


#' Generate sample from a distribution on a polygon
#'
#' @param n Sample size
#' @param gen_style_coords Function for generating samples
#' @param ... Optional parameters for the function gen_style_coords
#'
#' @returns sf object with coordinated but with 0 features
gen_coords <- function(n = 100, gen_style_coords, ...) {
  coords <- gen_style_coords(n, ...)
  while (nrow(coords) < n) {
    coords <- rbind(coords, gen_style_coords(n, ...))
  }
  dplyr::slice(coords, 1:n)
}


# Example with a country
n <- 20
country <- get_country("italy")

coords <- gen_coords(n, gen_unif_coords_country, country$sf_country,
                     country$bounding_box)
data <- coords %>%
  dplyr::mutate(a = 1:n)

tm_shape(country$sf_country) +
  tm_polygons() +
  tm_shape(data) +
  tm_bubbles(col = "black", fill = "red", size = 0.5)


# Example with a box
coords <- gen_coords(n, gen_unif_coords_box, -1, 1, -1, 1)
data <- coords %>%
  dplyr::mutate(a = 1:n)

plot(data)
