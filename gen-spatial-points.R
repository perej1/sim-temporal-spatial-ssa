library(tmap)
library(dplyr)
library(optparse)
library(ggplot2)


option_list <- list(
  make_option("--n_spat", type = "integer", default = 5,
              help = "Number of spatial locations at each time point"),
  make_option("--n_time", type = "integer", default = 3,
              help = "number of time points, indexing starts from 0"),
  make_option("--area", type = "character", default = "box",
              help = "map type, must belong to c('italy', 'finland', 'box')"),
  make_option("--seed", type = "integer", default = 123,
              help = "Seed for spatial locations"),
  make_option("--filename", type = "character", default = "example.pdf",
              help = "File name for the test figure")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
set.seed(opt$seed)


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

  data <- tibble::tibble(lat = lat, lon = lon) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

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

if (opt$area == "box") {
  coords <- gen_coords(opt$n_spat, gen_unif_coords_box)
} else if (opt$area %in% c("finland", "italy")) {
  country <- get_country(opt$area)
  coords <- gen_coords(opt$n_spat, gen_unif_coords_country, country$sf_country,
                       country$bounding_box)
} else {
  rlang::abort("Invalid area. Area must be in c('finland', 'italy', 'box')")
}


# Compute spatio-temporal locations
t <- 0:(opt$n_time - 1)
coords <- coords %>%
  replicate(opt$n_time, ., simplify = FALSE) %>%
  bind_rows() %>%
  dplyr::mutate(time = rep(t, each = opt$n_spat),
                a = rep(1:opt$n_time, each = opt$n_spat)) %>%
  sftime::st_sftime(sf_column_name = "geometry", time_column_name = "time")

if (opt$area == "box") {
  g <- ggplot() +
    geom_sf(data = coords, aes(color = a)) +
    facet_wrap(vars(time)) +
    xlim(0, 1) + ylim(0, 1)
  ggsave(paste0("figures/", opt$filename), g)
} else {
  g <- tm_shape(country$sf_country) +
    tm_polygons() +
    tm_shape(coords) +
    tm_facets(by = "time") +
    tm_bubbles(col = "black", fill = "a", size = 0.5)
  tmap_save(g, paste0("figures/", opt$filename))
}
