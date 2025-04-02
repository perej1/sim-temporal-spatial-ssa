library(tmap)
library(dplyr)

#' Generate uniform sample of points from Italy
#'
#' @param n Number of sampled points
#'
#' @returns plot of uniform points in Italy
gen_unif_sample_ita <- function(n = 100) {
  sf_italy <- 
    rnaturalearth::ne_countries(returnclass = "sf", scale = 'medium') %>% 
    dplyr::filter(name == "Italy") %>%
    sf::st_transform(crs = 4326)
  
  bounds <- rjson::fromJSON(file = "data/bounding-boxes.json")$IT[[2]]
  names(bounds) <- c("lon_min", "lat_min", "lon_max", "lat_max")
  
  lat <- stats::runif(n, bounds["lat_min"], bounds["lat_max"])
  lon <- stats::runif(n, bounds["lon_min"], bounds["lon_max"])
  
  data <- tibble::tibble(lat = lat, lon = lon, a = 1:n) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  data_it <- data %>%
    sf::st_filter(sf_italy, .predicate = sf::st_within)
  
  tm_shape(sf_italy) +
    tm_polygons() +
    tm_shape(data_it) +
    tm_bubbles(col = "a", scale = 0.5, title.col = "a")
}

gen_unif_sample_ita(100)
