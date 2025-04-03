library(tmap)
library(dplyr)

# Desired country polygon
sf_italy <- 
  rnaturalearth::ne_countries(returnclass = "sf", scale = 'medium') %>% 
  dplyr::filter(name == "Italy") %>%
  sf::st_transform(crs = 4326)

# Bounding box for the chosen country
bounds_italy <- rjson::fromJSON(file = "data/bounding-boxes.json")$IT[[2]]
names(bounds_italy) <- c("lon_min", "lat_min", "lon_max", "lat_max")


#' Generate uniform sample of points from Italy
#'
#' @param n Number of sampled points
#'
#' @returns plot of uniform points in Italy
gen_unif_coords <- function(n = 100, sf_country, bounds) {
  
  lat <- stats::runif(n, bounds["lat_min"], bounds["lat_max"])
  lon <- stats::runif(n, bounds["lon_min"], bounds["lon_max"])
  
  data <- tibble::tibble(lat = lat, lon = lon) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  data %>%
    sf::st_filter(sf_country, .predicate = sf::st_within)
}

gen_coords <- function(n = 100, sf_country, bounds, gen_style_coords) {
  coords <- gen_style_coords(n, sf_country, bounds)
  while (nrow(coords) < n){
    coords <- rbind(coords, gen_style_coords(n, sf_country, bounds))
  }
  dplyr::slice(coords, 1:n)
}


n <- 20
coords <- gen_coords(n, sf_italy, bounds_italy, gen_unif_coords)
data <- coords %>%
  dplyr::mutate(a = 1:n)

tm_shape(sf_italy) +
  tm_polygons() +
  tm_shape(data) +
  tm_bubbles(col = "black", fill = "red", size = 0.5)
