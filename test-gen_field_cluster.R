library(testthat)
source("functions.R")


test_that("Means and variances of segments match theoretical ones", {
  tol <- 100
  n_spatial <- 1000
  n_time <- 100
  x_prop <- c(33, 33, 34)
  y_prop <- 100
  time_prop <- c(30, 70)
  coords <- gen_coords(n_spatial, n_time, 123)
  segments <- compute_segments(coords, x_prop, y_prop, time_prop) %>%
    mutate(comb = interaction(x_segment, y_segment, time_segment)) %>%
    pull(comb)
  
  indices <- match(segments, levels(segments))
  names_mu_sigma <- levels(segments)[indices]
  
  mu <- 1:6
  mu_for_segment <- mu[indices]
  names(mu_for_segment) <- names_mu_sigma
  
  sigma <- c(rep(1,3), rep(10,3))
  sigma_for_segment <- sigma[indices]
  names(sigma_for_segment) <- names_mu_sigma
  
  data <- gen_field_cluster(coords, mu, sigma, x_prop, y_prop, time_prop)
  names(data) <- names_mu_sigma
  
  for (i in unique(names(data))) {
    mu_i <- unname(mu_for_segment[names(mu_for_segment) == i][1])
    mean_i <- mean(data[names(data) == i])
    
    print(paste0(i, " mu: ", mu_i))
    print(paste0(i, " mean: ", mean_i))
    
    sigma_i <- unname(sigma_for_segment[names(sigma_for_segment) == i][1])
    std_i <- sqrt(var(data[names(data) == i]))
    
    print(paste0(i, " sigma: ", sigma_i))
    print(paste0(i, " std: ", std_i))
    
    expect_equal(mu_i, mean_i, tolerance = tol)
    expect_equal(sigma_i, std_i, tolerance = tol)
  }
})
