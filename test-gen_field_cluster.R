library(testthat)
source("functions.R")


test_that("Means and variances of segments match theoretical ones", {
  tol <- 100
  ns <- 1000
  nt <- 100
  x_prop <- c(33, 33, 34)
  y_prop <- 100
  time_prop <- c(30, 70)
  coords <- gen_coords(ns, nt, 123)
  segments <- compute_segments(coords, x_prop, y_prop, time_prop) %>%
    mutate(comb = interaction(x_segment, y_segment, time_segment)) %>%
    pull(comb)

  indices <- match(segments, levels(segments))
  names_mu <- levels(segments)[indices]

  mu <- 1:6
  mu_for_segment <- mu[indices]
  names(mu_for_segment) <- names_mu

  data <- gen_field_cluster(coords, stats::rnorm(nt * ns), mu, x_prop, y_prop,
                            time_prop)
  names(data) <- names_mu

  for (i in unique(names(data))) {
    mu_i <- unname(mu_for_segment[names(mu_for_segment) == i][1])
    mean_i <- mean(data[names(data) == i])

    print(paste0(i, " mu: ", mu_i))
    print(paste0(i, " mean: ", mean_i))

    expect_equal(mu_i, mean_i, tolerance = tol)
  }
})
