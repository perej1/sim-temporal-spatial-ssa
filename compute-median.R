# Compute median, 1st quartile and 3rd quartile of performance measure for
# stationary and nonstationary subspaces
library(dplyr)

args <- readr::read_csv("sim-args.csv", col_types = "ciiiccclii")
quantiles <- tibble::tibble(stationary_lower_quartile = rep(NA, nrow(args)),
                            stationary_median = NA,
                            stationary_upper_quartile = NA,
                            nonstationary_lower_quartile = NA,
                            nonstationary_median = NA,
                            nonstationary_upper_quartile = NA)

for (i in seq_len(nrow(args))) {
  arg <- args[i, ]
  filename <- filename <- stringr::str_c(
    "latent_", arg$latent,
    "_n_spatial_", arg$n_spatial,
    "_n_time_", arg$n_time,
    "_m_", arg$m,
    "_x_blocks_", arg$x_blocks,
    "_y_blocks_", arg$y_blocks,
    "_time_blocks_", arg$time_blocks,
    "_random_eigenvect_", arg$random_eigenvect,
    "_seed_spatial_", arg$seed_spatial,
    "_seed_sim_", arg$seed_sim, ".csv"
  )
  q_i <- read.csv(stringr::str_c("results/perf/", filename)) %>%
    apply(2, quantile, probs = c(0.25, 0.5, 0.75)) %>%
    tibble::as_tibble()
  quantiles[i, ] <- tibble::tibble(
    stationary_lower_quartile = q_i$stationary[1],
    stationary_median = q_i$stationary[2],
    stationary_upper_quartile = q_i$stationary[3],
    nonstationary_lower_quartile = q_i$nonstationary[1],
    nonstationary_median = q_i$nonstationary[2],
    nonstationary_upper_quartile = q_i$nonstationary[3]
  )
}

bind_cols(args, quantiles) %>%
  readr::write_csv("results/perf-quantiles.csv")
