# Compute
# 1. median, 1st quartile and 3rd quartile of the performance measure for
# stationary and nonstationary subspaces, and
# 2. componentwise mean of eigenvalues for each simulation setting.

library(dplyr)

args <- readr::read_csv("sim-args.csv", col_types = "cciiiccclii")

# Initialize tibbles for statistics
perf_quantiles <- tibble::tibble(
  stationary_lower_quartile = rep(NA, nrow(args)),
  stationary_median = NA,
  stationary_upper_quartile = NA,
  nonstationary_lower_quartile = NA,
  nonstationary_median = NA,
  nonstationary_upper_quartile = NA
)

lambda_avg_osc <- tibble(
  f1 = rep(NA, nrow(args)),
  f2 = NA,
  f3 = NA,
  f4 = NA
)

lambda_avg_xyt2 <- tibble(
  f1 = rep(NA, nrow(args)),
  f2 = NA,
  f3 = NA,
  f4 = NA
)

lambda_avg_xyt3 <- tibble(
  f1 = rep(NA, nrow(args)),
  f2 = NA,
  f3 = NA,
  f4 = NA,
  f5 = NA,
  f6 = NA
)

lambda_avg_xyt4 <- tibble(
  f1 = rep(NA, nrow(args)),
  f2 = NA,
  f3 = NA,
  f4 = NA,
  f5 = NA,
  f6 = NA,
  f7 = NA,
  f8 = NA
)


# Compute statistics for each setting
for (i in seq_len(nrow(args))) {

  arg <- args[i, ]
  filename <- stringr::str_c(
    "mu_", arg$mu,
    "_epsilon_", arg$epsilon,
    "_ns_", arg$ns,
    "_nt_", arg$nt,
    "_m_", arg$m,
    "_xblocks_", arg$xblocks,
    "_yblocks_", arg$yblocks,
    "_tblocks_", arg$tblocks,
    "_random_eigen_", arg$random_eigen, ".csv"
  )

  # Compute 1st, 2nd and 3rd quartiles for performance
  q_i <- readr::read_csv(stringr::str_c("results/perf/", filename),
                         col_types = "dd") %>%
    apply(2, quantile, probs = c(0.25, 0.5, 0.75)) %>%
    tibble::as_tibble()

  perf_quantiles[i, ] <- tibble::tibble(
    stationary_lower_quartile = q_i$stationary[1],
    stationary_median = q_i$stationary[2],
    stationary_upper_quartile = q_i$stationary[3],
    nonstationary_lower_quartile = q_i$nonstationary[1],
    nonstationary_median = q_i$nonstationary[2],
    nonstationary_upper_quartile = q_i$nonstationary[3]
  )

  # Compute componentwise average eigenvalue
  path <- "results/eigen/"
  if (arg$mu == "osc") {
    lambda_avg_osc[i, ] <- readr::read_csv(stringr::str_c(path, filename),
                                           col_types = "dddd") %>%
      colMeans() %>%
      as.list() %>%
      tibble::as_tibble()
  } else if (arg$mu == "xyt2") {
    lambda_avg_xyt2[i, ] <- readr::read_csv(stringr::str_c(path, filename),
                                            col_types = "dddd") %>%
      colMeans() %>%
      as.list() %>%
      tibble::as_tibble()
  } else if (arg$mu == "xyt3") {
    lambda_avg_xyt3[i, ] <- readr::read_csv(stringr::str_c(path, filename),
                                            col_types = "dddddd") %>%
      colMeans() %>%
      as.list() %>%
      tibble::as_tibble()
  } else if (arg$mu == "xyt4") {
    lambda_avg_xyt4[i, ] <- readr::read_csv(stringr::str_c(path, filename),
                                            col_types = "dddddddd") %>%
      colMeans() %>%
      as.list() %>%
      tibble::as_tibble()
  }
}

bind_cols(args, perf_quantiles) %>%
  readr::write_csv("results/perf-quantiles.csv")

bind_cols(args, lambda_avg_osc) %>%
  tidyr::drop_na() %>%
  readr::write_csv("results/lambda-avg-osc.csv")

bind_cols(args, lambda_avg_xyt2) %>%
  tidyr::drop_na() %>%
  readr::write_csv("results/lambda-avg-xyt2.csv")

bind_cols(args, lambda_avg_xyt3) %>%
  tidyr::drop_na() %>%
  readr::write_csv("results/lambda-avg-xyt3.csv")

bind_cols(args, lambda_avg_xyt4) %>%
  tidyr::drop_na() %>%
  readr::write_csv("results/lambda-avg-xyt4.csv")
