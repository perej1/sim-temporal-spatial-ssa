# Generate arguments
library(dplyr)
library(tidyr)
library(tibble)

seg <- c("100", "50:50", "33:33:34", "25:25:25:25",
         "10:10:10:10:10:10:10:10:10:10",
         "5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5")

# Arguments for latent == "oscillating" when using our method
osc_method_arg <- expand_grid(
  mu = "oscillating",
  epsilon = c("no_dep", "loc_time_ind", "separable_blocks"),
  n = tibble(n_spatial = c(50, 100), n_time = c(100, 300)),
  m = 100,
  segments = tibble(
    x_blocks = seg[3:1],
    y_blocks = seg[c(3, 1, 2)],
    time_blocks = seg[c(3, 2, 2)]
  ),
  random_eigenvect = FALSE,
  seed_spatial = 123,
  seed_sim = 321,
  seed_cov = 541
) %>%
  unnest(c(n, segments))

# Arguments for latent == "oscillating" when using "random" estimate for the
# unmixing matrix
osc_random_arg <- expand_grid(
  mu = "oscillating",
  epsilon = c("no_dep", "loc_time_ind", "separable_blocks"),
  n = tibble(n_spatial = c(50, 100), n_time = c(100, 300)),
  m = 100,
  x_blocks = "100",
  y_blocks = "100",
  time_blocks = "100",
  random_eigenvect = TRUE,
  seed_spatial = 123,
  seed_sim = 321,
  seed_cov = 541
) %>%
  unnest(n)

# Arguments for latent == "spacetime" when using our method
spacetime_method_arg <- expand_grid(
  mu = "spacetime",
  epsilon = c("no_dep", "loc_time_ind", "separable_blocks"),
  n_spatial = 50,
  n_time = 100,
  m = 100,
  segments = tibble(
    x_blocks = seg[c(1, 2, 2, 2, 3:6)],
    y_blocks = seg[c(2, 1, 2, 2, 3:6)],
    time_blocks = seg[c(2, 2, 1, 2, 3:6)]
  ),
  random_eigenvect = FALSE,
  seed_spatial = 123,
  seed_sim = 321,
  seed_cov = 541
) %>%
  unnest(segments)

# Arguments for latent == "spacetime" when using "random" estimate for the
# unmixing matrix
spacetime_random_arg <- expand_grid(
  mu = "spacetime",
  epsilon = c("no_dep", "loc_time_ind", "separable_blocks"),
  n_spatial = 50,
  n_time = 100,
  m = 100,
  x_blocks = "100",
  y_blocks = "100",
  time_blocks = "100",
  random_eigenvect = TRUE,
  seed_spatial = 123,
  seed_sim = 321,
  seed_cov = 541
)

arg <- bind_rows(osc_method_arg, osc_random_arg, spacetime_method_arg,
                 spacetime_random_arg)

arg_vector <- sprintf(stringr::str_c("simulate-setting.R --mu %s --epsilon %s ",
                                     "--n_spatial %d --n_time %d --m %d ",
                                     "--x_blocks %s --y_blocks %s ",
                                     "--time_blocks %s --random_eigenvect %s ",
                                     "--seed_spatial %d --seed_sim %d ",
                                     "--seed_cov %d"),
  arg$mu,
  arg$epsilon,
  arg$n_spatial,
  arg$n_time,
  arg$m,
  arg$x_blocks,
  arg$y_blocks,
  arg$time_blocks,
  arg$random_eigenvect,
  arg$seed_spatial,
  arg$seed_sim,
  arg$seed_cov
)

readr::write_lines(arg_vector, "sim-args.txt")
readr::write_csv(arg, "sim-args.csv")
