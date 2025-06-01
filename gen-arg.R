# Generate arguments
library(dplyr)
library(tidyr)
library(tibble)

seg <- c("100", "50:50", "33:33:34", "25:25:25:25",
         "10:10:10:10:10:10:10:10:10:10",
         "5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5")

# Arguments for latent == "oscillating" when using our method
osc_no_dep_method_arg <- tibble(
  mu = "oscillating",
  epsilon = "no_dep",
  n = list(
    tibble(n_spatial = 50, n_time = 100),
    tibble(n_spatial = 100, n_time = 300),
    tibble(n_spatial = 200, n_time = 500)
  ),
  m = 100,
  segments = list(
    tibble(x_blocks = seg[3], y_blocks = seg[3], time_blocks = seg[3]),
    tibble(x_blocks = seg[2], y_blocks = seg[1], time_blocks = seg[2]),
    tibble(x_blocks = seg[1], y_blocks = seg[2], time_blocks = seg[2])
  ),
  random_eigenvect = FALSE,
  seed_spatial = 123,
  seed_sim = 321
) %>%
  expand(mu, epsilon, n, m, segments, random_eigenvect, seed_spatial,
         seed_sim) %>%
  unnest(c(n, segments))

osc_loc_time_ind_method_arg <- tibble(
  mu = "oscillating",
  epsilon = "loc_time_ind",
  n = list(
    tibble(n_spatial = 50, n_time = 100),
    tibble(n_spatial = 100, n_time = 300),
    tibble(n_spatial = 200, n_time = 500)
  ),
  m = 100,
  segments = list(
    tibble(x_blocks = seg[3], y_blocks = seg[3], time_blocks = seg[3]),
    tibble(x_blocks = seg[2], y_blocks = seg[1], time_blocks = seg[2]),
    tibble(x_blocks = seg[1], y_blocks = seg[2], time_blocks = seg[2])
  ),
  random_eigenvect = FALSE,
  seed_spatial = 123,
  seed_sim = 321
) %>%
  expand(mu, epsilon, n, m, segments, random_eigenvect, seed_spatial,
         seed_sim) %>%
  unnest(c(n, segments))

# Arguments for latent == "oscillating" when using "random" estimate for the
# unmixing matrix
osc_no_dep_random_arg <- tibble(
  mu = "oscillating",
  epsilon = "no_dep",
  n = list(
    tibble(n_spatial = 50, n_time = 100),
    tibble(n_spatial = 100, n_time = 300),
    tibble(n_spatial = 200, n_time = 500)
  ),
  m = 100,
  segments = list(
    tibble(x_blocks = seg[1], y_blocks = seg[1], time_blocks = seg[1])
  ),
  random_eigenvect = TRUE,
  seed_spatial = 123,
  seed_sim = 321
) %>%
  expand(mu, epsilon, n, m, segments, random_eigenvect, seed_spatial,
         seed_sim) %>%
  unnest(c(n, segments))

osc_loc_time_ind_random_arg <- tibble(
  mu = "oscillating",
  epsilon = "loc_time_ind",
  n = list(
    tibble(n_spatial = 50, n_time = 100),
    tibble(n_spatial = 100, n_time = 300),
    tibble(n_spatial = 200, n_time = 500)
  ),
  m = 100,
  segments = list(
    tibble(x_blocks = seg[1], y_blocks = seg[1], time_blocks = seg[1])
  ),
  random_eigenvect = TRUE,
  seed_spatial = 123,
  seed_sim = 321
) %>%
  expand(mu, epsilon, n, m, segments, random_eigenvect, seed_spatial,
         seed_sim) %>%
  unnest(c(n, segments))

# Arguments for latent == "spacetime" when using our method
spacetime_no_dep_method_arg <- tibble(
  mu = "spacetime",
  epsilon = "no_dep",
  n = list(
    tibble(n_spatial = 50, n_time = 100)
  ),
  m = 100,
  segments = list(
    tibble(x_blocks = seg[1], y_blocks = seg[2], time_blocks = seg[2]),
    tibble(x_blocks = seg[2], y_blocks = seg[1], time_blocks = seg[2]),
    tibble(x_blocks = seg[2], y_blocks = seg[2], time_blocks = seg[1]),
    tibble(x_blocks = seg[2], y_blocks = seg[2], time_blocks = seg[2]),
    tibble(x_blocks = seg[3], y_blocks = seg[3], time_blocks = seg[3]),
    tibble(x_blocks = seg[4], y_blocks = seg[4], time_blocks = seg[4]),
    tibble(x_blocks = seg[5], y_blocks = seg[5], time_blocks = seg[5]),
    tibble(x_blocks = seg[6], y_blocks = seg[6], time_blocks = seg[5])
  ),
  random_eigenvect = FALSE,
  seed_spatial = 123,
  seed_sim = 321
) %>%
  expand(mu, epsilon, n, m, segments, random_eigenvect, seed_spatial,
         seed_sim) %>%
  unnest(c(n, segments))

spacetime_loc_time_ind_method_arg <- tibble(
  mu = "spacetime",
  epsilon = "loc_time_ind",
  n = list(
    tibble(n_spatial = 50, n_time = 100)
  ),
  m = 100,
  segments = list(
    tibble(x_blocks = seg[1], y_blocks = seg[2], time_blocks = seg[2]),
    tibble(x_blocks = seg[2], y_blocks = seg[1], time_blocks = seg[2]),
    tibble(x_blocks = seg[2], y_blocks = seg[2], time_blocks = seg[1]),
    tibble(x_blocks = seg[2], y_blocks = seg[2], time_blocks = seg[2]),
    tibble(x_blocks = seg[3], y_blocks = seg[3], time_blocks = seg[3]),
    tibble(x_blocks = seg[4], y_blocks = seg[4], time_blocks = seg[4]),
    tibble(x_blocks = seg[5], y_blocks = seg[5], time_blocks = seg[5]),
    tibble(x_blocks = seg[6], y_blocks = seg[6], time_blocks = seg[5])
  ),
  random_eigenvect = FALSE,
  seed_spatial = 123,
  seed_sim = 321
) %>%
  expand(mu, epsilon, n, m, segments, random_eigenvect, seed_spatial,
         seed_sim) %>%
  unnest(c(n, segments))

# Arguments for latent == "spacetime" when using "random" estimate for the
# unmixing matrix
spacetime_random_arg <- tibble(
  method = list(
    tibble(mu = "spacetime", epsilon = "loc_time_ind"),
    tibble(mu = "spacetime", epsilon = "no_dep")
  ),
  n = list(
    tibble(n_spatial = 50, n_time = 100)
  ),
  m = 100,
  segments = list(
    tibble(x_blocks = seg[1], y_blocks = seg[1], time_blocks = seg[1])
  ),
  random_eigenvect = TRUE,
  seed_spatial = 123,
  seed_sim = 321
) %>%
  expand(method, n, m, segments, random_eigenvect, seed_spatial, seed_sim) %>%
  unnest(c(method, n, segments))

arg <- bind_rows(osc_no_dep_method_arg, osc_loc_time_ind_method_arg,
                 osc_no_dep_random_arg, osc_loc_time_ind_random_arg,
                 spacetime_no_dep_method_arg, spacetime_loc_time_ind_method_arg,
                 spacetime_random_arg)

arg_vector <- sprintf(stringr::str_c("simulate-setting.R --mu %s --epsilon %s ",
                                     "--n_spatial %d --n_time %d --m %d ",
                                     "--x_blocks %s --y_blocks %s ",
                                     "--time_blocks %s --random_eigenvect %s ",
                                     "--seed_spatial %d --seed_sim %d"),
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
  arg$seed_sim
)

readr::write_lines(arg_vector, "sim-args.txt")
readr::write_csv(arg, "sim-args.csv")
