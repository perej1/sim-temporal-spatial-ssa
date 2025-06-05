# Generate arguments
library(dplyr)
library(tidyr)
library(tibble)


seg <- c("100", "50:50", "33:33:34", "25:25:25:25",
         "10:10:10:10:10:10:10:10:10:10")

# Arguments for latent == "oscillating" when using our method
osc_method_arg <- expand_grid(
  mu = "osc",
  epsilon = c("noise", "seplow", "sephigh"),
  ns = 100,
  nt = 300,
  m = 100,
  segments = tibble(
    xblocks = seg[3:1],
    yblocks = seg[c(3, 1, 2)],
    tblocks = seg[c(3, 2, 2)]
  ),
  random_eigen = FALSE,
  seed_spatial = 123,
  seed_sim = 321
) %>%
  unnest(segments)

# Arguments for latent == "oscillating" when using "random" estimate for the
# unmixing matrix
osc_random_arg <- expand_grid(
  mu = "osc",
  epsilon = c("noise", "seplow", "sephigh"),
  ns = 100,
  nt = 300,
  m = 100,
  xblocks = "100",
  yblocks = "100",
  tblocks = "100",
  random_eigen = TRUE,
  seed_spatial = 123,
  seed_sim = 321
)

# Arguments for latent == "spacetime" when using our method
spacetime_method_arg <- expand_grid(
  mu = c("xyt2", "xyt3", "xyt4"),
  epsilon = c("noise", "seplow", "sephigh"),
  ns = 50,
  nt = 100,
  m = 100,
  segments = tibble(
    xblocks = seg[c(1, 2, 2, 2, 3:5)],
    yblocks = seg[c(2, 1, 2, 2, 3:5)],
    tblocks = seg[c(2, 2, 1, 2, 3:5)]
  ),
  random_eigen = FALSE,
  seed_spatial = 123,
  seed_sim = 321,
) %>%
  unnest(segments)

# Arguments for latent == "spacetime" when using "random" estimate for the
# unmixing matrix
spacetime_random_arg <- expand_grid(
  mu = c("xyt2", "xyt3", "xyt4"),
  epsilon = c("noise", "seplow", "sephigh"),
  ns = 50,
  nt = 100,
  m = 100,
  xblocks = "100",
  yblocks = "100",
  tblocks = "100",
  random_eigen = TRUE,
  seed_spatial = 123,
  seed_sim = 321
)

arg <- bind_rows(osc_method_arg, osc_random_arg, spacetime_method_arg,
                 spacetime_random_arg)

arg_vector <- sprintf(stringr::str_c("simulate-setting.R --mu %s --epsilon %s ",
                                     "--ns %d --nt %d --m %d ",
                                     "--xblocks %s --yblocks %s ",
                                     "--tblocks %s --random_eigen %s ",
                                     "--seed_spatial %d --seed_sim %d "),
  arg$mu,
  arg$epsilon,
  arg$ns,
  arg$nt,
  arg$m,
  arg$xblocks,
  arg$yblocks,
  arg$tblocks,
  arg$random_eigen,
  arg$seed_spatial,
  arg$seed_sim
)

readr::write_lines(arg_vector, "sim-args.txt")
readr::write_csv(arg, "sim-args.csv")
