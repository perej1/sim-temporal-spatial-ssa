# Have boxplots corresponding to scenarios.

library(ggplot2)
library(dplyr)

args <- readr::read_csv("sim-args.csv", col_types = "fiiiffflii")

# Extract desired scenarios for plotting
seg_labels_osc <- c("thirds", "yconstant", "xconstant")
osc_data <- args %>%
  filter(latent == "oscillating", random_eigenvect == FALSE, n_spatial == 500,
         n_time == 1000) %>%
  {
    stringr::str_c(
      "latent_", .$latent,
      "_n_spatial_", .$n_spatial,
      "_n_time_", .$n_time,
      "_m_", .$m,
      "_x_blocks_", .$x_blocks,
      "_y_blocks_", .$y_blocks,
      "_time_blocks_", .$time_blocks,
      "_random_eigenvect_", .$random_eigenvect,
      "_seed_spatial_", .$seed_spatial,
      "_seed_sim_", .$seed_sim,
      ".csv"
    )
  } %>%
  purrr::map(\(x) {
    readr::read_csv(stringr::str_c("results/perf/", x), col_types = "dd")
  }) %>%
  do.call(bind_rows, .) %>%
  tibble::add_column(segmentation = rep(seg_labels_osc, each = 100)) %>%
  tidyr::pivot_longer(cols = !segmentation, names_to = "type")

seg_labels_spacetime <- c("xconstant", "yconstant", "tconstant", "halfs",
                          "thirds", "quarters", "tenths")
spacetime_data <- args %>%
  filter(latent == "spacetime", random_eigenvect == FALSE,
         x_blocks != "5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5:5") %>%
  {
    stringr::str_c(
      "latent_", .$latent,
      "_n_spatial_", .$n_spatial,
      "_n_time_", .$n_time,
      "_m_", .$m,
      "_x_blocks_", .$x_blocks,
      "_y_blocks_", .$y_blocks,
      "_time_blocks_", .$time_blocks,
      "_random_eigenvect_", .$random_eigenvect,
      "_seed_spatial_", .$seed_spatial,
      "_seed_sim_", .$seed_sim,
      ".csv"
    )
  } %>%
  purrr::map(\(x) {
    readr::read_csv(stringr::str_c("results/perf/", x), col_types = "dd")
  }) %>%
  do.call(bind_rows, .) %>%
  tibble::add_column(segmentation = rep(seg_labels_spacetime, each = 100)) %>%
  tidyr::pivot_longer(cols = !segmentation, names_to = "type")

# Plotting
osc_plot <- osc_data %>%
  ggplot(aes(x = factor(segmentation), y = value, fill = factor(type))) +
  geom_boxplot() +
  xlab("Segmentation") +
  ylab("Performance") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major.y = element_line(colour = alpha("black", 0.2)),
        panel.grid.major.x = element_blank())
ggsave("plots/oscillating.png", osc_plot)

spacetime_plot <- spacetime_data %>%
  ggplot(aes(x = factor(segmentation), y = value, fill = factor(type))) +
  geom_boxplot() +
  xlab("Segmentation") +
  ylab("Performance") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major.y = element_line(colour = alpha("black", 0.2)),
        panel.grid.major.x = element_blank())
ggsave("plots/spacetime.png", spacetime_plot)
