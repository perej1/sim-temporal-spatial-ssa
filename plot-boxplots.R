# Have boxplots corresponding to scenarios.

library(ggplot2)
library(dplyr)

# Extract medians corresponding to cases random_eigenvect == TRUE
medians_random <- readr::read_csv("results/perf-quantiles.csv",
                                  col_types = "fiiifffliidddddd") %>%
  filter(random_eigenvect == TRUE) %>%
  filter((latent == "oscillating" & n_spatial == 500 & n_time == 1000) |
           (latent == "spacetime" & n_spatial == 50 & n_time == 100)) %>%
  select(latent, stationary_median, nonstationary_median) %>%
  tidyr::pivot_longer(!latent)

medians_random_osc <- medians_random %>%
  filter(latent == "oscillating")

medians_random_spacetime <- medians_random %>%
  filter(latent == "spacetime")


# Extract desired scenarios for plotting
args <- readr::read_csv("sim-args.csv", col_types = "fiiiffflii")

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

spacetime_data$segmentation <- factor(spacetime_data$segmentation,
                                      levels = seg_labels_spacetime,
                                      ordered = TRUE)


# Plotting
osc_plot <- osc_data %>%
  ggplot(aes(x = factor(segmentation), y = value, fill = factor(type))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = value, color = factor(name),
                 linetype = factor(name)),
             medians_random_osc,
             linewidth = 1.5) +
  xlab("Segmentation") +
  ylab("Performance") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major.y = element_line(colour = alpha("black", 0.2)),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(name = "Method",
                    values = c("stationary" = "skyblue",
                               "nonstationary" = "pink"),
                    labels = c("stationary" = "Stationary",
                               "nonstationary" = "Nonstationary")) +
  scale_x_discrete(labels = c("thirds" = "Thirds", "xconstant" = "Xconstant",
                              "yconstant" = "Yconstant")) +
  scale_color_manual(
    name = "Baseline",
    values = c("stationary_median" = "skyblue",
               "nonstationary_median" = "pink"),
    labels = c("stationary_median" = "Stationary",
               "nonstationary_median" = "Nonstationary")
  ) +
  scale_linetype_manual(
    name = "Baseline",
    values = c("stationary_median" = "dotted",
               "nonstationary_median" = "dashed"),
    labels = c("stationary_median" = "Stationary",
               "nonstationary_median" = "Nonstationary")
  ) +
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.6)))
ggsave("plots/oscillating.pdf", osc_plot)

spacetime_plot <- spacetime_data %>%
  ggplot(aes(x = factor(segmentation), y = value, fill = factor(type))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = value, color = factor(name),
                 linetype = factor(name)),
             medians_random_spacetime,
             linewidth = 1.5) +
  xlab("Segmentation") +
  ylab("Performance") +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        panel.grid.major.y = element_line(colour = alpha("black", 0.2)),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = c("stationary" = "skyblue",
                               "nonstationary" = "pink"),
                    labels = c("stationary" = "Stationary",
                               "nonstationary" = "Nonstationary"),
                    name = "Method") +
  scale_x_discrete(labels = c("thirds" = "Thirds",
                              "xconstant" = "Xconstant",
                              "yconstant" = "Yconstant",
                              "tconstant" = "Tconstant",
                              "halfs" = "Halfs",
                              "thirds" = "Thirds",
                              "quarters" = "Quarters",
                              "tenths" = "Tenths"),
  ) +
  scale_color_manual(
    name = "Baseline",
    values = c("stationary_median" = "skyblue",
               "nonstationary_median" = "pink"),
    labels = c("stationary_median" = "Stationary",
               "nonstationary_median" = "Nonstationary")
  ) +
  scale_linetype_manual(
    name = "Baseline",
    values = c("stationary_median" = "dotted",
               "nonstationary_median" = "dashed"),
    labels = c("stationary_median" = "Stationary",
               "nonstationary_median" = "Nonstationary")
  ) +
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.6)))
ggsave("plots/spacetime.pdf", spacetime_plot)
