# Have boxplots corresponding to scenarios.
library(ggplot2)
library(dplyr)
library(optparse)

option_list <- list(
  make_option("--epsilon", type = "character", default = "no_dep",
              help = stringr::str_c("For different options the latent field ",
                                    "has different dependence structure"))
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Extract medians corresponding to cases random_eigenvect == TRUE
medians_random <- readr::read_csv("results/perf-quantiles.csv",
                                  col_types = "cciiicccliiidddddd") %>%
  filter(epsilon == opt$epsilon) %>%
  filter(random_eigenvect == TRUE) %>%
  filter((mu == "oscillating" & n_spatial == 100 & n_time == 300) |
           (mu == "spacetime" & n_spatial == 50 & n_time == 100)) %>%
  select(mu, stationary_median, nonstationary_median) %>%
  tidyr::pivot_longer(!mu)

medians_random_osc <- medians_random %>%
  filter(mu == "oscillating")

medians_random_spacetime <- medians_random %>%
  filter(mu == "spacetime")


# Extract desired scenarios for plotting
args <- readr::read_csv("sim-args.csv", col_types = "ffiiifffliii")

seg_labels_osc <- c("thirds", "yconstant", "xconstant")
osc_data <- args %>%
  filter(mu == "oscillating", epsilon == opt$epsilon, random_eigenvect == FALSE,
         n_spatial == 100, n_time == 300) %>%
  {
    stringr::str_c(
      "mu_", .$mu,
      "_epsilon_", .$epsilon,
      "_n_spatial_", .$n_spatial,
      "_n_time_", .$n_time,
      "_m_", .$m,
      "_x_blocks_", .$x_blocks,
      "_y_blocks_", .$y_blocks,
      "_time_blocks_", .$time_blocks,
      "_random_eigenvect_", .$random_eigenvect,
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
  filter(mu == "spacetime", epsilon == opt$epsilon,
         random_eigenvect == FALSE) %>%
  {
    stringr::str_c(
      "mu_", .$mu,
      "_epsilon_", .$epsilon,
      "_n_spatial_", .$n_spatial,
      "_n_time_", .$n_time,
      "_m_", .$m,
      "_x_blocks_", .$x_blocks,
      "_y_blocks_", .$y_blocks,
      "_time_blocks_", .$time_blocks,
      "_random_eigenvect_", .$random_eigenvect,
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
ggsave(stringr::str_c("plots/","oscillating_", opt$epsilon, ".pdf"), osc_plot)

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
ggsave(stringr::str_c("plots/","spacetime_", opt$epsilon, ".pdf"),
       spacetime_plot)
