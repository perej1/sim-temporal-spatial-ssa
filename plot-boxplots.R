# Have boxplots corresponding to scenarios.
library(ggplot2)
library(dplyr)
library(optparse)

option_list <- list(
  make_option("--epsilon", type = "character", default = "noise",
              help = stringr::str_c("For different options the latent field ",
                                    "has different dependence structure"))
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Extract medians corresponding to cases random_eigenvect == TRUE
medians_random <- readr::read_csv("results/perf-quantiles.csv",
                                  col_types = "cciiicccliidddddd") %>%
  filter(epsilon == opt$epsilon) %>%
  filter(random_eigen == TRUE) %>%
  select(mu, stationary_median, nonstationary_median) %>%
  tidyr::pivot_longer(!mu)

medians_random_osc <- medians_random %>%
  filter(mu == "osc")

medians_random_xyt2 <- medians_random %>%
  filter(mu == "xyt2")

medians_random_xyt3 <- medians_random %>%
  filter(mu == "xyt3")

medians_random_xyt4 <- medians_random %>%
  filter(mu == "xyt4")

# Extract desired scenarios for plotting
args <- readr::read_csv("sim-args.csv", col_types = "ffiiiffflii")

seg_labels_osc <- c("thirds" = "A1", "yconstant" = "A2", "xconstant" = "A3")
osc_data <- args %>%
  filter(mu == "osc", epsilon == opt$epsilon, random_eigen == FALSE) %>%
  {
    stringr::str_c(
      "mu_", .$mu,
      "_epsilon_", .$epsilon,
      "_ns_", .$ns,
      "_nt_", .$nt,
      "_m_", .$m,
      "_xblocks_", .$xblocks,
      "_yblocks_", .$yblocks,
      "_tblocks_", .$tblocks,
      "_random_eigen_", .$random_eigen,
      ".csv"
    )
  } %>%
  purrr::map(\(x) {
    readr::read_csv(stringr::str_c("results/perf/", x), col_types = "dd")
  }) %>%
  do.call(bind_rows, .) %>%
  tibble::add_column(segmentation = rep(seg_labels_osc, each = 100)) %>%
  tidyr::pivot_longer(cols = !segmentation, names_to = "type")

seg_labels_xyt <- c("xconstant" = "B1", "yconstant" = "B2", "tconstant" = "B3",
                    "halfs" = "B4", "thirds" = "B5", "quarters" = "B6",
                    "tenths" = "B7")
xyt2_data <- args %>%
  filter(mu == "xyt2", epsilon == opt$epsilon,
         random_eigen == FALSE) %>%
  {
    stringr::str_c(
      "mu_", .$mu,
      "_epsilon_", .$epsilon,
      "_ns_", .$ns,
      "_nt_", .$nt,
      "_m_", .$m,
      "_xblocks_", .$xblocks,
      "_yblocks_", .$yblocks,
      "_tblocks_", .$tblocks,
      "_random_eigen_", .$random_eigen,
      ".csv"
    )
  } %>%
  purrr::map(\(x) {
    readr::read_csv(stringr::str_c("results/perf/", x), col_types = "dd")
  }) %>%
  do.call(bind_rows, .) %>%
  tibble::add_column(segmentation = rep(seg_labels_xyt, each = 100)) %>%
  tidyr::pivot_longer(cols = !segmentation, names_to = "type")

xyt2_data$segmentation <- factor(xyt2_data$segmentation,
                                 levels = seg_labels_xyt,
                                 ordered = TRUE)

xyt3_data <- args %>%
  filter(mu == "xyt3", epsilon == opt$epsilon,
         random_eigen == FALSE) %>%
  {
    stringr::str_c(
      "mu_", .$mu,
      "_epsilon_", .$epsilon,
      "_ns_", .$ns,
      "_nt_", .$nt,
      "_m_", .$m,
      "_xblocks_", .$xblocks,
      "_yblocks_", .$yblocks,
      "_tblocks_", .$tblocks,
      "_random_eigen_", .$random_eigen,
      ".csv"
    )
  } %>%
  purrr::map(\(x) {
    readr::read_csv(stringr::str_c("results/perf/", x), col_types = "dd")
  }) %>%
  do.call(bind_rows, .) %>%
  tibble::add_column(segmentation = rep(seg_labels_xyt, each = 100)) %>%
  tidyr::pivot_longer(cols = !segmentation, names_to = "type")

xyt3_data$segmentation <- factor(xyt3_data$segmentation,
                                 levels = seg_labels_xyt,
                                 ordered = TRUE)


xyt4_data <- args %>%
  filter(mu == "xyt4", epsilon == opt$epsilon,
         random_eigen == FALSE) %>%
  {
    stringr::str_c(
      "mu_", .$mu,
      "_epsilon_", .$epsilon,
      "_ns_", .$ns,
      "_nt_", .$nt,
      "_m_", .$m,
      "_xblocks_", .$xblocks,
      "_yblocks_", .$yblocks,
      "_tblocks_", .$tblocks,
      "_random_eigen_", .$random_eigen,
      ".csv"
    )
  } %>%
  purrr::map(\(x) {
    readr::read_csv(stringr::str_c("results/perf/", x), col_types = "dd")
  }) %>%
  do.call(bind_rows, .) %>%
  tibble::add_column(segmentation = rep(seg_labels_xyt, each = 100)) %>%
  tidyr::pivot_longer(cols = !segmentation, names_to = "type")

xyt4_data$segmentation <- factor(xyt4_data$segmentation,
                                 levels = seg_labels_xyt,
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
ggsave(stringr::str_c("plots/", "osc_", opt$epsilon, ".pdf"), osc_plot,
       dpi = 1000)

xyt2_plot <- xyt2_data %>%
  ggplot(aes(x = factor(segmentation), y = value, fill = factor(type))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = value, color = factor(name),
                 linetype = factor(name)),
             medians_random_xyt2,
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
ggsave(stringr::str_c("plots/", "xyt2_", opt$epsilon, ".pdf"), xyt2_plot,
       dpi = 1000)

xyt3_plot <- xyt3_data %>%
  ggplot(aes(x = factor(segmentation), y = value, fill = factor(type))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = value, color = factor(name),
                 linetype = factor(name)),
             medians_random_xyt3,
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
ggsave(stringr::str_c("plots/", "xyt3_", opt$epsilon, ".pdf"), xyt3_plot,
       dpi = 1000)

xyt4_plot <- xyt4_data %>%
  ggplot(aes(x = factor(segmentation), y = value, fill = factor(type))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = value, color = factor(name),
                 linetype = factor(name)),
             medians_random_xyt4,
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
ggsave(stringr::str_c("plots/", "xyt4_", opt$epsilon, ".pdf"), xyt4_plot,
       dpi = 1000)
