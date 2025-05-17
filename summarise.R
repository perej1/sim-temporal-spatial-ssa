# Summarise results by plotting boxplots

library(optparse)
library(ggplot2)
suppressMessages(library(dplyr))


option_list <- list(
  make_option("--n_spatial", type = "character", default = "10:50",
              help = "Spatial sample sizes"),
  make_option("--n_time", type = "character", default = "50:10",
              help = "Temporal sample sizes")
  )
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

n_spatial_arg <- as.integer(stringr::str_split_1(opt$n_spatial, ":"))
n_time_arg <- as.integer(stringr::str_split_1(opt$n_time, ":"))

args <- readr::read_csv("sim-args.csv", col_types = "iiicccliiii")
str(args)

# Check that given arguments are valid
if (length(n_spatial_arg) != length(n_time_arg)) {
  rlang::abort("Number of sample sizes in `n_spatial` and `n_time` must be the same")
}

if (!all(n_spatial_arg %in% args$n_spatial & n_time_arg %in% args$n_time)) {
  rlang::abort("Invalid sample sizes.")
}

n <- tibble::tibble(n_spatial = n_spatial_arg, n_time = n_time_arg)

args_not_n <- args %>%
  select(-n_spatial, -n_time) %>%
  distinct()

for (i in seq_len(nrow(args_not_n))) {
  arg_i <- args_not_n[i, ]
  perf_list <- list()
  for (j in seq_len(nrow(n))) {
    filename <- paste0("n_spatial_", n[j, ]$n_spatial,
                       "_n_time_", n[j, ]$n_time,
                       "_m_", arg_i$m,
                       "_x_blocks_", arg_i$x_blocks,
                       "_y_blocks_", arg_i$y_blocks,
                       "_time_blocks_", arg_i$time_blocks,
                       "_include_var_nonstationary_", arg_i$include_var_nonstationary,
                       "_dim_", arg_i$dim,
                       "_dim_nonstationary_", arg_i$dim_nonstationary,
                       "_seed_spatial_", arg_i$seed_spatial,
                       "_seed_sim_", arg_i$seed_sim, ".csv")
    perf_list[[j]] <- read.csv(stringr::str_c("results/perf/", filename))
    perf_list[[j]] <- perf_list[[j]] %>%
      tibble::add_column(n_spatial_time = stringr::str_c(n[j, ],
                                                         collapse = ":"))
  }
  # Make long tibble for plotting
  perfs <- do.call(bind_rows, perf_list) %>%
    tidyr::pivot_longer(cols = !n_spatial_time, names_to = "type")
  
  # Plotting
  stationary <- perfs %>%
    filter(type == "stationary") %>%
    ggplot(aes(x = n_spatial_time, y = value)) +
    geom_boxplot(fill = "skyblue")
  stationary
  
  labels <- sprintf("n_spatial = %d\nn_time = %d", n$n_spatial, n$n_time)
  
  nonstationary <- perfs %>%
    filter(type == "nonstationary") %>%
    ggplot(aes(x = n_spatial_time, y = value)) +
    geom_boxplot(fill = "pink") +
    xlab("") +
    ylab("Performance") +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          panel.grid.major.y = element_line(colour = alpha("black", 0.2)),
          panel.grid.major.x = element_blank()) + 
    scale_x_discrete(labels = labels)
  
  stationary <- perfs %>%
    filter(type == "stationary") %>%
    ggplot(aes(x = n_spatial_time, y = value)) +
    geom_boxplot(fill = "pink") +
    xlab("") +
    ylab("Performance") +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          panel.grid.major.y = element_line(colour = alpha("black", 0.2)),
          panel.grid.major.x = element_blank()) + 
    scale_x_discrete(labels = labels)
  
  plotname <- paste0("n_spatial_", opt$n_spatial,
                     "_n_time_", opt$n_time,
                     "_m_", arg_i$m,
                     "_x_blocks_", arg_i$x_blocks,
                     "_y_blocks_", arg_i$y_blocks,
                     "_time_blocks_", arg_i$time_blocks,
                     "_include_var_nonstationary_", arg_i$include_var_nonstationary,
                     "_dim_", arg_i$dim,
                     "_dim_nonstationary_", arg_i$dim_nonstationary,
                     "_seed_spatial_", arg_i$seed_spatial,
                     "_seed_sim_", arg_i$seed_sim, ".png")
  
  ggsave(stringr::str_c("plots/nonstationary/", plotname), nonstationary)
  ggsave(stringr::str_c("plots/stationary/", plotname), stationary)
  
  nonstationary
  stationary
}

