# Generate arguments

n_spatial <- c(10, 100)
n_time <- c(10, 100)
m <- 100
x_blocks <- c("33:33:34")
y_blocks <- c("33:33:34")
time_blocks <- c("33:33:34")
var <- c(FALSE, TRUE)
seed_spatial <- 123
seed_sim <- 321

arg <- expand.grid(
  n_spatial = n_spatial,
  n_time = n_time,
  m = m,
  x_blocks = x_blocks,
  y_blocks = y_blocks,
  time_blocks = time_blocks,
  var = var,
  seed_spatial = seed_spatial,
  seed_sim = seed_sim
)

arg_vector <- sprintf(paste0("simulate-setting.R --n_spatial %d --n_time %d ",
                             "--m %d --x_blocks %s --y_blocks %s ",
                             "--time_blocks %s --var %s --seed_spatial %d ",
                             "--seed_sim %d"),
  arg$n_spatial,
  arg$n_time,
  arg$m,
  arg$x_blocks,
  arg$y_blocks,
  arg$time_blocks,
  arg$var,
  arg$seed_spatial,
  arg$seed_sim
)


readr::write_lines(arg_vector, "sim-args.txt")
readr::write_csv(arg, "sim-args.csv")
