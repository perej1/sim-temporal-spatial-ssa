# sim-temporal-spatial-ssa
Simulations for an article about (non)stationary subspace analysis for
spatio-temporal data. In the simulations we focus only on nonstationarity with
respect to mean.

## Requirements

1. Clone the repository
    ```
    git clone git@github.com:perej1/sim-temporal-spatial-ssa.git
    ```

2. Install required packages with the `R` command
    ```
    renv::restore()
    ```

## Running the Simulation

Below, the purpose of each script is explained:

- `functions.R` - Global functions
- `simulate-setting.R` - Perform a simulation scenario corresponding to selected
  parameters
- `gen-arg.R` - Generate arguments for the simulation scenarios
- `sim-batch.sh` - Run a batch of simulation scenarios with different parameters
- `compute-stats.R` - Compute 1st, 2nd and 3rd quartiles of the performance
  measure for different simulation scenarios
- `plot-boxplots.R` - Summarise results by plotting boxplots for a certain
  dependence structure.
- `plot-batch.sh` - Plot boxplots for corresponding to all dependence strucures.
- `test-gen_field_cluster.R` - Unit test for the function `gen_field_cluster()`

Generate arguments by

```
Rscript gen-arg.R
```

Also, make directories `results/eigen` and `results/perf` by
```
mkdir results
cd results
mkdir eigen
mkdir perf
```

After this, scenarios given by `gen-arg.R` can be run with (in the root
directory of the project)
```
bash sim-batch.sh
```
Simulation results corresponding to each scenario are saved in the directory
`results/`.


For plotting, create the directory `plots/`. Then, plots can be produced with
```
bash plot-batch.sh
```
