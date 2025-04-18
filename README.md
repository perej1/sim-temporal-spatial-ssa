# sim-temporal-spatial-ssa
Simulations for an article about (non)stationary subspace analysis for temporal spatial data.

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

First, the purpose of each script is explained:

- `simulate-setting.R` - Perform simulation for a selected setting
- `sim-batch.sh` - Run a batch of simulations settings with different
  parameters
- `functions.R` - Global functions
- `gen-arg.R` - Generate arguments for simulation settings
- `test-gen_field_cluster.R` - Unit test for the function `gen_field_cluster()`

Then, generate arguments by

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

After this, corresponding scenarios can be run with (in the root directory of
the project)
```
bash sim-batch.sh
```

Simulation results corresponding to each scenario are saved in the directory
`results/`.
