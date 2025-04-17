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

First generate arguments by

```
Rscript gen-arg.R
```

After this, corresponding scenarios can be run with
```
bash sim-batch.sh
```
