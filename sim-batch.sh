#!/bin/bash

Rscript gen-spatial-points.R --n_spat 10 --n_time 4 --area box --seed 123 --filename box.pdf
Rscript gen-spatial-points.R --n_spat 10 --n_time 4 --area italy --seed 123 --filename italy.pdf
Rscript gen-spatial-points.R --n_spat 10 --n_time 4 --area finland --seed 123 --filename finland.pdf