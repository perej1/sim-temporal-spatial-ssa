#!/bin/bash

Rscript gen-spatial-points.R --n_spat 10 --n_time 10 --m 1 --x_blocks 33:33:34 --y_blocks 33:33:34 --time_blocks 33:33:34 --setting 1 --file results/test10.txt
Rscript gen-spatial-points.R --n_spat 100 --n_time 100 --m 1 --x_blocks 33:33:34 --y_blocks 33:33:34 --time_blocks 33:33:34 --setting 1 --file results/test100.txt
Rscript gen-spatial-points.R --n_spat 1000 --n_time 1000 --m 1 --x_blocks 33:33:34 --y_blocks 33:33:34 --time_blocks 33:33:34 --setting 1 --file results/test1000.txt