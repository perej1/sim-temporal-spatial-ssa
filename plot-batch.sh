#!/bin/bash

Rscript plot-boxplots.R --epsilon no_dep
Rscript plot-boxplots.R --epsilon loc_time_ind
Rscript plot-boxplots.R --epsilon separable_blocks
