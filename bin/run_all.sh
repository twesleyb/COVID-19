#!/bin/bash

# Run the analysis.

# Pull any changes from upstream repo and tidy up the data.
../analysis/0_tidy-global-reports.R

# Get subset of the data from the United States, and tidy it up.
../analysis/1_tidy-US-reports.R

# Generate plots for US States and provinces.
../analysis/2_plot-US-states.R
