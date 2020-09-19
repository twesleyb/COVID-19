#!/usr/bin/env bash

# title: 0_run-all.sh
# author: twab
# description: run the analysis:

./1_tidy-global-reports.R && \
	./2_tidy-US-reports.R && \
	./3_plot-US-states.R
