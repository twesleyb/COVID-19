#!/usr/bin/env Rscript

# Plot COVID cases

## User parameters to change:

## Input:
# * root/data/Global_COVID-19_Cases.csv

## Output:

#---------------------------------------------------------------------
## Set up the workspace.
#---------------------------------------------------------------------

here <- getwd()
root <- dirname(here)
renv::load(root,quiet=TRUE)

# Imports.
suppressPackageStartupMessages({
	library(grid)
	library(dplyr)
	library(gtable)
	library(TBmiscr)
	library(ggplot2)
	library(gridExtra)
	library(data.table)
})

# To install miscellaneous R functions from TBmicr:
#devtools::install_github("twesleyb/TBmiscr")

# Directories.
funcdir <- file.path(root,"R")
figsdir <- file.path(root,"figs")
datadir <- file.path(root,"data")

# Load any functions in R/
load_all()

# Load the data.
myfile <- file.path(datadir,"Global_COVID-19_Cases.csv")
dt_covid <- fread(myfile)

plot <- plot_covid_cases(dt_covid, country_region="Italy", 
			 province_state = NULL, category = "Deaths",
			 log=TRUE)
