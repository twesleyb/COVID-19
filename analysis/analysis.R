#!/usr/bin/env Rscript

# Analysis of COVID-19 data.

#---------------------------------------------------------------------
## Set up the workspace.
#---------------------------------------------------------------------

# Imports.
suppressPackageStartupMessages({
	library(dplyr)
	library(TBmiscr)
	library(data.table)
})

# Directories.
root <- getrd()
datadir <- file.path(root,"csse_covid_19_data/csse_covid_19_daily_reports")

#---------------------------------------------------------------------
## Load the data.
#---------------------------------------------------------------------

# Load the data.
data_files <- list.files(datadir,pattern=".csv",full.names=TRUE)
names(data_files) <- tools::file_path_sans_ext(basename(data_files))
all_data <- lapply(data_files,fread)

# Clean up the column names.
fix_colnames <- function(x) { 
	# Replace "/" and " " with "_".
	colnames(x) <- gsub("/|\\ ","_",colnames(x))
	return(x)
}
all_data <- lapply(all_data,fix_colnames)

# Merge the data into a single tidy dt.
dt <- rbindlist(all_data,use.names=TRUE,fill=TRUE,idcol="ID")

#--------------------------------------------------------------------
## Look at the data.
#--------------------------------------------------------------------

# Let's glance at the data from the US.
subdt <- dt %>% filter(Country_Region == "US", Province_State == "New York")
subdt_summary <- subdt %>% group_by(ID) %>% summarize(Confirmed = sum(Confirmed),
						      Deaths = sum(Deaths),
						      Recovered = sum(Recovered))

print(subdt_summary)
