#!/usr/bin/env Rscript

# Analysis of COVID-19 data.

#---------------------------------------------------------------------
## Set up the workspace.
#---------------------------------------------------------------------

# Imports.
suppressPackageStartupMessages({
	library(dplyr)
	library(TBmiscr)
	library(ggplot2)
	library(data.table)
	library(gtable)
	library(grid)
	library(gridExtra)
})

# Insure we have installed misc functions.
#devtools::install_github("twesleyb/TBmiscr")

# Directories.
root <- getrd()
datadir <- file.path(root,"csse_covid_19_data/csse_covid_19_daily_reports")
figsdir <- file.path(root,"figs")
funcdir <- file.path(root,"R")

# Load any functions in R/
invisible({ sapply(list.files(funcdir,pattern="*\\.R",full.names=TRUE),source) })

#---------------------------------------------------------------------
## Load the data.
#---------------------------------------------------------------------

# Make sure the repo is up to date with master.
# Assumes you have already added master repo as upstream.
pull <- "git pull upstream master"
status <- system(pull,intern=TRUE)

# Load the data.
data_files <- list.files(datadir,pattern=".csv",full.names=TRUE)
names(data_files) <- tools::file_path_sans_ext(basename(data_files))
all_data <- lapply(data_files,fread)

# Define a function that cleans up the column names.
fix_colnames <- function(x) { 
	# Replace "/" and " " with "_".
	colnames(x) <- gsub("/|\\ ","_",colnames(x))
	return(x)
}
all_data <- lapply(all_data,fix_colnames)

# Merge the data into a single tidy dt.
dt_covid <- rbindlist(all_data,use.names=TRUE,fill=TRUE,idcol="Date")

#--------------------------------------------------------------------
## Examine mortality rates for US states.
#--------------------------------------------------------------------

# Let's glance at the data from a US state.
country = "US"
state = "North Carolina"
plot <- plot_mortality_rate(dt_covid, country, state)
plot

#------------------------------------------------------------------------------
## Look at all US states.
#------------------------------------------------------------------------------
# Clean up the data from the United States.

# Get all data for the United States.
# Checked: United States is not in the data.
dt_US <- dt_covid %>% filter(Country_Region=="US")

# We can utilize the state dataset included in base R.
data(state)
states <- state.name
names(states) <- state.abb

# States does not include Washington D.C., because it it technically isn't a
# state, but we will add it.
states <- c(states,DC="Washington, D.C.")

# Check: are all US states in the data?
is_state <- dt_US$Province_State %in% states
all(is_state)
# Nope, so we need to figure out where/what these other places are.

# How many don't match? 
dt_states <- unique(dt_US$Province_State)
not_a_state <- unique(dt_states[dt_states %notin% states])
length(not_a_state)

## There are several cases that we need to account for:
# City only
# County, ID | ID is State abbreviation, e.g. TX
# Misc places.

# Fix City only case - Chicago.
#dt_US$Province_State <- gsub("Chicago$","Chicago, IL",dt_US$Province_State)
dt_US$Province_State[grepl("Chicago$",dt_US$Province_State)] <- "Illinois"

# Fix District of Columbia case.
idx <- grepl("District of Columbia$",dt_US$Province_State)
dt_US$Province_State[idx] <- "Washington, D.C."

# Remove the rows in which Province_State == "US" -- this doesn't make any
# sense.
dt_US <- dt_US %>% filter(Province_State != "US")

# Fix County, ID use case.
# Map state abbreviation to State name.
idx <- grepl(", [A-Z]{2}$",dt_US$Province_State)
state_ids <- sapply(strsplit(dt_US$Province_State[idx],", "),"[",2)
dt_US$Province_State[idx] <- states[state_ids]

# Check again, how many don't match? 
dt_states <- unique(dt_US$Province_State)
still_not_a_state <- unique(dt_states[dt_states %notin% states])
length(still_not_a_state)

# Let's combine the data from the Diamond Princess cruise ship.
idx <- grepl("Diamond Princess",dt_US$Province_State)
dt_US$Province_State[idx] <- "Diamond Princess"

# Let's combine data from the Grand Princess cruise ship. 
idx <- grepl("Grand Princess",dt_US$Province_State)
dt_US$Province_State[idx] <- "Grand Princess"

# We will consider these cruise ships as states for our purposes.
states <- c(states,DP="Diamond Princess",GP="Grand Princess")

# Okay, how many more?
dt_states <- unique(dt_US$Province_State)
not_a_state <- unique(dt_states[dt_states %notin% states])
length(not_a_state)

# Remove row where Province_State is "Recovered" this doesn't make sense.
dt_US <- dt_US %>% filter(Province_State != "Recovered")

# Fix "Jackson County, OR " - extra space case.
idx <- grepl("Jackson County, OR ",dt_US$Province_State)
dt_US$Province_State[idx] <- "Oregon"

# Let's combine data from the US Virgin Islands.
idx <- grepl("Virgin Islands",dt_US$Province_State)
dt_US$Province_State[idx] <- "Virgin Islands"

# Add this as a state.
states <- c(states,VI="Virgin Islands")

# Add Guam and Puerto Rico as well.
states <- c(states,GU="Guam",PR="Puerto Rico")

dt_states <- unique(dt_US$Province_State)
not_a_state <- unique(dt_states[dt_states %notin% states])
length(not_a_state)

# We will ignore the Wuhan Evacuee's.
dt_US <- dt_US %>% filter(Province_State == "Wuhan Evacuee")

# Finally, add American Samoa and Northern Mariana Islands to vector of states.
# The NMI is an American commonwealth.
states <- c(states,"AS" = "American Samoa", 
	    NMI="Northern Mariana Islands")

#---------------------------------------------------------------------
## We have data from US and a bunch of states.
#---------------------------------------------------------------------
# Generate plots for all US states, and associated terrotories.

# Earliest date: 
start <- as.POSIXct(min(dt$Date),format="%m-%d-%Y")
today <- Sys.Date()
elapsed <- ceiling(difftime(today,start))
elapsed

# Loop to generate plots for all states.
plots <- list()
for (state in states){
	## Generate the plot.
	plot <- plot_mortality_rate(dt_US, country="US", state)
	## Save the plot.
	# Generate a filename for saving the plot.
	namen <- paste("US",gsub(" ","_",state),sep="_")
	myfile <- file.path(figsdir,"US-States",
			    paste(namen,fig_format,sep="."))
	# Save.
	ggsave(myfile,plot,width=7,height=7,units="in")
	# Add plot to list.
	plots[[state]] <- plot
}

# Generate a markdown file for each.
