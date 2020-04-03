#!/usr/bin/env Rscript

# Analysis of COVID-19 data.

## User parameters to change.
fig_format = "png" # Figure format of US State plots.

## Input:
# COVID-19 daily reports in root/csse_covid_19_data/csse_covid_19_daily_reports

## Output:
# * [Modified-Date]_Global_COVID-19_Cases.csv - Data in tidy format.
# * [Modified-Date]_US_COVID-19_Cases.csv - Data in tidy format.

#---------------------------------------------------------------------
## Set up the workspace.
#---------------------------------------------------------------------

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

# To install miscellaneous functions from TBmicr:
#devtools::install_github("twesleyb/TBmiscr")

# Directories.
root <- getrd()
funcdir <- file.path(root,"R")
figsdir <- file.path(root,"figs")
datadir <- file.path(root,"csse_covid_19_data/csse_covid_19_daily_reports")

# Load any functions in R/
load_all()

#---------------------------------------------------------------------
## Load the data.
#---------------------------------------------------------------------

# Make sure the repo is up to date with master.
# Assumes you have already added master repo as upstream.
message("\nPulling data from upstream repository.")
pull <- "git pull upstream master"
status <- system(pull,intern=TRUE)

# Load the data.
data_files <- list.files(datadir,pattern=".csv",full.names=TRUE)
names(data_files) <- tools::file_path_sans_ext(basename(data_files))
all_data <- lapply(data_files,fread)

# Define a function that cleans-up the column names.
fix_colnames <- function(x) { 
	# Replace "/" and " " with "_".
	colnames(x) <- gsub("/|\\ ","_",colnames(x))
	return(x)
}
all_data <- lapply(all_data,fix_colnames)

# Merge the data into a single tidy dt.
dt_covid <- rbindlist(all_data,use.names=TRUE,fill=TRUE,idcol="Date")

# Sort the data.
dt_covid <- dt_covid %>% setorder(Country_Region,Province_State,Last_Update)

# Save the data.
namen <- paste(Sys.Date(),"Global_COVID-19_Cases.csv",sep="_")
myfile <- file.path(root,"data",namen)
fwrite(dt_covid,myfile)

# Some basic stats:
today <- Sys.Date()
start <- as.POSIXct(min(dt_covid$Date),format="%m-%d-%Y")
elapsed <- ceiling(difftime(today,start))
message(paste("\nElapsed time since first COVID-19 case:", elapsed,"days."))

#------------------------------------------------------------------------------
## Clean-up the data from the US.
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
#all(is_state)
# Nope, so we need to figure out where/what these other places are.

# How many don't match? 
dt_states <- unique(dt_US$Province_State)
not_a_state <- unique(dt_states[dt_states %notin% states])
#length(not_a_state)

## There are several cases that we need to account for:
# * City only
# * County, ID | ID is State abbreviation, e.g. TX
# * Misc places.

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
#length(still_not_a_state)

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
#length(not_a_state)

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

# Check again.
dt_states <- unique(dt_US$Province_State)
not_a_state <- unique(dt_states[dt_states %notin% states])
#length(not_a_state)

# We will ignore the Wuhan Evacuee's.
dt_US <- dt_US %>% filter(Province_State != "Wuhan Evacuee")

# Finally, add American Samoa and Northern Mariana Islands to vector of states.
# The NMI is an American commonwealth.
states <- c(states,"AS" = "American Samoa", 
	    NMI="Northern Mariana Islands")

# Final check:
is_state <- dt_US$Province_State %in% states
if (!all(is_state)) { stop("Some states are not recognized.") }

# Sort states by abbreviated name.
states <- states[order(names(states))]

# Status.
not_a_state <- states[which(states %notin% state.name)]
n <- length(states) - length(not_a_state)
message(paste("\nCollated COVID-19 cases from",n, "US states", "plus data from:\n",
	      paste(not_a_state,collapse=", ")))

# Sort the data.
dt_US <- dt_US %>% setorder(Country_Region,Province_State,Last_Update)

# Save the data.
namen <- paste(Sys.Date(),"US_COVID-19_Cases.csv",sep="_")
myfile <- file.path(root,"data",namen)
fwrite(dt_US,myfile)

#---------------------------------------------------------------------
## Generate plots for all US states.
#---------------------------------------------------------------------
# Generate plots for all US states and associated provinces.

# Loop to generate plots for all US states.
message("\nGenerating plots for all US States and provinces.")
plots <- list()
# Initialize progres bar.
pbar <- txtProgressBar(max=length(states),style=3)
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
	# Update progress bar.
	setTxtProgressBar(pbar,match(state,states))
}
close(pbar)

#---------------------------------------------------------------------
## Add plots to README.
#---------------------------------------------------------------------

# Create READMD.md
invisible({ file.create("README.md") })
f <- file("./README.md",open="w+")
write("# COVID-19 Deaths by US State\n",file=f,append=TRUE)

# Loop to add state plots to README.md
message("\nUpdating README.md")
for (state in states) {
	txt <- c("## TITLE","![STATE](../figs/US-States/US_STATE.png)","\n")
	lines <- gsub("TITLE",state,txt)
	lines <- gsub("STATE",gsub(" ","_",state),lines)
	# Append text.
	write(lines,file=f,append=TRUE,sep="\n")
}

# Close file.
close(f)

# Status.
message("\nDone!")

#---------------------------------------------------------------------
## Fit a curve.
#---------------------------------------------------------------------

# Plot of NY.
p1 <- plot_mortality_rate(dt_US, country="US", state="New York",log=F)
p2 <- plot_mortality_rate(dt_US, country="US", state="New York",log=T)

# Two methods of fitting an exponential growth function:
plot(log(time), log(y))


