#!/usr/bin/env Rscript

# Tidy up the COVID-19 data from the United States.

## User parameters to change.

## Input:
# * root/data/global_cases.csv

## Output:
# * root/data/united_states_cases.csv
# * root/data/united_states_cases.RData

#----------------------------------------------------------
## Functions
#----------------------------------------------------------

`%notin%` <- Negate(`%in%`)

#----------------------------------------------------------
## Set up the workspace
#----------------------------------------------------------

# Load renv.
here <- getwd()
root <- dirname(here)
renv::load(root,quiet=TRUE)

# Imports.
suppressPackageStartupMessages({
	library(dplyr)
	#library(TBmiscr)
	library(data.table)
})

# To install miscellaneous functions from TBmicr:
#devtools::install_github("twesleyb/TBmiscr")

# Directories.
funcdir <- file.path(root,"R")
figsdir <- file.path(root,"figs")
datadir <- file.path(root,"data")

# Load any functions in R/
devtools::load_all()

# Load the data.
dt_covid <- fread(file.path(datadir,"global_cases.csv"))

#------------------------------------------------------------------------------
## Clean-up the data from the US.
#------------------------------------------------------------------------------
# Clean up the data from the United States.

# Subset the data.
dt_US <- dt_covid %>% filter(Country_Region=="United States")

# We can utilize the state dataset included in base R.
data(state)
states <- state.name
names(states) <- state.abb

# States does not include Washington D.C., because it it technically isn't a
# state, but we will consider it in our list of US states.
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

# Add last modified column.
dt_US[["Last_Update"]] <- Sys.Date()

# Sort the data.
dt_US <- dt_US %>% setorder(Country_Region,Province_State,Date)

# Insure the data is a data.table.
dt_US <- as.data.table(dt_US)

# Save the data as csv.
namen <- "united_states_cases.csv"
myfile <- file.path(datadir,namen)
fwrite(dt_US,myfile)

# Save as RData for package.
namen <- "united_states_cases.RData"
united_states_cases <- dt_US
myfile <- file.path(datadir,namen)
save(united_states_cases,file=myfile,version=2)

# Summary.
not_a_state <- states[which(states %notin% state.name)]
n <- length(states) - length(not_a_state)
tab <- dt_US %>% group_by(Category) %>%
	summarize(Total=formatC(max(Cases),format="d",big.mark=","),.group="drop")

# Status.
message(paste("\nSummary of COVID-19 cases from",n,
	      "US states and", length(not_a_state),
	      "provinces:"))
knitr::kable(tab)

message("\n")
