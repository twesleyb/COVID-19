#!/usr/bin/env Rscript

# Plot COVID cases for every US state.

## User parameters to change:

## Input:
# * root/data/United_States_COVID-19_Cases.csv 

## Output:
# * plots for every state saved in README.md

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

# To install miscellaneous R functions from TBmicr:
#devtools::install_github("twesleyb/TBmiscr")

# Directories.
root <- getrd()
funcdir <- file.path(root,"R")
figsdir <- file.path(root,"figs")
datadir <- file.path(root,"data")

# Load any functions in R/
load_all()

# Load the data.
myfile <- file.path(datadir,"United_States_COVID-19_Cases.csv")
dt_US <- fread(myfile)

#---------------------------------------------------------------------
## Generate plots for all US states.
#---------------------------------------------------------------------
# Generate plots for all US states and associated provinces.

# Vector of US states.
states <- unique(dt_US$Province_State)

plot_states(states,category="Deaths",save=FALSE)

# Loop to generate and save plots for all US states.
plot_states <- function(states,categrory,save){
	message("\nGenerating plots for all US States and provinces.")
	plots <- list()
	# Initialize progres bar.
	pbar <- txtProgressBar(max=length(states),style=3)
	for (state in states){
		# Generate the plot.
		plot <- plot_covid_cases(dt_US, country_region="United States", 
					 province_state = state, category) 
		# Skip the loop's iteration if we are unable to generate a plot.
		if (is.null(plot)) { 
			next 
		}
		# Generate a filename for saving the plot.
		if (save) {
			namen <- paste("US",gsub(" ","_",state),sep="_")
			myfile <- file.path(figsdir,"US-States",
					    paste(namen,fig_format="png",sep="."))
			# Save.
			ggsave(myfile,plot,width=7,height=7,units="in")
		}
		# Add plot to list.
		plots[[state]] <- plot
		# Update progress bar.
		setTxtProgressBar(pbar,match(state,states))
	}
	close(pbar)
}

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

save.image()
quit()

# Plot of NY.
p1 <- plot_covid_cases(dt_US, country="US", state="New York",log=F)
p2 <- plot_covid_cases(dt_US, country="US", state="New York",log=T)

# Two methods of fitting an exponential growth function:

fit$coefficients

#Plot the fitted line
plot(time, y)
lines(time, time ^ fit$coefficients[2], col = "red")
