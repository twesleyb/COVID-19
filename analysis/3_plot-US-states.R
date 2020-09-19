#!/usr/bin/env Rscript

# Plot COVID cases for every US state.

## User parameters to change:
update_readme = TRUE
save_plots = TRUE

## Input:
# * root/data/united_states_cases.csv

## Output:
# plots for every state saved in:
# * root/analysis/README.md

#----------------------------------------------------------
## Set up the workspace
#----------------------------------------------------------

# Load renv
here <- getwd()
root <- dirname(here)
renv::load(root,quiet=TRUE)

# Imports
suppressPackageStartupMessages({
	library(grid)
	library(dplyr)
	library(gtable)
	#library(TBmiscr)
	library(ggplot2)
	library(gridExtra)
	library(data.table)
})

# To install miscellaneous R functions from TBmicr:
#devtools::install_github("twesleyb/TBmiscr")

# Project Directories:
funcdir <- file.path(root,"R")
figsdir <- file.path(root,"figs")
datadir <- file.path(root,"data")

# Load local functions and data in root/data and root/R
devtools::load_all()

# Load the data
data(united_states_cases)
dt_US <- united_states_cases

#---------------------------------------------------------------------
## Generate plots for all US states.
#---------------------------------------------------------------------
# Generate plots for all US states and associated provinces.

# Vector of US states.
states <- unique(dt_US$Province_State)

# Loop to generate and save plots for all US states.
message("\nGenerating plots for all US States and provinces.")
plots <- list()

# Initialize progres bar.
pbar <- txtProgressBar(max=length(states),style=3)

for (state in states){

	# Generate a state's plot
	plot <- plot_covid_cases(dt_US,
				 country_region="United States",
				 province_state = state,
				 cases = "Deaths",
				 log=TRUE)

	# Skip the loop's iteration if we are unable to generate a plot.
	if (is.null(plot)) {
		next
	}
	# Add plot to list.
	plots[[state]] <- plot
	# Update progress bar.
	setTxtProgressBar(pbar,match(state,states))
} # Ends loop.
close(pbar)

# Save the plots.
if (save_plots) {
	message("\nSaving plots...")
	pbar <- txtProgressBar(max=length(plots),style=3)
	for (state in names(plots)){
		# Generate a filename for saving the plot.
		namen <- paste("US",gsub(" ","_",state),sep="_")
		myfile <- file.path(figsdir,"US-States",
				    paste(namen,fig_format="png",sep="."))
		# Save.
		ggsave(myfile,plot=plots[[state]],width=7,height=7,units="in")
		setTxtProgressBar(pbar,match(state,names(plots)))
	}
} # Ends loop.
close(pbar)

#---------------------------------------------------------------------
## add plots to readme.
#---------------------------------------------------------------------

# Update README?
if (update_readme) {
	# Create READMD.md
	invisible({ file.create("README.md") })
	f <- file("./README.md",open="w+")
	write("# COVID-19 Deaths by US State\n",file=f,append=TRUE)
	write(paste0("_last update: ",Sys.Date(),"_\n"),file=f,append=TRUE)
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
}

# Status.
message("\nDone!")

quit()

#---------------------------------------------------------------------
## Examine NC.
#---------------------------------------------------------------------

# Get NC's plot.
nc <- "North Carolina"
plot <- plots[[nc]]

# Extract data from plot.
df <- plot$data

# Subset data -- remove data before first case.
subdf <- df %>% filter(Cases > 0)

# Log of x (time) and y (cases).
subdf$x <- log(subdf$"Time (days)")
subdf$y <- log(subdf$Cases)

# Linear fit.
fit <- lm(subdf$y ~ subdf$x)
r2 <- summary(fit)$r.squared

# Calculate p-value.
f <- summary(fit)$fstatistic
p <- pf(f[1], f[2], f[3], lower.tail=FALSE) # F-distribution
attributes(p) <- NULL

# Add linear fit to plot.
plot <- plot + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "black", linetype = "dashed")

# Annotate with stats.
plot <- plot + annotate("text", x = 0.5, y = 5, label = paste("R-sqr =",round(r2,3),"\n","P-val =",formatC(p,format="e")))

# Save.
ggsave("nc.tiff",plot)
