#!/usr/bin/env Rscript

# Analysis of COVID-19 data.

## User parameters:
country = "US"
state = "New York"
fig_format = "png"

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
devtools::install_github("twesleyb/TBmiscr")

# Directories.
root <- getrd()
datadir <- file.path(root,"csse_covid_19_data/csse_covid_19_daily_reports")
figsdir <- file.path(root,"figs")

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
## Look at the data.
#--------------------------------------------------------------------

dt_states <- dt_covid %>% filter(Country_Region == "US")

data(states)

#--------------------------------------------------------------------
##
#--------------------------------------------------------------------

# Define a function that generates mortality rate plot.
plot_mortality_rate <- function(dt_covid,country,state){
	# Data is COVID-19 data.table.
	# Summarize cases from a given country/state.
	subdt <- dt %>% filter(Country_Region == country, 
			       Province_State == state)
	subdt_summary <- subdt %>% group_by(Date) %>% 
		summarize(Confirmed = sum(Confirmed),
			  Deaths = sum(Deaths),
 		          Recovered = sum(Recovered)) %>%
	        as.data.table()
	# Plot deaths by day.
	df <- melt(subdt_summary,id.vars="Date",
		   variable.name="Category",value.name="N")
	# Generate plot.
        plot <- ggplot(df %>% filter(Category=="Deaths"),
		       aes(x=Date,y=N,group=Category,color=Category)) + 
		geom_point(size=1.5,color="darkred") + 
		geom_path(size=0.75,color="firebrick") + 
		ylab("Total Number of COVID-19 Deaths") + 
		ggtitle(paste(country,state,sep=": ")) + 
		theme(text = element_text(family = "Arial"),
		      plot.title = element_text(color = "black",
						size = 11, 
						face = "bold", 
						hjust = 0.5,
						family = "Arial"),
		      axis.title.x = element_text(color = "black",
						  size = 11, 
						  face = "bold",
						  family = "Arial"),
		      axis.title.y = element_text(color = "black", 
						  size = 11, 
						  face = "bold",
						  family = "Arial"),
		      axis.text.x = element_text(color = "black", 
						 size = 11, 
						 angle = 45, 
						 hjust = 1.0,
						 family = "Arial"),
		      panel.border = element_rect(colour = "black", 
						  fill=NA, 
						  size=0.75))
		## Add plot annotations.
		# Extract plot x and y limits.
		build <- ggplot_build(plot)
		xrange <- build$layout$panel_params[[1]][["x.range"]]
		yrange <- build$layout$panel_params[[1]][["y.range"]]
		names(xrange) <- names(yrange) <- c("min","max")
		xrange["delta"] <- diff(xrange)
		yrange["delta"] <- diff(yrange)
		# Annotate with data source.
		url <- "https://github.com/CSSEGISandData/COVID-19"
		plot <- plot + 
			annotate("text", 
				 fontface = "italic",
				 size = 3,
				 colour = "darkgray",
				 x=0.75*xrange["delta"], 
				 y=yrange["min"],
				 label = paste("Source:",url))
		# Annotate with case summary.
		tab <- df %>% group_by(Category) %>% 
			summarize("Total" = max(N))
		table_theme <- ttheme_default(base_size=11,
					      core=list(bg_params=list(fill="white")))
		g <- tableGrob(tab, rows = NULL, theme = table_theme)
		# Add borders to table.
		g <- gtable_add_grob(g,
		     grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
		     t = 1, b = nrow(tab)+1, l = 1, r = ncol(tab))
		g <- gtable_add_grob(g,
		     grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
		     t = 1, b = nrow(tab), l = 1, r = ncol(tab))
		g <- gtable_add_grob(g,
		     grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
		     t = 1, b = nrow(tab)-1, l = 1, r = ncol(tab))
		g <- gtable_add_grob(g,
		     grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)),
		     t = 1, b = nrow(tab)-2, l = 1, r = ncol(tab))
		plot <- plot + 
			annotation_custom(g, 
					  xmin = xrange["min"]-0.70*xrange["delta"],
					  xmax=xrange["max"],
					  ymin = yrange["min"] + 0.75 * yrange["delta"], 
					  ymax=yrange["max"])
		return(plot)
}

# Let's glance at the data from a US state.
plot1 <- plot_mortality_rate(country="US", state="North Carolina")
plot2 <- plot_mortality_rate(country="US", state="New York")

# Generate a filename for saving the plot.
country_state <- paste(sapply(c(country,state),function(x) gsub(" ","_",x)),
		       collapse="_")
namen <- paste(paste(Sys.Date(),country_state,sep="_"),fig_format,sep=".")
myfile <- file.path(figsdir,"US-States",namen)

# Save the plot.
ggsave(myfile,plot=plot2,width=7,height=7,units="in")

#------------------------------------------------------------------------------
## Look at all US states.
#------------------------------------------------------------------------------

# Get all data for the United States.
# Check: United States is not in the data.
dt_US <- dt_covid %>% filter(Country_Region=="US")

# We can utilize the state dataset included in base R.
data(state)
states <- state.name
names(states) <- state.abb

# States does not include Washington D.C., because it it technically isn't a
# state, so we will add it.
states <- c(states,DC="Washington, D.C.")

# Check: are all US states in the data?
is_state <- dt_US$Province_State %in% states
all(is_state)
# Nope, so we need to figure out where/what these other places are.

# How many don't match? 
dt_states <- unique(dt_US$Province_State)
not_a_state <- unique(dt_states[dt_states %notin% states])
length(not_a_state)

# There are several cases:
# City only
# County, ID | ID is State abbreviation, e.g. TX
# Misc places.

# Fix City only case - Chicago.
#dt_US$Province_State <- gsub("Chicago$","Chicago, IL",dt_US$Province_State)
dt_US$Province_State[grepl("Chicago$",dt_US$Province_State)] <- "Illinois"

# Remove the rows in which Province_State == "US" -- this doesn't make any
# sense.
dt_US <- dt_US %>% filter(Province_State != "US")

# Fix County, ID use case.
# Map abbreviation to State name.
idx <- grepl(", [A-Z]{2}$",dt_US$Province_State)
state_ids <- sapply(strsplit(dt_US$Province_State[idx],", "),"[",2)
dt_US$Province_State[idx] <- states[state_ids]

# Check again, how many don't match? 
dt_states <- unique(dt_US$Province_State)
still_not_a_state <- unique(dt_states[dt_states %notin% states])
length(still_not_a_state)

# Fix District of Columbia case.
idx <- grepl("District of Columbia$",dt_US$Province_State)
dt_US$Province_State[idx] <- "Washington, D.C."

# Let's combine the data from the Diamond Princess cruise ship.
idx <- grepl("Diamond Princess",dt_US$Province_State)
dt_US$Province_State[idx] <- "Diamond Princess"

# Let's combine data from the Grand Princess cruise ship. 
idx <- grepl("Grand Princess",dt_US$Province_State)
dt_US$Province_State[idx] <- "Grand Princess"

# We will consider this a state for our purposes
states <- c(states,GP="Grand Princess")

# Okay, how many more?
dt_states <- unique(dt_US$Province_State)
not_a_state <- unique(dt_states[dt_states %notin% states])
length(not_a_state)

# Remove row where Province_State is "Recovered" this doesn't make sense.
dt_US <- dt_US %>% filter(Province_State != "Recovered")

# Fix "Jackson County, OR " - extra space case.
idx <- grepl("Jackson County, OR ",dt_US$Province_State)
dt_US$Province_State[idx] <- "Oregon"

# Let's combine data from the Grand Princess cruise ship. 
idx <- grepl("Virgin Islands",dt_US$Province_State)
dt_US$Province_State[idx] <- "Virgin Islands"

# Add this as a state.
states <- c(states,VI="Virgin Islands")

# Add Guam and Puerto Rico to states vector.
states <- c(states,GU="Guam",PR="Puerto Rico")

dt_US %>% filter(Province_State == "Wuhan Evacuee")
