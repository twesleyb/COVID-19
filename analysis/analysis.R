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
})

# Directories.
root <- getrd()
datadir <- file.path(root,"csse_covid_19_data/csse_covid_19_daily_reports")

#---------------------------------------------------------------------
## Load the data.
#---------------------------------------------------------------------

# Make sure the repo is up to date with master.
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
dt <- rbindlist(all_data,use.names=TRUE,fill=TRUE,idcol="Date")

#--------------------------------------------------------------------
## Look at the data.
#--------------------------------------------------------------------

# Let's glance at the data from a US state.
country = "US"
state = "North Carolina"

plot_mortality_rate <- function(country,state){
	# Data is dt.
	# Summarize cases from a given country/state.
	subdt <- dt %>% filter(Country_Region == country, Province_State == state)
	subdt_summary <- subdt %>% group_by(Date) %>% 
		summarize(Confirmed = sum(Confirmed),
			  Deaths = sum(Deaths),
 		          Recovered = sum(Recovered))
	# Plot deaths by day.
	df <- melt(subdt_summary,id.vars="Date",
		   variable.name="Category",value.name="N") %>% 
	filter(Category=="Deaths")
        plot <- ggplot(df,aes(x=Date,y=N,group=Category,color=Category)) + 
		geom_point() + geom_path() + ylab("Number of Cases") + 
		ggtitle(paste(country,state,sep=": ")) + 
		theme(text = element_text(family = "Arial"),
		      plot.title = element_text(color = "black",size = 11, 
				        face = "bold", hjust = 0.5,
				        family = "Arial"),
		      axis.title.x = element_text(color = "black", 
				          size = 11, face = "bold",
				          family = "Arial"),
		      axis.title.y = element_text(color = "black", 
				          size = 11, face = "bold",
				          family = "Arial"),
		      axis.text.x = element_text(color = "black", size = 11, 
				         angle = 45, 
					 hjust = 1.0,
				         family = "Arial"),
		      panel.border = element_rect(colour = "black", fill=NA, size=0.75))
		return(plot)
}


# Check the global number of deaths.
df <- dt %>% group_by(Date) %>% 
	summarize("Deaths" = sum(Deaths,na.rm=TRUE))

plot <- ggplot(df,aes(x=Date,y=Deaths,group=1)) +
	geom_point() + geom_path() + ylab("Number of Deaths") + 
	ggtitle("Global Covid-19 Deaths") + 
	theme(text = element_text(family = "Arial"),
	      plot.title = element_text(color = "black",size = 11, 
				        face = "bold", hjust = 0.5,
				        family = "Arial"),
	      axis.title.x = element_text(color = "black", 
				          size = 11, face = "bold",
				          family = "Arial"),
	      axis.title.y = element_text(color = "black", 
				          size = 11, face = "bold",
				          family = "Arial"),
	      axis.text.x = element_text(color = "black", size = 11, 
				         angle = 45, 
					 hjust = 1.0,
				         family = "Arial"),
	      panel.border = element_rect(colour = "black", fill=NA, size=0.75))
plot
