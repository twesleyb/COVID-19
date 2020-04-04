#!/usr/bin/env Rscript

# Analysis of COVID-19 data.

## Input:
# COVID-19 daily reports in root/csse_covid_19_data/csse_covid_19_daily_reports/

## Output:
# * [Last-Modified-Date]_Global_COVID-19_Cases.csv 

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

# Load the data into a list.
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

# Combine the data into a single data.table. 
dt_combined <- rbindlist(all_data,use.names=TRUE,fill=TRUE,idcol="Date")

#--------------------------------------------------------------------
## Tidy the combined COVID-19 data.
#--------------------------------------------------------------------

# Melt the combined data into a tidy data.table.
dt_covid <-  melt(dt_combined,
		  id.vars=c("Country_Region","Province_State","Date"), 
		  measure.vars = c("Deaths","Confirmed","Recovered"),
		  variable.name="Category",value.name="Cases")

# Summarize the number of cases by day.
dt_covid <- dt_covid %>% 
	group_by(Country_Region,Province_State,Date,Category) %>%
	summarize("Number of Cases" = sum(Cases,na.rm=TRUE))

# Convert Date to standard universal time format.
dt_covid$Date <- as.POSIXct(dt_covid$Date,format="%m-%d-%Y",tz="UTC")

# Sort the data.
dt_covid <- dt_covid %>% setorder(Country_Region,Province_State,Category,Date)

#---------------------------------------------------------------------
## Clean-up the Countries column.
#---------------------------------------------------------------------

# Remove Country == Cruise Ship
# This isn't a country.
# Plus, countries like the US include this data.
dt_covid <- dt_covid %>% filter(Country_Region != "Cruise Ship")

# Bahamas
idx <- grepl("Bahamas", dt_covid$Country_Region)
dt_covid$Country_Region[idx] <- "Bahamas"

# Viet Nam
idx <- grepl("Viet Nam$", dt_covid$Country_Region)
dt_covid$Country_Region[idx] <- "Vietnam"

# Mainand China
idx <- grepl("Mainland China$", dt_covid$Country_Region)
dt_covid$Country_Region[idx] <- "China"

# Congo
idx <- grepl("Congo", dt_covid$Country_Region)
dt_covid$Country_Region[idx] <- "Republic of the Congo"

# Gambia
idx <- grepl("Gambia", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Gambia"

# Hong Kong
idx <- grepl("Hong Kong", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Hong Kong"

# Iran
idx <- grepl("Iran", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Iran"

# Moldova
idx <- grepl("Moldova", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Moldova"

# Russia
idx <- grepl("Russia", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Russia"

# South Korea
idx <- grepl("Korea", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "South Korea"

# Taiwan
idx <- grepl("Taiwan", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Taiwan"

# UK
idx <- grepl("United Kingdom", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "UK"

# Save the data.
namen <- paste(Sys.Date(),"Global_COVID-19_Cases.csv",sep="_")
myfile <- file.path(root,"data",namen)
fwrite(dt_covid,myfile)

#---------------------------------------------------------------------
## Summary statistics.
#---------------------------------------------------------------------
# Note: These numbers may differ from the JHU dashboard.

# Elapsed time since first COVID case.
today <- Sys.Date()
start <- min(dt_covid$Date)
elapsed <- ceiling(difftime(today,start))
message(paste("\nElapsed time since first COVID-19 case:", elapsed,"days."))

# Number of countries.
countries <- unique(dt_covid$Country_Region)
n <- length(countries)
message(paste("Data compiled from",n,"countries."))

# Summarize the total number of category::cases by country.
covid_summary <- dt_covid %>% group_by(Country_Region,Category) %>% 
	summarize(n = max(`Number of Cases`))

# Determine the global number of cases by category.
message("Summary of world-wide COVID-19 cases:")
tab <- covid_summary %>% group_by(Category) %>% 
	summarize(Total = formatC(sum(n),big.mark=","))
knitr::kable(tab)
