#!/usr/bin/env Rscript

# title: covid19
# author: twab
# description: Pull COVID-19 data from CSSE Github and then tidy it up

## Input:
# COVID-19 daily reports in:
# * root/csse_covid_19_data/csse_covid_19_daily_reports/
reports <- "csse_covid_19_data/csse_covid_19_daily_reports"

## Output:
# * root/data/global_cases.csv
# * root/data/global_cases.RData

#----------------------------------------------------------
## Set up the workspace.
#----------------------------------------------------------

# Activate renv
here <- getwd()
root <- dirname(here)
#renv::load(root,quiet=TRUE)

# Imports.
suppressPackageStartupMessages({
	library(dplyr)
	library(data.table)
})

# To install miscellaneous functions from TBmicr:
#devtools::install_github("twesleyb/TBmiscr")

# Directories.
funcdir <- file.path(root,"R")
figsdir <- file.path(root,"figs")
rdatdir <- file.path(root,"data")
datadir <- file.path(root, reports)

# Load any functions in R/
devtools::load_all()

#----------------------------------------------------------
## Load the data.
#----------------------------------------------------------

# Make sure that the local repository is up to date with
# master: CSSEGISandData/COVID-19.
# NOTE: `git pull` Assumes you have already added master
# repository as upstream:
# $ git remote add upstream git://github.com/CSSEGISandData/COVID-19.git
# $ git fetch upstream
# $ git pull upstream master

# To add a specific directory, ignoring others:
# $ git fetch upstream master
# $ git checkout upstream/master -- csse_covid_19_data/csse_covid_19_daily_reports

# Pull data from upstream repo.
message("\nPulling data from upstream repository.")
pull <- "git pull upstream master"
response <- system(pull,intern=TRUE)

# Stop if unable to read from remote repository.
if (is.null(attr(response,"status"))) {
	message(response)
} else {
	msg <- c("Unable to read from remote repository.\n",
		 "Have you added the upstream repository? In the terminal, try:\n",
		 "$ git remote add upstream git://github.com/CSSEGISandData/COVID-19.git\n",
		 "$ git fetch upstream master\n",
		 "$ git pull upstream master\n")
	stop(msg)
}

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
	summarize(Cases = sum(Cases,na.rm=TRUE),.groups="drop")

# Convert Date to standard universal time format.
dt_covid$Date <- as.POSIXct(dt_covid$Date,format="%m-%d-%Y",tz="UTC")

# Sort the data.
dt_covid <- dt_covid %>% setorder(Country_Region,Province_State,Category,Date)

#---------------------------------------------------------------------
## Clean-up the Countries column.
#---------------------------------------------------------------------
# Combine data from countries that are listed multiple times due to
# multiple names.

# Remove Country == Cruise Ship
# This isn't a country.
# Plus, countries like the US include this data.
dt_covid <- dt_covid %>% filter(Country_Region != "Cruise Ship")

# US == United States
idx <- grepl("US$", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "United States"

# Bahamas
idx <- grepl("Bahamas", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Bahamas"

# Viet Nam
idx <- grepl("Viet Nam$", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Vietnam"

# Mainand China
idx <- grepl("Mainland China$", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "China"

# Congo
idx <- grepl("Congo", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
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

# United Kingdom == UK
idx <- grepl("UK$", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "United Kingdom"

# Taipei
idx <- grepl("Taipei", dt_covid$Country_Region)
#unique(dt_covid$Country_Region[idx])
dt_covid$Country_Region[idx] <- "Taipei"

# Insure the data is a data.table
dt_covid <- as.data.table(dt_covid)

# Add last update column.
dt_covid[["Last_Update"]] <- Sys.Date()

# Save the data as csv.
myfile <- file.path(rdatdir,"global_cases.csv")
fwrite(dt_covid,myfile)

# Save the data as RData for R package.
# Specify version in order to avoid warning message when installing package.
myfile <- file.path(rdatdir,"global_cases.RData")
global_cases <- dt_covid
save(global_cases,file=myfile,version=2)

#---------------------------------------------------------------------
## Summary statistics.
#---------------------------------------------------------------------
# Note: These numbers may differ from the JHU dashboard.

# Elapsed time since first confirmed COVID case.
today <- Sys.Date()
start <- min(dt_covid[which(dt_covid$Category=="Confirmed"),Date])
elapsed <- ceiling(difftime(today,start))

# Number of countries.
countries <- unique(dt_covid$Country_Region)
n_countries <- length(countries)

# Summarize the total number of category::cases by country.
covid_summary <- dt_covid %>% group_by(Country_Region,Category) %>%
	summarize(n = max(Cases),.groups="drop")

# Determine the global number of cases by category.
message(paste0("\nSummary of COVID-19 cases compiled from ",
	       n_countries," countries since ",
	       start," (",elapsed," days):"))
tab <- covid_summary %>% group_by(Category) %>%
	summarize(Total = formatC(sum(n),format="d",big.mark=","),.groups="drop")
knitr::kable(tab)
message("\n")
