#!/usr/bin/env Rscript

root <- "~/projects/covid19"

devtools::load_all(root)

library(neten)
library(dplyr)
library(data.table)

data(united_states_cases)

dt_us <- united_states_cases

df <- dt_us %>% 
	group_by(Province_State, Category, Date) %>% 
	summarize(Cases = sum(Cases), .groups="drop")

dm <- df %>% subset(Category=="Deaths") %>%
	reshape2::dcast(Date ~ Province_State, value.var="Cases") %>%
	as.data.table() %>% as.matrix(rownames="Date")
dm[is.na(dm)] <- 0


# drop states with no deaths
drop <- names(which(apply(dm,2,function(x) all(x==0))))

# calc coorelation matrix

# spearman rank may be more appropriate?
adjm <- cor(dm[,!colnames(dm)==drop])

# save to file for clustering
myfile <- file.path(root,"rdata","adjm.csv")
adjm %>% as.data.table(keep.rownames="State") %>% 
	fwrite(myfile)

# neten
netw <- neten(adjm)

# save to file for clustering
myfile <- file.path(root,"rdata","netw.csv")
netw %>% as.data.table(keep.rownames="State") %>% 
	fwrite(myfile)
