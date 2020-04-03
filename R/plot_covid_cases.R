plot_covid_cases <- function(dt_covid,country,state,
				category=c("Deaths","Confirmed","Recovered"),
					   log=FALSE){

	## FIXME: Where are NAs coming from when log = TRUE?
	## FIXME: Change color based on category.
	## FIXME: Add slope calculation and linear fit.
	## FIXME: Adjust position of url.
	## FIXME: problem when data is all 0, e.g. American somoa.
	## CLEAN UP ORDEROF OPERATIONS!
	## Define a function that generates mortality rate plot.
	## Data is COVID-19 data.table.

	# Subset the data for a given country/state.
	subdt <- dt_covid %>% filter(Country_Region == country, 
			          Province_State == state)
	# Summarize cases by day.
	subdt_summary <- subdt %>% group_by(Date) %>% 
		summarize(Confirmed = sum(Confirmed,na.rm=TRUE),
			  Deaths = sum(Deaths,na.rm=TRUE),
 		          Recovered = sum(Recovered,na.rm=TRUE)) %>%
	        as.data.table()

	# Melt data into tidy df.
	df <- melt(subdt_summary,id.vars="Date",
		   variable.name="Category",value.name="n_Cases")

	# Calculate time since patient zero (+1).
	p0 <- df %>% filter(Category==category) %>% filter(N>0) %>%
		summarize(Date=min(Date))
	df$Time <- as.numeric(difftime(df$Date,p0[["Date"]],units="days") + 1)

	# Summarize COVID-19 cases.
	tab <- df %>% group_by(Category) %>% summarize("Total" = max(N))
	# Plot log(time) v log(n)?
	if (log) {
		# Trim time before p0.
		df <- df %>% filter(Time >= 1)
		df$Time <- log10(df$Time)
		df$N <- log10(df$N)
	}
	#fit = lm(log(df$N) ~ log(df$Time))
	#fit$coefficients
	# Generate plot.
        plot <- ggplot(df %>% filter(Category==category),
		       aes(x=Time,y=N,group=Category,color=Category)) + 
		geom_point(size=1.5,color="darkred") + 
		geom_path(size=0.75,color="firebrick") + 
		xlab(paste0("Time (Days since first case)")) + 
		ylab(paste0("Total Number of COVID-19 ", category)) + 
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
				 x=0.72*xrange["delta"], 
				 y=yrange["min"],
				 label = paste("Source:",url))
		# Annotate with case summary.
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
