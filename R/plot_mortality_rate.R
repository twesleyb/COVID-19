plot_mortality_rate <- function(dt_covid,country,state){
	# Define a function that generates mortality rate plot.
	# Data is COVID-19 data.table.
	# Summarize cases from a given country/state.
	subdt <- dt_covid %>% filter(Country_Region == country, 
			            Province_State == state)
	subdt_summary <- subdt %>% group_by(Date) %>% 
		summarize(Confirmed = sum(Confirmed,na.rm=TRUE),
			  Deaths = sum(Deaths,na.rm=TRUE),
 		          Recovered = sum(Recovered,na.rm=TRUE)) %>%
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
				 x=0.72*xrange["delta"], 
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
