#' plot_covid_cases
#' @import data.table (>= 1.12.8)
#' @export 
plot_covid_cases <- function(dt_covid,country_region,province_state,
			     category=c("Deaths","Confirmed","Recovered"),
			     log=FALSE){

	# Insure the provided country is in the data.
	check <- country_region %in% dt_covid$Country_Region
	if (!check) {  
		message(paste0(" Warning: '",country_region,"'",
			   " is not in dt_covid$Country_Region."))
		return(NULL)
	}

	# Insure that the provided state is in the data.
	# FIXME: doesn't work if no state is provided.
#	check <- province_state %in% dt_covid$Province_State
#	if (!check) {  
#		message(paste0(" Warning: '",province_state,"'",
#			   " is not in dt_covid$Province_State."))
#		return(NULL)
#	}

	# Subset the data for a given country/state.
	if (is.null(province_state)){
		# Just a country.
		subdt <- dt_covid %>% filter(Country_Region == country_region)
	} else {
		subdt <- dt_covid %>% filter(Country_Region == country_region, 
					     Province_State == province_state)
	}

	# Check that data subset is not of length 0.
	check <- dim(subdt)[1] > 0
	id <- paste(country_region,province_state,sep=":")
	if (!check) { 
		message(paste0(" Warning: There is no data for ",id,"."))
		return(NULL)
	}

	# Summarize cases by day.
	df <- subdt %>% group_by(Category,Date) %>% 
		summarize(Cases = sum(Cases)) %>% as.data.table()

	# Check that there are some cases.
	check <- any(df$Cases > 0)
	if (!check) { 
		message(paste0(" Warning: There is no data for ",id,"."))
		return(NULL)
	}

	# Determine date of case zero.
	p0 <- df %>% filter(Cases>0) %>% group_by(Category) %>% 
		summarize(Date=min(Date)) %>% 
		data.table::transpose(make.names="Category")

	# Check that there is a case of provided category.
	check <- category %in% colnames(p0)
	if (!check) {
		message(paste0(" Warning: There are no ",category," in ", id,"."))
		return(NULL)
	}

	# Calculate elapsed time in days.
	# Add 1 such that at t=0, cases=0; t=1, cases >= 1.
	df$"Time (days)" <- 1 + as.numeric(difftime(df$Date,p0[[category]],
					   units="days"))

	# Summarize COVID-19 cases.
	tab <- df %>% group_by(Category) %>% 
		summarize("Total" = max(Cases))
	tab$Total <- formatC(tab$Total,big.mark=",")

	# Define plot colors based on COVID category.
	plot_colors <- list("Deaths" = c(point="darkred", 
					 path="firebrick"),
			    "Recovered" = c(point="darkgreen",
					    path="green"),
			    "Confirmed" = c(point="darkorange",
					    path="orange"))

	# Generate plot.
	if (log) {
		# Take log of x (time) and y (cases).
		suppressWarnings({
		plot <- ggplot(df %>% filter(Category==category),
			       aes(x=log(`Time (days)`),
				   y=log(Cases),
				   group=Category,color=Category)) 
		})
	} else {
		plot <- ggplot(df %>% filter(Category==category),
			       aes(x=`Time (days)`,y=Cases,
				   group=Category,color=Category)) 
	}

	# Add geoms to plot. 
	plot <- plot +
		geom_point(size=1.5,
			   color=plot_colors[[category]]["point"],
			   na.rm=TRUE) +
		geom_path(size=0.75,
			  color=plot_colors[[category]]["path"],
			  na.rm=TRUE) +
		ggtitle(paste(country_region,province_state,sep=": ")) 

	# Customize plot theme. 
	plot <- plot + 
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

		# Extract plot build for adding annotations to plot.
	        # Suppress warnings which arise from NaNs in log(Time).
		build <- suppressWarnings({ggplot_build(plot)})
		xrange <- build$layout$panel_params[[1]][["x.range"]]
		yrange <- build$layout$panel_params[[1]][["y.range"]]
		names(xrange) <- names(yrange) <- c("min","max")
		xrange["delta"] <- diff(xrange)
		yrange["delta"] <- diff(yrange)

		# Annotate with data source url.
		url <- "https://github.com/CSSEGISandData/COVID-19"
		text_box <- annotate("text", 
				 fontface = "italic",
				 size = 3,
				 colour = "darkgray",
				 x=xrange["min"] + 0.72*xrange["delta"], 
				 y=yrange["min"],
				 label = paste("Source:",url))
		plot <- plot + text_box

		# Annotate with case summary table.
		table_theme <- ttheme_default(
			base_size=11,core=list(bg_params=list(fill="white")))
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

		# Add gtable to plot.
		plot <- plot + 
			annotation_custom(
				g,
			       	xmin = xrange["min"]-0.70*xrange["delta"],
			        xmax=xrange["max"],
			        ymin = yrange["min"] + 0.75 * yrange["delta"], 
			        ymax=yrange["max"])

		# Return the plot.
		return(plot)
}
