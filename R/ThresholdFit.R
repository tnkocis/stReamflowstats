# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

#run post clean up
ThresholdFit <- function(input, threshold){
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	if (missing(threshold))
		stop("Threshold data to extrat and fit is required.")
	
	 L <- length(input$Stats)
	 
	 daydf <- data.frame(dayfracabove= rep(NA, L), startyear=rep(NA,L))
	 voldf <- data.frame(volfracabove= rep(NA, L), startyear=rep(NA,L))
	
	 for(i in 1:L){
		 threshloc <- which(input$Stats[[i]]$Thresholds$Totals$Thresholds == threshold)
		 syear <- strsplit(names(input$Stats)[[i]], " - ")[[1]][[1]]
		 daydf$dayfracabove[[i]] <- input$Stats[[i]]$Thresholds$Totals$FracDaysAbove[[threshloc]]
		 daydf$startyear[[i]] <- syear
		 voldf$volfracabove[[i]] <- input$Stats[[i]]$Thresholds$Totals$Frac_Abv[[threshloc]]
		 voldf$startyear[[i]] <- syear
	 }
	 out <- list(daydf=daydf, voldf=voldf)
	 return(out)
		 
 }
	
