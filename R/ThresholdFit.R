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
	 voldf <- data.frame(volfracabove= rep(NA, L), volabvMAF=rep(NA, L), startyear=rep(NA,L))
	
	 for(i in 1:L){
		 threshloc <- which(input$Stats[[i]]$Thresholds$Totals$Thresholds == threshold)
		 syear <- strsplit(names(input$Stats)[[i]], " - ")[[1]][[1]]
		 daydf$dayfracabove[[i]] <- input$Stats[[i]]$Thresholds$Totals$FracDaysAbove[[threshloc]]
		 daydf$startyear[[i]] <- syear
		 voldf$volfracabove[[i]] <- input$Stats[[i]]$Thresholds$Totals$Frac_Abv[[threshloc]]
		 voldf$volabvMAF[[i]] <- input$Stats[[i]]$Thresholds$Totals$Volume_Abv_acfte6[[threshloc]]
		 voldf$startyear[[i]] <- syear
	 }
	 if(any(is.na(daydf))){
		 daydf <- daydf[-which(is.na(daydf)),]
	 } else {}
	 if(any(is.na(voldf))){
		 voldf <- voldf[-which(is.na(voldf)),]
	 } else{}
	 
	 out <- list(daydf=daydf, voldf=voldf)
	 return(out)
		 
 }
 
 ThresholdFitMonthly <- function(input, threshold){
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
	 
	 daydf <- vector("list",6)
	 voldf <- vector("list",6)
	 for(i in 1:6){
		daydf[[i]] <- data.frame(dayfracabove= rep(NA, L), startyear=rep(NA,L))
	 	voldf[[i]] <- data.frame(volfracabove= rep(NA, L), volabvMAF=rep(NA, L), startyear=rep(NA,L))
	}
	names(daydf) <- c("NOV","DEC","JAN","FEB", "MAR", "APR")
	names(voldf) <- c("NOV","DEC","JAN","FEB", "MAR", "APR")
	
	 for(i in 1:L){
		 for(k in 1:6){
			 threshloc <- which(input$Stats[[i]][[k]]$Thresholds$Totals$Thresholds == threshold)
			 syear <- strsplit(names(input$Stats)[[i]], " - ")[[1]][[1]]
			 daydf[[k]]$dayfracabove[[i]] <- input$Stats[[i]][[k]]$Thresholds$Totals$FracDaysAbove[[threshloc]]
			 daydf[[k]]$startyear[[i]] <- syear
			 voldf[[k]]$volfracabove[[i]] <- input$Stats[[i]][[k]]$Thresholds$Totals$Frac_Abv[[threshloc]]
			 voldf[[k]]$volabvMAF[[i]] <- input$Stats[[i]][[k]]$Thresholds$Totals$Volume_Abv_acfte6[[threshloc]]
			 voldf[[k]]$startyear[[i]] <- syear
	 	}
	 }
	 
	 for(k in 1:6){
		 if(any(is.na(daydf[[k]]))){
		 daydf[[k]] <- daydf[[k]][-which(is.na(daydf[[k]])),]
	 	} else {}
		if(any(is.na(voldf[[k]]))){
		 voldf[[k]] <- voldf[[k]][-which(is.na(voldf[[k]])),]
	 	} else{}
	 }

	 out <- list(daydf=daydf, voldf=voldf)
	 return(out)
	 
 }
	
