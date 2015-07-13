# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


FreqAnalysis <- function(input, vday){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
#
	if (missing(input))
		stop("Input data is required, by hydrologic year.")
	if (missing(vday))
		stop("Vector containing days to max required")
	
	z <- length(vday)
	daysmax <- vector("list", z)
	for(n in 1:z){
		daysmax[[n]] <- list()
		daysmax[[n]]$Date <- as.Date(rep(NA, length.out=length(input$Data)))
		daysmax[[n]]$Discharge_maf <- rep(NA, length.out=length(input$Data))
	}
	
	for(i in 1:length(input$Data)){
		ts.zoo <- zoo(input$Data[[i]]$Discharge_maf, input$Data[[i]]$Date)
		for(n in 1:z){
			ts.zoo.roll <- rollmean(ts.zoo, vday[[n]], fill=NA, align=c("center"))
			daysmax[[n]]$Discharge_maf[[i]]<- coredata(ts.zoo.roll[which.max(ts.zoo.roll)])
			daysmax[[n]]$Date[[i]]<- index(ts.zoo.roll[which.max(ts.zoo.roll)])
		}
	}
	
	for(n in 1:z){
		daysmax[[n]] <- as.data.frame(daysmax[[n]])
		names(daysmax)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
	}
	
	return(daysmax)
}