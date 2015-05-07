# TODO: Add comment
# 
# Author: tnkocis
###############################################################################


SplitHydroYear <- function(input){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	year <- as.numeric(format(input$Date,"%Y"))
	month <- as.numeric(format(input$Date,"%m"))
	day <- as.numeric(format(input$Date,"%d"))
	years <- seq(min(year), max(year), by=1)
	
	by_year <- list()
	for (i in seq(1,length(years)-1,1)){
		by_year[[i]] <- filter(input, (month >9 & year == years[i])| (month <10 & year == years[i]+1))
		names(by_year)[i] <- paste(years[i],"-",years[i]+1)
		by_year[[i]]["Discharge_ft3_day"] <- as.numeric(by_year[[i]]$Discharge_cfs)*86400
		by_year[[i]]["Discharge_acft_day"] <- by_year[[i]]$Discharge_ft3_day*(2.29568411e-5)
		by_year[[i]]["Discharge_acfte6_day"] <- by_year[[i]]$Discharge_acft_day*1e-6
	}
	
	stats <- list()
	for (i in 1:length(by_year)){
		stats[[i]] <- list()
		stats[[i]][["Values"]] <-list()
		stats[[i]][["Quantiles"]] <-list()
		stats[[i]][["Values"]]["Total_Q_acfte6"] <- sum(by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Quantiles"]][["Quantiles_acfte6"]] <-quantile(by_year[[i]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95), na.rm=TRUE )
		stats[[i]][["Values"]]["Mean_acfte6"] <- mean(by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["Median_acfte6"] <- median(by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["min_acfte6"] <- min(by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["max_acfte6"] <- max(by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		names(stats)[i] <- names(by_year)[i]
	}

	summary <- list()
	summary[["total_Q_yearly"]] <- numeric(length(stats))
	for (i in 1: length(stats)){
		summary$total_Q_yearly[i] <- stats[[i]][["Values"]][["Total_Q_acfte6"]]
	}
	summary$mean_Q <- mean(summary$total_Q_yearly, na.rm=TRUE)
	summary$std_dev_mean_Q <- sd(summary$total_Q_yearly, na.rm=TRUE)

	total <- list(Data=by_year, Stats=stats, Summary=summary)
	return(total)
}
