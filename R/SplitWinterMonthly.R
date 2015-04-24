# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


SplitWinterMonthly <- function(input){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	year <- as.numeric(format(input$raw$Date,"%Y"))
	month <- as.numeric(format(input$raw$Date,"%m"))
	day <- as.numeric(format(input$raw$Date,"%d"))
	years <- seq(min(year), max(year), by=1)
	
	wint_by_year <- list()
	months <- c(11,12,1,2,3,4)
	month_names <- c("NOV","DEC","JAN","FEB","MAR","APR")
	for (i in seq(1,length(years)-1,1)){
		wint_by_year[[i]] <- vector("list",length=6)
		for (n in 1:6){
			wint_by_year[[i]][[n]] <- list()
			year_select <- c(1)
			if(months[n]>10){
				year_select <- years[i]
			} else {
				year_select <- years[i]+1
			}
			wint_by_year[[i]][[n]] <- filter(input$raw, (month == months[n] & year == year_select))
			wint_by_year[[i]][[n]]["Discharge_ft3_day"] <- as.numeric(wint_by_year[[i]][[n]]$Discharge_cfs)*86400
			wint_by_year[[i]][[n]]["Discharge_acft_day"] <- wint_by_year[[i]][[n]]$Discharge_ft3_day*(2.29568411e-5)
			wint_by_year[[i]][[n]]["Discharge_acfte6_day"] <- wint_by_year[[i]][[n]]$Discharge_acft_day*1e-6
			names(wint_by_year[[i]])[n] <- month_names[n]
			names(wint_by_year)[i] <- paste(years[i],"-",years[i]+1)
		
		}
		}
	
	stats <- list()
	for (i in 1:length(wint_by_year)){
		stats[[i]] <- vector("list",length=6)
		for (n in 1:6){
			stats[[i]][[n]] <-list()
			stats[[i]][[n]][["Values"]] <-list()
			stats[[i]][[n]][["Quantiles"]] <-list()
			stats[[i]][[n]][["Values"]]["Total_Q_acfte6"] <- sum(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]])
			stats[[i]][[n]][["Quantiles"]][["Quantiles_acfte6"]] <-quantile(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95))
			stats[[i]][[n]][["Values"]]["Mean_acfte6"] <- mean(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]])
			stats[[i]][[n]][["Values"]]["Median_acfte6"] <- median(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]])
			stats[[i]][[n]][["Values"]]["min_acfte6"] <- min(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]])
			stats[[i]][[n]][["Values"]]["max_acfte6"] <- max(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]])
			names(stats[[i]])[n] <- month_names[n]
			names(stats)[i] <- names(wint_by_year)[i]
	}
	}
	total <- list(Data=wint_by_year, Stats=stats)
	return(total)
}
