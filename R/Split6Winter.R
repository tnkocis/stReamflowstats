# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


#split into years of 6 month winter data
Split6Winter <- function(input){
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

	wint_by_year <- list()
	for (i in seq(1,length(years)-1,1)){
		wint_by_year[[i]] <- filter(input, (month >10 & year == years[i])| (month <5 & year == years[i]+1))
		names(wint_by_year)[i] <- paste(years[i],"-",years[i]+1)
		wint_by_year[[i]]["Discharge_ft3_day"] <- as.numeric(wint_by_year[[i]]$Discharge_cfs)*86400
		wint_by_year[[i]]["Discharge_acft_day"] <- wint_by_year[[i]]$Discharge_ft3_day*(2.29568411e-5)
		wint_by_year[[i]]["Discharge_acfte6_day"] <- wint_by_year[[i]]$Discharge_acft_day*1e-6
	}
	
	stats <- list()
	for (i in 1:length(wint_by_year)){
		stats[[i]] <- list()
		stats[[i]][["Values"]] <-list()
		stats[[i]][["Quantiles"]] <-list()
		stats[[i]][["Values"]]["Total_Q_acfte6"] <- sum(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Quantiles"]][["Quantiles_acfte6"]] <-quantile(wint_by_year[[i]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		stats[[i]][["Values"]]["Mean_acfte6"] <- mean(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["Median_acfte6"] <- median(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["min_acfte6"] <- min(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["max_acfte6"] <- max(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]][["max_date"]] <- wint_by_year[[i]][["Date"]][which(wint_by_year[[i]][["Discharge_acfte6_day"]]==stats[[i]][["Values"]]["max_acfte6"])]
		stats[[i]][["Values"]][["min_date"]] <- wint_by_year[[i]][["Date"]][which(wint_by_year[[i]][["Discharge_acfte6_day"]]==stats[[i]][["Values"]]["min_acfte6"])]
		names(stats)[i] <- names(wint_by_year)[i]
	}
	
	all <- list()
	all[["Data"]]["Discharge_acfte6_day"] <- wint_by_year[[1]]["Discharge_acfte6_day"]
	all[["Data"]]["Date"] <- wint_by_year[[1]]["Date"]
	for (i in 2:length(wint_by_year)){
		all[["Data"]][["Discharge_acfte6_day"]] <- append(all[["Data"]][["Discharge_acfte6_day"]], wint_by_year[[i]][["Discharge_acfte6_day"]], after=length(all[["Data"]][["Discharge_acfte6_day"]]))
		all[["Data"]][["Date"]] <- append(all[["Data"]][["Date"]], wint_by_year[[i]][["Date"]], after=length(all[["Data"]][["Date"]]))
	}
	all[["Stats"]] <-list()
	all[["Stats"]]["Mean_acfte6"] <- mean(all[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
	all[["Stats"]]["Median_acfte6"] <- median(all[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
	all[["Stats"]]["min_acfte6"] <- min(all[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
	all[["Stats"]]["max_acfte6"] <- max(all[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
	all[["Stats"]][["min_date"]] <- all[["Data"]][["Date"]][which(all[["Data"]][["Discharge_acfte6_day"]]==all[["Stats"]]["min_acfte6"])]
	all[["Stats"]][["max_date"]] <- all[["Data"]][["Date"]][which(all[["Data"]][["Discharge_acfte6_day"]]==all[["Stats"]]["max_acfte6"])]
	all[["Stats"]][["Quantiles_acfte6"]] <- quantile(all[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	
	total <- list(Data=wint_by_year, Stats=stats, All=all)
	return(total)
}

