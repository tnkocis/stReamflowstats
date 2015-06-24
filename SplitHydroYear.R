# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


#split into years of hydro year data
SplitHydroYear <- function(input, index, percentiles){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	if (missing(index))
		stop("Index data required.")
	if (missing(percentiles))
		stop("Percentile data required.")
	year <- as.numeric(format(input$Date,"%Y"))
	month <- as.numeric(format(input$Date,"%m"))
	day <- as.numeric(format(input$Date,"%d"))
	years <- seq(min(year), max(year), by=1)
	
	wint_by_year <- list()
	for (i in seq(1,length(years)-1,1)){
		wint_by_year[[i]] <- filter(input, (month >9 & year == years[i])| (month <10 & year == years[i]+1))
		names(wint_by_year)[i] <- paste(years[i],"-",years[i]+1)
		wint_by_year[[i]]["Discharge_ft3_day"] <- as.numeric(wint_by_year[[i]]$Discharge_cfs)*86400
		wint_by_year[[i]]["Discharge_acft_day"] <- wint_by_year[[i]]$Discharge_ft3_day*(2.29568411e-5)
		wint_by_year[[i]]["Discharge_acfte6_day"] <- wint_by_year[[i]]$Discharge_acft_day*1e-6
	}
	
	stats <- list()
	for (i in 1:length(wint_by_year)){
		stats[[i]] <- list()
		stats[[i]][["Values"]] <-list()
		stats[[i]][["Values"]]["Total_Q_acfte6"] <- sum(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["Mean_acfte6"] <- mean(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["Median_acfte6"] <- median(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["min_acfte6"] <- min(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]]["max_acfte6"] <- max(wint_by_year[[i]][["Discharge_acfte6_day"]], na.rm=TRUE)
		stats[[i]][["Values"]][["max_date"]] <- wint_by_year[[i]][["Date"]][which(wint_by_year[[i]][["Discharge_acfte6_day"]]==stats[[i]][["Values"]]["max_acfte6"])]
		stats[[i]][["Values"]][["min_date"]] <- wint_by_year[[i]][["Date"]][which(wint_by_year[[i]][["Discharge_acfte6_day"]]==stats[[i]][["Values"]]["min_acfte6"])]
		
		stats[[i]][["Thresholds"]] <- list()
		stats[[i]][["Thresholds"]][["coded"]] <- list()
		#stats[[i]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(wint_by_year[[i]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		for (k in 1:length(percentiles)){
			for (n in 1:length(wint_by_year[[i]][["Discharge_acfte6_day"]])){
				if(is.na(wint_by_year[[i]][["Discharge_acfte6_day"]][[n]])){
					stats[[i]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				} else if (wint_by_year[[i]][["Discharge_acfte6_day"]][[n]] >= percentiles[[k]]){
					stats[[i]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 1
				} else if (wint_by_year[[i]][["Discharge_acfte6_day"]][[n]] < percentiles[[k]]){
					stats[[i]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 0	
				} else {
					stats[[i]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				}
			}
		}
		
#		stats[[i]][["Thresholds"]] <- list()
#		stats[[i]][["Thresholds"]][["coded"]] <- list()
#		stats[[i]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(wint_by_year[[i]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
#		for (k in 1:length(stats[[i]][["Thresholds"]][["Quantiles_acfte6"]])){
#			for (n in 1:length(wint_by_year[[i]][["Discharge_acfte6_day"]])){
#				if(wint_by_year[[i]][["Discharge_acfte6_day"]][[n]] >= stats[[i]][["Thresholds"]][["Quantiles_acfte6"]][[k]]){
#					stats[[i]][["Thresholds"]][["coded"]][[paste0("P",names(stats[[i]][["Thresholds"]][["Quantiles_acfte6"]][k]))]][[n]] <- 1
#				} else if (wint_by_year[[i]][["Discharge_acfte6_day"]][[n]] < stats[[i]][["Thresholds"]][["Quantiles_acfte6"]][[k]]){
#					stats[[i]][["Thresholds"]][["coded"]][[paste0("P",names(stats[[i]][["Thresholds"]][["Quantiles_acfte6"]][k]))]][[n]] <- 0	
#				} else {
#					stats[[i]][["Thresholds"]][["coded"]][[paste0("P",names(stats[[i]][["Thresholds"]][["Quantiles_acfte6"]][k]))]][[n]] <- NA
#				}
#			}
#		}
		
		
		stats[[i]][["Thresholds"]][["Totals"]] <- list()
		stats[[i]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (k in 1:length(stats[[i]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			stats[[i]][["Thresholds"]][["Totals"]][["DaysAbove"]][[k]] <- sum(stats[[i]][["Thresholds"]][["coded"]][[k]], na.rm=TRUE)
			stats[[i]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[k]] <- sum(stats[[i]][["Thresholds"]][["coded"]][[k]], na.rm=TRUE)/(length(stats[[i]][["Thresholds"]][["coded"]][[k]])-sum(is.na(stats[[i]][["Thresholds"]][["coded"]][[k]])))
			stats[[i]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[k]] <- sum(wint_by_year[[i]][["Discharge_acfte6_day"]][which(stats[[i]][["Thresholds"]][["coded"]][[k]]==1)], na.rm=TRUE)
			stats[[i]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[k]] <- stats[[i]][["Values"]][["Total_Q_acfte6"]]
			stats[[i]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[k]] <- stats[[i]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[k]]/stats[[i]][["Values"]][["Total_Q_acfte6"]]
		}
		stats[[i]][["Thresholds"]][["Totals"]] <- as.data.frame(stats[[i]][["Thresholds"]][["Totals"]])
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
	all[["Stats"]][["Total_Q_acfte6"]] <- sum(all[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
	all[["Stats"]][["Thresholds"]] <- list()
	all[["Stats"]][["Thresholds"]][["coded"]] <- list()
#	all[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(all[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
#	for (i in 1:length(all[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
#		for (n in 1:length(all[["Data"]][["Discharge_acfte6_day"]])){
#			if(all[["Data"]][["Discharge_acfte6_day"]][[n]] >= all[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
#				all[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(all[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
#			} else if (all[["Data"]][["Discharge_acfte6_day"]][[n]] < all[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
#				all[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(all[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
#			} else {
#				all[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(all[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
#			}
#		}
#	}
	
	for (k in 1:length(percentiles)){
		for (n in 1:length(all[["Data"]][["Discharge_acfte6_day"]])){
			if(is.na(all[["Data"]][["Discharge_acfte6_day"]][[n]])){
				all[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
			} else if (all[["Data"]][["Discharge_acfte6_day"]][[n]] >= percentiles[[k]]){
				all[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 1
			} else if (all[["Data"]][["Discharge_acfte6_day"]][[n]] < percentiles[[k]]){
				all[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 0	
			} else {
				all[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
			}
		}
	}
	all[["Stats"]][["Thresholds"]][["Totals"]] <- list()
	all[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
	for (i in 1:length(all[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
		all[["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(all[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)
		all[["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(all[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)/(length(all[["Stats"]][["Thresholds"]][["coded"]][[i]])-sum(is.na(all[["Stats"]][["Thresholds"]][["coded"]][[i]])))
		all[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(all[["Data"]][["Discharge_acfte6_day"]][which(all[["Stats"]][["Thresholds"]][["coded"]][[i]]==1)], na.rm=TRUE)
		all[["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-all[["Stats"]][["Total_Q_acfte6"]]
		all[["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- all[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/all[["Stats"]][["Total_Q_acfte6"]]
	}
	all[["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(all[["Stats"]][["Thresholds"]][["Totals"]])
	all[["Data"]] <- as.data.frame(all[["Data"]])
	
	XC <- list()
	XCyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==1)])
	if(length(XCyears) == 0){
		XC <- "No C Years in Input Dataset"
	} else {
		XC[["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XCyears[[1]]]][["Discharge_acfte6_day"]]
		XC[["Data"]][["Date"]] <- wint_by_year[[XCyears[[1]]]][["Date"]]
		for (i in 2:length(XCyears) ){
			XC[["Data"]][["Discharge_acfte6_day"]] <- append(XC[["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XCyears[[i]]]][["Discharge_acfte6_day"]], after=length(XC[["Data"]][["Discharge_acfte6_day"]]))
			XC[["Data"]][["Date"]] <-  append(XC[["Data"]][["Date"]], wint_by_year[[XCyears[[i]]]][["Date"]], after=length(XC[["Data"]][["Date"]]))
		}
		XC[["Stats"]] <-list()
		XC[["Stats"]]["Mean_acfte6"] <- mean(XC[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[["Stats"]]["Median_acfte6"] <- median(XC[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[["Stats"]]["min_acfte6"] <- min(XC[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[["Stats"]]["max_acfte6"] <- max(XC[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[["Stats"]][["min_date"]] <- XC[["Data"]][["Date"]][which(XC[["Data"]][["Discharge_acfte6_day"]]==XC[["Stats"]]["min_acfte6"])]
		XC[["Stats"]][["max_date"]] <- XC[["Data"]][["Date"]][which(XC[["Data"]][["Discharge_acfte6_day"]]==XC[["Stats"]]["max_acfte6"])]
		XC[["Stats"]][["Total_Q_acfte6"]] <- sum(XC[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[["Stats"]][["Thresholds"]] <- list()
		XC[["Stats"]][["Thresholds"]][["coded"]] <- list()
		#	XC[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XC[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		#	for (i in 1:length(XC[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
		#		for (n in 1:length(XC[["Data"]][["Discharge_acfte6_day"]])){
		#			if(XC[["Data"]][["Discharge_acfte6_day"]][[n]] >= XC[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XC[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XC[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
		#			} else if (XC[["Data"]][["Discharge_acfte6_day"]][[n]] < XC[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XC[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XC[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
		#			} else {
		#				XC[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XC[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
		#			}
		#		}
		#	}
		
		for (k in 1:length(percentiles)){
			for (n in 1:length(XC[["Data"]][["Discharge_acfte6_day"]])){
				if(is.na(XC[["Data"]][["Discharge_acfte6_day"]][[n]])){
					XC[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				} else if (XC[["Data"]][["Discharge_acfte6_day"]][[n]] >= percentiles[[k]]){
					XC[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 1
				} else if (XC[["Data"]][["Discharge_acfte6_day"]][[n]] < percentiles[[k]]){
					XC[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 0	
				} else {
					XC[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				}
			}
		}
		XC[["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XC[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XC[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XC[["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XC[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)
			XC[["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XC[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)/(length(XC[["Stats"]][["Thresholds"]][["coded"]][[i]])-sum(is.na(XC[["Stats"]][["Thresholds"]][["coded"]][[i]])))
			XC[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XC[["Data"]][["Discharge_acfte6_day"]][which(XC[["Stats"]][["Thresholds"]][["coded"]][[i]]==1)], na.rm=TRUE)
			XC[["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XC[["Stats"]][["Total_Q_acfte6"]]
			XC[["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XC[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XC[["Stats"]][["Total_Q_acfte6"]]
		}
		XC[["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XC[["Stats"]][["Thresholds"]][["Totals"]])
		XC[["Data"]] <- as.data.frame(XC[["Data"]])
	}
	
	XD <- list()
	XDyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==2)])
	if(length(XDyears) == 0){
		XD <- "No D Years in Input Dataset"
	} else {
		XD[["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XDyears[[1]]]][["Discharge_acfte6_day"]]
		XD[["Data"]][["Date"]] <- wint_by_year[[XDyears[[1]]]][["Date"]]
		for (i in 2:length(XDyears) ){
			XD[["Data"]][["Discharge_acfte6_day"]] <- append(XD[["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XDyears[[i]]]][["Discharge_acfte6_day"]], after=length(XD[["Data"]][["Discharge_acfte6_day"]]))
			XD[["Data"]][["Date"]] <-  append(XD[["Data"]][["Date"]], wint_by_year[[XDyears[[i]]]][["Date"]], after=length(XD[["Data"]][["Date"]]))
		}
		XD[["Stats"]] <-list()
		XD[["Stats"]]["Mean_acfte6"] <- mean(XD[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[["Stats"]]["Median_acfte6"] <- median(XD[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[["Stats"]]["min_acfte6"] <- min(XD[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[["Stats"]]["max_acfte6"] <- max(XD[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[["Stats"]][["min_date"]] <- XD[["Data"]][["Date"]][which(XD[["Data"]][["Discharge_acfte6_day"]]==XD[["Stats"]]["min_acfte6"])]
		XD[["Stats"]][["max_date"]] <- XD[["Data"]][["Date"]][which(XD[["Data"]][["Discharge_acfte6_day"]]==XD[["Stats"]]["max_acfte6"])]
		XD[["Stats"]][["Total_Q_acfte6"]] <- sum(XD[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[["Stats"]][["Thresholds"]] <- list()
		XD[["Stats"]][["Thresholds"]][["coded"]] <- list()
		#	XD[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XD[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		#	for (i in 1:length(XD[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
		#		for (n in 1:length(XD[["Data"]][["Discharge_acfte6_day"]])){
		#			if(XD[["Data"]][["Discharge_acfte6_day"]][[n]] >= XD[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XD[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XD[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
		#			} else if (XD[["Data"]][["Discharge_acfte6_day"]][[n]] < XD[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XD[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XD[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
		#			} else {
		#				XD[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XD[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
		#			}
		#		}
		#	}
		
		for (k in 1:length(percentiles)){
			for (n in 1:length(XD[["Data"]][["Discharge_acfte6_day"]])){
				if(is.na(XD[["Data"]][["Discharge_acfte6_day"]][[n]])){
					XD[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				} else if (XD[["Data"]][["Discharge_acfte6_day"]][[n]] >= percentiles[[k]]){
					XD[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 1
				} else if (XD[["Data"]][["Discharge_acfte6_day"]][[n]] < percentiles[[k]]){
					XD[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 0	
				} else {
					XD[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				}
			}
		}
		XD[["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XD[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XD[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XD[["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XD[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)
			XD[["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XD[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)/(length(XD[["Stats"]][["Thresholds"]][["coded"]][[i]])-sum(is.na(XD[["Stats"]][["Thresholds"]][["coded"]][[i]])))
			XD[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XD[["Data"]][["Discharge_acfte6_day"]][which(XD[["Stats"]][["Thresholds"]][["coded"]][[i]]==1)], na.rm=TRUE)
			XD[["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XD[["Stats"]][["Total_Q_acfte6"]]
			XD[["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XD[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XD[["Stats"]][["Total_Q_acfte6"]]
		}
		XD[["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XD[["Stats"]][["Thresholds"]][["Totals"]])
		XD[["Data"]] <- as.data.frame(XD[["Data"]])
	}
	
	XBN <- list()
	XBNyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==3)])
	if(length(XBNyears) == 0){
		XBN <- "No BN Years in Input Dataset"
	} else {
		XBN[["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XBNyears[[1]]]][["Discharge_acfte6_day"]]
		XBN[["Data"]][["Date"]] <- wint_by_year[[XBNyears[[1]]]][["Date"]]
		for (i in 2:length(XBNyears) ){
			XBN[["Data"]][["Discharge_acfte6_day"]] <- append(XBN[["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XBNyears[[i]]]][["Discharge_acfte6_day"]], after=length(XBN[["Data"]][["Discharge_acfte6_day"]]))
			XBN[["Data"]][["Date"]] <-  append(XBN[["Data"]][["Date"]], wint_by_year[[XBNyears[[i]]]][["Date"]], after=length(XBN[["Data"]][["Date"]]))
		}
		XBN[["Stats"]] <-list()
		XBN[["Stats"]]["Mean_acfte6"] <- mean(XBN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[["Stats"]]["Median_acfte6"] <- median(XBN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[["Stats"]]["min_acfte6"] <- min(XBN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[["Stats"]]["max_acfte6"] <- max(XBN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[["Stats"]][["min_date"]] <- XBN[["Data"]][["Date"]][which(XBN[["Data"]][["Discharge_acfte6_day"]]==XBN[["Stats"]]["min_acfte6"])]
		XBN[["Stats"]][["max_date"]] <- XBN[["Data"]][["Date"]][which(XBN[["Data"]][["Discharge_acfte6_day"]]==XBN[["Stats"]]["max_acfte6"])]
		XBN[["Stats"]][["Total_Q_acfte6"]] <- sum(XBN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[["Stats"]][["Thresholds"]] <- list()
		XBN[["Stats"]][["Thresholds"]][["coded"]] <- list()
		#	XBN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XBN[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		#	for (i in 1:length(XBN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
		#		for (n in 1:length(XBN[["Data"]][["Discharge_acfte6_day"]])){
		#			if(XBN[["Data"]][["Discharge_acfte6_day"]][[n]] >= XBN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XBN[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XBN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
		#			} else if (XBN[["Data"]][["Discharge_acfte6_day"]][[n]] < XBN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XBN[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XBN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
		#			} else {
		#				XBN[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XBN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
		#			}
		#		}
		#	}
		
		for (k in 1:length(percentiles)){
			for (n in 1:length(XBN[["Data"]][["Discharge_acfte6_day"]])){
				if(is.na(XBN[["Data"]][["Discharge_acfte6_day"]][[n]])){
					XBN[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				} else if (XBN[["Data"]][["Discharge_acfte6_day"]][[n]] >= percentiles[[k]]){
					XBN[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 1
				} else if (XBN[["Data"]][["Discharge_acfte6_day"]][[n]] < percentiles[[k]]){
					XBN[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 0	
				} else {
					XBN[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				}
			}
		}
		XBN[["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XBN[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XBN[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XBN[["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XBN[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)
			XBN[["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XBN[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)/(length(XBN[["Stats"]][["Thresholds"]][["coded"]][[i]])-sum(is.na(XBN[["Stats"]][["Thresholds"]][["coded"]][[i]])))
			XBN[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XBN[["Data"]][["Discharge_acfte6_day"]][which(XBN[["Stats"]][["Thresholds"]][["coded"]][[i]]==1)], na.rm=TRUE)
			XBN[["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XBN[["Stats"]][["Total_Q_acfte6"]]
			XBN[["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XBN[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XBN[["Stats"]][["Total_Q_acfte6"]]
		}
		XBN[["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XBN[["Stats"]][["Thresholds"]][["Totals"]])
		XBN[["Data"]] <- as.data.frame(XBN[["Data"]])
	}
	
	XAN <- list()
	XANyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==4)])
	if(length(XANyears) == 0){
		XAN <- "No AN Years in Input Dataset"
	} else {
		XAN[["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XANyears[[1]]]][["Discharge_acfte6_day"]]
		XAN[["Data"]][["Date"]] <- wint_by_year[[XANyears[[1]]]][["Date"]]
		for (i in 2:length(XANyears) ){
			XAN[["Data"]][["Discharge_acfte6_day"]] <- append(XAN[["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XANyears[[i]]]][["Discharge_acfte6_day"]], after=length(XAN[["Data"]][["Discharge_acfte6_day"]]))
			XAN[["Data"]][["Date"]] <-  append(XAN[["Data"]][["Date"]], wint_by_year[[XANyears[[i]]]][["Date"]], after=length(XAN[["Data"]][["Date"]]))
		}
		XAN[["Stats"]] <-list()
		XAN[["Stats"]]["Mean_acfte6"] <- mean(XAN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[["Stats"]]["Median_acfte6"] <- median(XAN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[["Stats"]]["min_acfte6"] <- min(XAN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[["Stats"]]["max_acfte6"] <- max(XAN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[["Stats"]][["min_date"]] <- XAN[["Data"]][["Date"]][which(XAN[["Data"]][["Discharge_acfte6_day"]]==XAN[["Stats"]]["min_acfte6"])]
		XAN[["Stats"]][["max_date"]] <- XAN[["Data"]][["Date"]][which(XAN[["Data"]][["Discharge_acfte6_day"]]==XAN[["Stats"]]["max_acfte6"])]
		XAN[["Stats"]][["Total_Q_acfte6"]] <- sum(XAN[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[["Stats"]][["Thresholds"]] <- list()
		XAN[["Stats"]][["Thresholds"]][["coded"]] <- list()
		#	XAN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XAN[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		#	for (i in 1:length(XAN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
		#		for (n in 1:length(XAN[["Data"]][["Discharge_acfte6_day"]])){
		#			if(XAN[["Data"]][["Discharge_acfte6_day"]][[n]] >= XAN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XAN[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XAN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
		#			} else if (XAN[["Data"]][["Discharge_acfte6_day"]][[n]] < XAN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XAN[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XAN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
		#			} else {
		#				XAN[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XAN[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
		#			}
		#		}
		#	}
		
		for (k in 1:length(percentiles)){
			for (n in 1:length(XAN[["Data"]][["Discharge_acfte6_day"]])){
				if(is.na(XAN[["Data"]][["Discharge_acfte6_day"]][[n]])){
					XAN[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				} else if (XAN[["Data"]][["Discharge_acfte6_day"]][[n]] >= percentiles[[k]]){
					XAN[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 1
				} else if (XAN[["Data"]][["Discharge_acfte6_day"]][[n]] < percentiles[[k]]){
					XAN[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 0	
				} else {
					XAN[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				}
			}
		}
		XAN[["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XAN[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XAN[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XAN[["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XAN[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)
			XAN[["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XAN[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)/(length(XAN[["Stats"]][["Thresholds"]][["coded"]][[i]])-sum(is.na(XAN[["Stats"]][["Thresholds"]][["coded"]][[i]])))
			XAN[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XAN[["Data"]][["Discharge_acfte6_day"]][which(XAN[["Stats"]][["Thresholds"]][["coded"]][[i]]==1)], na.rm=TRUE)
			XAN[["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XAN[["Stats"]][["Total_Q_acfte6"]]
			XAN[["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XAN[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XAN[["Stats"]][["Total_Q_acfte6"]]
		}
		XAN[["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XAN[["Stats"]][["Thresholds"]][["Totals"]])
		XAN[["Data"]] <- as.data.frame(XAN[["Data"]])
	}
	
	XW <- list()
	XWyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==5)])
	if(length(XWyears) == 0){
		XW <- "No W Years in Input Dataset"
	} else {
		XW[["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XWyears[[1]]]][["Discharge_acfte6_day"]]
		XW[["Data"]][["Date"]] <- wint_by_year[[XWyears[[1]]]][["Date"]]
		for (i in 2:length(XWyears) ){
			XW[["Data"]][["Discharge_acfte6_day"]] <- append(XW[["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XWyears[[i]]]][["Discharge_acfte6_day"]], after=length(XW[["Data"]][["Discharge_acfte6_day"]]))
			XW[["Data"]][["Date"]] <-  append(XW[["Data"]][["Date"]], wint_by_year[[XWyears[[i]]]][["Date"]], after=length(XW[["Data"]][["Date"]]))
		}
		XW[["Stats"]] <-list()
		XW[["Stats"]]["Mean_acfte6"] <- mean(XW[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[["Stats"]]["Median_acfte6"] <- median(XW[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[["Stats"]]["min_acfte6"] <- min(XW[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[["Stats"]]["max_acfte6"] <- max(XW[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[["Stats"]][["min_date"]] <- XW[["Data"]][["Date"]][which(XW[["Data"]][["Discharge_acfte6_day"]]==XW[["Stats"]]["min_acfte6"])]
		XW[["Stats"]][["max_date"]] <- XW[["Data"]][["Date"]][which(XW[["Data"]][["Discharge_acfte6_day"]]==XW[["Stats"]]["max_acfte6"])]
		XW[["Stats"]][["Total_Q_acfte6"]] <- sum(XW[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[["Stats"]][["Thresholds"]] <- list()
		XW[["Stats"]][["Thresholds"]][["coded"]] <- list()
		#	XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XW[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		#	for (i in 1:length(XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
		#		for (n in 1:length(XW[["Data"]][["Discharge_acfte6_day"]])){
		#			if(XW[["Data"]][["Discharge_acfte6_day"]][[n]] >= XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XW[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
		#			} else if (XW[["Data"]][["Discharge_acfte6_day"]][[n]] < XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
		#				XW[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
		#			} else {
		#				XW[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
		#			}
		#		}
		#	}
		
		for (k in 1:length(percentiles)){
			for (n in 1:length(XW[["Data"]][["Discharge_acfte6_day"]])){
				if(is.na(XW[["Data"]][["Discharge_acfte6_day"]][[n]])){
					XW[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				} else if (XW[["Data"]][["Discharge_acfte6_day"]][[n]] >= percentiles[[k]]){
					XW[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 1
				} else if (XW[["Data"]][["Discharge_acfte6_day"]][[n]] < percentiles[[k]]){
					XW[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 0	
				} else {
					XW[["Stats"]][["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
				}
			}
		}
		XW[["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XW[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XW[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XW[["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XW[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)
			XW[["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XW[["Stats"]][["Thresholds"]][["coded"]][[i]], na.rm=TRUE)/(length(XW[["Stats"]][["Thresholds"]][["coded"]][[i]])-sum(is.na(XW[["Stats"]][["Thresholds"]][["coded"]][[i]])))
			XW[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XW[["Data"]][["Discharge_acfte6_day"]][which(XW[["Stats"]][["Thresholds"]][["coded"]][[i]]==1)], na.rm=TRUE)
			XW[["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XW[["Stats"]][["Total_Q_acfte6"]]
			XW[["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XW[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XW[["Stats"]][["Total_Q_acfte6"]]
		}
		XW[["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XW[["Stats"]][["Thresholds"]][["Totals"]])
		XW[["Data"]] <- as.data.frame(XW[["Data"]])
	}
	
	total <- list(Data=wint_by_year, Stats=stats, All=all, C=XC, D=XD, BN=XBN, AN=XAN, W=XW)
	return(total)
}
