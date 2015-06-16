# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


SplitWinterMonthly <- function(input, index){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	if (missing(index))
		stop("Index data is required.")
	year <- as.numeric(format(input$Date,"%Y"))
	month <- as.numeric(format(input$Date,"%m"))
	day <- as.numeric(format(input$Date,"%d"))
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
			wint_by_year[[i]][[n]] <- filter(input, (month == months[n] & year == year_select))
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
			stats[[i]][[n]][["Values"]]["Total_Q_acfte6"] <- sum(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]], na.rm=TRUE)
			stats[[i]][[n]][["Values"]]["Mean_acfte6"] <- mean(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]], na.rm=TRUE)
			stats[[i]][[n]][["Values"]]["Median_acfte6"] <- median(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]], na.rm=TRUE)
			stats[[i]][[n]][["Values"]]["min_acfte6"] <- min(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]], na.rm=TRUE)
			stats[[i]][[n]][["Values"]]["max_acfte6"] <- max(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]], na.rm=TRUE)
			stats[[i]][[n]][["Thresholds"]] <- list()
			stats[[i]][[n]][["Thresholds"]][["coded"]] <- list()
			stats[[i]][[n]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
			for (k in 1:length(stats[[i]][[n]][["Thresholds"]][["Quantiles_acfte6"]])){
				for (m in 1:length(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]])){
					if(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]][[m]] >= stats[[i]][[n]][["Thresholds"]][["Quantiles_acfte6"]][[k]]){
						stats[[i]][[n]][["Thresholds"]][["coded"]][[paste0("P",names(stats[[i]][[n]][["Thresholds"]][["Quantiles_acfte6"]][k]))]][[m]] <- 1
					} else if (wint_by_year[[i]][[n]][["Discharge_acfte6_day"]][[m]] < stats[[i]][[n]][["Thresholds"]][["Quantiles_acfte6"]][[k]]){
						stats[[i]][[n]][["Thresholds"]][["coded"]][[paste0("P",names(stats[[i]][[n]][["Thresholds"]][["Quantiles_acfte6"]][k]))]][[m]] <- 0	
					} else {
						stats[[i]][[n]][["Thresholds"]][["coded"]][[paste0("P",names(stats[[i]][[n]][["Thresholds"]][["Quantiles_acfte6"]][k]))]][[m]] <- NA
					}
				}
			}
			stats[[i]][[n]][["Thresholds"]][["Totals"]] <- list()
			stats[[i]][[n]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
			for (k in 1:length(stats[[i]][[n]][["Thresholds"]][["Totals"]][["Thresholds"]])){
				stats[[i]][[n]][["Thresholds"]][["Totals"]][["DaysAbove"]][[k]] <- sum(stats[[i]][[n]][["Thresholds"]][["coded"]][[k]])
				stats[[i]][[n]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[k]] <- sum(stats[[i]][[n]][["Thresholds"]][["coded"]][[k]])/length(stats[[i]][[n]][["Thresholds"]][["coded"]][[k]])
				stats[[i]][[n]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[k]] <- sum(wint_by_year[[i]][[n]][["Discharge_acfte6_day"]][which(stats[[i]][[n]][["Thresholds"]][["coded"]][[k]]==1)])
				stats[[i]][[n]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[k]] <- stats[[i]][[n]][["Values"]][["Total_Q_acfte6"]]
				stats[[i]][[n]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[k]] <- stats[[i]][[n]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[k]]/stats[[i]][[n]][["Values"]][["Total_Q_acfte6"]]
			}
			stats[[i]][[n]][["Thresholds"]][["Totals"]] <- as.data.frame(stats[[i]][[n]][["Thresholds"]][["Totals"]])
			names(stats[[i]])[n] <- month_names[n]
			names(stats)[i] <- names(wint_by_year)[i]
	}
	}
	
	all <- list()
	for(k in 1:6){
		all[[k]] <- list()
		all[[k]][["Data"]]["Discharge_acfte6_day"] <- wint_by_year[[1]][[k]]["Discharge_acfte6_day"]
		all[[k]][["Data"]]["Date"] <- wint_by_year[[1]][[k]]["Date"]
		for (i in 2:length(wint_by_year)){
			all[[k]][["Data"]][["Discharge_acfte6_day"]] <- append(all[[k]][["Data"]][["Discharge_acfte6_day"]], wint_by_year[[i]][[k]][["Discharge_acfte6_day"]], after=length(all[[k]][["Data"]][["Discharge_acfte6_day"]]))
			all[[k]][["Data"]][["Date"]] <- append(all[[k]][["Data"]][["Date"]], wint_by_year[[i]][[k]][["Date"]], after=length(all[[k]][["Data"]][["Date"]]))
		}
		all[[k]][["Stats"]] <-list()
		all[[k]][["Stats"]]["Mean_acfte6"] <- mean(all[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		all[[k]][["Stats"]]["Median_acfte6"] <- median(all[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		all[[k]][["Stats"]]["min_acfte6"] <- min(all[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		all[[k]][["Stats"]]["max_acfte6"] <- max(all[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		all[[k]][["Stats"]][["min_date"]] <- all[[k]][["Data"]][["Date"]][which(all[[k]][["Data"]][["Discharge_acfte6_day"]]==all[[k]][["Stats"]]["min_acfte6"])]
		all[[k]][["Stats"]][["max_date"]] <- all[[k]][["Data"]][["Date"]][which(all[[k]][["Data"]][["Discharge_acfte6_day"]]==all[[k]][["Stats"]]["max_acfte6"])]
		all[[k]][["Stats"]][["Total_Q_acfte6"]] <- sum(all[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		all[[k]][["Stats"]][["Thresholds"]] <- list()
		all[[k]][["Stats"]][["Thresholds"]][["coded"]] <- list()
		all[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(all[[k]][["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		for (i in 1:length(all[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
			for (n in 1:length(all[[k]][["Data"]][["Discharge_acfte6_day"]])){
				if(all[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] >= all[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					all[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(all[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
				} else if (all[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] < all[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					all[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(all[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
				} else {
					all[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(all[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
				}
			}
		}
		all[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- list()
		all[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(all[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			all[[k]][["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(all[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			all[[k]][["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(all[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])/length(all[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			all[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(all[[k]][["Data"]][["Discharge_acfte6_day"]][which(all[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]]==1)])
			all[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-all[[k]][["Stats"]][["Total_Q_acfte6"]]
			all[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- all[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/all[[k]][["Stats"]][["Total_Q_acfte6"]]
		}
		all[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(all[[k]][["Stats"]][["Thresholds"]][["Totals"]])
		all[[k]][["Data"]] <- as.data.frame(all[[k]][["Data"]])
		names(all)[k] <- month_names[k]
	}

	XC <- list()
	XCyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==1)])
	for(k in 1:6){
		XC[[k]] <- list()
		XC[[k]][["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XCyears[[1]]]][[k]][["Discharge_acfte6_day"]]
		XC[[k]][["Data"]][["Date"]] <- wint_by_year[[XCyears[[1]]]][[k]][["Date"]]
		for (i in 2:length(XCyears) ){
			XC[[k]][["Data"]][["Discharge_acfte6_day"]] <- append(XC[[k]][["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XCyears[[i]]]][[k]][["Discharge_acfte6_day"]], after=length(XC[[k]][["Data"]][["Discharge_acfte6_day"]]))
			XC[[k]][["Data"]][["Date"]] <-  append(XC[[k]][["Data"]][["Date"]], wint_by_year[[XCyears[[i]]]][[k]][["Date"]], after=length(XC[[k]][["Data"]][["Date"]]))
		}
		XC[[k]][["Stats"]] <-list()
		XC[[k]][["Stats"]]["Mean_acfte6"] <- mean(XC[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[[k]][["Stats"]]["Median_acfte6"] <- median(XC[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[[k]][["Stats"]]["min_acfte6"] <- min(XC[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[[k]][["Stats"]]["max_acfte6"] <- max(XC[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[[k]][["Stats"]][["min_date"]] <- XC[[k]][["Data"]][["Date"]][which(XC[[k]][["Data"]][["Discharge_acfte6_day"]]==XC[[k]][["Stats"]]["min_acfte6"])]
		XC[[k]][["Stats"]][["max_date"]] <- XC[[k]][["Data"]][["Date"]][which(XC[[k]][["Data"]][["Discharge_acfte6_day"]]==XC[[k]][["Stats"]]["max_acfte6"])]
		XC[[k]][["Stats"]][["Total_Q_acfte6"]] <- sum(XC[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XC[[k]][["Stats"]][["Thresholds"]] <- list()
		XC[[k]][["Stats"]][["Thresholds"]][["coded"]] <- list()
		XC[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XC[[k]][["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		for (i in 1:length(XC[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
			for (n in 1:length(XC[[k]][["Data"]][["Discharge_acfte6_day"]])){
				if(XC[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] >= XC[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XC[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XC[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
				} else if (XC[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] < XC[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XC[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XC[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
				} else {
					XC[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XC[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
				}
			}
		}
		XC[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XC[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XC[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XC[[k]][["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XC[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XC[[k]][["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XC[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])/length(XC[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XC[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XC[[k]][["Data"]][["Discharge_acfte6_day"]][which(XC[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]]==1)])
			XC[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XC[[k]][["Stats"]][["Total_Q_acfte6"]]
			XC[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XC[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XC[[k]][["Stats"]][["Total_Q_acfte6"]]
		}
		XC[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XC[[k]][["Stats"]][["Thresholds"]][["Totals"]])
		XC[[k]][["Data"]] <- as.data.frame(XC[[k]][["Data"]])
		names(XC)[k] <- month_names[k]
	}
	
	
	XD <- list()
	XDyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==2)])
	for(k in 1:6){
		XD[[k]] <- list()
		XD[[k]][["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XDyears[[1]]]][[k]][["Discharge_acfte6_day"]]
		XD[[k]][["Data"]][["Date"]] <- wint_by_year[[XDyears[[1]]]][[k]][["Date"]]
		for (i in 2:length(XDyears) ){
			XD[[k]][["Data"]][["Discharge_acfte6_day"]] <- append(XD[[k]][["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XDyears[[i]]]][[k]][["Discharge_acfte6_day"]], after=length(XD[[k]][["Data"]][["Discharge_acfte6_day"]]))
			XD[[k]][["Data"]][["Date"]] <-  append(XD[[k]][["Data"]][["Date"]], wint_by_year[[XDyears[[i]]]][[k]][["Date"]], after=length(XD[[k]][["Data"]][["Date"]]))
		}
		XD[[k]][["Stats"]] <-list()
		XD[[k]][["Stats"]]["Mean_acfte6"] <- mean(XD[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[[k]][["Stats"]]["Median_acfte6"] <- median(XD[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[[k]][["Stats"]]["min_acfte6"] <- min(XD[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[[k]][["Stats"]]["max_acfte6"] <- max(XD[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[[k]][["Stats"]][["min_date"]] <- XD[[k]][["Data"]][["Date"]][which(XD[[k]][["Data"]][["Discharge_acfte6_day"]]==XD[[k]][["Stats"]]["min_acfte6"])]
		XD[[k]][["Stats"]][["max_date"]] <- XD[[k]][["Data"]][["Date"]][which(XD[[k]][["Data"]][["Discharge_acfte6_day"]]==XD[[k]][["Stats"]]["max_acfte6"])]
		XD[[k]][["Stats"]][["Total_Q_acfte6"]] <- sum(XD[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XD[[k]][["Stats"]][["Thresholds"]] <- list()
		XD[[k]][["Stats"]][["Thresholds"]][["coded"]] <- list()
		XD[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XD[[k]][["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		for (i in 1:length(XD[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
			for (n in 1:length(XD[[k]][["Data"]][["Discharge_acfte6_day"]])){
				if(XD[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] >= XD[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XD[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XD[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
				} else if (XD[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] < XD[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XD[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XD[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
				} else {
					XD[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XD[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
				}
			}
		}
		XD[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XD[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XD[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XD[[k]][["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XD[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XD[[k]][["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XD[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])/length(XD[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XD[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XD[[k]][["Data"]][["Discharge_acfte6_day"]][which(XD[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]]==1)])
			XD[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XD[[k]][["Stats"]][["Total_Q_acfte6"]]
			XD[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XD[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XD[[k]][["Stats"]][["Total_Q_acfte6"]]
		}
		XD[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XD[[k]][["Stats"]][["Thresholds"]][["Totals"]])
		XD[[k]][["Data"]] <- as.data.frame(XD[[k]][["Data"]])
		names(XD)[k] <- month_names[k]
	}
	
	
	XBN <- list()
	XBNyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==3)])
	for(k in 1:6){
		XBN[[k]] <- list()
		XBN[[k]][["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XBNyears[[1]]]][[k]][["Discharge_acfte6_day"]]
		XBN[[k]][["Data"]][["Date"]] <- wint_by_year[[XBNyears[[1]]]][[k]][["Date"]]
		for (i in 2:length(XBNyears) ){
			XBN[[k]][["Data"]][["Discharge_acfte6_day"]] <- append(XBN[[k]][["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XBNyears[[i]]]][[k]][["Discharge_acfte6_day"]], after=length(XBN[[k]][["Data"]][["Discharge_acfte6_day"]]))
			XBN[[k]][["Data"]][["Date"]] <-  append(XBN[[k]][["Data"]][["Date"]], wint_by_year[[XBNyears[[i]]]][[k]][["Date"]], after=length(XBN[[k]][["Data"]][["Date"]]))
		}
		XBN[[k]][["Stats"]] <-list()
		XBN[[k]][["Stats"]]["Mean_acfte6"] <- mean(XBN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[[k]][["Stats"]]["Median_acfte6"] <- median(XBN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[[k]][["Stats"]]["min_acfte6"] <- min(XBN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[[k]][["Stats"]]["max_acfte6"] <- max(XBN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[[k]][["Stats"]][["min_date"]] <- XBN[[k]][["Data"]][["Date"]][which(XBN[[k]][["Data"]][["Discharge_acfte6_day"]]==XBN[[k]][["Stats"]]["min_acfte6"])]
		XBN[[k]][["Stats"]][["max_date"]] <- XBN[[k]][["Data"]][["Date"]][which(XBN[[k]][["Data"]][["Discharge_acfte6_day"]]==XBN[[k]][["Stats"]]["max_acfte6"])]
		XBN[[k]][["Stats"]][["Total_Q_acfte6"]] <- sum(XBN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XBN[[k]][["Stats"]][["Thresholds"]] <- list()
		XBN[[k]][["Stats"]][["Thresholds"]][["coded"]] <- list()
		XBN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XBN[[k]][["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		for (i in 1:length(XBN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
			for (n in 1:length(XBN[[k]][["Data"]][["Discharge_acfte6_day"]])){
				if(XBN[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] >= XBN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XBN[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XBN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
				} else if (XBN[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] < XBN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XBN[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XBN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
				} else {
					XBN[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XBN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
				}
			}
		}
		XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XBN[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XBN[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])/length(XBN[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XBN[[k]][["Data"]][["Discharge_acfte6_day"]][which(XBN[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]]==1)])
			XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XBN[[k]][["Stats"]][["Total_Q_acfte6"]]
			XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XBN[[k]][["Stats"]][["Total_Q_acfte6"]]
		}
		XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XBN[[k]][["Stats"]][["Thresholds"]][["Totals"]])
		XBN[[k]][["Data"]] <- as.data.frame(XBN[[k]][["Data"]])
		names(XBN)[k] <- month_names[k]
	}
	
	
	XAN <- list()
	XANyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==4)])
	for(k in 1:6){
		XAN[[k]] <- list()
		XAN[[k]][["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XANyears[[1]]]][[k]][["Discharge_acfte6_day"]]
		XAN[[k]][["Data"]][["Date"]] <- wint_by_year[[XANyears[[1]]]][[k]][["Date"]]
		for (i in 2:length(XANyears) ){
			XAN[[k]][["Data"]][["Discharge_acfte6_day"]] <- append(XAN[[k]][["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XANyears[[i]]]][[k]][["Discharge_acfte6_day"]], after=length(XAN[[k]][["Data"]][["Discharge_acfte6_day"]]))
			XAN[[k]][["Data"]][["Date"]] <-  append(XAN[[k]][["Data"]][["Date"]], wint_by_year[[XANyears[[i]]]][[k]][["Date"]], after=length(XAN[[k]][["Data"]][["Date"]]))
		}
		XAN[[k]][["Stats"]] <-list()
		XAN[[k]][["Stats"]]["Mean_acfte6"] <- mean(XAN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[[k]][["Stats"]]["Median_acfte6"] <- median(XAN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[[k]][["Stats"]]["min_acfte6"] <- min(XAN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[[k]][["Stats"]]["max_acfte6"] <- max(XAN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[[k]][["Stats"]][["min_date"]] <- XAN[[k]][["Data"]][["Date"]][which(XAN[[k]][["Data"]][["Discharge_acfte6_day"]]==XAN[[k]][["Stats"]]["min_acfte6"])]
		XAN[[k]][["Stats"]][["max_date"]] <- XAN[[k]][["Data"]][["Date"]][which(XAN[[k]][["Data"]][["Discharge_acfte6_day"]]==XAN[[k]][["Stats"]]["max_acfte6"])]
		XAN[[k]][["Stats"]][["Total_Q_acfte6"]] <- sum(XAN[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XAN[[k]][["Stats"]][["Thresholds"]] <- list()
		XAN[[k]][["Stats"]][["Thresholds"]][["coded"]] <- list()
		XAN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XAN[[k]][["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		for (i in 1:length(XAN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
			for (n in 1:length(XAN[[k]][["Data"]][["Discharge_acfte6_day"]])){
				if(XAN[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] >= XAN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XAN[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XAN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
				} else if (XAN[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] < XAN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XAN[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XAN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
				} else {
					XAN[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XAN[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
				}
			}
		}
		XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XAN[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XAN[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])/length(XAN[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XAN[[k]][["Data"]][["Discharge_acfte6_day"]][which(XAN[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]]==1)])
			XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XAN[[k]][["Stats"]][["Total_Q_acfte6"]]
			XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XAN[[k]][["Stats"]][["Total_Q_acfte6"]]
		}
		XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XAN[[k]][["Stats"]][["Thresholds"]][["Totals"]])
		XAN[[k]][["Data"]] <- as.data.frame(XAN[[k]][["Data"]])
		names(XAN)[k] <- month_names[k]
	}
	
	XW <- list()
	XWyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==5)])
	for(k in 1:6){
		XW[[k]] <- list()
		XW[[k]][["Data"]][["Discharge_acfte6_day"]] <- wint_by_year[[XWyears[[1]]]][[k]][["Discharge_acfte6_day"]]
		XW[[k]][["Data"]][["Date"]] <- wint_by_year[[XWyears[[1]]]][[k]][["Date"]]
		for (i in 2:length(XWyears) ){
			XW[[k]][["Data"]][["Discharge_acfte6_day"]] <- append(XW[[k]][["Data"]][["Discharge_acfte6_day"]], wint_by_year[[XWyears[[i]]]][[k]][["Discharge_acfte6_day"]], after=length(XW[[k]][["Data"]][["Discharge_acfte6_day"]]))
			XW[[k]][["Data"]][["Date"]] <-  append(XW[[k]][["Data"]][["Date"]], wint_by_year[[XWyears[[i]]]][[k]][["Date"]], after=length(XW[[k]][["Data"]][["Date"]]))
		}
		XW[[k]][["Stats"]] <-list()
		XW[[k]][["Stats"]]["Mean_acfte6"] <- mean(XW[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[[k]][["Stats"]]["Median_acfte6"] <- median(XW[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[[k]][["Stats"]]["min_acfte6"] <- min(XW[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[[k]][["Stats"]]["max_acfte6"] <- max(XW[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[[k]][["Stats"]][["min_date"]] <- XW[[k]][["Data"]][["Date"]][which(XW[[k]][["Data"]][["Discharge_acfte6_day"]]==XW[[k]][["Stats"]]["min_acfte6"])]
		XW[[k]][["Stats"]][["max_date"]] <- XW[[k]][["Data"]][["Date"]][which(XW[[k]][["Data"]][["Discharge_acfte6_day"]]==XW[[k]][["Stats"]]["max_acfte6"])]
		XW[[k]][["Stats"]][["Total_Q_acfte6"]] <- sum(XW[[k]][["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
		XW[[k]][["Stats"]][["Thresholds"]] <- list()
		XW[[k]][["Stats"]][["Thresholds"]][["coded"]] <- list()
		XW[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XW[[k]][["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		for (i in 1:length(XW[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
			for (n in 1:length(XW[[k]][["Data"]][["Discharge_acfte6_day"]])){
				if(XW[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] >= XW[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XW[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
				} else if (XW[[k]][["Data"]][["Discharge_acfte6_day"]][[n]] < XW[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
					XW[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
				} else {
					XW[[k]][["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[[k]][["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
				}
			}
		}
		XW[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- list()
		XW[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
		for (i in 1:length(XW[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
			XW[[k]][["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XW[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XW[[k]][["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XW[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])/length(XW[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]])
			XW[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XW[[k]][["Data"]][["Discharge_acfte6_day"]][which(XW[[k]][["Stats"]][["Thresholds"]][["coded"]][[i]]==1)])
			XW[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XW[[k]][["Stats"]][["Total_Q_acfte6"]]
			XW[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XW[[k]][["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XW[[k]][["Stats"]][["Total_Q_acfte6"]]
		}
		XW[[k]][["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XW[[k]][["Stats"]][["Thresholds"]][["Totals"]])
		XW[[k]][["Data"]] <- as.data.frame(XW[[k]][["Data"]])
		names(XW)[k] <- month_names[k]
	}
	
	total <- list(Data=wint_by_year, Stats=stats, All=all, C=XC, D=XD, BN=XBN, AN=XAN, W=XW)
	return(total)
}
