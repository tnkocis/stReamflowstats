# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


#split into years of 6 month winter data
Split6Winter <- function(input, index){
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
	all[["Stats"]][["Total_Q_acfte6"]] <- sum(all[["Data"]][["Discharge_acfte6_day"]], na.rm=TRUE)
	all[["Stats"]][["Quantiles_acfte6"]] <- quantile(all[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	
	XC <- list()
	XCyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==1)])
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
	XC[["Stats"]][["Quantiles_acfte6"]] <- quantile(XC[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	
	XD <- list()
	XDyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==2)])
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
	XD[["Stats"]][["Quantiles_acfte6"]] <- quantile(XD[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	
	XBN <- list()
	XBNyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==3)])
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
	XBN[["Stats"]][["Quantiles_acfte6"]] <- quantile(XBN[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	
	XAN <- list()
	XANyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==4)])
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
	XAN[["Stats"]][["Quantiles_acfte6"]] <- quantile(XAN[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	
	XW <- list()
	XWyears <- which(names(wint_by_year) %in% index$Year[which(index$Index==5)])
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
	XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]] <- quantile(XW[["Data"]][["Discharge_acfte6_day"]], probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	for (i in 1:length(XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]])){
		for (n in 1:length(XW[["Data"]][["Discharge_acfte6_day"]])){
			if(XW[["Data"]][["Discharge_acfte6_day"]][[n]] >= XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
				XW[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 1
			} else if (XW[["Data"]][["Discharge_acfte6_day"]][[n]] < XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][[i]]){
				XW[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- 0	
			} else {
				XW[["Stats"]][["Thresholds"]][["coded"]][[paste0("P",names(XW[["Stats"]][["Thresholds"]][["Quantiles_acfte6"]][i]))]][[n]] <- NA
			}
		}
	}
	XW[["Stats"]][["Thresholds"]][["Totals"]] <- list()
	XW[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
	for (i in 1:length(XW[["Stats"]][["Thresholds"]][["Totals"]][["Thresholds"]])){
		XW[["Stats"]][["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(XW[["Stats"]][["Thresholds"]][["coded"]][[i]])
		XW[["Stats"]][["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(XW[["Stats"]][["Thresholds"]][["coded"]][[i]])/length(XW[["Stats"]][["Thresholds"]][["coded"]][[i]])
		XW[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(XW[["Data"]][["Discharge_acfte6_day"]][which(XW[["Stats"]][["Thresholds"]][["coded"]][[i]]==1)])
		XW[["Stats"]][["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-XW[["Stats"]][["Total_Q_acfte6"]]
		XW[["Stats"]][["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- XW[["Stats"]][["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/XW[["Stats"]][["Total_Q_acfte6"]]
	}
	XW[["Stats"]][["Thresholds"]][["Totals"]] <- as.data.frame(XW[["Stats"]][["Thresholds"]][["Totals"]])

	
	total <- list(Data=wint_by_year, Stats=stats, All=all, C=XC, D=XD, BN=XBN, AN=XAN, W=XW)
	return(total)
}

