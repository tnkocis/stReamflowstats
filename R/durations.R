# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


durations_stats <- function(input, width, threshold,thresholdname, Index){
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
	
	if(as.integer(width) != width){
		warning("Rounding argument 'width' to nearest integer.")
		width = round(width)
	}
	if(width < 1 | width %% 2 == 0)
		stop("Argument 'width' must be an odd positive integer.")    
	mid  <- (width - 1) %/% 2 + 1
	peaks <- rollapply(input$Discharge_cfs, width, function(x) which.max(x) == mid, 
			align = "center", fill=NA)
	peaks[input$Discharge_cfs < threshold] <-  FALSE
	numpeaks <- sum(peaks,na.rm=TRUE)
	datepeaks <- input$Date[which(peaks==TRUE)]
	peakflow <- input$Discharge_cfs[which(peaks==TRUE)]
	peakloc <- which(peaks==TRUE)
	threshlog <- input$Discharge_cfs >= threshold
	mon <- format(input$Date,"%m")
	yearchr <- paste(as.numeric(format(tail(input$Date,1),"%Y"))-1,"-",as.numeric(format(tail(input$Date,1),"%Y")))
	yearindex <- Index$Index[[which(Index$Year==yearchr)]]
	
	
	rlehy <- rle(threshlog)
	truevechy <- rlehy$lengths[which(rlehy$values==1)]
	if(length(truevechy)==0){
		minhy <- NA
		maxhy <- NA
		medianhy <- NA
		meanhy <- NA
		totalhy <- NA
	} else {
		minhy <- min(truevechy, na.rm=TRUE)
		maxhy <- max(truevechy, na.rm=TRUE)
		medianhy <- median(truevechy, na.rm=TRUE)
		meanhy <- mean(truevechy, na.rm=TRUE)
		totalhy <- sum(truevechy,na.rm=TRUE)
	}
	hydf <- data.frame(period=c("hy"), sthyyear=as.numeric(format(input$Date,"%Y")[[1]]), yeartype_index=yearindex, 
			min=minhy,max=maxhy,median=medianhy,mean=meanhy,total=totalhy)
	
	months3log <- format(input$Date,"%m")=="12"|format(input$Date,"%m")=="01"|format(input$Date,"%m")=="02"
	disch3 <- input$Discharge_cfs[months3log]
	thresh3log <- disch3 >=threshold
	rle3 <- rle(thresh3log)
	truevec3 <- rle3$lengths[which(rle3$values==1)]
	if(length(truevec3)==0){
		min3 <- NA
		max3 <- NA
		median3 <- NA
		mean3 <- NA
		total3 <- NA
	} else {
		min3 <- min(truevec3, na.rm=TRUE)
		max3 <- max(truevec3, na.rm=TRUE)
		median3 <- median(truevec3, na.rm=TRUE)
		mean3 <- mean(truevec3, na.rm=TRUE)
		total3 <- sum(truevec3, na.rm=TRUE)
	}
	months3df <- data.frame(period=c("mon3"), sthyyear=as.numeric(format(input$Date,"%Y")[[1]]), yeartype_index=yearindex, 
			min=min3,max=max3,median=median3,mean=mean3,total=total3)

	months6log <- format(input$Date,"%m")=="11"|format(input$Date,"%m")=="12"|format(input$Date,"%m")=="01"|format(input$Date,"%m")=="02"|format(input$Date,"%m")=="03"|format(input$Date,"%m")=="04"
	disch6 <- input$Discharge_cfs[months6log]
	thresh6log <- disch6 >=threshold
	rle6 <- rle(thresh6log)
	truevec6 <- rle6$lengths[which(rle6$values==1)]
	if(length(truevec6)==0){
		min6 <- NA
		max6 <- NA
		median6 <- NA
		mean6 <- NA
		total6 <- NA
	} else {
		min6 <- min(truevec6, na.rm=TRUE)
		max6 <- max(truevec6, na.rm=TRUE)
		median6 <- median(truevec6, na.rm=TRUE)
		mean6 <- mean(truevec6, na.rm=TRUE)
		total6 <- sum(truevec6, na.rm=TRUE)
	}
	months6df <- data.frame(period=c("mon6"), sthyyear=as.numeric(format(input$Date,"%Y")[[1]]), yeartype_index=yearindex, 
			min=min6,max=max6,median=median6,mean=mean6,total=total6)

	
		monthly_stats <- data.frame(period=rep(NA,12),sthyyear=rep(NA,12), yeartype_index=rep(NA,12), min=rep(NA,12), max=rep(NA,12),
				median=rep(NA,12), mean=rep(NA,12),  total=rep(threshold,12))
		months <- c("10","11","12","01","02","03","04","05","06","07","08","09")
		for(i in 1:length(months)){
			monlog <- format(input$Date,"%m")==months[[i]]
			dischmon <- input$Discharge_cfs[monlog]
			thresmonlog <- dischmon >= threshold
			monthly_stats$period[[i]] <- months[[i]]
			rlemon <- rle(thresmonlog)
			truevecmon <- rlemon$lengths[which(rlemon$values==1)]
			if(length(truevecmon)==0){
				monthly_stats$min[[i]] <- NA
				monthly_stats$max[[i]] <- NA
				monthly_stats$median[[i]] <- NA
				monthly_stats$mean[[i]] <- NA
				monthly_stats$total[[i]] <- NA
			} else {
				monthly_stats$min[[i]] <- min(truevecmon, na.rm=TRUE)
				monthly_stats$max[[i]] <- max(truevecmon, na.rm=TRUE)
				monthly_stats$median[[i]] <- median(truevecmon, na.rm=TRUE)
				monthly_stats$mean[[i]] <- mean(truevecmon, na.rm=TRUE)
				monthly_stats$total[[i]] <- sum(truevecmon, na.rm=TRUE)
			}
			monthly_stats$sthyyear[[i]] <- as.numeric(format(input$Date,"%Y"))[[1]]
			monthly_stats$yeartype_index[[i]] <- yearindex
		}
	
	finaldf <- rbind.data.frame(hydf, months3df, months6df, monthly_stats)
	return(finaldf)
}
