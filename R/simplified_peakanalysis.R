# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


simplified_peakanalysis <- function(input, width, threshold,thresholdname, mastertime, Index){
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
	
	if(numpeaks != 0){
		duration <- function(peakflows,loc){
			lengthbeforepeak <- loc-1
			lengthafterpeak <- length(peakflows)-loc
			start <- 0
			end <- 0
			k <- 0
			for(i in 1:lengthbeforepeak){
				test <- peakflows[[loc-i]]
				if(is.na(test)& (i == lengthbeforepeak)){
					start <- 99999
				} else if (is.na(test)& (i != lengthbeforepeak) & (k==0)){
					k <- i
					next
				} else if (is.na(test)& (i != lengthbeforepeak) & (k!=0)){
					next
				} else if (test==TRUE & k!=0){
					k <- 0
					next
				} else if(test==FALSE) {
					break
				} else if(i == lengthbeforepeak){ 
					start <- 99999
				} else {
					next
				}
			}
			if(start==99999){
				start <- NA
			}else if(k==0){
				start <- loc-(i-1)
			}else if(k!=0){
				start <- loc-(k-1)
			}
			
			k <- 0
			for(z in 1:lengthafterpeak){
				test <- peakflows[[loc+z]]
				if(is.na(test)& (z == lengthafterpeak)){
					end <- 99999
				} else if (is.na(test)& (z != lengthafterpeak) & (k==0)){
					k <- z
					next
				} else if (is.na(test)& (z != lengthafterpeak) & (k!=0)){
					next
				} else if (test==TRUE & k!=0){
					k <- 0
					next
				} else if(test==FALSE) {
					break
				} else if(z == lengthafterpeak){ 
					end <- 99999
				} else {
					next
				}
			}
			if(end==99999){
				end <- NA
			}else if(k==0){
				end <- loc+(z-1)
			}else if(k!=0){
				end <- loc+(k-1)
			}
			
			return(data.frame(start=start,end=end,duration=((end-start)+1)))
		}
		
		summary <- data.frame(peak_date=datepeaks, peak_flow=peakflow, thres_value=threshold, thres=thresholdname,
				start=rep(NA,length(datepeaks)),end=rep(NA,length(datepeaks)),
				duration=rep(NA, length(datepeaks)),vol_acft_event=rep(NA, length(datepeaks)), year=rep(NA, length(datepeaks)),
				yeartype_index=rep(NA, length(datepeaks)))
		eventvolume <- function(startloc, endloc, discharge){
			if(is.na(startloc)| is.na(endloc)){
				eventvol  <- NA
			} else {
				eventdischarge <- discharge[startloc:endloc]
				eventdischargeacft <- eventdischarge*86400*2.29568411e-5
				eventvol <- sum(eventdischargeacft,na.rm=TRUE)
			}
			return(eventvol)
		}
		for(i in 1:length(peakloc)){
			dur <- duration(threshlog,peakloc[[i]])
			summary$vol_acft_event[[i]] <- eventvolume(dur$start,dur$end,input$Discharge_cfs)
			if(is.na(dur$start)){
				summary$start[[i]] <- NA  
			} else { summary$start[[i]] <- input$Date[dur$start]}
			if(is.na(dur$end)){
				summary$end[[i]] <- NA 
			} else {summary$end[[i]] <- input$Date[dur$end]}
			summary$duration[[i]] <- dur$duration
			summary$year[[i]] <- as.numeric(format(head(input$Date,1),"%Y"))
			summary$yeartype_index[[i]] <- yearindex
		}
		
		
		summary$start <- as.Date(summary$start, format="%Y-%m-%d")
		summary$end <- as.Date(summary$end, format="%Y-%m-%d")
		peakmon <- format(summary$peak_date,"%m")
	 if(mastertime=="hy"){
			summary <- summary
			if(length(summary$peak_flow)==0){
				summary <- data.frame(peak_date=c(NA), peak_flow=c(NA), thres_value=c(NA), thres=c(NA),
						start=c(NA),end=c(NA),
						duration=c(NA),vol_acft_event=c(NA), year=as.numeric(format(head(input$Date,1),"%Y")), yeartype_index=yearindex)
			}
			firstdate <- as.Date(summary$start[[1]])
			enddate <- as.Date(tail(summary$end,1))
			if(is.na(firstdate)|is.na(enddate)){
				stats <- data.frame(TotVolAbv_acft=c(0), TotDaysAbv = c(0), 
						numpeaks = c(0), mean_peakflow = c(0), total_peakflow = c(0),
						year=as.numeric(format(head(input$Date,1),"%Y")), yeartype_index=yearindex)
			}else {
				daterangeloc <- which(input$Date==firstdate):which(input$Date==enddate)
				peakshy <- peaks[daterangeloc]
				numpeakshy <- sum(peakshy, na.rm=TRUE)
				threshloghy <- threshlog[daterangeloc]
				dischrange <- input$Discharge_cfs[daterangeloc]
				if (numpeakshy == 0 | is.na(numpeakshy)){
					peakflowhy <- NA
				} else {
					peakflowhy <- dischrange[which(peakshy==TRUE)]
				}
				if(all(is.na(threshlog))){
					totaldaysabv <- NA
				} else {totaldaysabv <- sum(threshloghy, na.rm=TRUE)}
				if(all(is.na(threshlog))){
					totalvolabv <- NA
				}else{ 
					totalvolabv <- sum(dischrange[threshloghy], na.rm=TRUE)*86400*2.29568411e-5
				}
				if(all(is.na(peakflow))){
					avgpeakflow <-0
				} else {
					avgpeakflow <- mean(peakflowhy, na.rm=TRUE)
				}
				if(all(is.na(peakflow))){
					total_peakflow <- 0
				} else {
					total_peakflow <- sum(peakflowhy, na.rm=TRUE)
				}
				
				stats <- data.frame(TotVolAbv_acft=totalvolabv, TotDaysAbv = totaldaysabv, 
						numpeaks = numpeakshy, mean_peakflow = avgpeakflow, total_peakflow=total_peakflow,
						year=as.numeric(format(head(input$Date,1),"%Y")), yeartype_index=yearindex)
			}
			} else {
				stop("mastertime error")
			}
	} else {
		summary <- data.frame(peak_date=c(NA), peak_flow=c(NA), thres_value=c(NA), thres=c(NA),
				start=c(NA),end=c(NA),
				duration=c(NA),vol_acft_event=c(NA), year=as.numeric(format(head(input$Date,1),"%Y")), yeartype_index=yearindex)
		stats <- data.frame(TotVolAbv_acft=c(0), TotDaysAbv = c(0), 
				numpeaks = c(0), mean_peakflow = c(0),total_peakflow=c(0),
				year=as.numeric(format(head(input$Date,1),"%Y")), yeartype_index=yearindex)
	}
	if(mastertime=="hy"){
		monthly_stats <- data.frame(month=rep(NA,12),TotVolAbv_acft=rep(NA,12), TotDaysAbv=rep(NA,12), numpeaks=rep(NA,12), mean_peakflow=rep(NA,12),
				total_peakflow=rep(NA,12), sthyyear=rep(NA,12), yeartype_index=rep(NA,12), threshold=rep(threshold,12))
		months <- c("10","11","12","01","02","03","04","05","06","07","08","09")
		for(i in 1:length(months)){
			monlog <- format(input$Date,"%m")==months[[i]]
			dischmon <- input$Discharge_cfs[monlog]
			thresmonlog <- dischmon >= threshold
			monthly_stats$month[[i]] <- months[[i]]
			monthly_stats$TotDaysAbv[[i]] <-  sum(thresmonlog, na.rm=TRUE)
			vols <- dischmon[thresmonlog]
			if(length(vols)==0){
				monthly_stats$TotVolAbv_acft[[i]] <- 0
			} else {
				monthly_stats$TotVolAbv_acft[[i]] <-sum(vols,na.rm=TRUE)*86400*2.29568411e-5
			}
			monthly_stats$sthyyear[[i]] <- as.numeric(format(input$Date,"%Y"))[[1]]
			
			peaksmonlog <- format(datepeaks,"%m")==months[[i]]
			numpeaksmon <- sum(peaksmonlog, na.rm=TRUE)
			monthly_stats$numpeaks[[i]] <- numpeaksmon
			
			peaksmon <- peakflow[peaksmonlog]
			if(numpeaksmon==0){
				meanpeakflowmon <- 0
			}else{
				meanpeakflowmon <- mean(peaksmon, na.rm=TRUE)
			}
			totalpeakflowmon <- sum(peaksmon, na.rm=TRUE)
			
			monthly_stats$mean_peakflow[[i]] <- meanpeakflowmon
			monthly_stats$total_peakflow[[i]] <- totalpeakflowmon
			
			monthly_stats$yeartype_index[[i]] <- yearindex
			}
		
	}else {
		stop("mastertime error")
	}
	return(list(summary=summary, stats=stats, monthly_stats=monthly_stats))
}

