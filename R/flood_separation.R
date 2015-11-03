# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################
library(EcoHydRology)

streamflow <- american$`11446500`$Winter_3mon$Data[[91]]$Discharge_cfs
streamflowdate <- american$`11446500`$Winter_3mon$Data[[91]]$Date
baseflowtest <- BaseflowSeparation(streamflow, filter_parameter = 0.9, passes = 1)
floodflow <- streamflow - baseflowtest$bt 
zeroline <- rep(0,length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x90line <- rep(x90_3mon[["90%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x95line <- rep(x90_3mon[["95%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x80line <- rep(x90_3mon[["80%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))

plot(streamflowdate, streamflow, type="l",ylim=c(0,20000))
lines(streamflowdate, baseflowtest$bt)
lines(streamflowdate, floodflow)
lines(streamflowdate, zeroline, col="red")
lines(streamflowdate, x90line, col="red")
lines(streamflowdate, x95line, col="red")
lines(streamflowdate, x80line, col="red")

	strlist <- vector("list",length(american$`11446500`$Winter_3mon$Data)-57)
for (i in 57:length(american$`11446500`$Winter_3mon$Data)){
	streamflow <- american$`11446500`$Winter_3mon$Data[[i]]$Discharge_cfs
	streamflowdate <- american$`11446500`$Winter_3mon$Data[[i]]$Date
	baseflowtest <- BaseflowSeparation(streamflow, filter_parameter = 0.9, passes = 3)
	floodflow <- streamflow - baseflowtest$bt 
	num <- seq(1,length(american$`11446500`$Winter_3mon$Data[[i]]$Discharge_cfs),1)
	strlist[[i-56]]<- data.frame(streamflow=streamflow, date=streamflowdate, baseflow= baseflowtest, floodflows=floodflow, num=num)	
	names(strlist)[[i-56]] <- names(american$`11446500`$Winter_3mon$Data)[[i]]
}
plot(strlist$`1960 - 1961`$num, strlist$`1960 - 1961`$streamflow, type="n",ylim=c(0,60000))
for(i in 1:15){
	lines(strlist[[i]]$num,strlist[[i]]$floodflows)
}
lines(strlist[[1]]$num, x90line, col="red")
lines(strlist[[1]]$num, x95line, col="red")
lines(strlist[[1]]$num, x80line, col="red")


x90_3mon <- quantile(american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day/(86400*2.29568411e-5*1e-6),probs=c(0.5,0.6,0.7,0.8,0.9,0.95), na.rm=TRUE)

x3monall <- american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day/(86400*2.29568411e-5*1e-6)

line <- rep(x90_3mon[["90%"]],length(american$`11446500`$Winter_3mon$All$Data$Date))
line2 <- rep(x90_3mon[["95%"]],length(american$`11446500`$Winter_3mon$All$Data$Date))

plot(american$`11446500`$Winter_3mon$All$Data$Date,american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day/(86400*2.29568411e-5*1e-6),
		type="l", xlim=c(as.Date("1900-12-01", format="%Y-%m-%d"),as.Date("2015-12-01", format="%Y-%m-%d")))
lines(american$`11446500`$Winter_3mon$All$Data$Date, line)
lines(american$`11446500`$Winter_3mon$All$Data$Date, line2)


peaks <- function(input, width, threshold,thresholdname, mastertime){
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
	#input 6mon data for 3 month analysis
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
	  datepeaks <- input$Date[which(peaks==TRUE)]
	  peakflow <- input$Discharge_cfs[which(peaks==TRUE)]
	  numpeaks <- sum(peaks,na.rm=TRUE)
	  peakloc <- which(peaks==TRUE)
	  threshlog <- input$Discharge_cfs >= threshold
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
										  duration=rep(NA, length(datepeaks)),vol_acft_event=rep(NA, length(datepeaks)), year=rep(NA, length(datepeaks)))
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
			  	summary$year[[i]] <- format(tail(input$Date,1),"%Y")
		  }
		  
	
		  summary$start <- as.Date(summary$start, format="%Y-%m-%d")
		  summary$end <- as.Date(summary$end, format="%Y-%m-%d")
		  peakmon <- format(summary$peak_date,"%m")
		  if(mastertime=="3mon"){
			  summary <- summary[which(peakmon == "12" | peakmon == "01" | peakmon== "02"),]
			  if(length(summary$peak_flow)==0){
				  summary <- data.frame(peak_date=c(NA), peak_flow=c(NA), thres_value=c(NA), thres=c(NA),
						  start=c(NA),end=c(NA),
						  duration=c(NA),vol_acft_event=c(NA), year=format(tail(input$Date,1),"%Y"))
			  }
		  } else {
			  stop("mastertime error")
		  }
	} else {
		summary <- data.frame(peak_date=c(NA), peak_flow=c(NA), thres_value=c(NA), thres=c(NA),
					  start=c(NA),end=c(NA),
					  duration=c(NA),vol_acft_event=c(NA), year=format(tail(input$Date,1),"%Y"))
	}
	  return(summary)
}

peakflows <- vector("list", length=length(american$`11446500`$Winter_6mon$Data))
names(peakflows) <- names(american$`11446500`$Winter_6mon$Data)
for(i in 1:length(american$`11446500`$Winter_6mon$Data)){
	peakflows[[i]] <-  peaks(input=american$`11446500`$Winter_6mon$Data[[i]],width=3, threshold=x90_3mon[["95%"]], thresholdname="95%", mastertime="3mon")
	
}
peakstest <- peaks(input=american$`11446500`$Winter_6mon$Data[[3]],width=3, threshold=x90_3mon[["95%"]], thresholdname="95%", mastertime="3mon")