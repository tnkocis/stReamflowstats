# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################
library(EcoHydRology)

streamflow <- american$`11446500`$Winter_3mon$Data[[1]]$Discharge_cfs
streamflowdate <- american$`11446500`$Winter_3mon$Data[[1]]$Date
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


peaks <- function(input, width, threshold,thresholdname, mastertime, Index){
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
	  mon <- format(input$Date,"%m")
	  yearchr <- paste(as.numeric(format(tail(input$Date,1),"%Y"))-1,"-",as.numeric(format(tail(input$Date,1),"%Y")))
	  yearindex <- Index$Index[[which(Index$Year==yearchr)]]
#	  if(mastertime == "3mon"){
#		} else {
#			stats <- "mastertime error"
#		}
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
			  	summary$year[[i]] <- as.numeric(format(tail(input$Date,1),"%Y"))
				summary$yeartype_index[[i]] <- yearindex
		  }
		  
	
		  summary$start <- as.Date(summary$start, format="%Y-%m-%d")
		  summary$end <- as.Date(summary$end, format="%Y-%m-%d")
		  peakmon <- format(summary$peak_date,"%m")
		  if(mastertime=="3mon"){
			  summary <- summary[which(peakmon == "12" | peakmon == "01" | peakmon== "02"),]
			  if(length(summary$peak_flow)==0){
				  summary <- data.frame(peak_date=c(NA), peak_flow=c(NA), thres_value=c(NA), thres=c(NA),
						  start=c(NA),end=c(NA),
						  duration=c(NA),vol_acft_event=c(NA), year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
			  }
			  firstdate <- as.Date(summary$start[[1]])
			  enddate <- as.Date(tail(summary$end,1))
			  if(is.na(firstdate)|is.na(enddate)){
						  stats <- data.frame(TotVolAbv_acft=c(0), TotDaysAbv = c(0), 
								  numpeaks = c(0), mean_peakflow = c(NA),
								  year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
			  }else {
				  daterangeloc <- which(input$Date==firstdate):which(input$Date==enddate)
				  peaks3mon <- peaks[daterangeloc]
				  numpeaks3mon <- sum(peaks3mon, na.rm=TRUE)
				  threshlog3mon <- threshlog[daterangeloc]
				  dischrange <- input$Discharge_cfs[daterangeloc]
				  if (numpeaks3mon == 0 | is.na(numpeaks3mon)){
					  peakflow3mon <- NA
				  } else {
					  peakflow3mon <- dischrange[which(peaks3mon==TRUE)]
				  }
				  if(all(is.na(threshlog))){
					  totaldaysabv <- NA
				  } else {totaldaysabv <- sum(threshlog3mon, na.rm=TRUE)}
				  if(all(is.na(threshlog))){
					  totalvolabv <- NA
				  }else{ 
					  totalvolabv <- sum(dischrange[threshlog3mon], na.rm=TRUE)*86400*2.29568411e-5
				  }
				  if(all(is.na(peakflow))){
					  avgpeakflow <-NA
				  } else {
					  avgpeakflow <- mean(peakflow3mon, na.rm=TRUE)
				  }

				  stats <- data.frame(TotVolAbv_acft=totalvolabv, TotDaysAbv = totaldaysabv, 
						  numpeaks = numpeaks3mon, mean_peakflow = avgpeakflow,
						  year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
			  }
		  } else {
			  stop("mastertime error")
		  }
	} else {
		summary <- data.frame(peak_date=c(NA), peak_flow=c(NA), thres_value=c(NA), thres=c(NA),
					  start=c(NA),end=c(NA),
					  duration=c(NA),vol_acft_event=c(NA), year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
		stats <- data.frame(TotVolAbv_acft=c(0), TotDaysAbv = c(0), 
					  numpeaks = c(0), mean_peakflow = c(NA),
					  year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
	}
	  return(list(summary=summary, stats=stats))
}

peakflows <- vector("list", length=length(american$`11446500`$Winter_6mon$Data))
names(peakflows) <- names(american$`11446500`$Winter_6mon$Data)
for(i in 1:length(american$`11446500`$Winter_6mon$Data)){
	peakflows[[i]] <-  peaks(input=american$`11446500`$Winter_6mon$Data[[i]],width=3, threshold=x90_3mon[["90%"]], thresholdname="95%", mastertime="3mon", Index=american$`11446500`$Index)
}
peakflowsstatslist <- vector("list", length(peakflows))
for(i in 1:length(peakflows)){
	peakflowsstatslist[[i]] <- peakflows[[i]][[2]]
}
peakflowstats <- do.call(rbind.data.frame,peakflowsstatslist)
peakflowstats$color <- rep(NA, length(peakflowstats$year))
peakflowstats$color[which(peakflowstats$yeartype_index==1)] <- "red"
peakflowstats$color[which(peakflowstats$yeartype_index==2)] <- "orange"
peakflowstats$color[which(peakflowstats$yeartype_index==3)] <- "yellow"
peakflowstats$color[which(peakflowstats$yeartype_index==4)] <- "green"
peakflowstats$color[which(peakflowstats$yeartype_index==5)] <- "blue"

peakflowstats1970 <- peakflowstats[peakflowstats$year >=1970,]
peakflowstatspreimp <-  peakflowstats[peakflowstats$year <1970,]
plot(peakflowstats$TotDaysAbv,peakflowstats$TotVolAbv_acft, col=peakflowstats$color)
plot(peakflowstats$TotDaysAbv,peakflowstats$numpeaks, col=peakflowstats$color)
plot(peakflowstats$TotVolAbv_acft,peakflowstats$numpeaks, col=peakflowstats$color)
plot(peakflowstats$year,peakflowstats$TotVolAbv_acft, col=peakflowstats$color)
pdf(file="C:\\Users\\tiffn_000\\Desktop\\Figures\\stats.pdf", width=11, height=8.5)
par(mfrow=c(2,3))
plot(peakflowstats1970$TotDaysAbv,peakflowstats1970$TotVolAbv_acft, bg=peakflowstats1970$color, pch=24, xlim=c(0,60), ylim=c(0,3000000),
		xlab="Total Days Above", ylab="Total Volume Above (acft)")
points(peakflowstatspreimp$TotDaysAbv,peakflowstatspreimp$TotVolAbv_acft, col=peakflowstatspreimp$color)

plot(peakflowstats1970$TotDaysAbv,peakflowstats1970$numpeaks, bg=peakflowstats1970$color, pch=24, xlim=c(0,60), ylim=c(0,10),
		xlab="Total Days Above", ylab="Number of Peaks")
points(peakflowstatspreimp$TotDaysAbv,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color)

plot(peakflowstats1970$TotVolAbv_acft,peakflowstats1970$numpeaks, bg=peakflowstats1970$color,pch=24, xlim=c(0,3000000), ylim=c(0,10),
		xlab="Total Volume Above (acft)", ylab="Number of Peaks")
points(peakflowstatspreimp$TotVolAbv_acft,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color)

plot(peakflowstats1970$year,peakflowstats1970$TotVolAbv_acft, bg=peakflowstats1970$color,pch=24, xlim=c(1900,2015), ylim=c(0,3000000),
		xlab="Year", ylab="Total Volume Above (acft)")
points(peakflowstatspreimp$year,peakflowstatspreimp$TotVolAbv_acft, col=peakflowstatspreimp$color)

plot(peakflowstats1970$year,peakflowstats1970$numpeaks, bg=peakflowstats1970$color,pch=24, xlim=c(1900,2015), ylim=c(0,10),
		xlab="Year", ylab="Number of Peaks")
points(peakflowstatspreimp$year,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color)

plot(peakflowstats1970$year,peakflowstats1970$TotDaysAbv, bg=peakflowstats1970$color,pch=24, xlim=c(1900,2015), ylim=c(0,60),
		xlab="Year", ylab="Total Days Above")
points(peakflowstatspreimp$year,peakflowstatspreimp$TotDaysAbv, col=peakflowstatspreimp$color)
dev.off()

points(peakflowstatspreimp$TotDaysAbv,peakflowstatspreimp$TotVolAbv_acft, col=peakflowstatspreimp$color, xlim=c(0,60), ylim=c(0,2500000))
points(peakflowstatspreimp$TotDaysAbv,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color,xlim=c(0,60), ylim=c(0,10))
plot(peakflowstatspreimp$TotVolAbv_acft,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color)
plot(peakflowstatspreimp$year,peakflowstatspreimp$TotVolAbv_acft, col=peakflowstatspreimp$color)

peakstest <- peaks(input=american$`11446500`$Winter_6mon$Data[[3]],width=3, threshold=x90_3mon[["95%"]], thresholdname="95%", mastertime="3mon")

#baseflow flood separation##
library(hydrostats)
library(zoo)

mov_discharge <- rollapply(american$`11446500`$Winter_3mon$Data[[91]]$Discharge_cfs, 3, function(x) mean(x, na.rm=TRUE),
		align = "center", fill=NA)
mov_disch <- data.frame(Q=mov_discharge, Date=american$`11446500`$Winter_3mon$Data[[1]]$Date)
streamflow <- american$`11446500`$Winter_3mon$Data[[91]]$Discharge_cfs
streamflowdate <- american$`11446500`$Winter_3mon$Data[[91]]$Date
baseflowtest <- baseflows(mov_disch, a = 0.82, n.reflected = 30, ts="daily")

floodflow <- american$`11446500`$Winter_3mon$Data[[91]]$Discharge_cfs - baseflowtest$bf
zeroline <- rep(0,length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x90line <- rep(x90_3mon[["90%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x95line <- rep(x90_3mon[["95%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x80line <- rep(x90_3mon[["80%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))

par(mfrow=c(1,1))
plot(baseflowtest$Date, streamflow, type="l",ylim=c(0,5000))
lines(baseflowtest$Date, baseflowtest$bf)
lines(baseflowtest$Date, floodflow)
lines(baseflowtest$Date, zeroline, col="red")
lines(baseflowtest$Date, x90line, col="red")
lines(baseflowtest$Date, x95line, col="red")
lines(streamflowdate, x80line, col="red")