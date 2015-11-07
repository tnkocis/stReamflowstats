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
								  numpeaks = c(0), mean_peakflow = c(0), total_peakflow = c(0),
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
					  avgpeakflow <-0
				  } else {
					  avgpeakflow <- mean(peakflow3mon, na.rm=TRUE)
				  }
				  if(all(is.na(peakflow))){
					  total_peakflow <- 0
				  } else {
					  total_peakflow <- sum(peakflow3mon, na.rm=TRUE)
				  }

				  stats <- data.frame(TotVolAbv_acft=totalvolabv, TotDaysAbv = totaldaysabv, 
						  numpeaks = numpeaks3mon, mean_peakflow = avgpeakflow, total_peakflow=total_peakflow,
						  year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
			  }
		  } else if(mastertime=="hy"){
			  summary <- summary
			  if(length(summary$peak_flow)==0){
				  summary <- data.frame(peak_date=c(NA), peak_flow=c(NA), thres_value=c(NA), thres=c(NA),
						  start=c(NA),end=c(NA),
						  duration=c(NA),vol_acft_event=c(NA), year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
			  }
			  firstdate <- as.Date(summary$start[[1]])
			  enddate <- as.Date(tail(summary$end,1))
			  if(is.na(firstdate)|is.na(enddate)){
				  stats <- data.frame(TotVolAbv_acft=c(0), TotDaysAbv = c(0), 
						  numpeaks = c(0), mean_peakflow = c(0), total_peakflow = c(0),
						  year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
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
						  year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
			  }
		  }else {
			  stop("mastertime error")
		  }
	} else {
		summary <- data.frame(peak_date=c(NA), peak_flow=c(NA), thres_value=c(NA), thres=c(NA),
					  start=c(NA),end=c(NA),
					  duration=c(NA),vol_acft_event=c(NA), year=as.numeric(format(tail(input$Date,1),"%Y")), yeartype_index=yearindex)
		stats <- data.frame(TotVolAbv_acft=c(0), TotDaysAbv = c(0), 
					  numpeaks = c(0), mean_peakflow = c(0),total_peakflow=c(0),
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
#############################

peakflowssummarylist <- vector("list", length(peakflows))
for(i in 1:length(peakflows)){
	peakflowssummarylist[[i]] <- peakflows[[i]][[1]]
}
peakflowsummary <- do.call(rbind.data.frame,peakflowssummarylist)

peakflowstats$volday_is_zero <- rep(NA, length(peakflowstats$TotDaysAbv))
for(i in 1:length(peakflowstats$TotDaysAbv)){
	if(peakflowstats$TotDaysAbv[[i]]==0){
	peakflowstats$volday_is_zero[[i]] <- 1
}else {peakflowstats$volday_is_zero[[i]] <- 0}
}
peakflowstats$volday_is_zero_cumsum <- cumsum(peakflowstats$volday_is_zero)

gmtest <- glm(peakflowstats$TotVolAbv_acft[peakflowstats$TotVolAbv_acft != 0]~peakflowstats$year[peakflowstats$TotVolAbv_acft != 0], family=gaussian)
volpredict <- predict(gmtest)
glmzeropre <- glm(peakflowstats$volday_is_zero_cumsum[peakflowstats$year<1957]~peakflowstats$year[peakflowstats$year<1957], family=gaussian)
prezeropredict <- predict(glmzeropre)
glmzeropost <- glm(peakflowstats$volday_is_zero_cumsum[peakflowstats$year>1956]~peakflowstats$year[peakflowstats$year>1956], family=gaussian)
postzeropredict <- predict(glmzeropost)

pdf(file="C:\\Users\\tiffn_000\\Desktop\\Figures\\zerosep.pdf", width=8.5, height=11)
par(mfrow=c(2,1))
plot(peakflowstats$year[peakflowstats$TotVolAbv_acft != 0],peakflowstats$TotVolAbv_acft[peakflowstats$TotVolAbv_acft != 0], xlab="Year",ylab="Total Volume Above 95% (acft)")
lines(peakflowstats$year[peakflowstats$TotVolAbv_acft != 0],volpredict)

plot(peakflowstats$year,peakflowstats$volday_is_zero_cumsum, xlab="Year", ylab="Cumulative Number of Years With Zero Flow Above 95%")
lines(peakflowstats$year[peakflowstats$year<1957],prezeropredict)
lines(peakflowstats$year[peakflowstats$year>1956],postzeropredict)
dev.off()


######hyroyear#########
peakflowshy90 <- vector("list", length=length(american$`11446500`$HydroYear$Data))
names(peakflowshy90) <- names(american$`11446500`$HydroYear$Data)
for(i in 1:length(american$`11446500`$HydroYear$Data)){
	peakflowshy90[[i]] <-  peaks(input=american$`11446500`$HydroYear$Data[[i]],width=3, threshold=x90_3mon[["90%"]], thresholdname="90%", mastertime="hy", Index=american$`11446500`$Index)
}
peakflowsstatslisthy90 <- vector("list", length(peakflowshy90))
for(i in 1:length(peakflowshy90)){
	peakflowsstatslisthy90[[i]] <- peakflowshy90[[i]][[2]]
}
peakflowstatshy90 <- do.call(rbind.data.frame,peakflowsstatslisthy90)

peakflowssummarylisthy90 <- vector("list", length(peakflowshy90))
for(i in 1:length(peakflowshy90)){
	peakflowssummarylisthy90[[i]] <- peakflowshy90[[i]][[1]]
}
peakflowsummaryhy90 <- do.call(rbind.data.frame,peakflowssummarylisthy90)

peakflowstatshy90$volday_is_zero <- rep(NA, length(peakflowstatshy90$TotDaysAbv))
for(i in 1:length(peakflowstatshy90$TotDaysAbv)){
	if(peakflowstatshy90$TotDaysAbv[[i]]==0){
		peakflowstatshy90$volday_is_zero[[i]] <- 1
	}else {peakflowstatshy90$volday_is_zero[[i]] <- 0}
}
peakflowstatshy90$volday_is_zero_cumsum <- cumsum(peakflowstatshy90$volday_is_zero)

gmtesthy90 <- glm(peakflowstatshy90$TotVolAbv_acft[peakflowstatshy90$TotVolAbv_acft != 0]~peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0], family=gaussian)
volpredicthy90 <- predict(gmtesthy90)
glmzeroprehy90 <- glm(peakflowstatshy90$volday_is_zero_cumsum[peakflowstatshy90$year<1957]~peakflowstatshy90$year[peakflowstatshy90$year<1957], family=gaussian)
prezeropredicthy90 <- predict(glmzeroprehy90)
glmzeroposthy90 <- glm(peakflowstatshy90$volday_is_zero_cumsum[peakflowstatshy90$year>1956]~peakflowstatshy90$year[peakflowstatshy90$year>1956], family=gaussian)
postzeropredicthy90 <- predict(glmzeroposthy90)

pdf(file="C:\\Users\\tiffn_000\\Desktop\\Figures\\zerosephy90.pdf", width=8.5, height=11)
par(mfrow=c(2,1))
plot(peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0],peakflowstatshy90$TotVolAbv_acft[peakflowstatshy90$TotVolAbv_acft != 0], xlab="Year",ylab="Total Volume Above 90% (acft)")
lines(peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0],volpredicthy90)

plot(peakflowstatshy90$year,peakflowstatshy90$volday_is_zero_cumsum, xlab="Year", ylab="Cumulative Number of Years With Zero Flow Above 90%")
lines(peakflowstatshy90$year[peakflowstatshy90$year<1957],prezeropredicthy90)
lines(peakflowstatshy90$year[peakflowstatshy90$year>1956],postzeropredicthy90)
dev.off()


hypeakplots <- function(input){
	library(ggplot2)
	startyear <- format(input$peak_date[[1]],"%Y")
	startmon <- format(input$peak_date[[1]],"%m")
	endyear <- format(tail(input$peak_date,1),"%Y")
	endmon <- format(tail(input$peak_date,1),"%m")
	
	yrseq <- unique(input$year)
	
	pkdatem <- format(input$peak_date,"%m")
	pkdatey <- format(input$peak_date,"%Y")
	octlog <- pkdatem=="10"
	novlog <- pkdatem=="11" 
	declog <- pkdatem=="12" 
	janlog <- pkdatem=="01" 
	feblog <- pkdatem=="02" 
	marlog <- pkdatem=="03" 
	aprlog <- pkdatem=="04" 
	maylog <- pkdatem=="05" 
	junlog <- pkdatem=="06" 
	jullog <- pkdatem=="07" 
	auglog <- pkdatem=="08" 
	seplog <- pkdatem=="09" 
	
	octcount <- sum( octlog , na.rm=TRUE)
	novcount <- sum( novlog , na.rm=TRUE)
	deccount <- sum( declog , na.rm=TRUE)
	jancount <- sum( janlog , na.rm=TRUE)
	febcount <- sum( feblog , na.rm=TRUE)
	marcount <- sum( marlog , na.rm=TRUE)
	aprcount <- sum( aprlog , na.rm=TRUE)
	maycount <- sum( maylog , na.rm=TRUE)
	juncount <- sum( junlog , na.rm=TRUE)
	julcount <- sum( jullog , na.rm=TRUE)
	augcount <- sum( auglog , na.rm=TRUE)
	sepcount <- sum( seplog , na.rm=TRUE)
	
	octdf <- data.frame(st_hy_year=yrseq, numpeaks_oct=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_oct=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		octdf$numpeaks_oct[[i]] <- sum(pkdatem=="10"&pkdatey==yrseq[[i]], na.rm=TRUE)
		octdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(octdf$numpeaks_oct[[i]]==0&octdf$totpeaks_yr[[i]]==0){
			octdf$prop_peaks_oct[[i]] <- 0
		}else {
			octdf$prop_peaks_oct[[i]] <- octdf$numpeaks_oct[[i]]/octdf$totpeaks_yr[[i]]
		}
	}
	
	novdf <- data.frame(st_hy_year=yrseq, numpeaks_nov=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_nov=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		novdf$numpeaks_nov[[i]] <- sum(pkdatem=="11"&pkdatey==yrseq[[i]], na.rm=TRUE)
		novdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(novdf$numpeaks_nov[[i]]==0&novdf$totpeaks_yr[[i]]==0){
			novdf$prop_peaks_nov[[i]] <- 0
		}else {
			novdf$prop_peaks_nov[[i]] <- novdf$numpeaks_nov[[i]]/novdf$totpeaks_yr[[i]]
		}
	}
	
	decdf <- data.frame(st_hy_year=yrseq, numpeaks_dec=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_dec=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		decdf$numpeaks_dec[[i]] <- sum(pkdatem=="12"&pkdatey==yrseq[[i]], na.rm=TRUE)
		decdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(decdf$numpeaks_dec[[i]]==0&decdf$totpeaks_yr[[i]]==0){
			decdf$prop_peaks_dec[[i]] <- 0
		}else {
			decdf$prop_peaks_dec[[i]] <- decdf$numpeaks_dec[[i]]/decdf$totpeaks_yr[[i]]
		}
	}
	
	jandf <- data.frame(st_hy_year=yrseq, numpeaks_jan=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_jan=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		jandf$numpeaks_jan[[i]] <- sum(pkdatem=="01"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		jandf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(jandf$numpeaks_jan[[i]]==0&jandf$totpeaks_yr[[i]]==0){
			jandf$prop_peaks_jan[[i]] <- 0
		}else {
			jandf$prop_peaks_jan[[i]] <- jandf$numpeaks_jan[[i]]/jandf$totpeaks_yr[[i]]
		}
	}
	
	febdf <- data.frame(st_hy_year=yrseq, numpeaks_feb=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_feb=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		febdf$numpeaks_feb[[i]] <- sum(pkdatem=="02"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		febdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(febdf$numpeaks_feb[[i]]==0&febdf$totpeaks_yr[[i]]==0){
			febdf$prop_peaks_feb[[i]] <- 0
		}else {
			febdf$prop_peaks_feb[[i]] <- febdf$numpeaks_feb[[i]]/febdf$totpeaks_yr[[i]]
		}
	}
	
	mardf <- data.frame(st_hy_year=yrseq, numpeaks_mar=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_mar=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		mardf$numpeaks_mar[[i]] <- sum(pkdatem=="03"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		mardf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(mardf$numpeaks_mar[[i]]==0&mardf$totpeaks_yr[[i]]==0){
			mardf$prop_peaks_mar[[i]] <- 0
		}else {
			mardf$prop_peaks_mar[[i]] <- mardf$numpeaks_mar[[i]]/mardf$totpeaks_yr[[i]]
		}
	}
	
	aprdf <- data.frame(st_hy_year=yrseq, numpeaks_apr=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_apr=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		aprdf$numpeaks_apr[[i]] <- sum(pkdatem=="04"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		aprdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(aprdf$numpeaks_apr[[i]]==0&aprdf$totpeaks_yr[[i]]==0){
			aprdf$prop_peaks_apr[[i]] <- 0
		}else {
			aprdf$prop_peaks_apr[[i]] <- aprdf$numpeaks_apr[[i]]/aprdf$totpeaks_yr[[i]]
		}
	}
	
	maydf <- data.frame(st_hy_year=yrseq, numpeaks_may=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_may=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		maydf$numpeaks_may[[i]] <- sum(pkdatem=="05"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		maydf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(maydf$numpeaks_may[[i]]==0&maydf$totpeaks_yr[[i]]==0){
			maydf$prop_peaks_may[[i]] <- 0
		}else {
			maydf$prop_peaks_may[[i]] <- maydf$numpeaks_may[[i]]/maydf$totpeaks_yr[[i]]
		}
	}
	
	jundf <- data.frame(st_hy_year=yrseq, numpeaks_jun=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_jun=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		jundf$numpeaks_jun[[i]] <- sum(pkdatem=="06"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		jundf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(jundf$numpeaks_jun[[i]]==0&jundf$totpeaks_yr[[i]]==0){
			jundf$prop_peaks_jun[[i]] <- 0
		}else {
			jundf$prop_peaks_jun[[i]] <- jundf$numpeaks_jun[[i]]/jundf$totpeaks_yr[[i]]
		}
	}
	
	
	juldf <- data.frame(st_hy_year=yrseq, numpeaks_jul=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_jul=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		juldf$numpeaks_jul[[i]] <- sum(pkdatem=="07"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		juldf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(juldf$numpeaks_jul[[i]]==0&juldf$totpeaks_yr[[i]]==0){
			juldf$prop_peaks_jul[[i]] <- 0
		}else {
			juldf$prop_peaks_jul[[i]] <- juldf$numpeaks_jul[[i]]/juldf$totpeaks_yr[[i]]
		}
	}
	
	augdf <- data.frame(st_hy_year=yrseq, numpeaks_aug=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_aug=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		augdf$numpeaks_aug[[i]] <- sum(pkdatem=="08"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		augdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(augdf$numpeaks_aug[[i]]==0&augdf$totpeaks_yr[[i]]==0){
			augdf$prop_peaks_aug[[i]] <- 0
		}else {
			augdf$prop_peaks_aug[[i]] <- augdf$numpeaks_aug[[i]]/augdf$totpeaks_yr[[i]]
		}
	}
	
	sepdf <- data.frame(st_hy_year=yrseq, numpeaks_sep=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_sep=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		sepdf$numpeaks_sep[[i]] <- sum(pkdatem=="09"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		sepdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(sepdf$numpeaks_sep[[i]]==0&sepdf$totpeaks_yr[[i]]==0){
			sepdf$prop_peaks_sep[[i]] <- 0
		}else {
			sepdf$prop_peaks_sep[[i]] <- sepdf$numpeaks_sep[[i]]/sepdf$totpeaks_yr[[i]]
		}
	}
	
	plotoctnum <- ggplot(data=octdf,mapping=aes(x=st_hy_year,y=numpeaks_oct))+
			geom_bar(stat="identity", color="blue")+ ggtitle("oct") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotnovnum <- ggplot(data=novdf,mapping=aes(x=st_hy_year,y=numpeaks_nov))+
			geom_bar(stat="identity", color="blue")+ ggtitle("nov") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotdecnum <- ggplot(data=decdf,mapping=aes(x=st_hy_year,y=numpeaks_dec))+
			geom_bar(stat="identity", color="blue")+ ggtitle("dec") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotjannum <- ggplot(data=jandf,mapping=aes(x=st_hy_year,y=numpeaks_jan))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jan") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotfebnum <- ggplot(data=febdf,mapping=aes(x=st_hy_year,y=numpeaks_feb))+
			geom_bar(stat="identity", color="blue")+ ggtitle("feb") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotmarnum <- ggplot(data=mardf,mapping=aes(x=st_hy_year,y=numpeaks_mar))+
			geom_bar(stat="identity", color="blue")+ ggtitle("mar") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotaprnum <- ggplot(data=aprdf,mapping=aes(x=st_hy_year,y=numpeaks_apr))+
			geom_bar(stat="identity", color="blue")+ ggtitle("apr") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotmaynum <- ggplot(data=maydf,mapping=aes(x=st_hy_year,y=numpeaks_may))+
			geom_bar(stat="identity", color="blue")+ ggtitle("may") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotjunnum <- ggplot(data=jundf,mapping=aes(x=st_hy_year,y=numpeaks_jun))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jun") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotjulnum <- ggplot(data=juldf,mapping=aes(x=st_hy_year,y=numpeaks_jul))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jul") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotaugnum <- ggplot(data=augdf,mapping=aes(x=st_hy_year,y=numpeaks_aug))+
			geom_bar(stat="identity", color="blue")+ ggtitle("aug") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotsepnum <- ggplot(data=sepdf,mapping=aes(x=st_hy_year,y=numpeaks_sep))+
			geom_bar(stat="identity", color="blue")+ ggtitle("sep") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	numplots <- list(plotoctnum,plotnovnum,plotdecnum,plotjannum,plotfebnum,plotmarnum,plotaprnum,plotmaynum,
			plotjunnum,plotjulnum,plotaugnum,plotsepnum)
	names(numplots)<-c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP")
	
	plotoctprop <- ggplot(data=octdf,mapping=aes(x=st_hy_year,y=prop_peaks_oct))+
			geom_bar(stat="identity", color="blue")+ ggtitle("oct") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotnovprop <- ggplot(data=novdf,mapping=aes(x=st_hy_year,y=prop_peaks_nov))+
			geom_bar(stat="identity", color="blue")+ ggtitle("nov") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotdecprop <- ggplot(data=decdf,mapping=aes(x=st_hy_year,y=prop_peaks_dec))+
			geom_bar(stat="identity", color="blue")+ ggtitle("dec") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotjanprop <- ggplot(data=jandf,mapping=aes(x=st_hy_year,y=prop_peaks_jan))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jan") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotfebprop <- ggplot(data=febdf,mapping=aes(x=st_hy_year,y=prop_peaks_feb))+
			geom_bar(stat="identity", color="blue")+ ggtitle("feb") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotmarprop <- ggplot(data=mardf,mapping=aes(x=st_hy_year,y=prop_peaks_mar))+
			geom_bar(stat="identity", color="blue")+ ggtitle("mar") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotaprprop <- ggplot(data=aprdf,mapping=aes(x=st_hy_year,y=prop_peaks_apr))+
			geom_bar(stat="identity", color="blue")+ ggtitle("apr") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotmayprop <- ggplot(data=maydf,mapping=aes(x=st_hy_year,y=prop_peaks_may))+
			geom_bar(stat="identity", color="blue")+ ggtitle("may") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotjunprop <- ggplot(data=jundf,mapping=aes(x=st_hy_year,y=prop_peaks_jun))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jun") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotjulprop <- ggplot(data=juldf,mapping=aes(x=st_hy_year,y=prop_peaks_jul))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jul") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotaugprop <- ggplot(data=augdf,mapping=aes(x=st_hy_year,y=prop_peaks_aug))+
			geom_bar(stat="identity", color="blue")+ ggtitle("aug") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotsepprop <- ggplot(data=sepdf,mapping=aes(x=st_hy_year,y=prop_peaks_sep))+
			geom_bar(stat="identity", color="blue")+ ggtitle("sep") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	propplots <- list(plotoctprop,plotnovprop,plotdecprop,plotjanprop,plotfebprop,plotmarprop,plotaprprop,plotmayprop,
			plotjunprop,plotjulprop,plotaugprop,plotsepprop)
	names(propplots)<-c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP")
	
	return(list(numplots=numplots, propplots=propplots))
}

