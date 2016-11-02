# TODO: Add comment
# 
# Author: tnkocis
###############################################################################

edit_simplified_peakanalysis <- function(input, width, threshold,thresholdname, mastertime, Index){
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
					volshy <- dischrange[thresmonloghy]
					volsabvhy <- volshy - threshold
					totalvolabv <-sum(volsabvhy,na.rm=TRUE)*86400*2.29568411e-5
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
				volsabv <- vols - threshold
				monthly_stats$TotVolAbv_acft[[i]] <-sum(volsabv,na.rm=TRUE)*86400*2.29568411e-5
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

# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

library(dplyr)
library(hydroTSM)
library(dataRetrieval)

SacV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_svi.txt")
SJV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_sji.txt")
yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")


load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\functions_5_13_16",".RData", sep=""))


cleanupHYfrom6MON <- function(inputhy, input6mon){
	if (missing(inputhy))
		stop("Input data to clean is required.")
	if (missing(input6mon))
		stop("Input data to clean is required.")
	for(i in length(inputhy$Data):1){
		if(sum(input6mon$Data[[i]]$Available, na.rm=TRUE)<165){
			inputhy$Data[[i]] <- NULL
		}
	}	
	return(inputhy)
}


	datetable <- read.csv("C:\\Users\\tiffn_000\\Documents\\date_table_short.csv",stringsAsFactors=FALSE, colClasses=c("integer","character","character","character"))
#	txtgauges <-  list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\", pattern=".csv")
#	txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
#	txtgauges <- txtgauges[txtgauges != ""]
	spbatch_g <- datetable$gauge
	
	
	spbatch <- vector("list", length(spbatch_g))
	for(z in 1:length(spbatch_g)){
		spbatch[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",spbatch_g[[z]],".csv",sep=""), header=TRUE,stringsAsFactors=FALSE)
		spbatch[[z]]$raw$Date <- as.Date(spbatch[[z]]$raw$Date, "%Y-%m-%d")
		
		spbatch[[z]]$raw <- RemoveLeapDays(spbatch[[z]]$raw)
		
		spbatch[[z]]$Index$Valley <- "SJV"
		spbatch[[z]]$Index$Index <- yeartype_old$SJI
		spbatch[[z]]$Index$Year <- yeartype_old$Year
		
		###DATA PROCESSING
		spbatch[[z]]$prep <- prepdata(spbatch[[z]]$raw)
		spbatch[[z]]$Availability <- DataAvailability(spbatch[[z]]$raw)
		spbatch[[z]]$thresholds_maf <- thresholds(spbatch[[z]]$prep)
		
		
		if(all(spbatch[[z]]$thresholds_maf==0)){
		} else {
			
#			spbatch[[z]]$Winter_3mon <- Split3Winter(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholds_maf)
			spbatch[[z]]$Winter_6mon <- Split6Winter(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholds_maf)
#			spbatch[[z]]$Winter_monthly <- SplitWinterMonthly(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholds_maf)
			spbatch[[z]]$HydroYear <- SplitHydroYear(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholds_maf)	
			spbatch[[z]]$HydroYear <- cleanupHYfrom6MON(spbatch[[z]]$HydroYear,spbatch[[z]]$Winter_6mon)
#			spbatch[[z]]$Winter_6mon <- cleanup6MON(spbatch[[z]]$Winter_6mon)
#			spbatch[[z]]$Winter_3mon <- cleanup3MON(spbatch[[z]]$Winter_3mon)			
		}
	}
	names(spbatch) <- spbatch_g
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_full_record_spbatch_",1,".RData",sep=""))

load( "C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_base.RData")
load( "C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_93_spbatch.RData")
spbatch <- spbatch_93_unlist
spbatch_93_unlist <- NULL
save.image( "C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_93_spbatch.RData")

for(y in 90:90){	
	for(z in 1:1){
		batchnum <- z
#		load( "C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_93_spbatch.RData")
		#		load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\functions_5_13_16",".RData", sep=""))
		
		thresholdchoice <- y
		thresholdtext <- paste(thresholdchoice,"%",sep="")
		
		#		for(k in 1:length(spbatch)){
		#			spbatch[[k]]$thresholds_maf <- thresholds(spbatch[[k]]$prep)
		#		}
		
		test_peakflows <- vector("list", length(spbatch))
		test_peakflowstats <- vector("list", length(spbatch))
		test_peakflowsummary <- vector("list", length(spbatch))
		test_peakflowmonthlystats <- vector("list", length(spbatch))
		test_peakflowsdf <- vector("list", length(spbatch))
		for(k in 1:length(spbatch)){
			test_peakflows[[k]] <- vector("list",1)
			names(test_peakflows[[k]]) <- c("peakflows")
			test_peakflows[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$HydroYear$Data))
			for(i in 1:length(spbatch[[k]]$HydroYear$Data)){
				test_peakflows[[k]]$peakflows[[i]] <- edit_simplified_peakanalysis(input=spbatch[[k]]$HydroYear$Data[[i]],
						width=3, threshold=(eval(parse(text=paste("spbatch[[k]]$thresholds_maf$P",thresholdchoice,"maf/(86400*2.29568411e-5*1e-6)",sep="")))), 
						thresholdname=thresholdtext, mastertime="hy", Index=spbatch[[k]]$Index)
				
			}
			test_peakflowstats[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
			for(i in 1:length(test_peakflows[[k]]$peakflows))	{
				test_peakflowstats[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[2]]
			}
			test_peakflowsummary[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
			for(i in 1:length(test_peakflows[[k]]$peakflows))	{
				test_peakflowsummary[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[1]]
			}
			test_peakflowmonthlystats[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
			for(i in 1:length(test_peakflows[[k]]$peakflows))	{
				test_peakflowmonthlystats[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[3]]
			}
			test_peakflowsdf[[k]] <- vector("list",3)
			names(test_peakflowsdf[[k]]) <- c("pfstatsdf","pfsummarydf","pfmonthlystats")
			test_peakflowsdf[[k]]$pfstatsdf <- do.call(rbind.data.frame,test_peakflowstats[[k]])
			test_peakflowsdf[[k]]$pfsummarydf <- do.call(rbind.data.frame,test_peakflowsummary[[k]])
			test_peakflowsdf[[k]]$pfmonthlystats <- do.call(rbind.data.frame,test_peakflowmonthlystats[[k]])
			test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero <- rep(NA, length(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv))
			for(i in 1:length(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv)){
				if(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv[[i]]==0){
					test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 1
				}else{
					test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 0
				}
			}
			test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero_cumsum <- cumsum(test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero)	
		}
		names(test_peakflowsdf)<- names(spbatch)
		names(test_peakflows)<- names(spbatch)
		names(test_peakflowstats)<- names(spbatch)
		names(test_peakflowsummary)<- names(spbatch)
		names(test_peakflowmonthlystats)<- names(spbatch)
		test_split <- vector("list", length(spbatch))
		for(k in 1:length(spbatch)){
			test_split[[k]]<- peakflowanalysis_split(test_peakflowsdf[[k]]$pfmonthlystats)
		}
		names(test_split)<- names(spbatch)	
		
		test_peakflowmags_full <- vector("list", length(spbatch))
		for(k in 1:length(spbatch)){
			test_peakflowmags_full[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],1800)
			
		}
		names(test_peakflowmags_full) <- names(spbatch)
		
		test_peakflowmags_full_bind <- test_peakflowmags_full[[1]]
		for(k in 2:length(spbatch)){
			for(i in 1:6){
				for(l in 1:15){
					test_peakflowmags_full_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_full_bind[[i]][[l]],
							test_peakflowmags_full[[k]][[i]][[l]])
				}
			}
		}
		
		
		test_peakflowmags_postimp <- vector("list", length(spbatch))
		for(k in 1:length(spbatch)){
			#change here for post-imp period?
			if(names(test_split)[[k]]%in%SacV_gauges$site_no){
				postimpyear <- 1969
			}else{
				postimpyear <- 1988
			}
			test_peakflowmags_postimp[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],postimpyear)
			
		}
		names(test_peakflowmags_postimp) <- names(spbatch)
		
		test_peakflowmags_postimp_bind <- test_peakflowmags_postimp[[1]]
		for(k in 2:length(spbatch)){
			for(i in 1:6){
				for(l in 1:15){
					test_peakflowmags_postimp_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_postimp_bind[[i]][[l]],
							test_peakflowmags_postimp[[k]][[i]][[l]])
				}
			}
		}
		
		
		#		for(i in 1:15){
		#			write.csv(test_peakflowmags_full_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\all\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$all)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\W\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$W)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\AN\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$AN)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\BN\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$BN)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\D\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$D)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\C\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$C)[[i]],".csv", sep=""))
		#		}
		#		
		
#		save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\testtest33_percentile_",thresholdchoice,"_spbatch_",batchnum,".RData", sep=""))
		save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_spbatch93_percentile_",thresholdchoice,".RData", sep=""))
		
		
	}	
}

#thresholdsvect <- c(90:99)	
#gwmodel_data <- vector("list", length(thresholdsvect))
#for(w in 1:length(thresholdsvect)){
#	percnum <- w
#	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\testtest_percentile_",thresholdsvect[[percnum]],"_spbatch_",1,".RData", sep=""))
#	gwmodel_data[[percnum]] <- test_split
#}
#names(gwmodel_data)<- thresholdsvect
#
#baseyear <- data.frame(sthyyear=seq(1900,2016,1))
#final_data <- vector("list",10)
#for(i in 1:length(final_data)){
#	final_data[[i]] <- vector("list",6)
#	for(j in 1:length(final_data[[i]])){
#		final_kw_all <- merge(baseyear,gwmodel_data[[i]]$kw_final$all[[j+4]][,c("TotVolAbv_acft","sthyyear")],by.x="sthyyear",by.y="sthyyear",all.x=TRUE)
#		final_kw_all$TotVolAbv_acft[is.na(final_kw_all$TotVolAbv_acft)] <- 0
#		final_11211300_all <- merge(baseyear,gwmodel_data[[i]]$`11211300`$all[[j+4]][,c("TotVolAbv_acft","sthyyear")],by.x="sthyyear",by.y="sthyyear",all.x=TRUE)
#		final_11211300_all$TotVolAbv_acft[is.na(final_11211300_all$TotVolAbv_acft)] <- 0
#		final_11211790_all <- merge(baseyear,gwmodel_data[[i]]$`11211790`$all[[j+4]][,c("TotVolAbv_acft","sthyyear")],by.x="sthyyear",by.y="sthyyear",all.x=TRUE)
#		final_11211790_all$TotVolAbv_acft[is.na(final_11211790_all$TotVolAbv_acft)] <- 0
#		final_tul_all <- merge(baseyear,gwmodel_data[[i]]$tul_final$all[[j+4]][,c("TotVolAbv_acft","sthyyear")],by.x="sthyyear",by.y="sthyyear",all.x=TRUE)
#		final_tul_all$TotVolAbv_acft[is.na(final_tul_all$TotVolAbv_acft)] <- 0
#		
#		final_data[[i]][[j]] <- data.frame(IRV420_acft=(final_kw_all$TotVolAbv_acft+final_11211300_all$TotVolAbv_acft),
#				IRV421_acft=final_11211790_all$TotVolAbv_acft,
#				IRV10_acft=final_tul_all$TotVolAbv_acft,
#				sthyyear=baseyear$sthyyear)
#	}
#	names(final_data[[i]]) <- c("nov","dec","jan","feb","mar","apr")
#}
#names(final_data) <- names(gwmodel_data)
#
#for(i in 1:length(final_data)){
#	for(j in 1:length(final_data[[i]])){
#		final_data[[i]][[j]]$endhyyear <- final_data[[i]][[j]]$sthyyear+1
#		write.csv(final_data[[i]][[j]],file=paste("C:/Users/tiffn_000/Documents/",names(final_data)[[i]],names(final_data[[i]])[[j]],".csv",sep=""))
#	}
#}
