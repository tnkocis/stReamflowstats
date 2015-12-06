# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


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



######################
spall <- unique(impairmentsdf$downstreamgauge)

for(i in 2:7){
	batchnum <- i
	spbatch <- spall[(15*i-14):(15*i)]
	spbatch <- spbatch[!is.na(spbatch)]
	
	spbatch_g <- as.numeric(spbatch)
	
	
	txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
	txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
	txtgauges <- txtgauges[txtgauges != ""]
	spbatch_g <- spbatch_g[which(spbatch_g %in% txtgauges)]
	
	
	spbatch <- vector("list", length(spbatch_g))
	for(z in 1:length(spbatch_g)){
		spbatch[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",spbatch_g[[z]],".csv",sep=""), header=TRUE)
		spbatch[[z]]$raw$Date <- as.Date(spbatch[[z]]$raw$Date, "%Y-%m-%d")
		
		spbatch[[z]]$raw <- RemoveLeapDays(spbatch[[z]]$raw)
		
		if(as.numeric(spbatch[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
			spbatch[[z]]$Index$Valley <- "SacV"
			spbatch[[z]]$Index$Index <- yeartype_old$SVI
			spbatch[[z]]$Index$Year <- yeartype_old$Year
		} else if(as.numeric(spbatch[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
			spbatch[[z]]$Index$Valley <- "SJV"
			spbatch[[z]]$Index$Index <- yeartype_old$SJI
			spbatch[[z]]$Index$Year <- yeartype_old$Year
		} else {
			spbatch[[z]]$Index$Valley <- "ERROR"
			print(paste("Error",spbatch[[z]]$raw$site_no[[1]]))
		}
		
		###DATA PROCESSING
		spbatch[[z]]$prep <- prepdata(spbatch[[z]]$raw)
		spbatch[[z]]$Availability <- DataAvailability(spbatch[[z]]$raw)
		spbatch[[z]]$dams <- damsearch(spbatch[[z]], impairmentsdf)
		spbatch[[z]]$thresholdsdams_maf <- thresholdsdams(spbatch[[z]]$prep, spbatch[[z]]$dams)
		
	
		if(all(spbatch[[z]]$thresholdsdams_maf==0)){
		} else {
			
			spbatch[[z]]$Winter_3mon <- Split3Winter(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholdsdams_maf)
			spbatch[[z]]$Winter_6mon <- Split6Winter(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholdsdams_maf)
			spbatch[[z]]$Winter_monthly <- SplitWinterMonthly(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholdsdams_maf)
			spbatch[[z]]$HydroYear <- SplitHydroYear(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholdsdams_maf)	
			spbatch[[z]]$HydroYear <- cleanupHY(spbatch[[z]]$HydroYear)
			spbatch[[z]]$Winter_6mon <- cleanup6MON(spbatch[[z]]$Winter_6mon)
			spbatch[[z]]$Winter_3mon <- cleanup3MON(spbatch[[z]]$Winter_3mon)
			spbatch[[z]]$thresholdsdams3mon_maf <- thresholdsdams3mon(spbatch[[z]]$Winter_3mon, spbatch[[z]]$dams)
			
		}
	}
	names(spbatch) <- spbatch_g
	
	spbatch_peakflows <- vector("list", length(spbatch))
	spbatch_peakflowstats <- vector("list", length(spbatch))
	spbatch_peakflowsummary <- vector("list", length(spbatch))
	spbatch_peakflowmonthlystats <- vector("list", length(spbatch))
	spbatch_peakflowsdf <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		spbatch_peakflows[[k]] <- vector("list",1)
		names(spbatch_peakflows[[k]]) <- c("peakflows")
		spbatch_peakflows[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$HydroYear$Data))
		for(i in 1:length(spbatch[[k]]$HydroYear$Data)){
			spbatch_peakflows[[k]]$peakflows[[i]] <- peakanalysis(input=spbatch[[k]]$HydroYear$Data[[i]],
					width=3, threshold=(spbatch[[k]]$thresholdsdams_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="hy", Index=spbatch[[k]]$Index)
		}
		spbatch_peakflowstats[[k]] <- vector("list",length(spbatch_peakflows[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows[[k]]$peakflows))	{
			spbatch_peakflowstats[[k]][[i]] <- spbatch_peakflows[[k]]$peakflows[[i]][[2]]
		}
		spbatch_peakflowsummary[[k]] <- vector("list",length(spbatch_peakflows[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows[[k]]$peakflows))	{
			spbatch_peakflowsummary[[k]][[i]] <- spbatch_peakflows[[k]]$peakflows[[i]][[1]]
		}
		spbatch_peakflowmonthlystats[[k]] <- vector("list",length(spbatch_peakflows[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows[[k]]$peakflows))	{
			spbatch_peakflowmonthlystats[[k]][[i]] <- spbatch_peakflows[[k]]$peakflows[[i]][[3]]
		}
		spbatch_peakflowsdf[[k]] <- vector("list",3)
		names(spbatch_peakflowsdf[[k]]) <- c("pfstatsdf","pfsummarydf","pfmonthlystats")
		spbatch_peakflowsdf[[k]]$pfstatsdf <- do.call(rbind.data.frame,spbatch_peakflowstats[[k]])
		spbatch_peakflowsdf[[k]]$pfsummarydf <- do.call(rbind.data.frame,spbatch_peakflowsummary[[k]])
		spbatch_peakflowsdf[[k]]$pfmonthlystats <- do.call(rbind.data.frame,spbatch_peakflowmonthlystats[[k]])
		spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero <- rep(NA, length(spbatch_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv))
		for(i in 1:length(spbatch_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv)){
			if(spbatch_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv[[i]]==0){
				spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 1
			}else{
				spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 0
			}
		}
		spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero_cumsum <- cumsum(spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero)	
	}
	names(spbatch_peakflowsdf)<- names(spbatch)
	names(spbatch_peakflows)<- names(spbatch)
	names(spbatch_peakflowstats)<- names(spbatch)
	names(spbatch_peakflowsummary)<- names(spbatch)
	names(spbatch_peakflowmonthlystats)<- names(spbatch)
	
	for(k in 1:length(spbatch)){
		damsunique <- data.frame(year=unique(spbatch[[k]]$dams$YEAR_BUILT), totalcapacity_yr = rep(NA,length(unique(spbatch[[k]]$dams$YEAR_BUILT))))
		for(i in 1:length(damsunique$year)){
			damsunique$totalcapacity_yr[[i]] <- sum(spbatch[[k]]$dams$CAPACITY__[which(spbatch[[k]]$dams$YEAR_BUILT==damsunique$year[[i]])])	
		}
		spbatch[[k]]$damsunique <- damsunique
	}
	
	spbatch_peakflows3 <- vector("list", length(spbatch))
	spbatch_peakflowstats3 <- vector("list", length(spbatch))
	spbatch_peakflowsummary3 <- vector("list", length(spbatch))
	spbatch_peakflowmonthlystats3 <- vector("list", length(spbatch))
	spbatch_peakflowsdf3 <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		spbatch_peakflows3[[k]] <- vector("list",1)
		names(spbatch_peakflows3[[k]]) <- c("peakflows")
		spbatch_peakflows3[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$Winter_3mon$Data))
		for(i in 1:length(spbatch[[k]]$Winter_3mon$Data)){
			spbatch_peakflows3[[k]]$peakflows[[i]] <- peakanalysis(input=spbatch[[k]]$Winter_3mon$Data[[i]],
					width=3, threshold=(spbatch[[k]]$thresholdsdams3mon_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="3mon", Index=spbatch[[k]]$Index)
		}
		spbatch_peakflowstats3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowstats3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[2]]
		}
		spbatch_peakflowsummary3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowsummary3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[1]]
		}
		spbatch_peakflowmonthlystats3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowmonthlystats3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[3]]
		}
		spbatch_peakflowsdf3[[k]] <- vector("list",3)
		names(spbatch_peakflowsdf3[[k]]) <- c("pfstatsdf3","pfsummarydf3","pfmonthlystats3")
		spbatch_peakflowsdf3[[k]]$pfstatsdf3 <- do.call(rbind.data.frame,spbatch_peakflowstats3[[k]])
		spbatch_peakflowsdf3[[k]]$pfsummarydf3 <- do.call(rbind.data.frame,spbatch_peakflowsummary3[[k]])
		spbatch_peakflowsdf3[[k]]$pfmonthlystats3 <- do.call(rbind.data.frame,spbatch_peakflowmonthlystats3[[k]])
		spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero <- rep(NA, length(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv))
		for(i in 1:length(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv)){
			if(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv[[i]]==0){
				spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero[[i]] <- 1
			}else{
				spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero[[i]] <- 0
			}
		}
		spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero_cumsum <- cumsum(spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero)	
	}
	names(spbatch_peakflowsdf3)<- names(spbatch)
	names(spbatch_peakflows3)<- names(spbatch)
	names(spbatch_peakflowstats3)<- names(spbatch)
	names(spbatch_peakflowsummary3)<- names(spbatch)
	names(spbatch_peakflowmonthlystats3)<- names(spbatch)

	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\spbatch_",batchnum,".RData", sep=""))
}


for(i in 1:7){
	batchnum <- i
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\spbatch_",batchnum,".RData", sep=""))
	
	
	peakflowtrends <- function(pfstatsdf, gauge, year){
		library(zoo)
		
		meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft,5,na.pad=TRUE)
		meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft,10,na.pad=TRUE)
		meantotvol20 <- rollmean(pfstatsdf$TotVolAbv_acft,20,na.pad=TRUE)
		meantotvol30 <- rollmean(pfstatsdf$TotVolAbv_acft,30,na.pad=TRUE)
		
		MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft)
		MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
		MKmeantotvol5 <- MannKendall(meantotvol5)
		MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
		MKmeantotvol10 <- MannKendall(meantotvol10)
		MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))
		MKmeantotvol20 <- MannKendall(meantotvol20)
		MKmeantotvol20 <- data.frame(tau=MKmeantotvol20$tau[[1]],p2=MKmeantotvol20$sl[[1]],window=c(20),measure=c("totvolabv"))
		MKmeantotvol30 <- MannKendall(meantotvol30)
		MKmeantotvol30 <- data.frame(tau=MKmeantotvol30$tau[[1]],p2=MKmeantotvol30$sl[[1]],window=c(30),measure=c("totvolabv"))
		MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10,MKmeantotvol20,MKmeantotvol30)
		
		SNmeantotvol1 <- lm(pfstatsdf$TotVolAbv_acft ~ pfstatsdf$year)
		SNmeantotvol1 <- data.frame(slope=SNmeantotvol1$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol1))/var(residuals(SNmeantotvol1))),window=c(1),measure=c("totvolabv"))
		SNmeantotvol5 <- lm(meantotvol5 ~ pfstatsdf$year)
		SNmeantotvol5 <- data.frame(slope=SNmeantotvol5$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol5))/var(residuals(SNmeantotvol5))),window=c(5),measure=c("totvolabv"))
		SNmeantotvol10 <- lm(meantotvol10 ~ pfstatsdf$year)
		SNmeantotvol10 <- data.frame(slope=SNmeantotvol10$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol10))/var(residuals(SNmeantotvol10))),window=c(10),measure=c("totvolabv"))
		SNmeantotvol20 <- lm(meantotvol20 ~ pfstatsdf$year)
		SNmeantotvol20 <- data.frame(slope=SNmeantotvol20$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol20))/var(residuals(SNmeantotvol20))),window=c(20),measure=c("totvolabv"))
		SNmeantotvol30 <- lm(meantotvol30 ~ pfstatsdf$year)
		SNmeantotvol30 <- data.frame(slope=SNmeantotvol30$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol30))/var(residuals(SNmeantotvol30))),window=c(30),measure=c("totvolabv"))
		SNmeantotvol <- rbind.data.frame(SNmeantotvol1,SNmeantotvol5,SNmeantotvol10,SNmeantotvol20,SNmeantotvol30)
		
		
		############################
		meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv,5,na.pad=TRUE)
		meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv,10,na.pad=TRUE)
		meantotdays20 <- rollmean(pfstatsdf$TotDaysAbv,20,na.pad=TRUE)
		meantotdays30 <- rollmean(pfstatsdf$TotDaysAbv,30,na.pad=TRUE)
		
		MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv)
		MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
		MKmeantotdays5 <- MannKendall(meantotdays5)
		MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
		MKmeantotdays10 <- MannKendall(meantotdays10)
		MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
		MKmeantotdays20 <- MannKendall(meantotdays20)
		MKmeantotdays20 <- data.frame(tau=MKmeantotdays20$tau[[1]],p2=MKmeantotdays20$sl[[1]],window=c(20),measure=c("totdaysabv"))
		MKmeantotdays30 <- MannKendall(meantotdays30)
		MKmeantotdays30 <- data.frame(tau=MKmeantotdays30$tau[[1]],p2=MKmeantotdays30$sl[[1]],window=c(30),measure=c("totdaysabv"))
		MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10,MKmeantotdays20,MKmeantotdays30)
		
		SNmeantotdays1 <- lm(pfstatsdf$TotDaysAbv ~ pfstatsdf$year)
		SNmeantotdays1 <- data.frame(slope=SNmeantotdays1$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays1))/var(residuals(SNmeantotdays1))),window=c(1),measure=c("totdaysabv"))
		SNmeantotdays5 <- lm(meantotdays5 ~ pfstatsdf$year)
		SNmeantotdays5 <- data.frame(slope=SNmeantotdays5$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays5))/var(residuals(SNmeantotdays5))),window=c(5),measure=c("totdaysabv"))
		SNmeantotdays10 <- lm(meantotdays10 ~ pfstatsdf$year)
		SNmeantotdays10 <- data.frame(slope=SNmeantotdays10$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays10))/var(residuals(SNmeantotdays10))),window=c(10),measure=c("totdaysabv"))
		SNmeantotdays20 <- lm(meantotdays20 ~ pfstatsdf$year)
		SNmeantotdays20 <- data.frame(slope=SNmeantotdays20$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays20))/var(residuals(SNmeantotdays20))),window=c(20),measure=c("totdaysabv"))
		SNmeantotdays30 <- lm(meantotdays30 ~ pfstatsdf$year)
		SNmeantotdays30 <- data.frame(slope=SNmeantotdays30$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays30))/var(residuals(SNmeantotdays30))),window=c(30),measure=c("totdaysabv"))
		SNmeantotdays <- rbind.data.frame(SNmeantotdays1,SNmeantotdays5,SNmeantotdays10,SNmeantotdays20,SNmeantotdays30)
		
		####################
		
		meannumpks5 <- rollmean(pfstatsdf$numpeaks,5,na.pad=TRUE)
		meannumpks10 <- rollmean(pfstatsdf$numpeaks,10,na.pad=TRUE)
		meannumpks20 <- rollmean(pfstatsdf$numpeaks,20,na.pad=TRUE)
		meannumpks30 <- rollmean(pfstatsdf$numpeaks,30,na.pad=TRUE)
		
		MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks)
		MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
		MKmeannumpks5 <- MannKendall(meannumpks5)
		MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
		MKmeannumpks10 <- MannKendall(meannumpks10)
		MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
		MKmeannumpks20 <- MannKendall(meannumpks20)
		MKmeannumpks20 <- data.frame(tau=MKmeannumpks20$tau[[1]],p2=MKmeannumpks20$sl[[1]],window=c(20),measure=c("numpeaksabv"))
		MKmeannumpks30 <- MannKendall(meannumpks30)
		MKmeannumpks30 <- data.frame(tau=MKmeannumpks30$tau[[1]],p2=MKmeannumpks30$sl[[1]],window=c(30),measure=c("numpeaksabv"))
		MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10,MKmeannumpks20,MKmeannumpks30)
		
		SNmeannumpks1 <- lm(pfstatsdf$TotDaysAbv ~ pfstatsdf$year)
		SNmeannumpks1 <- data.frame(slope=SNmeannumpks1$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks1))/var(residuals(SNmeannumpks1))),window=c(1),measure=c("numpeaksabv"))
		SNmeannumpks5 <- lm(meannumpks5 ~ pfstatsdf$year)
		SNmeannumpks5 <- data.frame(slope=SNmeannumpks5$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks5))/var(residuals(SNmeannumpks5))),window=c(5),measure=c("numpeaksabv"))
		SNmeannumpks10 <- lm(meannumpks10 ~ pfstatsdf$year)
		SNmeannumpks10 <- data.frame(slope=SNmeannumpks10$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks10))/var(residuals(SNmeannumpks10))),window=c(10),measure=c("numpeaksabv"))
		SNmeannumpks20 <- lm(meannumpks20 ~ pfstatsdf$year)
		SNmeannumpks20 <- data.frame(slope=SNmeannumpks20$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks20))/var(residuals(SNmeannumpks20))),window=c(20),measure=c("numpeaksabv"))
		SNmeannumpks30 <- lm(meannumpks30 ~ pfstatsdf$year)
		SNmeannumpks30 <- data.frame(slope=SNmeannumpks30$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks30))/var(residuals(SNmeannumpks30))),window=c(30),measure=c("numpeaksabv"))
		SNmeannumpks <- rbind.data.frame(SNmeannumpks1,SNmeannumpks5,SNmeannumpks10,SNmeannumpks20,SNmeannumpks30)
		
		####################
		
		
		meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow,5,na.pad=TRUE)
		meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow,10,na.pad=TRUE)
		meanmeanpks20 <- rollmean(pfstatsdf$mean_peakflow,20,na.pad=TRUE)
		meanmeanpks30 <- rollmean(pfstatsdf$mean_peakflow,30,na.pad=TRUE)
		
		MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow)
		MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
		MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
		MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
		MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
		MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
		MKmeanmeanpks20 <- MannKendall(meanmeanpks20)
		MKmeanmeanpks20 <- data.frame(tau=MKmeanmeanpks20$tau[[1]],p2=MKmeanmeanpks20$sl[[1]],window=c(20),measure=c("meanpeaksabv"))
		MKmeanmeanpks30 <- MannKendall(meanmeanpks30)
		MKmeanmeanpks30 <- data.frame(tau=MKmeanmeanpks30$tau[[1]],p2=MKmeanmeanpks30$sl[[1]],window=c(30),measure=c("meanpeaksabv"))
		MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10,MKmeanmeanpks20,MKmeanmeanpks30)
		
		SNmeanmeanpks1 <- lm(pfstatsdf$TotDaysAbv ~ pfstatsdf$year)
		SNmeanmeanpks1 <- data.frame(slope=SNmeanmeanpks1$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks1))/var(residuals(SNmeanmeanpks1))),window=c(1),measure=c("meanpeaksabv"))
		SNmeanmeanpks5 <- lm(meanmeanpks5 ~ pfstatsdf$year)
		SNmeanmeanpks5 <- data.frame(slope=SNmeanmeanpks5$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks5))/var(residuals(SNmeanmeanpks5))),window=c(5),measure=c("meanpeaksabv"))
		SNmeanmeanpks10 <- lm(meanmeanpks10 ~ pfstatsdf$year)
		SNmeanmeanpks10 <- data.frame(slope=SNmeanmeanpks10$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks10))/var(residuals(SNmeanmeanpks10))),window=c(10),measure=c("meanpeaksabv"))
		SNmeanmeanpks20 <- lm(meanmeanpks20 ~ pfstatsdf$year)
		SNmeanmeanpks20 <- data.frame(slope=SNmeanmeanpks20$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks20))/var(residuals(SNmeanmeanpks20))),window=c(20),measure=c("meanpeaksabv"))
		SNmeanmeanpks30 <- lm(meanmeanpks30 ~ pfstatsdf$year)
		SNmeanmeanpks30 <- data.frame(slope=SNmeanmeanpks30$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks30))/var(residuals(SNmeanmeanpks30))),window=c(30),measure=c("meanpeaksabv"))
		SNmeanmeanpks <- rbind.data.frame(SNmeanmeanpks1,SNmeanmeanpks5,SNmeanmeanpks10,SNmeanmeanpks20,SNmeanmeanpks30)
		
		####################
		
		meantotpks5 <- rollmean(pfstatsdf$total_peakflow,5,na.pad=TRUE)
		meantotpks10 <- rollmean(pfstatsdf$total_peakflow,10,na.pad=TRUE)
		meantotpks20 <- rollmean(pfstatsdf$total_peakflow,20,na.pad=TRUE)
		meantotpks30 <- rollmean(pfstatsdf$total_peakflow,30,na.pad=TRUE)
		
		MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow)
		MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
		MKmeantotpks5 <- MannKendall(meantotpks5)
		MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
		MKmeantotpks10 <- MannKendall(meantotpks10)
		MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
		MKmeantotpks20 <- MannKendall(meantotpks20)
		MKmeantotpks20 <- data.frame(tau=MKmeantotpks20$tau[[1]],p2=MKmeantotpks20$sl[[1]],window=c(20),measure=c("totpeakflwabv"))
		MKmeantotpks30 <- MannKendall(meantotpks30)
		MKmeantotpks30 <- data.frame(tau=MKmeantotpks30$tau[[1]],p2=MKmeantotpks30$sl[[1]],window=c(30),measure=c("totpeakflwabv"))
		MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10,MKmeantotpks20,MKmeantotpks30)
		
		SNmeantotpks1 <- lm(pfstatsdf$TotDaysAbv ~ pfstatsdf$year)
		SNmeantotpks1 <- data.frame(slope=SNmeantotpks1$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks1))/var(residuals(SNmeantotpks1))),window=c(1),measure=c("totpeakflwabv"))
		SNmeantotpks5 <- lm(meantotpks5 ~ pfstatsdf$year)
		SNmeantotpks5 <- data.frame(slope=SNmeantotpks5$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks5))/var(residuals(SNmeantotpks5))),window=c(5),measure=c("totpeakflwabv"))
		SNmeantotpks10 <- lm(meantotpks10 ~ pfstatsdf$year)
		SNmeantotpks10 <- data.frame(slope=SNmeantotpks10$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks10))/var(residuals(SNmeantotpks10))),window=c(10),measure=c("totpeakflwabv"))
		SNmeantotpks20 <- lm(meantotpks20 ~ pfstatsdf$year)
		SNmeantotpks20 <- data.frame(slope=SNmeantotpks20$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks20))/var(residuals(SNmeantotpks20))),window=c(20),measure=c("totpeakflwabv"))
		SNmeantotpks30 <- lm(meantotpks30 ~ pfstatsdf$year)
		SNmeantotpks30 <- data.frame(slope=SNmeantotpks30$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks30))/var(residuals(SNmeantotpks30))),window=c(30),measure=c("totpeakflwabv"))
		SNmeantotpks <- rbind.data.frame(SNmeantotpks1,SNmeantotpks5,SNmeantotpks10,SNmeantotpks20,SNmeantotpks30)
		
		####################
		
		MKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
		SNdfs <- rbind.data.frame(SNmeantotvol,SNmeantotdays,SNmeannumpks,SNmeanmeanpks,SNmeantotpks)
		
		finaldf <- merge(MKdfs,SNdfs,by=c("window","measure"))
		finaldf$gauge <- gauge
		
		
		meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$year>year)],5,na.pad=TRUE)
		meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$year>year)],10,na.pad=TRUE)
		
		
		MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$year>year)])
		MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
		MKmeantotvol5 <- MannKendall(meantotvol5)
		MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
		MKmeantotvol10 <- MannKendall(meantotvol10)
		MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))
		MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10)
		
		SNmeantotvol1 <- lm(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotvol1 <- data.frame(slope=SNmeantotvol1$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol1))/var(residuals(SNmeantotvol1))),window=c(1),measure=c("totvolabv"))
		SNmeantotvol5 <- lm(meantotvol5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotvol5 <- data.frame(slope=SNmeantotvol5$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol5))/var(residuals(SNmeantotvol5))),window=c(5),measure=c("totvolabv"))
		SNmeantotvol10 <- lm(meantotvol10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotvol10 <- data.frame(slope=SNmeantotvol10$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol10))/var(residuals(SNmeantotvol10))),window=c(10),measure=c("totvolabv"))
		SNmeantotvol <- rbind.data.frame(SNmeantotvol1,SNmeantotvol5,SNmeantotvol10)
		
		
		############################
		meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)],5,na.pad=TRUE)
		meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)],10,na.pad=TRUE)
		
		
		MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)])
		MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
		MKmeantotdays5 <- MannKendall(meantotdays5)
		MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
		MKmeantotdays10 <- MannKendall(meantotdays10)
		MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
		MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10)
		
		SNmeantotdays1 <- lm(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotdays1 <- data.frame(slope=SNmeantotdays1$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays1))/var(residuals(SNmeantotdays1))),window=c(1),measure=c("totdaysabv"))
		SNmeantotdays5 <- lm(meantotdays5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotdays5 <- data.frame(slope=SNmeantotdays5$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays5))/var(residuals(SNmeantotdays5))),window=c(5),measure=c("totdaysabv"))
		SNmeantotdays10 <- lm(meantotdays10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotdays10 <- data.frame(slope=SNmeantotdays10$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays10))/var(residuals(SNmeantotdays10))),window=c(10),measure=c("totdaysabv"))
		SNmeantotdays <- rbind.data.frame(SNmeantotdays1,SNmeantotdays5,SNmeantotdays10)
		
		####################
		
		meannumpks5 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$year>year)],5,na.pad=TRUE)
		meannumpks10 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$year>year)],10,na.pad=TRUE)
		
		MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks[which(pfstatsdf$year>year)])
		MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
		MKmeannumpks5 <- MannKendall(meannumpks5)
		MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
		MKmeannumpks10 <- MannKendall(meannumpks10)
		MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
		MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10)
		
		SNmeannumpks1 <- lm(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeannumpks1 <- data.frame(slope=SNmeannumpks1$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks1))/var(residuals(SNmeannumpks1))),window=c(1),measure=c("numpeaksabv"))
		SNmeannumpks5 <- lm(meannumpks5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeannumpks5 <- data.frame(slope=SNmeannumpks5$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks5))/var(residuals(SNmeannumpks5))),window=c(5),measure=c("numpeaksabv"))
		SNmeannumpks10 <- lm(meannumpks10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeannumpks10 <- data.frame(slope=SNmeannumpks10$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks10))/var(residuals(SNmeannumpks10))),window=c(10),measure=c("numpeaksabv"))
		SNmeannumpks <- rbind.data.frame(SNmeannumpks1,SNmeannumpks5,SNmeannumpks10)
		
		####################
		
		
		meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$year>year)],5,na.pad=TRUE)
		meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$year>year)],10,na.pad=TRUE)
		
		
		MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow[which(pfstatsdf$year>year)])
		MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
		MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
		MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
		MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
		MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
		MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10)
		
		SNmeanmeanpks1 <- lm(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeanmeanpks1 <- data.frame(slope=SNmeanmeanpks1$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks1))/var(residuals(SNmeanmeanpks1))),window=c(1),measure=c("meanpeaksabv"))
		SNmeanmeanpks5 <- lm(meanmeanpks5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeanmeanpks5 <- data.frame(slope=SNmeanmeanpks5$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks5))/var(residuals(SNmeanmeanpks5))),window=c(5),measure=c("meanpeaksabv"))
		SNmeanmeanpks10 <- lm(meanmeanpks10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeanmeanpks10 <- data.frame(slope=SNmeanmeanpks10$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks10))/var(residuals(SNmeanmeanpks10))),window=c(10),measure=c("meanpeaksabv"))
		SNmeanmeanpks <- rbind.data.frame(SNmeanmeanpks1,SNmeanmeanpks5,SNmeanmeanpks10)
		
		####################
		
		meantotpks5 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$year>year)],5,na.pad=TRUE)
		meantotpks10 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$year>year)],10,na.pad=TRUE)
		
		MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow[which(pfstatsdf$year>year)])
		MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
		MKmeantotpks5 <- MannKendall(meantotpks5)
		MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
		MKmeantotpks10 <- MannKendall(meantotpks10)
		MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
		MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10)
		
		SNmeantotpks1 <- lm(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotpks1 <- data.frame(slope=SNmeantotpks1$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks1))/var(residuals(SNmeantotpks1))),window=c(1),measure=c("totpeakflwabv"))
		SNmeantotpks5 <- lm(meantotpks5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotpks5 <- data.frame(slope=SNmeantotpks5$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks5))/var(residuals(SNmeantotpks5))),window=c(5),measure=c("totpeakflwabv"))
		SNmeantotpks10 <- lm(meantotpks10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
		SNmeantotpks10 <- data.frame(slope=SNmeantotpks10$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks10))/var(residuals(SNmeantotpks10))),window=c(10),measure=c("totpeakflwabv"))
		SNmeantotpks <- rbind.data.frame(SNmeantotpks1,SNmeantotpks5,SNmeantotpks10)
		
		####################
		
		impMKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
		impSNdfs <- rbind.data.frame(SNmeantotvol,SNmeantotdays,SNmeannumpks,SNmeanmeanpks,SNmeantotpks)
		
		impfinaldf <- merge(impMKdfs,impSNdfs,by=c("window","measure"))
		impfinaldf$gauge <- gauge
		finallist <- list(trend_dams=impfinaldf,trend_full=finaldf)
		
		return(finallist)
	}
	
	batchnum <- i
	
	active_basins <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\active_basins.csv", header=TRUE)
	active_basins <- active_basins[which(active_basins$site_no%in%unique(impairmentsdf$downstreamgauge)),]
	impairments_basins <- merge(impairmentsdf,active_basins, by.x="downstreamgauge", by.y="site_no", all=TRUE, sort=FALSE)
	write.csv(impairments_basins, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\impairments.csv")
	impairments_tulare<- impairments_basins[which(impairments_basins$basin=="tulare"),]
	impairments_sac <- impairments_basins[which(impairments_basins$basin=="sac"),]
	impairments_sj <- impairments_basins[which(impairments_basins$basin=="sj"),]
	unimp_gauges <- impairments_basins[which(is.na(impairments_basins$DSTR_GAUGE)),]
	
	analysis_year <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\analysis_years.csv", header=TRUE)
	analysis_year <- unique(analysis_year)
	
	
	spbatch_peakflows3 <- vector("list", length(spbatch))
	spbatch_peakflowstats3 <- vector("list", length(spbatch))
	spbatch_peakflowsummary3 <- vector("list", length(spbatch))
	spbatch_peakflowmonthlystats3 <- vector("list", length(spbatch))
	spbatch_peakflowsdf3 <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		spbatch_peakflows3[[k]] <- vector("list",1)
		names(spbatch_peakflows3[[k]]) <- c("peakflows")
		spbatch_peakflows3[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$Winter_6mon$Data))
		for(i in 1:length(spbatch[[k]]$Winter_6mon$Data)){
			spbatch_peakflows3[[k]]$peakflows[[i]] <- peakanalysis(input=spbatch[[k]]$Winter_6mon$Data[[i]],
					width=3, threshold=(spbatch[[k]]$thresholdsdams_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="3mon", Index=spbatch[[k]]$Index)
		}
		spbatch_peakflowstats3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowstats3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[2]]
		}
		spbatch_peakflowsummary3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowsummary3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[1]]
		}
		spbatch_peakflowmonthlystats3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowmonthlystats3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[3]]
		}
		spbatch_peakflowsdf3[[k]] <- vector("list",3)
		names(spbatch_peakflowsdf3[[k]]) <- c("pfstatsdf3","pfsummarydf3","pfmonthlystats3")
		spbatch_peakflowsdf3[[k]]$pfstatsdf3 <- do.call(rbind.data.frame,spbatch_peakflowstats3[[k]])
		spbatch_peakflowsdf3[[k]]$pfsummarydf3 <- do.call(rbind.data.frame,spbatch_peakflowsummary3[[k]])
		spbatch_peakflowsdf3[[k]]$pfmonthlystats3 <- do.call(rbind.data.frame,spbatch_peakflowmonthlystats3[[k]])
		spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero <- rep(NA, length(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv))
		for(i in 1:length(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv)){
			if(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv[[i]]==0){
				spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero[[i]] <- 1
			}else{
				spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero[[i]] <- 0
			}
		}
		spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero_cumsum <- cumsum(spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero)	
	}
	names(spbatch_peakflowsdf3)<- names(spbatch)
	names(spbatch_peakflows3)<- names(spbatch)
	names(spbatch_peakflowstats3)<- names(spbatch)
	names(spbatch_peakflowsummary3)<- names(spbatch)
	names(spbatch_peakflowmonthlystats3)<- names(spbatch)

	spbatch_pktrends3mon <- vector("list",length(spbatch))
	for(i in 1:length(spbatch_peakflowsdf3)){
		spbatch_pktrends3mon[[i]] <- peakflowtrends(spbatch_peakflowsdf3[[i]]$pfstatsdf3, names(spbatch_peakflowsdf3)[[i]],analysis_year$analysis_year[which(spbatch[[i]]$raw$site_no[[1]]==analysis_year$downstreamgauge)] )
	}
	names(spbatch_pktrends3mon)<-names(spbatch_peakflowsdf3)
	
	spbatch_pktrendshy <- vector("list",length(spbatch))
	for(i in 1:length(spbatch_peakflowsdf)){
		spbatch_pktrendshy[[i]] <- peakflowtrends(spbatch_peakflowsdf[[i]]$pfstatsdf, names(spbatch_peakflowsdf)[[i]],analysis_year$analysis_year[which(spbatch[[i]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
	}
	names(spbatch_pktrendshy)<-names(spbatch_peakflowsdf)
	
	for(i in 1:length(spbatch_pktrends3mon)){
		write.csv(spbatch_pktrends3mon[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\mon3\\pktrends3mon_dam",names(spbatch_pktrends3mon)[[i]],".csv",sep=""))
		write.csv(spbatch_pktrends3mon[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\mon3\\pktrends3mon_full",names(spbatch_pktrends3mon)[[i]],".csv",sep=""))
		
	}
	
	for(i in 1:length(spbatch_pktrendshy)){
		write.csv(spbatch_pktrendshy[[i]][[1]] , file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\full\\pktrendshy_dam",names(spbatch_pktrendshy)[[i]],".csv",sep=""))
		write.csv(spbatch_pktrendshy[[i]][[2]] , file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\full\\pktrendshy_full",names(spbatch_pktrendshy)[[i]],".csv",sep=""))
		
	}
	
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\spbatch_",batchnum,".RData", sep=""))
}

for(i in 1:7){
	batchnum <- i
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\spbatch_",batchnum,".RData", sep=""))
	
	peakflowmags <- function(pfstatsdf, gauge, year){
		mean_totvol_TAF <- mean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$TotVolAbv_acft!=0&pfstatsdf$year>year)])/1000
		mean_totdays <- mean(pfstatsdf$TotDaysAbv[which(pfstatsdf$TotDaysAbv!=0&pfstatsdf$year>year)])
		mean_numpeaks <- mean(pfstatsdf$numpeaks[which(pfstatsdf$numpeaks!=0&pfstatsdf$year>year)])
		frac_zero <- length(which(pfstatsdf$numpeaks==0&pfstatsdf$year>year))/length(pfstatsdf$numpeaks[which(pfstatsdf$year>year)])
		frac_nonzero <- 1-frac_zero
		num_zero <-length(which(pfstatsdf$numpeaks==0&pfstatsdf$year>year))
		num_nonzero <-length(which(pfstatsdf$numpeaks!=0&pfstatsdf$year>year))
		
		meandf <- data.frame(mean_totvol_TAF =mean_totvol_TAF ,mean_totdays=mean_totdays,mean_numpeaks=mean_numpeaks,
				frac_zero=frac_zero,
				frac_nonzero=frac_nonzero,
				num_zero=num_zero,
				num_nonzero=num_nonzero,
				gauge=gauge,
				styear=year)
		
		return(meandf)
	}
	
	spbatch_peakflowmags_hy <- vector("list", length(spbatch_peakflowsdf))
	for(i in 1:length(spbatch_peakflowsdf)){
		spbatch_peakflowmags_hy[[i]] <- peakflowmags(spbatch_peakflowsdf[[i]]$pfstatsdf, names(spbatch_peakflowsdf)[[i]],1800)
	}
	spbatch_peakflowmagsdf_hy <- do.call(rbind.data.frame, spbatch_peakflowmags_hy)
	write.csv(spbatch_peakflowmagsdf_hy, file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\hy\\spbatch_peakflowmagsdf_hy_",batchnum,".csv",sep=""))
	
	
	
	spbatch_peakflowmags_3mon <- vector("list", length(spbatch_peakflowsdf3))
	for(i in 1:length(spbatch_peakflowsdf3)){
		spbatch_peakflowmags_3mon[[i]] <- peakflowmags(spbatch_peakflowsdf3[[i]]$pfstatsdf3, names(spbatch_peakflowsdf3)[[i]],1800)
	}
	spbatch_peakflowmagsdf_3mon <- do.call(rbind.data.frame, spbatch_peakflowmags_3mon)
	write.csv(spbatch_peakflowmagsdf_3mon, file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\mon3\\spbatch_peakflowmagsdf_mon3_",batchnum,".csv",sep=""))
	

	
	spbatch_peakflowmags_hydams <- vector("list", length(spbatch_peakflowsdf))
	for(i in 1:length(spbatch_peakflowsdf)){
		spbatch_peakflowmags_hydams[[i]] <- peakflowmags(spbatch_peakflowsdf[[i]]$pfstatsdf, names(spbatch_peakflowsdf)[[i]],year=
						analysis_year$analysis_year[which(spbatch[[i]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
	}
	spbatch_peakflowmagsdf_hydams <- do.call(rbind.data.frame, spbatch_peakflowmags_hydams)
	write.csv(spbatch_peakflowmagsdf_hydams, file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\hy\\spbatch_peakflowmagsdf_hy_dams_",batchnum,".csv",sep=""))
	
	
	spbatch_peakflowmags_3mondams <- vector("list", length(spbatch_peakflowsdf3))
	for(i in 1:length(spbatch_peakflowsdf3)){
		spbatch_peakflowmags_3mondams[[i]] <- peakflowmags(spbatch_peakflowsdf3[[i]]$pfstatsdf3, names(spbatch_peakflowsdf3)[[i]],year=
						analysis_year$analysis_year[which(spbatch[[i]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
	}
	spbatch_peakflowmagsdf_3mondams <- do.call(rbind.data.frame, spbatch_peakflowmags_3mondams)
	write.csv(spbatch_peakflowmagsdf_3mondams, file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\mon3\\spbatch_peakflowmagsdf_mon3_dams_",batchnum,".csv",sep=""))
	
	
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\spbatch_",batchnum,".RData", sep=""))
}


pkmagshy_full <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\hy")
pkmagshy_full_list <- vector("list", length(pkmagshy_full))
for(i in 1:length(pkmagshy_full)){
	pkmagshy_full_list[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\hy\\",pkmagshy_full[[i]],sep=""), header=TRUE, sep=",")
}
pkmagshy_full_df <- do.call(rbind.data.frame,pkmagshy_full_list)
write.csv(pkmagshy_full_df,"C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_hy_full.csv")

pkmagshy_dams <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\hy")
pkmagshy_dams_list <- vector("list", length(pkmagshy_dams))
for(i in 1:length(pkmagshy_dams)){
	pkmagshy_dams_list[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\hy\\",pkmagshy_dams[[i]],sep=""), header=TRUE, sep=",")
}
pkmagshy_dams_df <- do.call(rbind.data.frame,pkmagshy_dams_list)
write.csv(pkmagshy_dams_df,"C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_hy_dams.csv")

pkmagsmon3_full <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\mon3")
pkmagsmon3_full_list <- vector("list", length(pkmagsmon3_full))
for(i in 1:length(pkmagsmon3_full)){
	pkmagsmon3_full_list[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\mon3\\",pkmagsmon3_full[[i]],sep=""), header=TRUE, sep=",")
}
pkmagsmon3_full_df <- do.call(rbind.data.frame,pkmagsmon3_full_list)
write.csv(pkmagsmon3_full_df,"C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_mon3_full.csv")

pkmagsmon3_dams <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\mon3")
pkmagsmon3_dams_list <- vector("list", length(pkmagsmon3_dams))
for(i in 1:length(pkmagsmon3_dams)){
	pkmagsmon3_dams_list[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\mon3\\",pkmagsmon3_dams[[i]],sep=""), header=TRUE, sep=",")
}
pkmagsmon3_dams_df <- do.call(rbind.data.frame,pkmagsmon3_dams_list)
write.csv(pkmagsmon3_dams_df,"C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_mon3_dams.csv")




active_basins <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\active_basins.csv", header=TRUE)
active_basins <- active_basins[which(active_basins$site_no%in%unique(impairmentsdf$downstreamgauge)),]
impairments_basins <- merge(impairmentsdf,active_basins, by.x="downstreamgauge", by.y="site_no", all=TRUE, sort=FALSE)
write.csv(impairments_basins, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\impairments.csv")
impairments_tulare<- impairments_basins[which(impairments_basins$basin=="tulare"),]
impairments_sac <- impairments_basins[which(impairments_basins$basin=="sac"),]
impairments_sj <- impairments_basins[which(impairments_basins$basin=="sj"),]
unimp_gauges <- impairments_basins[which(is.na(impairments_basins$DSTR_GAUGE)),]

analysis_year <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\analysis_years.csv", header=TRUE)
analysis_year <- unique(analysis_year)



rlmnvol <- rollmean(spbatch_peakflowsdf[[3]]$pfstatsdf$TotVolAbv_acft,5, na.pad=TRUE)
pval <- summary(lm(rlmnvol[which(spbatch_peakflowsdf[[3]]$pfstatsdf$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))] ~ spbatch_peakflowsdf[[3]]$pfstatsdf$year[which(spbatch_peakflowsdf[[3]]$pfstatsdf$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))]) )$coefficients[2,4]
pval <- data.frame(pval=round(pval,3))
qplot(spbatch_peakflowsdf[[3]]$pfstatsdf$year,rlmnvol, geom="line")+geom_smooth(method="lm",se=FALSE)+
		geom_smooth(aes(x=spbatch_peakflowsdf[[3]]$pfstatsdf$year[which(spbatch_peakflowsdf[[3]]$pfstatsdf$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))], y=rlmnvol[which(spbatch_peakflowsdf[[3]]$pfstatsdf$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))]),
				method="lm",se=FALSE, color="red")+
		xlab("Year")+
		ylab("Volume Above 90% (acft)")+
		ggtitle(paste(spbatch[[3]]$raw$site_no[[1]],", ", analysis_year$basin[which(analysis_year$downstreamgauge==spbatch[[3]]$raw$site_no[[1]])], ", pval=", pval$pval, sep=""))

rlmnvol <- rollmean(spbatch_peakflowsdf3[[3]]$pfstatsdf3$TotVolAbv_acft,1, na.pad=TRUE)
pval <- summary(lm(rlmnvol[which(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))] ~ spbatch_peakflowsdf3[[3]]$pfstatsdf3$year[which(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))]) )$coefficients[2,4]
pval <- data.frame(pval=round(pval,3))
qplot(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year,rlmnvol, geom="point")+geom_smooth(method="lm",se=FALSE)+
		geom_smooth(aes(x=spbatch_peakflowsdf3[[3]]$pfstatsdf3$year[which(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))], y=rlmnvol[which(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))]),
				method="lm",se=FALSE, color="red")+
		xlab("Year")+
		ylab("Volume Above 90% (acft)")+
		ggtitle(paste(spbatch[[3]]$raw$site_no[[1]],", ", analysis_year$basin[which(analysis_year$downstreamgauge==spbatch[[3]]$raw$site_no[[1]])], ", pval=", pval$pval, sep=""))


trendvol <- read.csv(file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_totvol.csv", header=TRUE)
trenddays<- read.csv(file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_totdays.csv", header=TRUE)
trendnum <- read.csv(file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_numpks.csv", header=TRUE)
mags <- read.csv(file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_mon3_dams.csv", header=TRUE)

merged <- merge(mags,trendvol, by="gauge")
merged <- merge(merged,trenddays, by="gauge")
merged <- merge(merged,trendnum, by="gauge")
write.csv(merged, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\merged_3mon_dams.csv")



testarea <- hypeakplotsstats(spbatch_peakflowsdf[[3]]$pfmonthlystats, names(spbatch_peakflowsdf)[[3]])










