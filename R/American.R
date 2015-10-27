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




american_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\Data\\American\\AR_60.txt")
american_g <- as.numeric(american_gauges$SITENO)


#MKTMONdayamerican <- vector("list",6)
#names(MKTMONdayamerican)<- c("NOV","DEC","JAN","FEB","MAR","APR")
#for(k in 1:6){
#	MKTMONdayamerican[[k]] <- data.frame(tau=rep(NA, length(american_g)), pvalue=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#}
#MKT6MONdayamerican <- data.frame(tau=rep(NA, length(american_g)), pvalue=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#MKT3MONdayamerican <- data.frame(tau=rep(NA, length(american_g)), pvalue=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#MKTHYdayamerican <- data.frame(tau=rep(NA, length(american_g)), pvalue=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#
#MKTMONvolamerican <- vector("list",6)
#names(MKTMONvolamerican)<- c("NOV","DEC","JAN","FEB","MAR","APR")
#for(k in 1:6){
#	MKTMONvolamerican[[k]] <- data.frame(tau=rep(NA, length(american_g)), pvalue=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#}
#MKT6MONvolamerican <- data.frame(tau=rep(NA, length(american_g)), pvalue=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#MKT3MONvolamerican <- data.frame(tau=rep(NA, length(american_g)), pvalue=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#MKTHYvolamerican <- data.frame(tau=rep(NA, length(american_g)), pvalue=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#
#
#
#
#######################
#FracAboveMONdayamerican <- vector("list",6)
#names(FracAboveMONdayamerican)<- c("NOV","DEC","JAN","FEB","MAR","APR")
#for(k in 1:6){
#	FracAboveMONdayamerican[[k]] <- data.frame(FracAboveday=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#}
#FracAbove6MONdayamerican <- data.frame(FracAboveday=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#FracAbove3MONdayamerican <- data.frame(FracAboveday=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#FracAboveHYdayamerican <- data.frame(FracAboveday=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#
#FracAboveMONvolamerican <- vector("list",6)
#names(FracAboveMONvolamerican)<- c("NOV","DEC","JAN","FEB","MAR","APR")
#for(k in 1:6){
#	FracAboveMONvolamerican[[k]] <- data.frame(FracAbovevol=rep(NA, length(american_g)), VolAbvMAF=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#}
#FracAbove6MONvolamerican <- data.frame(FracAbovevol=rep(NA, length(american_g)), VolAbvMAF=rep(NA, length(american_g)), gauge=rep(NA, length(american_g)))
#FracAbove3MONvolamerican <- data.frame(FracAbovevol=rep(NA, length(american_g)),  VolAbvMAF=rep(NA, length(american_g)),gauge=rep(NA, length(american_g)))
#FracAboveHYvolamerican <- data.frame(FracAbovevol=rep(NA, length(american_g)),  VolAbvMAF=rep(NA, length(american_g)),gauge=rep(NA, length(american_g)))
#
#################################


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
american_g <- american_g[which(american_g %in% txtgauges)]
#length(american_g)
#american <- vector("list", 5)

american <- vector("list", length(american_g))
for(z in 1:length(american_g)){
#	american <- list()
	american[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",american_g[[z]],".csv",sep=""), header=TRUE)
#	american$raw2$site_no <- as.numeric(american$raw2$site_no)
#	american$raw <- readNWISdv(american_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	american[[z]]$raw$Date <- as.Date(american[[z]]$raw$Date, "%Y-%m-%d")
	
	american[[z]]$raw <- RemoveLeapDays(american[[z]]$raw)
	
	##yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(american[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		american[[z]]$Index$Valley <- "SacV"
		american[[z]]$Index$Index <- yeartype_old$SVI
		american[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(american[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		american[[z]]$Index$Valley <- "SJV"
		american[[z]]$Index$Index <- yeartype_old$SJI
		american[[z]]$Index$Year <- yeartype_old$Year
	} else {
		american[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",american[[z]]$raw$site_no[[1]]))
	}
	
#	if(as.numeric(SJ$raw$site_no[[1]]) %in% SacV_gauges$site_no){
#		SJ$Index$Valley <- "SacV"
#		SJ$Index$Index <- YEARTYPEqdf$SacV_num
#		SJ$Index$Year <- YEARTYPEqdf$Year
#	} else if(as.numeric(SJ$raw$site_no[[1]]) %in% SJV_gauges$site_no){
#		SJ$Index$Valley <- "SJV"
#		SJ$Index$Index <- YEARTYPEqdf$SJV_num
#		SJ$Index$Year <- YEARTYPEqdf$Year
#	} else {
#		SJ$Index$Valley <- "ERROR"
#		print(paste("Error",SJ$raw$site_no[[1]]))
#	}
#	
	
	###DATA PROCESSING
	american[[z]]$prep <- prepdata(american[[z]]$raw)
	american[[z]]$Availability <- DataAvailability(american[[z]]$raw)
	american[[z]]$thresholds_maf <- thresholds(american[[z]]$prep)
	if(all(american[[z]]$thresholds_maf==0)){
	} else {
#	american$record_stats <- record_stats(american$prep, american$thresholds_maf)
		american[[z]]$Winter_3mon <- Split3Winter(american[[z]]$prep, american[[z]]$Index, american[[z]]$thresholds_maf)
		american[[z]]$Winter_6mon <- Split6Winter(american[[z]]$prep, american[[z]]$Index, american[[z]]$thresholds_maf)
		american[[z]]$Winter_monthly <- SplitWinterMonthly(american[[z]]$prep, american[[z]]$Index, american[[z]]$thresholds_maf)
		american[[z]]$HydroYear <- SplitHydroYear(american[[z]]$prep, american[[z]]$Index, american[[z]]$thresholds_maf)	
		american[[z]]$HydroYear <- cleanupHY(american[[z]]$HydroYear)
		american[[z]]$Winter_6mon <- cleanup6MON(american[[z]]$Winter_6mon)
		american[[z]]$Winter_3mon <- cleanup3MON(american[[z]]$Winter_3mon)
		
	}
	}
#		#rewrite below to write to a single output df
#		american[[z]]$ThresholdFit6MON <- ThresholdFit(american[[z]]$Winter_6mon, 0.9)
#		american[[z]]$ThresholdFit3MON <- ThresholdFit(american[[z]]$Winter_3mon, 0.9)
#		american[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(american[[z]]$Winter_monthly, 0.9)
#		american[[z]]$ThresholdFitHY <- ThresholdFit(american[[z]]$HydroYear, 0.9)
#		
#		
#		american[[z]]$MKT6MON <- MKT(american[[z]]$ThresholdFit6MON)
#		american[[z]]$MKT3MON <- MKT(american[[z]]$ThresholdFit3MON)
#		american[[z]]$MKTMON  <- MKT(american[[z]]$ThresholdFitMON)
#		american[[z]]$MKTHY <- MKT(american[[z]]$ThresholdFitHY)
#		
#		MKT6MONdayamerican$tau[[z]] <- american[[z]]$MKT6MON$MKTday[[1]][[1]]
#		MKT6MONdayamerican$pvalue[[z]] <- american[[z]]$MKT6MON$MKTday[[2]][[1]]
#		MKT6MONdayamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		MKT3MONdayamerican$tau[[z]] <- american[[z]]$MKT3MON$MKTday[[1]][[1]]
#		MKT3MONdayamerican$pvalue[[z]] <- american[[z]]$MKT3MON$MKTday[[2]][[1]]
#		MKT3MONdayamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		MKTHYdayamerican$tau[[z]] <- american[[z]]$MKTHY$MKTday[[1]][[1]]
#		MKTHYdayamerican$pvalue[[z]] <- american[[z]]$MKTHY$MKTday[[2]][[1]]
#		MKTHYdayamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			MKTMONdayamerican[[k]]$tau[[z]] <- american[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
#			MKTMONdayamerican[[k]]$pvalue[[z]] <- american[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
#			MKTMONdayamerican[[k]]$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		}
#		
#		MKT6MONvolamerican$tau[[z]] <- american[[z]]$MKT6MON$MKTvol[[1]][[1]]
#		MKT6MONvolamerican$pvalue[[z]] <- american[[z]]$MKT6MON$MKTvol[[2]][[1]]
#		MKT6MONvolamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		MKT3MONvolamerican$tau[[z]] <- american[[z]]$MKT3MON$MKTvol[[1]][[1]]
#		MKT3MONvolamerican$pvalue[[z]] <- american[[z]]$MKT3MON$MKTvol[[2]][[1]]
#		MKT3MONvolamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		MKTHYvolamerican$tau[[z]] <- american[[z]]$MKTHY$MKTvol[[1]][[1]]
#		MKTHYvolamerican$pvalue[[z]] <- american[[z]]$MKTHY$MKTvol[[2]][[1]]
#		MKTHYvolamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			MKTMONvolamerican[[k]]$tau[[z]] <- american[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
#			MKTMONvolamerican[[k]]$pvalue[[z]] <- american[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
#			MKTMONvolamerican[[k]]$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		}
#		
#		
#		##########################
#		american[[z]]$FracAbove6MON <- FracAboveExtract(american[[z]]$ThresholdFit6MON)
#		american[[z]]$FracAbove3MON <- FracAboveExtract(american[[z]]$ThresholdFit3MON)
#		american[[z]]$FracAboveMON  <- FracAboveExtract(american[[z]]$ThresholdFitMON)
#		american[[z]]$FracAboveHY <- FracAboveExtract(american[[z]]$ThresholdFitHY)
#		
#		FracAbove6MONdayamerican$FracAboveday[[z]] <- mean(american[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONdayamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONdayamerican$FracAboveday[[z]] <- mean(american[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONdayamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYdayamerican$FracAboveday[[z]] <- mean(american[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYdayamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONdayamerican[[k]]$FracAboveday[[z]] <- mean(american[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONdayamerican[[k]]$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvolamerican$FracAbovevol[[z]] <- mean(american[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvolamerican$VolAbvMAF[[z]] <- mean(american[[z]]$FracAbove6MON$VolAbvMAF, na.rm=TRUE)
#		FracAbove6MONvolamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvolamerican$FracAbovevol[[z]] <- mean(american[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvolamerican$VolAbvMAF[[z]] <- mean(american[[z]]$FracAbove3MON$VolAbvMAF, na.rm=TRUE)
#		FracAbove3MONvolamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvolamerican$FracAbovevol[[z]] <- mean(american[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvolamerican$VolAbvMAF[[z]] <- mean(american[[z]]$FracAboveHY$VolAbvMAF, na.rm=TRUE)
#		FracAboveHYvolamerican$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvolamerican[[k]]$FracAbovevol[[z]] <- mean(american[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvolamerican[[k]]$VolAbvMAF[[z]] <- mean(american[[z]]$FracAboveMON[[k]]$VolAbvMAF, na.rm=TRUE)
#			FracAboveMONvolamerican[[k]]$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
#		}
#		###############################################################
#		
##		FracAbove6MONday$FracAboveday[[z]] <- gls(american[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
##		FracAbove6MONday$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
##		
##		FracAbove3MONday$FracAboveday[[z]] <- mean(american[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
##		FracAbove3MONday$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
##		
##		FracAboveHYday$FracAboveday[[z]] <- mean(american[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
##		FracAboveHYday$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
##		
##		for(k in 1:6){
##			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(american[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
##			FracAboveMONday[[k]]$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
##		}
##		
##		FracAbove6MONvol$FracAbovevol[[z]] <- mean(american[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
##		FracAbove6MONvol$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
##		
##		FracAbove3MONvol$FracAbovevol[[z]] <- mean(american[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
##		FracAbove3MONvol$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
##		
##		FracAboveHYvol$FracAbovevol[[z]] <- mean(american[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
##		FracAboveHYvol$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
##		
##		for(k in 1:6){
##			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(american[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
##			FracAboveMONvol[[k]]$gauge[[z]] <- american[[z]]$raw$site_no[[1]]
##		}
#		
#		#############################
#		
#	}
#	
#	if(any(z==seq(5,200,10))){
#		save.image()
#	}
#}	
#
#save.image()
#
names(american) <- american_g
#
for(i in 1:length(american)){
	american[[i]]$Winter_monthly <- cleanupMON(american[[i]]$Winter_monthly)
#	american[[i]]$daysmax$W3MON <- FreqAnalysis(american[[i]]$Winter_3mon,c(1,3,7),american[[i]]$Index)
#	american[[i]]$daysmax$W6MON <- FreqAnalysis(american[[i]]$Winter_6mon,c(1,3,7),american[[i]]$Index)
#	american[[i]]$daysmax$HY <- FreqAnalysis(american[[i]]$HydroYear,c(1,3,7),american[[i]]$Index)
#	american[[i]]$daysmax$WMON <- FreqAnalysisMonthly(american[[i]]$Winter_monthly,c(1,3,7),american[[i]]$Index)
}


MKTNOV <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$Winter_monthly$All$NOV$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$NOV$Data$Date,"%Y")==1957)[[1]]:length(american[[i]]$Winter_monthly$All$NOV$Data$Discharge_acfte6_day)])
	MKTNOV$tau[[i]] <- MK$tau
	MKTNOV$p2s[[i]] <- MK$sl
	MKTNOV$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKTNOV$start[[i]] <- american[[i]]$Winter_monthly$All$NOV$Data$Date[[which(format(american[[i]]$Winter_monthly$All$NOV$Data$Date,"%Y")==1957)[[1]]]]
	MKTNOV$end_date[[i]] <- tail(american[[i]]$Winter_monthly$All$NOV$Data$Date,1)
}
MKTNOV$start <- as.Date(MKTNOV$start)
MKTNOV$end_date <- as.Date(MKTNOV$end_date)


MKTDEC <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==1957)[[1]]:length(american[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKTDEC$tau[[i]] <- MK$tau
	MKTDEC$p2s[[i]] <- MK$sl
	MKTDEC$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKTDEC$start[[i]] <- american[[i]]$Winter_monthly$All$DEC$Data$Date[[which(format(american[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==1957)[[1]]]]
	MKTDEC$end_date[[i]] <- tail(american[[i]]$Winter_monthly$All$DEC$Data$Date,1)
}
MKTDEC$start <- as.Date(MKTDEC$start)
MKTDEC$end_date <- as.Date(MKTDEC$end_date)


MKTJAN <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==1958)[[1]]:length(american[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKTJAN$tau[[i]] <- MK$tau
	MKTJAN$p2s[[i]] <- MK$sl
	MKTJAN$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKTJAN$start[[i]] <- american[[i]]$Winter_monthly$All$JAN$Data$Date[[which(format(american[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==1958)[[1]]]]
	MKTJAN$end_date[[i]] <- tail(american[[i]]$Winter_monthly$All$JAN$Data$Date,1)
}
MKTJAN$start <- as.Date(MKTJAN$start)
MKTJAN$end_date <- as.Date(MKTJAN$end_date)


MKTFEB <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==1958)[[1]]:length(american[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKTFEB$tau[[i]] <- MK$tau
	MKTFEB$p2s[[i]] <- MK$sl
	MKTFEB$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKTFEB$start[[i]] <- american[[i]]$Winter_monthly$All$FEB$Data$Date[[which(format(american[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==1958)[[1]]]]
	MKTFEB$end_date[[i]] <- tail(american[[i]]$Winter_monthly$All$FEB$Data$Date,1)
}
MKTFEB$start <- as.Date(MKTFEB$start)
MKTFEB$end_date <- as.Date(MKTFEB$end_date)

MKTMAR <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==1958)[[1]]:length(american[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKTMAR$tau[[i]] <- MK$tau
	MKTMAR$p2s[[i]] <- MK$sl
	MKTMAR$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKTMAR$start[[i]] <- american[[i]]$Winter_monthly$All$MAR$Data$Date[[which(format(american[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==1958)[[1]]]]
	MKTMAR$end_date[[i]] <- tail(american[[i]]$Winter_monthly$All$MAR$Data$Date,1)
}
MKTMAR$start <- as.Date(MKTMAR$start)
MKTMAR$end_date <- as.Date(MKTMAR$end_date)

MKTAPR <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==1958)[[1]]:length(american[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKTAPR$tau[[i]] <- MK$tau
	MKTAPR$p2s[[i]] <- MK$sl
	MKTAPR$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKTAPR$start[[i]] <- american[[i]]$Winter_monthly$All$APR$Data$Date[[which(format(american[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==1958)[[1]]]]
	MKTAPR$end_date[[i]] <- tail(american[[i]]$Winter_monthly$All$APR$Data$Date,1)
}
MKTAPR$start <- as.Date(MKTAPR$start)
MKTAPR$end_date <- as.Date(MKTAPR$end_date)

write.csv(MKTNOV, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTNOV.csv")
write.csv(MKTDEC, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTDEC.csv")
write.csv(MKTJAN, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTJAN.csv")
write.csv(MKTFEB, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTFEB.csv")
write.csv(MKTMAR, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTMAR.csv")
write.csv(MKTAPR, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTAPR.csv")

MKT6MON <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")=="1957-11")[[1]]:length(american[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKT6MON$tau[[i]] <- MK$tau
	MKT6MON$p2s[[i]] <- MK$sl
	MKT6MON$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKT6MON$start[[i]] <- american[[i]]$Winter_6mon$All$Data$Date[[which(format(american[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")=="1957-11")[[1]]]]
	MKT6MON$end_date[[i]] <- tail(american[[i]]$Winter_6mon$All$Data$Date,1)
}
MKT6MON$start <- as.Date(MKT6MON$start)
MKT6MON$end_date <- as.Date(MKT6MON$end_date)
write.csv(MKT6MON, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT6MON.csv")

MKT3MON <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")=="1957-12")[[1]]:length(american[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKT3MON$tau[[i]] <- MK$tau
	MKT3MON$p2s[[i]] <- MK$sl
	MKT3MON$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKT3MON$start[[i]] <- american[[i]]$Winter_3mon$All$Data$Date[[which(format(american[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")=="1957-12")[[1]]]]
	MKT3MON$end_date[[i]] <- tail(american[[i]]$Winter_3mon$All$Data$Date,1)
}
MKT3MON$start <- as.Date(MKT3MON$start)
MKT3MON$end_date <- as.Date(MKT3MON$end_date)
write.csv(MKT3MON, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT3MON.csv")


MKTHY <- data.frame(tau = rep(NA, length(american)), p2s = rep(NA, length(american)), station=rep(NA,length(american)),start = rep(NA, length(american)), end_date = rep(NA, length(american)))
for(i in 1:length(american)){
	MK <- MannKendall(american[[i]]$HydroYear$All$Data$Discharge_acfte6_day[which(format(american[[i]]$HydroYear$All$Data$Date,"%Y-%m")=="1957-10")[[1]]:length(american[[i]]$HydroYear$All$Data$Discharge_acfte6_day)])
	MKTHY$tau[[i]] <- MK$tau
	MKTHY$p2s[[i]] <- MK$sl
	MKTHY$station[[i]] <- american[[i]]$raw$site_no[[1]]
	MKTHY$start[[i]] <- american[[i]]$HydroYear$All$Data$Date[[which(format(american[[i]]$HydroYear$All$Data$Date,"%Y-%m")=="1957-10")[[1]]]]
	MKTHY$end_date[[i]] <- tail(american[[i]]$HydroYear$All$Data$Date,1)
}
MKTHY$start <- as.Date(MKTHY$start)
MKTHY$end_date <- as.Date(MKTHY$end_date)
write.csv(MKTHY, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTHY.csv")

######################
#######################
#######################

MKT11427000_1941 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	MK <- MannKendall(american$`11427000`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1941-12")[[1]]:length(american$`11427000`$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKT11427000_1941$tau[[1]] <- MK$tau
	MKT11427000_1941$p2s[[1]] <- MK$sl
	MKT11427000_1941$station[[1]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1941$start[[1]] <- american$`11427000`$Winter_3mon$All$Data$Date[[which(format(american$`11427000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1941-12")[[1]]]]
	MKT11427000_1941$end_date[[1]] <- tail(american$`11427000`$Winter_3mon$All$Data$Date,1)
	MKT11427000_1941$period[[1]] <- "3MON"

	MK <- MannKendall(american$`11427000`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1941-11")[[1]]:length(american$`11427000`$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKT11427000_1941$tau[[2]] <- MK$tau
	MKT11427000_1941$p2s[[2]] <- MK$sl
	MKT11427000_1941$station[[2]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1941$start[[2]] <- american$`11427000`$Winter_6mon$All$Data$Date[[which(format(american$`11427000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1941-11")[[1]]]]
	MKT11427000_1941$end_date[[2]] <- tail(american$`11427000`$Winter_6mon$All$Data$Date,1)
	MKT11427000_1941$period[[2]] <- "6MON"

	MK <- MannKendall(american$`11427000`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11427000`$HydroYear$All$Data$Date,"%Y-%m")=="1941-10")[[1]]:length(american$`11427000`$HydroYear$All$Data$Discharge_acfte6_day)])
	MKT11427000_1941$tau[[3]] <- MK$tau
	MKT11427000_1941$p2s[[3]] <- MK$sl
	MKT11427000_1941$station[[3]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1941$start[[3]] <- american$`11427000`$HydroYear$All$Data$Date[[which(format(american$`11427000`$HydroYear$All$Data$Date,"%Y-%m")=="1941-10")[[1]]]]
	MKT11427000_1941$end_date[[3]] <- tail(american$`11427000`$HydroYear$All$Data$Date,1)
	MKT11427000_1941$period[[3]] <- "HY"

	MK <- MannKendall(american$`11427000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1942)[[1]]:length(american$`11427000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKT11427000_1941$tau[[4]] <- MK$tau
	MKT11427000_1941$p2s[[4]] <- MK$sl
	MKT11427000_1941$station[[4]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1941$start[[4]] <- american$`11427000`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1942)[[1]]]]
	MKT11427000_1941$end_date[[4]] <- tail(american$`11427000`$Winter_monthly$All$JAN$Data$Date,1)
	MKT11427000_1941$period[[4]] <- "JAN"

	MK <- MannKendall(american$`11427000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1942)[[1]]:length(american$`11427000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKT11427000_1941$tau[[5]] <- MK$tau
	MKT11427000_1941$p2s[[5]] <- MK$sl
	MKT11427000_1941$station[[5]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1941$start[[5]] <- american$`11427000`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1942)[[1]]]]
	MKT11427000_1941$end_date[[5]] <- tail(american$`11427000`$Winter_monthly$All$FEB$Data$Date,1)
	MKT11427000_1941$period[[5]] <- "FEB"
	
	MK <- MannKendall(american$`11427000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1942)[[1]]:length(american$`11427000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKT11427000_1941$tau[[6]] <- MK$tau
	MKT11427000_1941$p2s[[6]] <- MK$sl
	MKT11427000_1941$station[[6]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1941$start[[6]] <- american$`11427000`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1942)[[1]]]]
	MKT11427000_1941$end_date[[6]] <- tail(american$`11427000`$Winter_monthly$All$MAR$Data$Date,1)
	MKT11427000_1941$period[[6]] <- "MAR"
	
	MK <- MannKendall(american$`11427000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$APR$Data$Date,"%Y")==1942)[[1]]:length(american$`11427000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKT11427000_1941$tau[[7]] <- MK$tau
	MKT11427000_1941$p2s[[7]] <- MK$sl
	MKT11427000_1941$station[[7]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1941$start[[7]] <- american$`11427000`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$APR$Data$Date,"%Y")==1942)[[1]]]]
	MKT11427000_1941$end_date[[7]] <- tail(american$`11427000`$Winter_monthly$All$APR$Data$Date,1)
	MKT11427000_1941$period[[7]] <- "APR"

	MK <- MannKendall(american$`11427000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1941)[[1]]:length(american$`11427000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKT11427000_1941$tau[[8]] <- MK$tau
	MKT11427000_1941$p2s[[8]] <- MK$sl
	MKT11427000_1941$station[[8]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1941$start[[8]] <- american$`11427000`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1941)[[1]]]]
	MKT11427000_1941$end_date[[8]] <- tail(american$`11427000`$Winter_monthly$All$DEC$Data$Date,1)
	MKT11427000_1941$period[[8]] <- "DEC"

MKT11427000_1941$start <- as.Date(MKT11427000_1941$start)
MKT11427000_1941$end_date <- as.Date(MKT11427000_1941$end_date)
write.csv(MKT11427000_1941, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11427000_1941.csv")

MKT11427000_1981 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	MK <- MannKendall(american$`11427000`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1981-12")[[1]]:length(american$`11427000`$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKT11427000_1981$tau[[1]] <- MK$tau
	MKT11427000_1981$p2s[[1]] <- MK$sl
	MKT11427000_1981$station[[1]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1981$start[[1]] <- american$`11427000`$Winter_3mon$All$Data$Date[[which(format(american$`11427000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1981-12")[[1]]]]
	MKT11427000_1981$end_date[[1]] <- tail(american$`11427000`$Winter_3mon$All$Data$Date,1)
	MKT11427000_1981$period[[1]] <- "3MON"
	
	MK <- MannKendall(american$`11427000`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1981-11")[[1]]:length(american$`11427000`$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKT11427000_1981$tau[[2]] <- MK$tau
	MKT11427000_1981$p2s[[2]] <- MK$sl
	MKT11427000_1981$station[[2]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1981$start[[2]] <- american$`11427000`$Winter_6mon$All$Data$Date[[which(format(american$`11427000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1981-11")[[1]]]]
	MKT11427000_1981$end_date[[2]] <- tail(american$`11427000`$Winter_6mon$All$Data$Date,1)
	MKT11427000_1981$period[[2]] <- "6MON"
	
	MK <- MannKendall(american$`11427000`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11427000`$HydroYear$All$Data$Date,"%Y-%m")=="1981-10")[[1]]:length(american$`11427000`$HydroYear$All$Data$Discharge_acfte6_day)])
	MKT11427000_1981$tau[[3]] <- MK$tau
	MKT11427000_1981$p2s[[3]] <- MK$sl
	MKT11427000_1981$station[[3]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1981$start[[3]] <- american$`11427000`$HydroYear$All$Data$Date[[which(format(american$`11427000`$HydroYear$All$Data$Date,"%Y-%m")=="1981-10")[[1]]]]
	MKT11427000_1981$end_date[[3]] <- tail(american$`11427000`$HydroYear$All$Data$Date,1)
	MKT11427000_1981$period[[3]] <- "HY"
	
	MK <- MannKendall(american$`11427000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1982)[[1]]:length(american$`11427000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKT11427000_1981$tau[[4]] <- MK$tau
	MKT11427000_1981$p2s[[4]] <- MK$sl
	MKT11427000_1981$station[[4]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1981$start[[4]] <- american$`11427000`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1982)[[1]]]]
	MKT11427000_1981$end_date[[4]] <- tail(american$`11427000`$Winter_monthly$All$JAN$Data$Date,1)
	MKT11427000_1981$period[[4]] <- "JAN"
	
	MK <- MannKendall(american$`11427000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1982)[[1]]:length(american$`11427000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKT11427000_1981$tau[[5]] <- MK$tau
	MKT11427000_1981$p2s[[5]] <- MK$sl
	MKT11427000_1981$station[[5]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1981$start[[5]] <- american$`11427000`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1982)[[1]]]]
	MKT11427000_1981$end_date[[5]] <- tail(american$`11427000`$Winter_monthly$All$FEB$Data$Date,1)
	MKT11427000_1981$period[[5]] <- "FEB"
	
	MK <- MannKendall(american$`11427000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1982)[[1]]:length(american$`11427000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKT11427000_1981$tau[[6]] <- MK$tau
	MKT11427000_1981$p2s[[6]] <- MK$sl
	MKT11427000_1981$station[[6]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1981$start[[6]] <- american$`11427000`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1982)[[1]]]]
	MKT11427000_1981$end_date[[6]] <- tail(american$`11427000`$Winter_monthly$All$MAR$Data$Date,1)
	MKT11427000_1981$period[[6]] <- "MAR"
	
	MK <- MannKendall(american$`11427000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$APR$Data$Date,"%Y")==1982)[[1]]:length(american$`11427000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKT11427000_1981$tau[[7]] <- MK$tau
	MKT11427000_1981$p2s[[7]] <- MK$sl
	MKT11427000_1981$station[[7]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1981$start[[7]] <- american$`11427000`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$APR$Data$Date,"%Y")==1982)[[1]]]]
	MKT11427000_1981$end_date[[7]] <- tail(american$`11427000`$Winter_monthly$All$APR$Data$Date,1)
	MKT11427000_1981$period[[7]] <- "APR"

	MK <- MannKendall(american$`11427000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11427000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1981)[[1]]:length(american$`11427000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKT11427000_1981$tau[[8]] <- MK$tau
	MKT11427000_1981$p2s[[8]] <- MK$sl
	MKT11427000_1981$station[[8]] <- american$`11427000`$raw$site_no[[1]]
	MKT11427000_1981$start[[8]] <- american$`11427000`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11427000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1981)[[1]]]]
	MKT11427000_1981$end_date[[8]] <- tail(american$`11427000`$Winter_monthly$All$DEC$Data$Date,1)
	MKT11427000_1981$period[[8]] <- "DEC"
	
MKT11427000_1981$start <- as.Date(MKT11427000_1981$start)
MKT11427000_1981$end_date <- as.Date(MKT11427000_1981$end_date)
write.csv(MKT11427000_1981, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11427000_1981.csv")

MKT11427500 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	MK <- MannKendall(american$`11427500`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11427500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1965-12")[[1]]:length(american$`11427500`$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKT11427500$tau[[1]] <- MK$tau
	MKT11427500$p2s[[1]] <- MK$sl
	MKT11427500$station[[1]] <- american$`11427500`$raw$site_no[[1]]
	MKT11427500$start[[1]] <- american$`11427500`$Winter_3mon$All$Data$Date[[which(format(american$`11427500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1965-12")[[1]]]]
	MKT11427500$end_date[[1]] <- tail(american$`11427500`$Winter_3mon$All$Data$Date,1)
	MKT11427500$period[[1]] <- "3MON"
	
	MK <- MannKendall(american$`11427500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11427500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1965-11")[[1]]:length(american$`11427500`$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKT11427500$tau[[2]] <- MK$tau
	MKT11427500$p2s[[2]] <- MK$sl
	MKT11427500$station[[2]] <- american$`11427500`$raw$site_no[[1]]
	MKT11427500$start[[2]] <- american$`11427500`$Winter_6mon$All$Data$Date[[which(format(american$`11427500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1965-11")[[1]]]]
	MKT11427500$end_date[[2]] <- tail(american$`11427500`$Winter_6mon$All$Data$Date,1)
	MKT11427500$period[[2]] <- "6MON"
	
	MK <- MannKendall(american$`11427500`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11427500`$HydroYear$All$Data$Date,"%Y-%m")=="1965-10")[[1]]:length(american$`11427500`$HydroYear$All$Data$Discharge_acfte6_day)])
	MKT11427500$tau[[3]] <- MK$tau
	MKT11427500$p2s[[3]] <- MK$sl
	MKT11427500$station[[3]] <- american$`11427500`$raw$site_no[[1]]
	MKT11427500$start[[3]] <- american$`11427500`$HydroYear$All$Data$Date[[which(format(american$`11427500`$HydroYear$All$Data$Date,"%Y-%m")=="1965-10")[[1]]]]
	MKT11427500$end_date[[3]] <- tail(american$`11427500`$HydroYear$All$Data$Date,1)
	MKT11427500$period[[3]] <- "HY"
	
	MK <- MannKendall(american$`11427500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11427500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1966)[[1]]:length(american$`11427500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKT11427500$tau[[4]] <- MK$tau
	MKT11427500$p2s[[4]] <- MK$sl
	MKT11427500$station[[4]] <- american$`11427500`$raw$site_no[[1]]
	MKT11427500$start[[4]] <- american$`11427500`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11427500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1966)[[1]]]]
	MKT11427500$end_date[[4]] <- tail(american$`11427500`$Winter_monthly$All$JAN$Data$Date,1)
	MKT11427500$period[[4]] <- "JAN"
	
	MK <- MannKendall(american$`11427500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11427500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1966)[[1]]:length(american$`11427500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKT11427500$tau[[5]] <- MK$tau
	MKT11427500$p2s[[5]] <- MK$sl
	MKT11427500$station[[5]] <- american$`11427500`$raw$site_no[[1]]
	MKT11427500$start[[5]] <- american$`11427500`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11427500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1966)[[1]]]]
	MKT11427500$end_date[[5]] <- tail(american$`11427500`$Winter_monthly$All$FEB$Data$Date,1)
	MKT11427500$period[[5]] <- "FEB"
	
	MK <- MannKendall(american$`11427500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11427500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1966)[[1]]:length(american$`11427500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKT11427500$tau[[6]] <- MK$tau
	MKT11427500$p2s[[6]] <- MK$sl
	MKT11427500$station[[6]] <- american$`11427500`$raw$site_no[[1]]
	MKT11427500$start[[6]] <- american$`11427500`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11427500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1966)[[1]]]]
	MKT11427500$end_date[[6]] <- tail(american$`11427500`$Winter_monthly$All$MAR$Data$Date,1)
	MKT11427500$period[[6]] <- "MAR"
	
	MK <- MannKendall(american$`11427500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11427500`$Winter_monthly$All$APR$Data$Date,"%Y")==1966)[[1]]:length(american$`11427500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKT11427500$tau[[7]] <- MK$tau
	MKT11427500$p2s[[7]] <- MK$sl
	MKT11427500$station[[7]] <- american$`11427500`$raw$site_no[[1]]
	MKT11427500$start[[7]] <- american$`11427500`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11427500`$Winter_monthly$All$APR$Data$Date,"%Y")==1966)[[1]]]]
	MKT11427500$end_date[[7]] <- tail(american$`11427500`$Winter_monthly$All$APR$Data$Date,1)
	MKT11427500$period[[7]] <- "APR"

	MK <- MannKendall(american$`11427500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11427500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1965)[[1]]:length(american$`11427500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKT11427500$tau[[8]] <- MK$tau
	MKT11427500$p2s[[8]] <- MK$sl
	MKT11427500$station[[8]] <- american$`11427500`$raw$site_no[[1]]
	MKT11427500$start[[8]] <- american$`11427500`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11427500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1965)[[1]]]]
	MKT11427500$end_date[[8]] <- tail(american$`11427500`$Winter_monthly$All$DEC$Data$Date,1)
	MKT11427500$period[[8]] <- "DEC"

MKT11427500$start <- as.Date(MKT11427500$start)
MKT11427500$end_date <- as.Date(MKT11427500$end_date)
write.csv(MKT11427500, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11427500.csv")


MKT11433500 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	MK <- MannKendall(american$`11433500`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11433500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1966-12")[[1]]:length(american$`11433500`$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKT11433500$tau[[1]] <- MK$tau
	MKT11433500$p2s[[1]] <- MK$sl
	MKT11433500$station[[1]] <- american$`11433500`$raw$site_no[[1]]
	MKT11433500$start[[1]] <- american$`11433500`$Winter_3mon$All$Data$Date[[which(format(american$`11433500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1966-12")[[1]]]]
	MKT11433500$end_date[[1]] <- tail(american$`11433500`$Winter_3mon$All$Data$Date,1)
	MKT11433500$period[[1]] <- "3MON"
	
	MK <- MannKendall(american$`11433500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11433500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1966-11")[[1]]:length(american$`11433500`$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKT11433500$tau[[2]] <- MK$tau
	MKT11433500$p2s[[2]] <- MK$sl
	MKT11433500$station[[2]] <- american$`11433500`$raw$site_no[[1]]
	MKT11433500$start[[2]] <- american$`11433500`$Winter_6mon$All$Data$Date[[which(format(american$`11433500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1966-11")[[1]]]]
	MKT11433500$end_date[[2]] <- tail(american$`11433500`$Winter_6mon$All$Data$Date,1)
	MKT11433500$period[[2]] <- "6MON"
	
	MK <- MannKendall(american$`11433500`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11433500`$HydroYear$All$Data$Date,"%Y-%m")=="1966-10")[[1]]:length(american$`11433500`$HydroYear$All$Data$Discharge_acfte6_day)])
	MKT11433500$tau[[3]] <- MK$tau
	MKT11433500$p2s[[3]] <- MK$sl
	MKT11433500$station[[3]] <- american$`11433500`$raw$site_no[[1]]
	MKT11433500$start[[3]] <- american$`11433500`$HydroYear$All$Data$Date[[which(format(american$`11433500`$HydroYear$All$Data$Date,"%Y-%m")=="1966-10")[[1]]]]
	MKT11433500$end_date[[3]] <- tail(american$`11433500`$HydroYear$All$Data$Date,1)
	MKT11433500$period[[3]] <- "HY"
	
	MK <- MannKendall(american$`11433500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11433500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1967)[[1]]:length(american$`11433500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKT11433500$tau[[4]] <- MK$tau
	MKT11433500$p2s[[4]] <- MK$sl
	MKT11433500$station[[4]] <- american$`11433500`$raw$site_no[[1]]
	MKT11433500$start[[4]] <- american$`11433500`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11433500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1967)[[1]]]]
	MKT11433500$end_date[[4]] <- tail(american$`11433500`$Winter_monthly$All$JAN$Data$Date,1)
	MKT11433500$period[[4]] <- "JAN"
	
	MK <- MannKendall(american$`11433500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11433500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1967)[[1]]:length(american$`11433500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKT11433500$tau[[5]] <- MK$tau
	MKT11433500$p2s[[5]] <- MK$sl
	MKT11433500$station[[5]] <- american$`11433500`$raw$site_no[[1]]
	MKT11433500$start[[5]] <- american$`11433500`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11433500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1967)[[1]]]]
	MKT11433500$end_date[[5]] <- tail(american$`11433500`$Winter_monthly$All$FEB$Data$Date,1)
	MKT11433500$period[[5]] <- "FEB"
	
	MK <- MannKendall(american$`11433500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11433500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1967)[[1]]:length(american$`11433500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKT11433500$tau[[6]] <- MK$tau
	MKT11433500$p2s[[6]] <- MK$sl
	MKT11433500$station[[6]] <- american$`11433500`$raw$site_no[[1]]
	MKT11433500$start[[6]] <- american$`11433500`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11433500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1967)[[1]]]]
	MKT11433500$end_date[[6]] <- tail(american$`11433500`$Winter_monthly$All$MAR$Data$Date,1)
	MKT11433500$period[[6]] <- "MAR"
	
	MK <- MannKendall(american$`11433500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11433500`$Winter_monthly$All$APR$Data$Date,"%Y")==1967)[[1]]:length(american$`11433500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKT11433500$tau[[7]] <- MK$tau
	MKT11433500$p2s[[7]] <- MK$sl
	MKT11433500$station[[7]] <- american$`11433500`$raw$site_no[[1]]
	MKT11433500$start[[7]] <- american$`11433500`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11433500`$Winter_monthly$All$APR$Data$Date,"%Y")==1967)[[1]]]]
	MKT11433500$end_date[[7]] <- tail(american$`11433500`$Winter_monthly$All$APR$Data$Date,1)
	MKT11433500$period[[7]] <- "APR"
	
	MK <- MannKendall(american$`11433500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11433500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1966)[[1]]:length(american$`11433500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKT11433500$tau[[8]] <- MK$tau
	MKT11433500$p2s[[8]] <- MK$sl
	MKT11433500$station[[8]] <- american$`11433500`$raw$site_no[[1]]
	MKT11433500$start[[8]] <- american$`11433500`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11433500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1966)[[1]]]]
	MKT11433500$end_date[[8]] <- tail(american$`11433500`$Winter_monthly$All$DEC$Data$Date,1)
	MKT11433500$period[[8]] <- "DEC"
	
	MKT11433500$start <- as.Date(MKT11433500$start)
	MKT11433500$end_date <- as.Date(MKT11433500$end_date)
write.csv(MKT11433500, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11433500.csv")

MKT11436000 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	MK <- MannKendall(american$`11436000`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11436000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1922-12")[[1]]:length(american$`11436000`$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKT11436000$tau[[1]] <- MK$tau
	MKT11436000$p2s[[1]] <- MK$sl
	MKT11436000$station[[1]] <- american$`11436000`$raw$site_no[[1]]
	MKT11436000$start[[1]] <- american$`11436000`$Winter_3mon$All$Data$Date[[which(format(american$`11436000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1922-12")[[1]]]]
	MKT11436000$end_date[[1]] <- tail(american$`11436000`$Winter_3mon$All$Data$Date,1)
	MKT11436000$period[[1]] <- "3MON"
	
	MK <- MannKendall(american$`11436000`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11436000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1922-11")[[1]]:length(american$`11436000`$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKT11436000$tau[[2]] <- MK$tau
	MKT11436000$p2s[[2]] <- MK$sl
	MKT11436000$station[[2]] <- american$`11436000`$raw$site_no[[1]]
	MKT11436000$start[[2]] <- american$`11436000`$Winter_6mon$All$Data$Date[[which(format(american$`11436000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1922-11")[[1]]]]
	MKT11436000$end_date[[2]] <- tail(american$`11436000`$Winter_6mon$All$Data$Date,1)
	MKT11436000$period[[2]] <- "6MON"
	
	MK <- MannKendall(american$`11436000`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11436000`$HydroYear$All$Data$Date,"%Y-%m")=="1922-10")[[1]]:length(american$`11436000`$HydroYear$All$Data$Discharge_acfte6_day)])
	MKT11436000$tau[[3]] <- MK$tau
	MKT11436000$p2s[[3]] <- MK$sl
	MKT11436000$station[[3]] <- american$`11436000`$raw$site_no[[1]]
	MKT11436000$start[[3]] <- american$`11436000`$HydroYear$All$Data$Date[[which(format(american$`11436000`$HydroYear$All$Data$Date,"%Y-%m")=="1922-10")[[1]]]]
	MKT11436000$end_date[[3]] <- tail(american$`11436000`$HydroYear$All$Data$Date,1)
	MKT11436000$period[[3]] <- "HY"
	
	MK <- MannKendall(american$`11436000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11436000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1923)[[1]]:length(american$`11436000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKT11436000$tau[[4]] <- MK$tau
	MKT11436000$p2s[[4]] <- MK$sl
	MKT11436000$station[[4]] <- american$`11436000`$raw$site_no[[1]]
	MKT11436000$start[[4]] <- american$`11436000`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11436000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1923)[[1]]]]
	MKT11436000$end_date[[4]] <- tail(american$`11436000`$Winter_monthly$All$JAN$Data$Date,1)
	MKT11436000$period[[4]] <- "JAN"
	
	MK <- MannKendall(american$`11436000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11436000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1923)[[1]]:length(american$`11436000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKT11436000$tau[[5]] <- MK$tau
	MKT11436000$p2s[[5]] <- MK$sl
	MKT11436000$station[[5]] <- american$`11436000`$raw$site_no[[1]]
	MKT11436000$start[[5]] <- american$`11436000`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11436000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1923)[[1]]]]
	MKT11436000$end_date[[5]] <- tail(american$`11436000`$Winter_monthly$All$FEB$Data$Date,1)
	MKT11436000$period[[5]] <- "FEB"
	
	MK <- MannKendall(american$`11436000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11436000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1923)[[1]]:length(american$`11436000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKT11436000$tau[[6]] <- MK$tau
	MKT11436000$p2s[[6]] <- MK$sl
	MKT11436000$station[[6]] <- american$`11436000`$raw$site_no[[1]]
	MKT11436000$start[[6]] <- american$`11436000`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11436000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1923)[[1]]]]
	MKT11436000$end_date[[6]] <- tail(american$`11436000`$Winter_monthly$All$MAR$Data$Date,1)
	MKT11436000$period[[6]] <- "MAR"
	
	MK <- MannKendall(american$`11436000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11436000`$Winter_monthly$All$APR$Data$Date,"%Y")==1923)[[1]]:length(american$`11436000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKT11436000$tau[[7]] <- MK$tau
	MKT11436000$p2s[[7]] <- MK$sl
	MKT11436000$station[[7]] <- american$`11436000`$raw$site_no[[1]]
	MKT11436000$start[[7]] <- american$`11436000`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11436000`$Winter_monthly$All$APR$Data$Date,"%Y")==1923)[[1]]]]
	MKT11436000$end_date[[7]] <- tail(american$`11436000`$Winter_monthly$All$APR$Data$Date,1)
	MKT11436000$period[[7]] <- "APR"
	
	MK <- MannKendall(american$`11436000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11436000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1922)[[1]]:length(american$`11436000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKT11436000$tau[[8]] <- MK$tau
	MKT11436000$p2s[[8]] <- MK$sl
	MKT11436000$station[[8]] <- american$`11436000`$raw$site_no[[1]]
	MKT11436000$start[[8]] <- american$`11436000`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11436000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1922)[[1]]]]
	MKT11436000$end_date[[8]] <- tail(american$`11436000`$Winter_monthly$All$DEC$Data$Date,1)
	MKT11436000$period[[8]] <- "DEC"
	
	MKT11436000$start <- as.Date(MKT11436000$start)
	MKT11436000$end_date <- as.Date(MKT11436000$end_date)
write.csv(MKT11436000, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11436000.csv")

MKT11437000 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11437000`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11437000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1922-12")[[1]]:length(american$`11437000`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11437000$tau[[1]] <- MK$tau
MKT11437000$p2s[[1]] <- MK$sl
MKT11437000$station[[1]] <- american$`11437000`$raw$site_no[[1]]
MKT11437000$start[[1]] <- american$`11437000`$Winter_3mon$All$Data$Date[[which(format(american$`11437000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1922-12")[[1]]]]
MKT11437000$end_date[[1]] <- tail(american$`11437000`$Winter_3mon$All$Data$Date,1)
MKT11437000$period[[1]] <- "3MON"

MK <- MannKendall(american$`11437000`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11437000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1922-11")[[1]]:length(american$`11437000`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11437000$tau[[2]] <- MK$tau
MKT11437000$p2s[[2]] <- MK$sl
MKT11437000$station[[2]] <- american$`11437000`$raw$site_no[[1]]
MKT11437000$start[[2]] <- american$`11437000`$Winter_6mon$All$Data$Date[[which(format(american$`11437000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1922-11")[[1]]]]
MKT11437000$end_date[[2]] <- tail(american$`11437000`$Winter_6mon$All$Data$Date,1)
MKT11437000$period[[2]] <- "6MON"

MK <- MannKendall(american$`11437000`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11437000`$HydroYear$All$Data$Date,"%Y-%m")=="1922-10")[[1]]:length(american$`11437000`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11437000$tau[[3]] <- MK$tau
MKT11437000$p2s[[3]] <- MK$sl
MKT11437000$station[[3]] <- american$`11437000`$raw$site_no[[1]]
MKT11437000$start[[3]] <- american$`11437000`$HydroYear$All$Data$Date[[which(format(american$`11437000`$HydroYear$All$Data$Date,"%Y-%m")=="1922-10")[[1]]]]
MKT11437000$end_date[[3]] <- tail(american$`11437000`$HydroYear$All$Data$Date,1)
MKT11437000$period[[3]] <- "HY"

MK <- MannKendall(american$`11437000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11437000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1923)[[1]]:length(american$`11437000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11437000$tau[[4]] <- MK$tau
MKT11437000$p2s[[4]] <- MK$sl
MKT11437000$station[[4]] <- american$`11437000`$raw$site_no[[1]]
MKT11437000$start[[4]] <- american$`11437000`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11437000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1923)[[1]]]]
MKT11437000$end_date[[4]] <- tail(american$`11437000`$Winter_monthly$All$JAN$Data$Date,1)
MKT11437000$period[[4]] <- "JAN"

MK <- MannKendall(american$`11437000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11437000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1923)[[1]]:length(american$`11437000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11437000$tau[[5]] <- MK$tau
MKT11437000$p2s[[5]] <- MK$sl
MKT11437000$station[[5]] <- american$`11437000`$raw$site_no[[1]]
MKT11437000$start[[5]] <- american$`11437000`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11437000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1923)[[1]]]]
MKT11437000$end_date[[5]] <- tail(american$`11437000`$Winter_monthly$All$FEB$Data$Date,1)
MKT11437000$period[[5]] <- "FEB"

MK <- MannKendall(american$`11437000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11437000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1923)[[1]]:length(american$`11437000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11437000$tau[[6]] <- MK$tau
MKT11437000$p2s[[6]] <- MK$sl
MKT11437000$station[[6]] <- american$`11437000`$raw$site_no[[1]]
MKT11437000$start[[6]] <- american$`11437000`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11437000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1923)[[1]]]]
MKT11437000$end_date[[6]] <- tail(american$`11437000`$Winter_monthly$All$MAR$Data$Date,1)
MKT11437000$period[[6]] <- "MAR"

MK <- MannKendall(american$`11437000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11437000`$Winter_monthly$All$APR$Data$Date,"%Y")==1923)[[1]]:length(american$`11437000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11437000$tau[[7]] <- MK$tau
MKT11437000$p2s[[7]] <- MK$sl
MKT11437000$station[[7]] <- american$`11437000`$raw$site_no[[1]]
MKT11437000$start[[7]] <- american$`11437000`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11437000`$Winter_monthly$All$APR$Data$Date,"%Y")==1923)[[1]]]]
MKT11437000$end_date[[7]] <- tail(american$`11437000`$Winter_monthly$All$APR$Data$Date,1)
MKT11437000$period[[7]] <- "APR"

MK <- MannKendall(american$`11437000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11437000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1922)[[1]]:length(american$`11437000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11437000$tau[[8]] <- MK$tau
MKT11437000$p2s[[8]] <- MK$sl
MKT11437000$station[[8]] <- american$`11437000`$raw$site_no[[1]]
MKT11437000$start[[8]] <- american$`11437000`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11437000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1922)[[1]]]]
MKT11437000$end_date[[8]] <- tail(american$`11437000`$Winter_monthly$All$DEC$Data$Date,1)
MKT11437000$period[[8]] <- "DEC"

MKT11437000$start <- as.Date(MKT11437000$start)
MKT11437000$end_date <- as.Date(MKT11437000$end_date)
write.csv(MKT11437000, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11437000.csv")


#####################

MKT11439000 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11439000`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11439000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1923-12")[[1]]:length(american$`11439000`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11439000$tau[[1]] <- MK$tau
MKT11439000$p2s[[1]] <- MK$sl
MKT11439000$station[[1]] <- american$`11439000`$raw$site_no[[1]]
MKT11439000$start[[1]] <- american$`11439000`$Winter_3mon$All$Data$Date[[which(format(american$`11439000`$Winter_3mon$All$Data$Date,"%Y-%m")=="1923-12")[[1]]]]
MKT11439000$end_date[[1]] <- tail(american$`11439000`$Winter_3mon$All$Data$Date,1)
MKT11439000$period[[1]] <- "3MON"

MK <- MannKendall(american$`11439000`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11439000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1923-11")[[1]]:length(american$`11439000`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11439000$tau[[2]] <- MK$tau
MKT11439000$p2s[[2]] <- MK$sl
MKT11439000$station[[2]] <- american$`11439000`$raw$site_no[[1]]
MKT11439000$start[[2]] <- american$`11439000`$Winter_6mon$All$Data$Date[[which(format(american$`11439000`$Winter_6mon$All$Data$Date,"%Y-%m")=="1923-11")[[1]]]]
MKT11439000$end_date[[2]] <- tail(american$`11439000`$Winter_6mon$All$Data$Date,1)
MKT11439000$period[[2]] <- "6MON"

MK <- MannKendall(american$`11439000`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11439000`$HydroYear$All$Data$Date,"%Y-%m")=="1923-10")[[1]]:length(american$`11439000`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11439000$tau[[3]] <- MK$tau
MKT11439000$p2s[[3]] <- MK$sl
MKT11439000$station[[3]] <- american$`11439000`$raw$site_no[[1]]
MKT11439000$start[[3]] <- american$`11439000`$HydroYear$All$Data$Date[[which(format(american$`11439000`$HydroYear$All$Data$Date,"%Y-%m")=="1923-10")[[1]]]]
MKT11439000$end_date[[3]] <- tail(american$`11439000`$HydroYear$All$Data$Date,1)
MKT11439000$period[[3]] <- "HY"

MK <- MannKendall(american$`11439000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11439000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1924)[[1]]:length(american$`11439000`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11439000$tau[[4]] <- MK$tau
MKT11439000$p2s[[4]] <- MK$sl
MKT11439000$station[[4]] <- american$`11439000`$raw$site_no[[1]]
MKT11439000$start[[4]] <- american$`11439000`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11439000`$Winter_monthly$All$JAN$Data$Date,"%Y")==1924)[[1]]]]
MKT11439000$end_date[[4]] <- tail(american$`11439000`$Winter_monthly$All$JAN$Data$Date,1)
MKT11439000$period[[4]] <- "JAN"

MK <- MannKendall(american$`11439000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11439000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1924)[[1]]:length(american$`11439000`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11439000$tau[[5]] <- MK$tau
MKT11439000$p2s[[5]] <- MK$sl
MKT11439000$station[[5]] <- american$`11439000`$raw$site_no[[1]]
MKT11439000$start[[5]] <- american$`11439000`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11439000`$Winter_monthly$All$FEB$Data$Date,"%Y")==1924)[[1]]]]
MKT11439000$end_date[[5]] <- tail(american$`11439000`$Winter_monthly$All$FEB$Data$Date,1)
MKT11439000$period[[5]] <- "FEB"

MK <- MannKendall(american$`11439000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11439000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1924)[[1]]:length(american$`11439000`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11439000$tau[[6]] <- MK$tau
MKT11439000$p2s[[6]] <- MK$sl
MKT11439000$station[[6]] <- american$`11439000`$raw$site_no[[1]]
MKT11439000$start[[6]] <- american$`11439000`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11439000`$Winter_monthly$All$MAR$Data$Date,"%Y")==1924)[[1]]]]
MKT11439000$end_date[[6]] <- tail(american$`11439000`$Winter_monthly$All$MAR$Data$Date,1)
MKT11439000$period[[6]] <- "MAR"

MK <- MannKendall(american$`11439000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11439000`$Winter_monthly$All$APR$Data$Date,"%Y")==1924)[[1]]:length(american$`11439000`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11439000$tau[[7]] <- MK$tau
MKT11439000$p2s[[7]] <- MK$sl
MKT11439000$station[[7]] <- american$`11439000`$raw$site_no[[1]]
MKT11439000$start[[7]] <- american$`11439000`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11439000`$Winter_monthly$All$APR$Data$Date,"%Y")==1924)[[1]]]]
MKT11439000$end_date[[7]] <- tail(american$`11439000`$Winter_monthly$All$APR$Data$Date,1)
MKT11439000$period[[7]] <- "APR"

MK <- MannKendall(american$`11439000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11439000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1923)[[1]]:length(american$`11439000`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11439000$tau[[8]] <- MK$tau
MKT11439000$p2s[[8]] <- MK$sl
MKT11439000$station[[8]] <- american$`11439000`$raw$site_no[[1]]
MKT11439000$start[[8]] <- american$`11439000`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11439000`$Winter_monthly$All$DEC$Data$Date,"%Y")==1923)[[1]]]]
MKT11439000$end_date[[8]] <- tail(american$`11439000`$Winter_monthly$All$DEC$Data$Date,1)
MKT11439000$period[[8]] <- "DEC"

MKT11439000$start <- as.Date(MKT11439000$start)
MKT11439000$end_date <- as.Date(MKT11439000$end_date)
write.csv(MKT11439000, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11439000.csv")

######################


MKT11439500 <- data.frame(period = rep(NA, 8), tau = rep(NA,8), p2s = rep(NA,8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11439500`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11439500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1923-12")[[1]]:length(american$`11439500`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11439500$tau[[1]] <- MK$tau
MKT11439500$p2s[[1]] <- MK$sl
MKT11439500$station[[1]] <- american$`11439500`$raw$site_no[[1]]
MKT11439500$start[[1]] <- american$`11439500`$Winter_3mon$All$Data$Date[[which(format(american$`11439500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1923-12")[[1]]]]
MKT11439500$end_date[[1]] <- tail(american$`11439500`$Winter_3mon$All$Data$Date,1)
MKT11439500$period[[1]] <- "3MON"

MK <- MannKendall(american$`11439500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11439500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1923-11")[[1]]:length(american$`11439500`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11439500$tau[[2]] <- MK$tau
MKT11439500$p2s[[2]] <- MK$sl
MKT11439500$station[[2]] <- american$`11439500`$raw$site_no[[1]]
MKT11439500$start[[2]] <- american$`11439500`$Winter_6mon$All$Data$Date[[which(format(american$`11439500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1923-11")[[1]]]]
MKT11439500$end_date[[2]] <- tail(american$`11439500`$Winter_6mon$All$Data$Date,1)
MKT11439500$period[[2]] <- "6MON"

MK <- MannKendall(american$`11439500`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11439500`$HydroYear$All$Data$Date,"%Y-%m")=="1923-10")[[1]]:length(american$`11439500`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11439500$tau[[3]] <- MK$tau
MKT11439500$p2s[[3]] <- MK$sl
MKT11439500$station[[3]] <- american$`11439500`$raw$site_no[[1]]
MKT11439500$start[[3]] <- american$`11439500`$HydroYear$All$Data$Date[[which(format(american$`11439500`$HydroYear$All$Data$Date,"%Y-%m")=="1923-10")[[1]]]]
MKT11439500$end_date[[3]] <- tail(american$`11439500`$HydroYear$All$Data$Date,1)
MKT11439500$period[[3]] <- "HY"

MK <- MannKendall(american$`11439500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11439500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1924)[[1]]:length(american$`11439500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11439500$tau[[4]] <- MK$tau
MKT11439500$p2s[[4]] <- MK$sl
MKT11439500$station[[4]] <- american$`11439500`$raw$site_no[[1]]
MKT11439500$start[[4]] <- american$`11439500`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11439500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1924)[[1]]]]
MKT11439500$end_date[[4]] <- tail(american$`11439500`$Winter_monthly$All$JAN$Data$Date,1)
MKT11439500$period[[4]] <- "JAN"

MK <- MannKendall(american$`11439500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11439500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1924)[[1]]:length(american$`11439500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11439500$tau[[5]] <- MK$tau
MKT11439500$p2s[[5]] <- MK$sl
MKT11439500$station[[5]] <- american$`11439500`$raw$site_no[[1]]
MKT11439500$start[[5]] <- american$`11439500`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11439500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1924)[[1]]]]
MKT11439500$end_date[[5]] <- tail(american$`11439500`$Winter_monthly$All$FEB$Data$Date,1)
MKT11439500$period[[5]] <- "FEB"

MK <- MannKendall(american$`11439500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11439500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1924)[[1]]:length(american$`11439500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11439500$tau[[6]] <- MK$tau
MKT11439500$p2s[[6]] <- MK$sl
MKT11439500$station[[6]] <- american$`11439500`$raw$site_no[[1]]
MKT11439500$start[[6]] <- american$`11439500`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11439500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1924)[[1]]]]
MKT11439500$end_date[[6]] <- tail(american$`11439500`$Winter_monthly$All$MAR$Data$Date,1)
MKT11439500$period[[6]] <- "MAR"

MK <- MannKendall(american$`11439500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11439500`$Winter_monthly$All$APR$Data$Date,"%Y")==1924)[[1]]:length(american$`11439500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11439500$tau[[7]] <- MK$tau
MKT11439500$p2s[[7]] <- MK$sl
MKT11439500$station[[7]] <- american$`11439500`$raw$site_no[[1]]
MKT11439500$start[[7]] <- american$`11439500`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11439500`$Winter_monthly$All$APR$Data$Date,"%Y")==1924)[[1]]]]
MKT11439500$end_date[[7]] <- tail(american$`11439500`$Winter_monthly$All$APR$Data$Date,1)
MKT11439500$period[[7]] <- "APR"

MK <- MannKendall(american$`11439500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11439500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1923)[[1]]:length(american$`11439500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11439500$tau[[8]] <- MK$tau
MKT11439500$p2s[[8]] <- MK$sl
MKT11439500$station[[8]] <- american$`11439500`$raw$site_no[[1]]
MKT11439500$start[[8]] <- american$`11439500`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11439500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1923)[[1]]]]
MKT11439500$end_date[[8]] <- tail(american$`11439500`$Winter_monthly$All$DEC$Data$Date,1)
MKT11439500$period[[8]] <- "DEC"

MKT11439500$start <- as.Date(MKT11439500$start)
MKT11439500$end_date <- as.Date(MKT11439500$end_date)
write.csv(MKT11439500, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11439500.csv")

############

MKT11439501 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11439501`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11439501`$Winter_3mon$All$Data$Date,"%Y-%m")=="1923-12")[[1]]:length(american$`11439501`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11439501$tau[[1]] <- MK$tau
MKT11439501$p2s[[1]] <- MK$sl
MKT11439501$station[[1]] <- american$`11439501`$raw$site_no[[1]]
MKT11439501$start[[1]] <- american$`11439501`$Winter_3mon$All$Data$Date[[which(format(american$`11439501`$Winter_3mon$All$Data$Date,"%Y-%m")=="1923-12")[[1]]]]
MKT11439501$end_date[[1]] <- tail(american$`11439501`$Winter_3mon$All$Data$Date,1)
MKT11439501$period[[1]] <- "3MON"

MK <- MannKendall(american$`11439501`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11439501`$Winter_6mon$All$Data$Date,"%Y-%m")=="1923-11")[[1]]:length(american$`11439501`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11439501$tau[[2]] <- MK$tau
MKT11439501$p2s[[2]] <- MK$sl
MKT11439501$station[[2]] <- american$`11439501`$raw$site_no[[1]]
MKT11439501$start[[2]] <- american$`11439501`$Winter_6mon$All$Data$Date[[which(format(american$`11439501`$Winter_6mon$All$Data$Date,"%Y-%m")=="1923-11")[[1]]]]
MKT11439501$end_date[[2]] <- tail(american$`11439501`$Winter_6mon$All$Data$Date,1)
MKT11439501$period[[2]] <- "6MON"

MK <- MannKendall(american$`11439501`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11439501`$HydroYear$All$Data$Date,"%Y-%m")=="1923-10")[[1]]:length(american$`11439501`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11439501$tau[[3]] <- MK$tau
MKT11439501$p2s[[3]] <- MK$sl
MKT11439501$station[[3]] <- american$`11439501`$raw$site_no[[1]]
MKT11439501$start[[3]] <- american$`11439501`$HydroYear$All$Data$Date[[which(format(american$`11439501`$HydroYear$All$Data$Date,"%Y-%m")=="1923-10")[[1]]]]
MKT11439501$end_date[[3]] <- tail(american$`11439501`$HydroYear$All$Data$Date,1)
MKT11439501$period[[3]] <- "HY"

MK <- MannKendall(american$`11439501`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11439501`$Winter_monthly$All$JAN$Data$Date,"%Y")==1924)[[1]]:length(american$`11439501`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11439501$tau[[4]] <- MK$tau
MKT11439501$p2s[[4]] <- MK$sl
MKT11439501$station[[4]] <- american$`11439501`$raw$site_no[[1]]
MKT11439501$start[[4]] <- american$`11439501`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11439501`$Winter_monthly$All$JAN$Data$Date,"%Y")==1924)[[1]]]]
MKT11439501$end_date[[4]] <- tail(american$`11439501`$Winter_monthly$All$JAN$Data$Date,1)
MKT11439501$period[[4]] <- "JAN"

MK <- MannKendall(american$`11439501`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11439501`$Winter_monthly$All$FEB$Data$Date,"%Y")==1924)[[1]]:length(american$`11439501`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11439501$tau[[5]] <- MK$tau
MKT11439501$p2s[[5]] <- MK$sl
MKT11439501$station[[5]] <- american$`11439501`$raw$site_no[[1]]
MKT11439501$start[[5]] <- american$`11439501`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11439501`$Winter_monthly$All$FEB$Data$Date,"%Y")==1924)[[1]]]]
MKT11439501$end_date[[5]] <- tail(american$`11439501`$Winter_monthly$All$FEB$Data$Date,1)
MKT11439501$period[[5]] <- "FEB"

MK <- MannKendall(american$`11439501`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11439501`$Winter_monthly$All$MAR$Data$Date,"%Y")==1924)[[1]]:length(american$`11439501`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11439501$tau[[6]] <- MK$tau
MKT11439501$p2s[[6]] <- MK$sl
MKT11439501$station[[6]] <- american$`11439501`$raw$site_no[[1]]
MKT11439501$start[[6]] <- american$`11439501`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11439501`$Winter_monthly$All$MAR$Data$Date,"%Y")==1924)[[1]]]]
MKT11439501$end_date[[6]] <- tail(american$`11439501`$Winter_monthly$All$MAR$Data$Date,1)
MKT11439501$period[[6]] <- "MAR"

MK <- MannKendall(american$`11439501`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11439501`$Winter_monthly$All$APR$Data$Date,"%Y")==1924)[[1]]:length(american$`11439501`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11439501$tau[[7]] <- MK$tau
MKT11439501$p2s[[7]] <- MK$sl
MKT11439501$station[[7]] <- american$`11439501`$raw$site_no[[1]]
MKT11439501$start[[7]] <- american$`11439501`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11439501`$Winter_monthly$All$APR$Data$Date,"%Y")==1924)[[1]]]]
MKT11439501$end_date[[7]] <- tail(american$`11439501`$Winter_monthly$All$APR$Data$Date,1)
MKT11439501$period[[7]] <- "APR"

MK <- MannKendall(american$`11439501`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11439501`$Winter_monthly$All$DEC$Data$Date,"%Y")==1923)[[1]]:length(american$`11439501`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11439501$tau[[8]] <- MK$tau
MKT11439501$p2s[[8]] <- MK$sl
MKT11439501$station[[8]] <- american$`11439501`$raw$site_no[[1]]
MKT11439501$start[[8]] <- american$`11439501`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11439501`$Winter_monthly$All$DEC$Data$Date,"%Y")==1923)[[1]]]]
MKT11439501$end_date[[8]] <- tail(american$`11439501`$Winter_monthly$All$DEC$Data$Date,1)
MKT11439501$period[[8]] <- "DEC"

MKT11439501$start <- as.Date(MKT11439501$start)
MKT11439501$end_date <- as.Date(MKT11439501$end_date)
write.csv(MKT11439501, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11439501.csv")


#################################
MKT11441500 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11441500`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11441500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1959-12")[[1]]:length(american$`11441500`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11441500$tau[[1]] <- MK$tau
MKT11441500$p2s[[1]] <- MK$sl
MKT11441500$station[[1]] <- american$`11441500`$raw$site_no[[1]]
MKT11441500$start[[1]] <- american$`11441500`$Winter_3mon$All$Data$Date[[which(format(american$`11441500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1959-12")[[1]]]]
MKT11441500$end_date[[1]] <- tail(american$`11441500`$Winter_3mon$All$Data$Date,1)
MKT11441500$period[[1]] <- "3MON"

MK <- MannKendall(american$`11441500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11441500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1959-11")[[1]]:length(american$`11441500`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11441500$tau[[2]] <- MK$tau
MKT11441500$p2s[[2]] <- MK$sl
MKT11441500$station[[2]] <- american$`11441500`$raw$site_no[[1]]
MKT11441500$start[[2]] <- american$`11441500`$Winter_6mon$All$Data$Date[[which(format(american$`11441500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1959-11")[[1]]]]
MKT11441500$end_date[[2]] <- tail(american$`11441500`$Winter_6mon$All$Data$Date,1)
MKT11441500$period[[2]] <- "6MON"

MK <- MannKendall(american$`11441500`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11441500`$HydroYear$All$Data$Date,"%Y-%m")=="1959-10")[[1]]:length(american$`11441500`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11441500$tau[[3]] <- MK$tau
MKT11441500$p2s[[3]] <- MK$sl
MKT11441500$station[[3]] <- american$`11441500`$raw$site_no[[1]]
MKT11441500$start[[3]] <- american$`11441500`$HydroYear$All$Data$Date[[which(format(american$`11441500`$HydroYear$All$Data$Date,"%Y-%m")=="1959-10")[[1]]]]
MKT11441500$end_date[[3]] <- tail(american$`11441500`$HydroYear$All$Data$Date,1)
MKT11441500$period[[3]] <- "HY"

MK <- MannKendall(american$`11441500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11441500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1960)[[1]]:length(american$`11441500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11441500$tau[[4]] <- MK$tau
MKT11441500$p2s[[4]] <- MK$sl
MKT11441500$station[[4]] <- american$`11441500`$raw$site_no[[1]]
MKT11441500$start[[4]] <- american$`11441500`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11441500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1960)[[1]]]]
MKT11441500$end_date[[4]] <- tail(american$`11441500`$Winter_monthly$All$JAN$Data$Date,1)
MKT11441500$period[[4]] <- "JAN"

MK <- MannKendall(american$`11441500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11441500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1960)[[1]]:length(american$`11441500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11441500$tau[[5]] <- MK$tau
MKT11441500$p2s[[5]] <- MK$sl
MKT11441500$station[[5]] <- american$`11441500`$raw$site_no[[1]]
MKT11441500$start[[5]] <- american$`11441500`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11441500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1960)[[1]]]]
MKT11441500$end_date[[5]] <- tail(american$`11441500`$Winter_monthly$All$FEB$Data$Date,1)
MKT11441500$period[[5]] <- "FEB"

MK <- MannKendall(american$`11441500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11441500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1960)[[1]]:length(american$`11441500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11441500$tau[[6]] <- MK$tau
MKT11441500$p2s[[6]] <- MK$sl
MKT11441500$station[[6]] <- american$`11441500`$raw$site_no[[1]]
MKT11441500$start[[6]] <- american$`11441500`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11441500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1960)[[1]]]]
MKT11441500$end_date[[6]] <- tail(american$`11441500`$Winter_monthly$All$MAR$Data$Date,1)
MKT11441500$period[[6]] <- "MAR"

MK <- MannKendall(american$`11441500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11441500`$Winter_monthly$All$APR$Data$Date,"%Y")==1960)[[1]]:length(american$`11441500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11441500$tau[[7]] <- MK$tau
MKT11441500$p2s[[7]] <- MK$sl
MKT11441500$station[[7]] <- american$`11441500`$raw$site_no[[1]]
MKT11441500$start[[7]] <- american$`11441500`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11441500`$Winter_monthly$All$APR$Data$Date,"%Y")==1960)[[1]]]]
MKT11441500$end_date[[7]] <- tail(american$`11441500`$Winter_monthly$All$APR$Data$Date,1)
MKT11441500$period[[7]] <- "APR"

MK <- MannKendall(american$`11441500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11441500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1959)[[1]]:length(american$`11441500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11441500$tau[[8]] <- MK$tau
MKT11441500$p2s[[8]] <- MK$sl
MKT11441500$station[[8]] <- american$`11441500`$raw$site_no[[1]]
MKT11441500$start[[8]] <- american$`11441500`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11441500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1959)[[1]]]]
MKT11441500$end_date[[8]] <- tail(american$`11441500`$Winter_monthly$All$DEC$Data$Date,1)
MKT11441500$period[[8]] <- "DEC"

MKT11441500$start <- as.Date(MKT11441500$start)
MKT11441500$end_date <- as.Date(MKT11441500$end_date)
write.csv(MKT11441500, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11441500.csv")


################

MKT11443500 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11443500`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1967-12")[[1]]:length(american$`11443500`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11443500$tau[[1]] <- MK$tau
MKT11443500$p2s[[1]] <- MK$sl
MKT11443500$station[[1]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[1]] <- american$`11443500`$Winter_3mon$All$Data$Date[[which(format(american$`11443500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1967-12")[[1]]]]
MKT11443500$end_date[[1]] <- tail(american$`11443500`$Winter_3mon$All$Data$Date,1)
MKT11443500$period[[1]] <- "3MON"

MK <- MannKendall(american$`11443500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1967-11")[[1]]:length(american$`11443500`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11443500$tau[[2]] <- MK$tau
MKT11443500$p2s[[2]] <- MK$sl
MKT11443500$station[[2]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[2]] <- american$`11443500`$Winter_6mon$All$Data$Date[[which(format(american$`11443500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1967-11")[[1]]]]
MKT11443500$end_date[[2]] <- tail(american$`11443500`$Winter_6mon$All$Data$Date,1)
MKT11443500$period[[2]] <- "6MON"

MK <- MannKendall(american$`11443500`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11443500`$HydroYear$All$Data$Date,"%Y-%m")=="1967-10")[[1]]:length(american$`11443500`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11443500$tau[[3]] <- MK$tau
MKT11443500$p2s[[3]] <- MK$sl
MKT11443500$station[[3]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[3]] <- american$`11443500`$HydroYear$All$Data$Date[[which(format(american$`11443500`$HydroYear$All$Data$Date,"%Y-%m")=="1967-10")[[1]]]]
MKT11443500$end_date[[3]] <- tail(american$`11443500`$HydroYear$All$Data$Date,1)
MKT11443500$period[[3]] <- "HY"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1968)[[1]]:length(american$`11443500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11443500$tau[[4]] <- MK$tau
MKT11443500$p2s[[4]] <- MK$sl
MKT11443500$station[[4]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[4]] <- american$`11443500`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1968)[[1]]]]
MKT11443500$end_date[[4]] <- tail(american$`11443500`$Winter_monthly$All$JAN$Data$Date,1)
MKT11443500$period[[4]] <- "JAN"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1968)[[1]]:length(american$`11443500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11443500$tau[[5]] <- MK$tau
MKT11443500$p2s[[5]] <- MK$sl
MKT11443500$station[[5]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[5]] <- american$`11443500`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1968)[[1]]]]
MKT11443500$end_date[[5]] <- tail(american$`11443500`$Winter_monthly$All$FEB$Data$Date,1)
MKT11443500$period[[5]] <- "FEB"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1968)[[1]]:length(american$`11443500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11443500$tau[[6]] <- MK$tau
MKT11443500$p2s[[6]] <- MK$sl
MKT11443500$station[[6]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[6]] <- american$`11443500`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1968)[[1]]]]
MKT11443500$end_date[[6]] <- tail(american$`11443500`$Winter_monthly$All$MAR$Data$Date,1)
MKT11443500$period[[6]] <- "MAR"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$APR$Data$Date,"%Y")==1968)[[1]]:length(american$`11443500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11443500$tau[[7]] <- MK$tau
MKT11443500$p2s[[7]] <- MK$sl
MKT11443500$station[[7]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[7]] <- american$`11443500`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$APR$Data$Date,"%Y")==1968)[[1]]]]
MKT11443500$end_date[[7]] <- tail(american$`11443500`$Winter_monthly$All$APR$Data$Date,1)
MKT11443500$period[[7]] <- "APR"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1967)[[1]]:length(american$`11443500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11443500$tau[[8]] <- MK$tau
MKT11443500$p2s[[8]] <- MK$sl
MKT11443500$station[[8]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[8]] <- american$`11443500`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1967)[[1]]]]
MKT11443500$end_date[[8]] <- tail(american$`11443500`$Winter_monthly$All$DEC$Data$Date,1)
MKT11443500$period[[8]] <- "DEC"

MKT11443500$start <- as.Date(MKT11443500$start)
MKT11443500$end_date <- as.Date(MKT11443500$end_date)
write.csv(MKT11443500, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11443500.csv")

##########################

MKT11443500 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11443500`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1967-12")[[1]]:length(american$`11443500`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11443500$tau[[1]] <- MK$tau
MKT11443500$p2s[[1]] <- MK$sl
MKT11443500$station[[1]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[1]] <- american$`11443500`$Winter_3mon$All$Data$Date[[which(format(american$`11443500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1967-12")[[1]]]]
MKT11443500$end_date[[1]] <- tail(american$`11443500`$Winter_3mon$All$Data$Date,1)
MKT11443500$period[[1]] <- "3MON"

MK <- MannKendall(american$`11443500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1967-11")[[1]]:length(american$`11443500`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11443500$tau[[2]] <- MK$tau
MKT11443500$p2s[[2]] <- MK$sl
MKT11443500$station[[2]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[2]] <- american$`11443500`$Winter_6mon$All$Data$Date[[which(format(american$`11443500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1967-11")[[1]]]]
MKT11443500$end_date[[2]] <- tail(american$`11443500`$Winter_6mon$All$Data$Date,1)
MKT11443500$period[[2]] <- "6MON"

MK <- MannKendall(american$`11443500`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11443500`$HydroYear$All$Data$Date,"%Y-%m")=="1967-10")[[1]]:length(american$`11443500`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11443500$tau[[3]] <- MK$tau
MKT11443500$p2s[[3]] <- MK$sl
MKT11443500$station[[3]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[3]] <- american$`11443500`$HydroYear$All$Data$Date[[which(format(american$`11443500`$HydroYear$All$Data$Date,"%Y-%m")=="1967-10")[[1]]]]
MKT11443500$end_date[[3]] <- tail(american$`11443500`$HydroYear$All$Data$Date,1)
MKT11443500$period[[3]] <- "HY"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1968)[[1]]:length(american$`11443500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11443500$tau[[4]] <- MK$tau
MKT11443500$p2s[[4]] <- MK$sl
MKT11443500$station[[4]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[4]] <- american$`11443500`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1968)[[1]]]]
MKT11443500$end_date[[4]] <- tail(american$`11443500`$Winter_monthly$All$JAN$Data$Date,1)
MKT11443500$period[[4]] <- "JAN"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1968)[[1]]:length(american$`11443500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11443500$tau[[5]] <- MK$tau
MKT11443500$p2s[[5]] <- MK$sl
MKT11443500$station[[5]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[5]] <- american$`11443500`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1968)[[1]]]]
MKT11443500$end_date[[5]] <- tail(american$`11443500`$Winter_monthly$All$FEB$Data$Date,1)
MKT11443500$period[[5]] <- "FEB"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1968)[[1]]:length(american$`11443500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11443500$tau[[6]] <- MK$tau
MKT11443500$p2s[[6]] <- MK$sl
MKT11443500$station[[6]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[6]] <- american$`11443500`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1968)[[1]]]]
MKT11443500$end_date[[6]] <- tail(american$`11443500`$Winter_monthly$All$MAR$Data$Date,1)
MKT11443500$period[[6]] <- "MAR"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$APR$Data$Date,"%Y")==1968)[[1]]:length(american$`11443500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11443500$tau[[7]] <- MK$tau
MKT11443500$p2s[[7]] <- MK$sl
MKT11443500$station[[7]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[7]] <- american$`11443500`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$APR$Data$Date,"%Y")==1968)[[1]]]]
MKT11443500$end_date[[7]] <- tail(american$`11443500`$Winter_monthly$All$APR$Data$Date,1)
MKT11443500$period[[7]] <- "APR"

MK <- MannKendall(american$`11443500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11443500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1967)[[1]]:length(american$`11443500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11443500$tau[[8]] <- MK$tau
MKT11443500$p2s[[8]] <- MK$sl
MKT11443500$station[[8]] <- american$`11443500`$raw$site_no[[1]]
MKT11443500$start[[8]] <- american$`11443500`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11443500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1967)[[1]]]]
MKT11443500$end_date[[8]] <- tail(american$`11443500`$Winter_monthly$All$DEC$Data$Date,1)
MKT11443500$period[[8]] <- "DEC"

MKT11443500$start <- as.Date(MKT11443500$start)
MKT11443500$end_date <- as.Date(MKT11443500$end_date)
write.csv(MKT11443500, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11443500.csv")

#######################

MKT11446500_1981 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1981-12")[[1]]:length(american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11446500_1981$tau[[1]] <- MK$tau
MKT11446500_1981$p2s[[1]] <- MK$sl
MKT11446500_1981$station[[1]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1981$start[[1]] <- american$`11446500`$Winter_3mon$All$Data$Date[[which(format(american$`11446500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1981-12")[[1]]]]
MKT11446500_1981$end_date[[1]] <- tail(american$`11446500`$Winter_3mon$All$Data$Date,1)
MKT11446500_1981$period[[1]] <- "3MON"

MK <- MannKendall(american$`11446500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1981-11")[[1]]:length(american$`11446500`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11446500_1981$tau[[2]] <- MK$tau
MKT11446500_1981$p2s[[2]] <- MK$sl
MKT11446500_1981$station[[2]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1981$start[[2]] <- american$`11446500`$Winter_6mon$All$Data$Date[[which(format(american$`11446500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1981-11")[[1]]]]
MKT11446500_1981$end_date[[2]] <- tail(american$`11446500`$Winter_6mon$All$Data$Date,1)
MKT11446500_1981$period[[2]] <- "6MON"

MK <- MannKendall(american$`11446500`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11446500`$HydroYear$All$Data$Date,"%Y-%m")=="1981-10")[[1]]:length(american$`11446500`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11446500_1981$tau[[3]] <- MK$tau
MKT11446500_1981$p2s[[3]] <- MK$sl
MKT11446500_1981$station[[3]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1981$start[[3]] <- american$`11446500`$HydroYear$All$Data$Date[[which(format(american$`11446500`$HydroYear$All$Data$Date,"%Y-%m")=="1981-10")[[1]]]]
MKT11446500_1981$end_date[[3]] <- tail(american$`11446500`$HydroYear$All$Data$Date,1)
MKT11446500_1981$period[[3]] <- "HY"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1982)[[1]]:length(american$`11446500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11446500_1981$tau[[4]] <- MK$tau
MKT11446500_1981$p2s[[4]] <- MK$sl
MKT11446500_1981$station[[4]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1981$start[[4]] <- american$`11446500`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1982)[[1]]]]
MKT11446500_1981$end_date[[4]] <- tail(american$`11446500`$Winter_monthly$All$JAN$Data$Date,1)
MKT11446500_1981$period[[4]] <- "JAN"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1982)[[1]]:length(american$`11446500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11446500_1981$tau[[5]] <- MK$tau
MKT11446500_1981$p2s[[5]] <- MK$sl
MKT11446500_1981$station[[5]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1981$start[[5]] <- american$`11446500`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1982)[[1]]]]
MKT11446500_1981$end_date[[5]] <- tail(american$`11446500`$Winter_monthly$All$FEB$Data$Date,1)
MKT11446500_1981$period[[5]] <- "FEB"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1982)[[1]]:length(american$`11446500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11446500_1981$tau[[6]] <- MK$tau
MKT11446500_1981$p2s[[6]] <- MK$sl
MKT11446500_1981$station[[6]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1981$start[[6]] <- american$`11446500`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1982)[[1]]]]
MKT11446500_1981$end_date[[6]] <- tail(american$`11446500`$Winter_monthly$All$MAR$Data$Date,1)
MKT11446500_1981$period[[6]] <- "MAR"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$APR$Data$Date,"%Y")==1982)[[1]]:length(american$`11446500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11446500_1981$tau[[7]] <- MK$tau
MKT11446500_1981$p2s[[7]] <- MK$sl
MKT11446500_1981$station[[7]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1981$start[[7]] <- american$`11446500`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$APR$Data$Date,"%Y")==1982)[[1]]]]
MKT11446500_1981$end_date[[7]] <- tail(american$`11446500`$Winter_monthly$All$APR$Data$Date,1)
MKT11446500_1981$period[[7]] <- "APR"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1981)[[1]]:length(american$`11446500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11446500_1981$tau[[8]] <- MK$tau
MKT11446500_1981$p2s[[8]] <- MK$sl
MKT11446500_1981$station[[8]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1981$start[[8]] <- american$`11446500`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1981)[[1]]]]
MKT11446500_1981$end_date[[8]] <- tail(american$`11446500`$Winter_monthly$All$DEC$Data$Date,1)
MKT11446500_1981$period[[8]] <- "DEC"

MKT11446500_1981$start <- as.Date(MKT11446500_1981$start)
MKT11446500_1981$end_date <- as.Date(MKT11446500_1981$end_date)
write.csv(MKT11446500_1981, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11446500_1981.csv")

#############

MKT11446500_1967 <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
MK <- MannKendall(american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1967-12")[[1]]:length(american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day)])
MKT11446500_1967$tau[[1]] <- MK$tau
MKT11446500_1967$p2s[[1]] <- MK$sl
MKT11446500_1967$station[[1]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1967$start[[1]] <- american$`11446500`$Winter_3mon$All$Data$Date[[which(format(american$`11446500`$Winter_3mon$All$Data$Date,"%Y-%m")=="1967-12")[[1]]]]
MKT11446500_1967$end_date[[1]] <- tail(american$`11446500`$Winter_3mon$All$Data$Date,1)
MKT11446500_1967$period[[1]] <- "3MON"

MK <- MannKendall(american$`11446500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1967-11")[[1]]:length(american$`11446500`$Winter_6mon$All$Data$Discharge_acfte6_day)])
MKT11446500_1967$tau[[2]] <- MK$tau
MKT11446500_1967$p2s[[2]] <- MK$sl
MKT11446500_1967$station[[2]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1967$start[[2]] <- american$`11446500`$Winter_6mon$All$Data$Date[[which(format(american$`11446500`$Winter_6mon$All$Data$Date,"%Y-%m")=="1967-11")[[1]]]]
MKT11446500_1967$end_date[[2]] <- tail(american$`11446500`$Winter_6mon$All$Data$Date,1)
MKT11446500_1967$period[[2]] <- "6MON"

MK <- MannKendall(american$`11446500`$HydroYear$All$Data$Discharge_acfte6_day[which(format(american$`11446500`$HydroYear$All$Data$Date,"%Y-%m")=="1967-10")[[1]]:length(american$`11446500`$HydroYear$All$Data$Discharge_acfte6_day)])
MKT11446500_1967$tau[[3]] <- MK$tau
MKT11446500_1967$p2s[[3]] <- MK$sl
MKT11446500_1967$station[[3]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1967$start[[3]] <- american$`11446500`$HydroYear$All$Data$Date[[which(format(american$`11446500`$HydroYear$All$Data$Date,"%Y-%m")=="1967-10")[[1]]]]
MKT11446500_1967$end_date[[3]] <- tail(american$`11446500`$HydroYear$All$Data$Date,1)
MKT11446500_1967$period[[3]] <- "HY"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1968)[[1]]:length(american$`11446500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
MKT11446500_1967$tau[[4]] <- MK$tau
MKT11446500_1967$p2s[[4]] <- MK$sl
MKT11446500_1967$station[[4]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1967$start[[4]] <- american$`11446500`$Winter_monthly$All$JAN$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$JAN$Data$Date,"%Y")==1968)[[1]]]]
MKT11446500_1967$end_date[[4]] <- tail(american$`11446500`$Winter_monthly$All$JAN$Data$Date,1)
MKT11446500_1967$period[[4]] <- "JAN"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1968)[[1]]:length(american$`11446500`$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
MKT11446500_1967$tau[[5]] <- MK$tau
MKT11446500_1967$p2s[[5]] <- MK$sl
MKT11446500_1967$station[[5]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1967$start[[5]] <- american$`11446500`$Winter_monthly$All$FEB$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$FEB$Data$Date,"%Y")==1968)[[1]]]]
MKT11446500_1967$end_date[[5]] <- tail(american$`11446500`$Winter_monthly$All$FEB$Data$Date,1)
MKT11446500_1967$period[[5]] <- "FEB"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1968)[[1]]:length(american$`11446500`$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
MKT11446500_1967$tau[[6]] <- MK$tau
MKT11446500_1967$p2s[[6]] <- MK$sl
MKT11446500_1967$station[[6]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1967$start[[6]] <- american$`11446500`$Winter_monthly$All$MAR$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$MAR$Data$Date,"%Y")==1968)[[1]]]]
MKT11446500_1967$end_date[[6]] <- tail(american$`11446500`$Winter_monthly$All$MAR$Data$Date,1)
MKT11446500_1967$period[[6]] <- "MAR"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$APR$Data$Date,"%Y")==1968)[[1]]:length(american$`11446500`$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
MKT11446500_1967$tau[[7]] <- MK$tau
MKT11446500_1967$p2s[[7]] <- MK$sl
MKT11446500_1967$station[[7]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1967$start[[7]] <- american$`11446500`$Winter_monthly$All$APR$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$APR$Data$Date,"%Y")==1968)[[1]]]]
MKT11446500_1967$end_date[[7]] <- tail(american$`11446500`$Winter_monthly$All$APR$Data$Date,1)
MKT11446500_1967$period[[7]] <- "APR"

MK <- MannKendall(american$`11446500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american$`11446500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1967)[[1]]:length(american$`11446500`$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
MKT11446500_1967$tau[[8]] <- MK$tau
MKT11446500_1967$p2s[[8]] <- MK$sl
MKT11446500_1967$station[[8]] <- american$`11446500`$raw$site_no[[1]]
MKT11446500_1967$start[[8]] <- american$`11446500`$Winter_monthly$All$DEC$Data$Date[[which(format(american$`11446500`$Winter_monthly$All$DEC$Data$Date,"%Y")==1967)[[1]]]]
MKT11446500_1967$end_date[[8]] <- tail(american$`11446500`$Winter_monthly$All$DEC$Data$Date,1)
MKT11446500_1967$period[[8]] <- "DEC"

MKT11446500_1967$start <- as.Date(MKT11446500_1967$start)
MKT11446500_1967$end_date <- as.Date(MKT11446500_1967$end_date)
write.csv(MKT11446500_1967, file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT11446500_1967.csv")
