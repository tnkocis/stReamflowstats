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






