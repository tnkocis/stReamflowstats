# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


write.csv(USGS11377100$Daysmax$df$X1DayMaxQ_maf, file="C:\\Users\\tiffn_000\\Desktop\\Data\\sac11377100_1daymax.csv")
write.csv(USGS11377100$Daysmax$df$X3DayMaxQ_maf, file="C:\\Users\\tiffn_000\\Desktop\\Data\\sac11377100_3daymax.csv")
write.csv(USGS11377100$Daysmax$df$X7DayMaxQ_maf, file="C:\\Users\\tiffn_000\\Desktop\\Data\\sac11377100_7daymax.csv")

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




######################




active_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\Data\\Active_gauges\\active.csv")
active_g <- as.numeric(active_g$Gauge)


MKTMONdayactive <- vector("list",6)
names(MKTMONdayactive)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONdayactive[[k]] <- data.frame(tau=rep(NA, length(active_g)), pvalue=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
}
MKT6MONdayactive <- data.frame(tau=rep(NA, length(active_g)), pvalue=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
MKT3MONdayactive <- data.frame(tau=rep(NA, length(active_g)), pvalue=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
MKTHYdayactive <- data.frame(tau=rep(NA, length(active_g)), pvalue=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))

MKTMONvolactive <- vector("list",6)
names(MKTMONvolactive)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONvolactive[[k]] <- data.frame(tau=rep(NA, length(active_g)), pvalue=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
}
MKT6MONvolactive <- data.frame(tau=rep(NA, length(active_g)), pvalue=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
MKT3MONvolactive <- data.frame(tau=rep(NA, length(active_g)), pvalue=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
MKTHYvolactive <- data.frame(tau=rep(NA, length(active_g)), pvalue=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))




######################
FracAboveMONdayactive <- vector("list",6)
names(FracAboveMONdayactive)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONdayactive[[k]] <- data.frame(FracAboveday=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
}
FracAbove6MONdayactive <- data.frame(FracAboveday=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
FracAbove3MONdayactive <- data.frame(FracAboveday=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
FracAboveHYdayactive <- data.frame(FracAboveday=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))

FracAboveMONvolactive <- vector("list",6)
names(FracAboveMONvolactive)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONvolactive[[k]] <- data.frame(FracAbovevol=rep(NA, length(active_g)), VolAbvMAF=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
}
FracAbove6MONvolactive <- data.frame(FracAbovevol=rep(NA, length(active_g)), VolAbvMAF=rep(NA, length(active_g)), gauge=rep(NA, length(active_g)))
FracAbove3MONvolactive <- data.frame(FracAbovevol=rep(NA, length(active_g)),  VolAbvMAF=rep(NA, length(active_g)),gauge=rep(NA, length(active_g)))
FracAboveHYvolactive <- data.frame(FracAbovevol=rep(NA, length(active_g)),  VolAbvMAF=rep(NA, length(active_g)),gauge=rep(NA, length(active_g)))

################################


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
active_g <- active_g[which(active_g %in% txtgauges)]
#length(active_g)
#active <- vector("list", 5)

blah <- vector("list", length(active_g))
for(z in 1:length(active_g)){
#	active <- list()
	blah[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",active_g[[z]],".csv",sep=""), header=TRUE)
#	active$raw2$site_no <- as.numeric(active$raw2$site_no)
#	active$raw <- readNWISdv(active_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	blah[[z]]$raw$Date <- as.Date(blah[[z]]$raw$Date, "%Y-%m-%d")
	
	blah[[z]]$raw <- RemoveLeapDays(blah[[z]]$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(blah[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		blah[[z]]$Index$Valley <- "SacV"
		blah[[z]]$Index$Index <- yeartype_old$SVI
		blah[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(blah[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		blah[[z]]$Index$Valley <- "SJV"
		blah[[z]]$Index$Index <- yeartype_old$SJI
		blah[[z]]$Index$Year <- yeartype_old$Year
	} else {
		blah[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",blah[[z]]$raw$site_no[[1]]))
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
	blah[[z]]$prep <- prepdata(blah[[z]]$raw)
	blah[[z]]$Availability <- DataAvailability(blah[[z]]$raw)
	blah[[z]]$thresholds_maf <- thresholds(blah[[z]]$prep)
	if(all(blah[[z]]$thresholds_maf==0)){
	} else {
#	blah$record_stats <- record_stats(blah$prep, blah$thresholds_maf)
		blah[[z]]$Winter_3mon <- Split3Winter(blah[[z]]$prep, blah[[z]]$Index, blah[[z]]$thresholds_maf)
		blah[[z]]$Winter_6mon <- Split6Winter(blah[[z]]$prep, blah[[z]]$Index, blah[[z]]$thresholds_maf)
		blah[[z]]$Winter_monthly <- SplitWinterMonthly(blah[[z]]$prep, blah[[z]]$Index, blah[[z]]$thresholds_maf)
		blah[[z]]$HydroYear <- SplitHydroYear(blah[[z]]$prep, blah[[z]]$Index, blah[[z]]$thresholds_maf)	
		blah[[z]]$HydroYear <- cleanupHY(blah[[z]]$HydroYear)
		blah[[z]]$Winter_6mon <- cleanup6MON(blah[[z]]$Winter_6mon)
		blah[[z]]$Winter_3mon <- cleanup3MON(blah[[z]]$Winter_3mon)
		
		
		#rewrite below to write to a single output df
		blah[[z]]$ThresholdFit6MON <- ThresholdFit(blah[[z]]$Winter_6mon, 0.9)
		blah[[z]]$ThresholdFit3MON <- ThresholdFit(blah[[z]]$Winter_3mon, 0.9)
		blah[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(blah[[z]]$Winter_monthly, 0.9)
		blah[[z]]$ThresholdFitHY <- ThresholdFit(blah[[z]]$HydroYear, 0.9)
		
		
		blah[[z]]$MKT6MON <- MKT(blah[[z]]$ThresholdFit6MON)
		blah[[z]]$MKT3MON <- MKT(blah[[z]]$ThresholdFit3MON)
		blah[[z]]$MKTMON  <- MKT(blah[[z]]$ThresholdFitMON)
		blah[[z]]$MKTHY <- MKT(blah[[z]]$ThresholdFitHY)
		
		MKT6MONdayblah$tau[[z]] <- blah[[z]]$MKT6MON$MKTday[[1]][[1]]
		MKT6MONdayblah$pvalue[[z]] <- blah[[z]]$MKT6MON$MKTday[[2]][[1]]
		MKT6MONdayblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		MKT3MONdayblah$tau[[z]] <- blah[[z]]$MKT3MON$MKTday[[1]][[1]]
		MKT3MONdayblah$pvalue[[z]] <- blah[[z]]$MKT3MON$MKTday[[2]][[1]]
		MKT3MONdayblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		MKTHYdayblah$tau[[z]] <- blah[[z]]$MKTHY$MKTday[[1]][[1]]
		MKTHYdayblah$pvalue[[z]] <- blah[[z]]$MKTHY$MKTday[[2]][[1]]
		MKTHYdayblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONdayblah[[k]]$tau[[z]] <- blah[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
			MKTMONdayblah[[k]]$pvalue[[z]] <- blah[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
			MKTMONdayblah[[k]]$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		}
		
		MKT6MONvolblah$tau[[z]] <- blah[[z]]$MKT6MON$MKTvol[[1]][[1]]
		MKT6MONvolblah$pvalue[[z]] <- blah[[z]]$MKT6MON$MKTvol[[2]][[1]]
		MKT6MONvolblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		MKT3MONvolblah$tau[[z]] <- blah[[z]]$MKT3MON$MKTvol[[1]][[1]]
		MKT3MONvolblah$pvalue[[z]] <- blah[[z]]$MKT3MON$MKTvol[[2]][[1]]
		MKT3MONvolblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		MKTHYvolblah$tau[[z]] <- blah[[z]]$MKTHY$MKTvol[[1]][[1]]
		MKTHYvolblah$pvalue[[z]] <- blah[[z]]$MKTHY$MKTvol[[2]][[1]]
		MKTHYvolblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONvolblah[[k]]$tau[[z]] <- blah[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
			MKTMONvolblah[[k]]$pvalue[[z]] <- blah[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
			MKTMONvolblah[[k]]$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		}
		
		
		##########################
		blah[[z]]$FracAbove6MON <- FracAboveExtract(blah[[z]]$ThresholdFit6MON)
		blah[[z]]$FracAbove3MON <- FracAboveExtract(blah[[z]]$ThresholdFit3MON)
		blah[[z]]$FracAboveMON  <- FracAboveExtract(blah[[z]]$ThresholdFitMON)
		blah[[z]]$FracAboveHY <- FracAboveExtract(blah[[z]]$ThresholdFitHY)
		
		FracAbove6MONdayblah$FracAboveday[[z]] <- mean(blah[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
		FracAbove6MONdayblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		FracAbove3MONdayblah$FracAboveday[[z]] <- mean(blah[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
		FracAbove3MONdayblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		FracAboveHYdayblah$FracAboveday[[z]] <- mean(blah[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
		FracAboveHYdayblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONdayblah[[k]]$FracAboveday[[z]] <- mean(blah[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
			FracAboveMONdayblah[[k]]$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		}
		
		FracAbove6MONvolblah$FracAbovevol[[z]] <- mean(blah[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
		FracAbove6MONvolblah$VolAbvMAF[[z]] <- mean(blah[[z]]$FracAbove6MON$VolAbvMAF, na.rm=TRUE)
		FracAbove6MONvolblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		FracAbove3MONvolblah$FracAbovevol[[z]] <- mean(blah[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
		FracAbove3MONvolblah$VolAbvMAF[[z]] <- mean(blah[[z]]$FracAbove3MON$VolAbvMAF, na.rm=TRUE)
		FracAbove3MONvolblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		FracAboveHYvolblah$FracAbovevol[[z]] <- mean(blah[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
		FracAboveHYvolblah$VolAbvMAF[[z]] <- mean(blah[[z]]$FracAboveHY$VolAbvMAF, na.rm=TRUE)
		FracAboveHYvolblah$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONvolblah[[k]]$FracAbovevol[[z]] <- mean(blah[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
			FracAboveMONvolblah[[k]]$VolAbvMAF[[z]] <- mean(blah[[z]]$FracAboveMON[[k]]$VolAbvMAF, na.rm=TRUE)
			FracAboveMONvolblah[[k]]$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
		}
		###############################################################
		
#		FracAbove6MONday$FracAboveday[[z]] <- gls(blah[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONday$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONday$FracAboveday[[z]] <- mean(blah[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONday$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYday$FracAboveday[[z]] <- mean(blah[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYday$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(blah[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONday[[k]]$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvol$FracAbovevol[[z]] <- mean(blah[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvol$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvol$FracAbovevol[[z]] <- mean(blah[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvol$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvol$FracAbovevol[[z]] <- mean(blah[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvol$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(blah[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvol[[k]]$gauge[[z]] <- blah[[z]]$raw$site_no[[1]]
#		}
		
		#############################
		
	}
	
	if(any(z==seq(5,200,10))){
		save.image()
	}
}	

save.image()

names(blah) <- blah_g

for(i in 1:length(blah)){
	blah[[i]]$Winter_monthly <- cleanupMON(blah[[i]]$Winter_monthly)
	blah[[i]]$daysmax$W3MON <- FreqAnalysis(blah[[i]]$Winter_3mon,c(1,3,7),blah[[i]]$Index)
	blah[[i]]$daysmax$W6MON <- FreqAnalysis(blah[[i]]$Winter_6mon,c(1,3,7),blah[[i]]$Index)
	blah[[i]]$daysmax$HY <- FreqAnalysis(blah[[i]]$HydroYear,c(1,3,7),blah[[i]]$Index)
	blah[[i]]$daysmax$WMON <- FreqAnalysisMonthly(blah[[i]]$Winter_monthly,c(1,3,7),blah[[i]]$Index)
}





