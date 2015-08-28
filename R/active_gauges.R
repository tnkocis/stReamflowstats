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

active <- vector("list", length(active_g))
for(z in 1:length(active_g)){
#	active <- list()
	active[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",active_g[[z]],".csv",sep=""), header=TRUE)
#	active$raw2$site_no <- as.numeric(active$raw2$site_no)
#	active$raw <- readNWISdv(active_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	active[[z]]$raw$Date <- as.Date(active[[z]]$raw$Date, "%Y-%m-%d")
	
	active[[z]]$raw <- RemoveLeapDays(active[[z]]$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(active[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		active[[z]]$Index$Valley <- "SacV"
		active[[z]]$Index$Index <- yeartype_old$SVI
		active[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(active[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		active[[z]]$Index$Valley <- "SJV"
		active[[z]]$Index$Index <- yeartype_old$SJI
		active[[z]]$Index$Year <- yeartype_old$Year
	} else {
		active[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",active[[z]]$raw$site_no[[1]]))
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
	active[[z]]$prep <- prepdata(active[[z]]$raw)
	active[[z]]$Availability <- DataAvailability(active[[z]]$raw)
	active[[z]]$thresholds_maf <- thresholds(active[[z]]$prep)
	if(all(active[[z]]$thresholds_maf==0)){
	} else {
#	active$record_stats <- record_stats(active$prep, active$thresholds_maf)
		active[[z]]$Winter_3mon <- Split3Winter(active[[z]]$prep, active[[z]]$Index, active[[z]]$thresholds_maf)
		active[[z]]$Winter_6mon <- Split6Winter(active[[z]]$prep, active[[z]]$Index, active[[z]]$thresholds_maf)
		active[[z]]$Winter_monthly <- SplitWinterMonthly(active[[z]]$prep, active[[z]]$Index, active[[z]]$thresholds_maf)
		active[[z]]$HydroYear <- SplitHydroYear(active[[z]]$prep, active[[z]]$Index, active[[z]]$thresholds_maf)	
		active[[z]]$HydroYear <- cleanupHY(active[[z]]$HydroYear)
		active[[z]]$Winter_6mon <- cleanup6MON(active[[z]]$Winter_6mon)
		active[[z]]$Winter_3mon <- cleanup3MON(active[[z]]$Winter_3mon)
		
		
		#rewrite below to write to a single output df
		active[[z]]$ThresholdFit6MON <- ThresholdFit(active[[z]]$Winter_6mon, 0.9)
		active[[z]]$ThresholdFit3MON <- ThresholdFit(active[[z]]$Winter_3mon, 0.9)
		active[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(active[[z]]$Winter_monthly, 0.9)
		active[[z]]$ThresholdFitHY <- ThresholdFit(active[[z]]$HydroYear, 0.9)
		
		
		active[[z]]$MKT6MON <- MKT(active[[z]]$ThresholdFit6MON)
		active[[z]]$MKT3MON <- MKT(active[[z]]$ThresholdFit3MON)
		active[[z]]$MKTMON  <- MKT(active[[z]]$ThresholdFitMON)
		active[[z]]$MKTHY <- MKT(active[[z]]$ThresholdFitHY)
		
		MKT6MONdayactive$tau[[z]] <- active[[z]]$MKT6MON$MKTday[[1]][[1]]
		MKT6MONdayactive$pvalue[[z]] <- active[[z]]$MKT6MON$MKTday[[2]][[1]]
		MKT6MONdayactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		MKT3MONdayactive$tau[[z]] <- active[[z]]$MKT3MON$MKTday[[1]][[1]]
		MKT3MONdayactive$pvalue[[z]] <- active[[z]]$MKT3MON$MKTday[[2]][[1]]
		MKT3MONdayactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		MKTHYdayactive$tau[[z]] <- active[[z]]$MKTHY$MKTday[[1]][[1]]
		MKTHYdayactive$pvalue[[z]] <- active[[z]]$MKTHY$MKTday[[2]][[1]]
		MKTHYdayactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONdayactive[[k]]$tau[[z]] <- active[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
			MKTMONdayactive[[k]]$pvalue[[z]] <- active[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
			MKTMONdayactive[[k]]$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		}
		
		MKT6MONvolactive$tau[[z]] <- active[[z]]$MKT6MON$MKTvol[[1]][[1]]
		MKT6MONvolactive$pvalue[[z]] <- active[[z]]$MKT6MON$MKTvol[[2]][[1]]
		MKT6MONvolactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		MKT3MONvolactive$tau[[z]] <- active[[z]]$MKT3MON$MKTvol[[1]][[1]]
		MKT3MONvolactive$pvalue[[z]] <- active[[z]]$MKT3MON$MKTvol[[2]][[1]]
		MKT3MONvolactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		MKTHYvolactive$tau[[z]] <- active[[z]]$MKTHY$MKTvol[[1]][[1]]
		MKTHYvolactive$pvalue[[z]] <- active[[z]]$MKTHY$MKTvol[[2]][[1]]
		MKTHYvolactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONvolactive[[k]]$tau[[z]] <- active[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
			MKTMONvolactive[[k]]$pvalue[[z]] <- active[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
			MKTMONvolactive[[k]]$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		}
		
		
		##########################
		active[[z]]$FracAbove6MON <- FracAboveExtract(active[[z]]$ThresholdFit6MON)
		active[[z]]$FracAbove3MON <- FracAboveExtract(active[[z]]$ThresholdFit3MON)
		active[[z]]$FracAboveMON  <- FracAboveExtract(active[[z]]$ThresholdFitMON)
		active[[z]]$FracAboveHY <- FracAboveExtract(active[[z]]$ThresholdFitHY)
		
		FracAbove6MONdayactive$FracAboveday[[z]] <- mean(active[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
		FracAbove6MONdayactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		FracAbove3MONdayactive$FracAboveday[[z]] <- mean(active[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
		FracAbove3MONdayactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		FracAboveHYdayactive$FracAboveday[[z]] <- mean(active[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
		FracAboveHYdayactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONdayactive[[k]]$FracAboveday[[z]] <- mean(active[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
			FracAboveMONdayactive[[k]]$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		}
		
		FracAbove6MONvolactive$FracAbovevol[[z]] <- mean(active[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
		FracAbove6MONvolactive$VolAbvMAF[[z]] <- mean(active[[z]]$FracAbove6MON$VolAbvMAF, na.rm=TRUE)
		FracAbove6MONvolactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		FracAbove3MONvolactive$FracAbovevol[[z]] <- mean(active[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
		FracAbove3MONvolactive$VolAbvMAF[[z]] <- mean(active[[z]]$FracAbove3MON$VolAbvMAF, na.rm=TRUE)
		FracAbove3MONvolactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		FracAboveHYvolactive$FracAbovevol[[z]] <- mean(active[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
		FracAboveHYvolactive$VolAbvMAF[[z]] <- mean(active[[z]]$FracAboveHY$VolAbvMAF, na.rm=TRUE)
		FracAboveHYvolactive$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONvolactive[[k]]$FracAbovevol[[z]] <- mean(active[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
			FracAboveMONvolactive[[k]]$VolAbvMAF[[z]] <- mean(active[[z]]$FracAboveMON[[k]]$VolAbvMAF, na.rm=TRUE)
			FracAboveMONvolactive[[k]]$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
		}
		###############################################################
		
#		FracAbove6MONday$FracAboveday[[z]] <- gls(active[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONday$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONday$FracAboveday[[z]] <- mean(active[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONday$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYday$FracAboveday[[z]] <- mean(active[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYday$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(active[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONday[[k]]$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvol$FracAbovevol[[z]] <- mean(active[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvol$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvol$FracAbovevol[[z]] <- mean(active[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvol$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvol$FracAbovevol[[z]] <- mean(active[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvol$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(active[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvol[[k]]$gauge[[z]] <- active[[z]]$raw$site_no[[1]]
#		}
		
		#############################
	}
	
	if(any(z==seq(5,200,10))){
		save.image()
	}
}	

save.image()

names(active) <- active_g






