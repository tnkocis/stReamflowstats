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




sacupper_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Streamflow\\Sacramento\\sacupper\\TXT\\Y80_gauges_sacu.txt")
sacupper_g <- as.numeric(sacupper_g$SITENO)


MKTMONdaysacupper <- vector("list",6)
names(MKTMONdaysacupper)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONdaysacupper[[k]] <- data.frame(tau=rep(NA, length(sacupper_g)), pvalue=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
}
MKT6MONdaysacupper <- data.frame(tau=rep(NA, length(sacupper_g)), pvalue=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
MKT3MONdaysacupper <- data.frame(tau=rep(NA, length(sacupper_g)), pvalue=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
MKTHYdaysacupper <- data.frame(tau=rep(NA, length(sacupper_g)), pvalue=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))

MKTMONvolsacupper <- vector("list",6)
names(MKTMONvolsacupper)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONvolsacupper[[k]] <- data.frame(tau=rep(NA, length(sacupper_g)), pvalue=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
}
MKT6MONvolsacupper <- data.frame(tau=rep(NA, length(sacupper_g)), pvalue=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
MKT3MONvolsacupper <- data.frame(tau=rep(NA, length(sacupper_g)), pvalue=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
MKTHYvolsacupper <- data.frame(tau=rep(NA, length(sacupper_g)), pvalue=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))




######################
FracAboveMONdaysacupper <- vector("list",6)
names(FracAboveMONdaysacupper)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONdaysacupper[[k]] <- data.frame(FracAboveday=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
}
FracAbove6MONdaysacupper <- data.frame(FracAboveday=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
FracAbove3MONdaysacupper <- data.frame(FracAboveday=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
FracAboveHYdaysacupper <- data.frame(FracAboveday=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))

FracAboveMONvolsacupper <- vector("list",6)
names(FracAboveMONvolsacupper)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONvolsacupper[[k]] <- data.frame(FracAbovevol=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
}
FracAbove6MONvolsacupper <- data.frame(FracAbovevol=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
FracAbove3MONvolsacupper <- data.frame(FracAbovevol=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
FracAboveHYvolsacupper <- data.frame(FracAbovevol=rep(NA, length(sacupper_g)), gauge=rep(NA, length(sacupper_g)))
################################


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
sacupper_g <- sacupper_g[which(sacupper_g %in% txtgauges)]
#length(sacupper_g)
#sacupper <- vector("list", 5)

sacupper <- vector("list", length(sacupper_g))
for(z in 1:length(sacupper_g)){
#	sacupper <- list()
	sacupper[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",sacupper_g[[z]],".csv",sep=""), header=TRUE)
#	sacupper$raw2$site_no <- as.numeric(sacupper$raw2$site_no)
#	sacupper$raw <- readNWISdv(sacupper_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	sacupper[[z]]$raw$Date <- as.Date(sacupper[[z]]$raw$Date, "%Y-%m-%d")
	
	sacupper[[z]]$raw <- RemoveLeapDays(sacupper[[z]]$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(sacupper[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		sacupper[[z]]$Index$Valley <- "SacV"
		sacupper[[z]]$Index$Index <- yeartype_old$SVI
		sacupper[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(sacupper[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		sacupper[[z]]$Index$Valley <- "SJV"
		sacupper[[z]]$Index$Index <- yeartype_old$SJV
		sacupper[[z]]$Index$Year <- yeartype_old$Year
	} else {
		sacupper[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",sacupper[[z]]$raw$site_no[[1]]))
	}
	
#	if(as.numeric(sacupper$raw$site_no[[1]]) %in% SacV_gauges$site_no){
#		sacupper$Index$Valley <- "SacV"
#		sacupper$Index$Index <- YEARTYPEqdf$SacV_num
#		sacupper$Index$Year <- YEARTYPEqdf$Year
#	} else if(as.numeric(sacupper$raw$site_no[[1]]) %in% SJV_gauges$site_no){
#		sacupper$Index$Valley <- "SJV"
#		sacupper$Index$Index <- YEARTYPEqdf$SJV_num
#		sacupper$Index$Year <- YEARTYPEqdf$Year
#	} else {
#		sacupper$Index$Valley <- "ERROR"
#		print(paste("Error",sacupper$raw$site_no[[1]]))
#	}
#	
	
	###DATA PROCESSING
	sacupper[[z]]$prep <- prepdata(sacupper[[z]]$raw)
	sacupper[[z]]$Availability <- DataAvailability(sacupper[[z]]$raw)
	sacupper[[z]]$thresholds_maf <- thresholds(sacupper[[z]]$prep)
	if(all(sacupper[[z]]$thresholds_maf==0)){
	} else {
#	sacupper$record_stats <- record_stats(sacupper$prep, sacupper$thresholds_maf)
		sacupper[[z]]$Winter_3mon <- Split3Winter(sacupper[[z]]$prep, sacupper[[z]]$Index, sacupper[[z]]$thresholds_maf)
		sacupper[[z]]$Winter_6mon <- Split6Winter(sacupper[[z]]$prep, sacupper[[z]]$Index, sacupper[[z]]$thresholds_maf)
		sacupper[[z]]$Winter_monthly <- SplitWinterMonthly(sacupper[[z]]$prep, sacupper[[z]]$Index, sacupper[[z]]$thresholds_maf)
		sacupper[[z]]$HydroYear <- SplitHydroYear(sacupper[[z]]$prep, sacupper[[z]]$Index, sacupper[[z]]$thresholds_maf)	
		sacupper[[z]]$HydroYear <- cleanupHY(sacupper[[z]]$HydroYear)
		sacupper[[z]]$Winter_6mon <- cleanup6MON(sacupper[[z]]$Winter_6mon)
		sacupper[[z]]$Winter_3mon <- cleanup3MON(sacupper[[z]]$Winter_3mon)
		
		
		#rewrite below to write to a single output df
		sacupper[[z]]$ThresholdFit6MON <- ThresholdFit(sacupper[[z]]$Winter_6mon, 0.9)
		sacupper[[z]]$ThresholdFit3MON <- ThresholdFit(sacupper[[z]]$Winter_3mon, 0.9)
		sacupper[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(sacupper[[z]]$Winter_monthly, 0.9)
		sacupper[[z]]$ThresholdFitHY <- ThresholdFit(sacupper[[z]]$HydroYear, 0.9)
		
		
		sacupper[[z]]$MKT6MON <- MKT(sacupper[[z]]$ThresholdFit6MON)
		sacupper[[z]]$MKT3MON <- MKT(sacupper[[z]]$ThresholdFit3MON)
		sacupper[[z]]$MKTMON  <- MKT(sacupper[[z]]$ThresholdFitMON)
		sacupper[[z]]$MKTHY <- MKT(sacupper[[z]]$ThresholdFitHY)
		
		MKT6MONdaysacupper$tau[[z]] <- sacupper[[z]]$MKT6MON$MKTday[[1]][[1]]
		MKT6MONdaysacupper$pvalue[[z]] <- sacupper[[z]]$MKT6MON$MKTday[[2]][[1]]
		MKT6MONdaysacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		MKT3MONdaysacupper$tau[[z]] <- sacupper[[z]]$MKT3MON$MKTday[[1]][[1]]
		MKT3MONdaysacupper$pvalue[[z]] <- sacupper[[z]]$MKT3MON$MKTday[[2]][[1]]
		MKT3MONdaysacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		MKTHYdaysacupper$tau[[z]] <- sacupper[[z]]$MKTHY$MKTday[[1]][[1]]
		MKTHYdaysacupper$pvalue[[z]] <- sacupper[[z]]$MKTHY$MKTday[[2]][[1]]
		MKTHYdaysacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONdaysacupper[[k]]$tau[[z]] <- sacupper[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
			MKTMONdaysacupper[[k]]$pvalue[[z]] <- sacupper[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
			MKTMONdaysacupper[[k]]$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		}
		
		MKT6MONvolsacupper$tau[[z]] <- sacupper[[z]]$MKT6MON$MKTvol[[1]][[1]]
		MKT6MONvolsacupper$pvalue[[z]] <- sacupper[[z]]$MKT6MON$MKTvol[[2]][[1]]
		MKT6MONvolsacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		MKT3MONvolsacupper$tau[[z]] <- sacupper[[z]]$MKT3MON$MKTvol[[1]][[1]]
		MKT3MONvolsacupper$pvalue[[z]] <- sacupper[[z]]$MKT3MON$MKTvol[[2]][[1]]
		MKT3MONvolsacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		MKTHYvolsacupper$tau[[z]] <- sacupper[[z]]$MKTHY$MKTvol[[1]][[1]]
		MKTHYvolsacupper$pvalue[[z]] <- sacupper[[z]]$MKTHY$MKTvol[[2]][[1]]
		MKTHYvolsacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONvolsacupper[[k]]$tau[[z]] <- sacupper[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
			MKTMONvolsacupper[[k]]$pvalue[[z]] <- sacupper[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
			MKTMONvolsacupper[[k]]$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		}
		
		
		##########################
		sacupper[[z]]$FracAbove6MON <- FracAboveExtract(sacupper[[z]]$ThresholdFit6MON)
		sacupper[[z]]$FracAbove3MON <- FracAboveExtract(sacupper[[z]]$ThresholdFit3MON)
		sacupper[[z]]$FracAboveMON  <- FracAboveExtract(sacupper[[z]]$ThresholdFitMON)
		sacupper[[z]]$FracAboveHY <- FracAboveExtract(sacupper[[z]]$ThresholdFitHY)
		
		FracAbove6MONdaysacupper$FracAboveday[[z]] <- mean(sacupper[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
		FracAbove6MONdaysacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		FracAbove3MONdaysacupper$FracAboveday[[z]] <- mean(sacupper[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
		FracAbove3MONdaysacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		FracAboveHYdaysacupper$FracAboveday[[z]] <- mean(sacupper[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
		FracAboveHYdaysacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONdaysacupper[[k]]$FracAboveday[[z]] <- mean(sacupper[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
			FracAboveMONdaysacupper[[k]]$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		}
		
		FracAbove6MONvolsacupper$FracAbovevol[[z]] <- mean(sacupper[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
		FracAbove6MONvolsacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		FracAbove3MONvolsacupper$FracAbovevol[[z]] <- mean(sacupper[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
		FracAbove3MONvolsacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		FracAboveHYvolsacupper$FracAbovevol[[z]] <- mean(sacupper[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
		FracAboveHYvolsacupper$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONvolsacupper[[k]]$FracAbovevol[[z]] <- mean(sacupper[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
			FracAboveMONvolsacupper[[k]]$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
		}
		###############################################################
		
#		FracAbove6MONday$FracAboveday[[z]] <- gls(sacupper[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONday$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONday$FracAboveday[[z]] <- mean(sacupper[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONday$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYday$FracAboveday[[z]] <- mean(sacupper[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYday$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(sacupper[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONday[[k]]$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvol$FracAbovevol[[z]] <- mean(sacupper[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvol$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvol$FracAbovevol[[z]] <- mean(sacupper[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvol$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvol$FracAbovevol[[z]] <- mean(sacupper[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvol$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(sacupper[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvol[[k]]$gauge[[z]] <- sacupper[[z]]$raw$site_no[[1]]
#		}
		
		#############################
	}
	
	if(any(z==seq(5,200,10))){
		save.image()
	}
}	

save.image()

names(sacupper) <- sacupper_g


