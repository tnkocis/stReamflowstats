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




SJ_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Streamflow\\SanJoaquin\\TXT\\Y80_gauges_sj.txt")
SJ_g <- as.numeric(SJ_g$SITENO)


MKTMONdaySJ <- vector("list",6)
names(MKTMONdaySJ)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONdaySJ[[k]] <- data.frame(tau=rep(NA, length(SJ_g)), pvalue=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
}
MKT6MONdaySJ <- data.frame(tau=rep(NA, length(SJ_g)), pvalue=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
MKT3MONdaySJ <- data.frame(tau=rep(NA, length(SJ_g)), pvalue=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
MKTHYdaySJ <- data.frame(tau=rep(NA, length(SJ_g)), pvalue=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))

MKTMONvolSJ <- vector("list",6)
names(MKTMONvolSJ)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONvolSJ[[k]] <- data.frame(tau=rep(NA, length(SJ_g)), pvalue=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
}
MKT6MONvolSJ <- data.frame(tau=rep(NA, length(SJ_g)), pvalue=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
MKT3MONvolSJ <- data.frame(tau=rep(NA, length(SJ_g)), pvalue=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
MKTHYvolSJ <- data.frame(tau=rep(NA, length(SJ_g)), pvalue=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))




######################
FracAboveMONdaySJ <- vector("list",6)
names(FracAboveMONdaySJ)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONdaySJ[[k]] <- data.frame(FracAboveday=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
}
FracAbove6MONdaySJ <- data.frame(FracAboveday=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
FracAbove3MONdaySJ <- data.frame(FracAboveday=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
FracAboveHYdaySJ <- data.frame(FracAboveday=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))

FracAboveMONvolSJ <- vector("list",6)
names(FracAboveMONvolSJ)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONvolSJ[[k]] <- data.frame(FracAbovevol=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
}
FracAbove6MONvolSJ <- data.frame(FracAbovevol=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
FracAbove3MONvolSJ <- data.frame(FracAbovevol=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
FracAboveHYvolSJ <- data.frame(FracAbovevol=rep(NA, length(SJ_g)), gauge=rep(NA, length(SJ_g)))
################################


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
SJ_g <- SJ_g[which(SJ_g %in% txtgauges)]
#length(SJ_g)
#SJ <- vector("list", 5)

SJ <- vector("list", length(SJ_g))
for(z in 1:length(SJ_g)){
#	SJ <- list()
	SJ[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",SJ_g[[z]],".csv",sep=""), header=TRUE)
#	SJ$raw2$site_no <- as.numeric(SJ$raw2$site_no)
#	SJ$raw <- readNWISdv(SJ_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	SJ[[z]]$raw$Date <- as.Date(SJ[[z]]$raw$Date, "%Y-%m-%d")
	
	SJ[[z]]$raw <- RemoveLeapDays(SJ[[z]]$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(SJ[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		SJ[[z]]$Index$Valley <- "SacV"
		SJ[[z]]$Index$Index <- yeartype_old$SVI
		SJ[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(SJ[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		SJ[[z]]$Index$Valley <- "SJV"
		SJ[[z]]$Index$Index <- yeartype_old$SJV
		SJ[[z]]$Index$Year <- yeartype_old$Year
	} else {
		SJ[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",SJ[[z]]$raw$site_no[[1]]))
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
	SJ[[z]]$prep <- prepdata(SJ[[z]]$raw)
	SJ[[z]]$Availability <- DataAvailability(SJ[[z]]$raw)
	SJ[[z]]$thresholds_maf <- thresholds(SJ[[z]]$prep)
	if(all(SJ[[z]]$thresholds_maf==0)){
	} else {
#	SJ$record_stats <- record_stats(SJ$prep, SJ$thresholds_maf)
		SJ[[z]]$Winter_3mon <- Split3Winter(SJ[[z]]$prep, SJ[[z]]$Index, SJ[[z]]$thresholds_maf)
		SJ[[z]]$Winter_6mon <- Split6Winter(SJ[[z]]$prep, SJ[[z]]$Index, SJ[[z]]$thresholds_maf)
		SJ[[z]]$Winter_monthly <- SplitWinterMonthly(SJ[[z]]$prep, SJ[[z]]$Index, SJ[[z]]$thresholds_maf)
		SJ[[z]]$HydroYear <- SplitHydroYear(SJ[[z]]$prep, SJ[[z]]$Index, SJ[[z]]$thresholds_maf)	
		SJ[[z]]$HydroYear <- cleanupHY(SJ[[z]]$HydroYear)
		SJ[[z]]$Winter_6mon <- cleanup6MON(SJ[[z]]$Winter_6mon)
		SJ[[z]]$Winter_3mon <- cleanup3MON(SJ[[z]]$Winter_3mon)
		
		
		#rewrite below to write to a single output df
		SJ[[z]]$ThresholdFit6MON <- ThresholdFit(SJ[[z]]$Winter_6mon, 0.9)
		SJ[[z]]$ThresholdFit3MON <- ThresholdFit(SJ[[z]]$Winter_3mon, 0.9)
		SJ[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(SJ[[z]]$Winter_monthly, 0.9)
		SJ[[z]]$ThresholdFitHY <- ThresholdFit(SJ[[z]]$HydroYear, 0.9)
		
		
		SJ[[z]]$MKT6MON <- MKT(SJ[[z]]$ThresholdFit6MON)
		SJ[[z]]$MKT3MON <- MKT(SJ[[z]]$ThresholdFit3MON)
		SJ[[z]]$MKTMON  <- MKT(SJ[[z]]$ThresholdFitMON)
		SJ[[z]]$MKTHY <- MKT(SJ[[z]]$ThresholdFitHY)
		
		MKT6MONdaySJ$tau[[z]] <- SJ[[z]]$MKT6MON$MKTday[[1]][[1]]
		MKT6MONdaySJ$pvalue[[z]] <- SJ[[z]]$MKT6MON$MKTday[[2]][[1]]
		MKT6MONdaySJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		MKT3MONdaySJ$tau[[z]] <- SJ[[z]]$MKT3MON$MKTday[[1]][[1]]
		MKT3MONdaySJ$pvalue[[z]] <- SJ[[z]]$MKT3MON$MKTday[[2]][[1]]
		MKT3MONdaySJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		MKTHYdaySJ$tau[[z]] <- SJ[[z]]$MKTHY$MKTday[[1]][[1]]
		MKTHYdaySJ$pvalue[[z]] <- SJ[[z]]$MKTHY$MKTday[[2]][[1]]
		MKTHYdaySJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONdaySJ[[k]]$tau[[z]] <- SJ[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
			MKTMONdaySJ[[k]]$pvalue[[z]] <- SJ[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
			MKTMONdaySJ[[k]]$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		}
		
		MKT6MONvolSJ$tau[[z]] <- SJ[[z]]$MKT6MON$MKTvol[[1]][[1]]
		MKT6MONvolSJ$pvalue[[z]] <- SJ[[z]]$MKT6MON$MKTvol[[2]][[1]]
		MKT6MONvolSJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		MKT3MONvolSJ$tau[[z]] <- SJ[[z]]$MKT3MON$MKTvol[[1]][[1]]
		MKT3MONvolSJ$pvalue[[z]] <- SJ[[z]]$MKT3MON$MKTvol[[2]][[1]]
		MKT3MONvolSJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		MKTHYvolSJ$tau[[z]] <- SJ[[z]]$MKTHY$MKTvol[[1]][[1]]
		MKTHYvolSJ$pvalue[[z]] <- SJ[[z]]$MKTHY$MKTvol[[2]][[1]]
		MKTHYvolSJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONvolSJ[[k]]$tau[[z]] <- SJ[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
			MKTMONvolSJ[[k]]$pvalue[[z]] <- SJ[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
			MKTMONvolSJ[[k]]$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		}
		
		
		##########################
		SJ[[z]]$FracAbove6MON <- FracAboveExtract(SJ[[z]]$ThresholdFit6MON)
		SJ[[z]]$FracAbove3MON <- FracAboveExtract(SJ[[z]]$ThresholdFit3MON)
		SJ[[z]]$FracAboveMON  <- FracAboveExtract(SJ[[z]]$ThresholdFitMON)
		SJ[[z]]$FracAboveHY <- FracAboveExtract(SJ[[z]]$ThresholdFitHY)
		
		FracAbove6MONdaySJ$FracAboveday[[z]] <- mean(SJ[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
		FracAbove6MONdaySJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		FracAbove3MONdaySJ$FracAboveday[[z]] <- mean(SJ[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
		FracAbove3MONdaySJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		FracAboveHYdaySJ$FracAboveday[[z]] <- mean(SJ[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
		FracAboveHYdaySJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONdaySJ[[k]]$FracAboveday[[z]] <- mean(SJ[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
			FracAboveMONdaySJ[[k]]$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		}
		
		FracAbove6MONvolSJ$FracAbovevol[[z]] <- mean(SJ[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
		FracAbove6MONvolSJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		FracAbove3MONvolSJ$FracAbovevol[[z]] <- mean(SJ[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
		FracAbove3MONvolSJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		FracAboveHYvolSJ$FracAbovevol[[z]] <- mean(SJ[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
		FracAboveHYvolSJ$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONvolSJ[[k]]$FracAbovevol[[z]] <- mean(SJ[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
			FracAboveMONvolSJ[[k]]$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
		}
		###############################################################
		
#		FracAbove6MONday$FracAboveday[[z]] <- gls(SJ[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONday$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONday$FracAboveday[[z]] <- mean(SJ[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONday$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYday$FracAboveday[[z]] <- mean(SJ[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYday$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(SJ[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONday[[k]]$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvol$FracAbovevol[[z]] <- mean(SJ[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvol$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvol$FracAbovevol[[z]] <- mean(SJ[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvol$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvol$FracAbovevol[[z]] <- mean(SJ[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvol$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(SJ[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvol[[k]]$gauge[[z]] <- SJ[[z]]$raw$site_no[[1]]
#		}
		
		#############################
	}
	
	if(any(z==seq(5,200,10))){
		save.image()
	}
}	

save.image()

names(SJ) <- SJ_g




