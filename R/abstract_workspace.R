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




unimpaired_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\unimpaired_70.csv")
unimpaired_g <- as.numeric(unimpaired_g$Unimpaired)


MKTMONday <- vector("list",6)
names(MKTMONday)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONday[[k]] <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
MKT6MONday <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
MKT3MONday <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
MKTHYday <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))

MKTMONvol <- vector("list",6)
names(MKTMONvol)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONvol[[k]] <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
MKT6MONvol <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
MKT3MONvol <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
MKTHYvol <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))




######################
FracAboveMONday <- vector("list",6)
names(FracAboveMONday)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONday[[k]] <- data.frame(FracAboveday=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
FracAbove6MONday <- data.frame(FracAboveday=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
FracAbove3MONday <- data.frame(FracAboveday=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
FracAboveHYday <- data.frame(FracAboveday=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))

FracAboveMONvol <- vector("list",6)
names(FracAboveMONvol)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONvol[[k]] <- data.frame(FracAbovevol=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
FracAbove6MONvol <- data.frame(FracAbovevol=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
FracAbove3MONvol <- data.frame(FracAbovevol=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
FracAboveHYvol <- data.frame(FracAbovevol=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
################################


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
unimpaired_g <- unimpaired_g[which(unimpaired_g %in% txtgauges)]
#length(unimpaired_g)
#unimpaired <- vector("list", 5)

unimpaired <- vector("list", length(unimpaired_g))
for(z in 1:length(unimpaired_g)){
#	unimpaired <- list()
	unimpaired[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",unimpaired_g[[z]],".csv",sep=""), header=TRUE)
#	unimpaired$raw2$site_no <- as.numeric(unimpaired$raw2$site_no)
#	unimpaired$raw <- readNWISdv(unimpaired_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	unimpaired[[z]]$raw$Date <- as.Date(unimpaired[[z]]$raw$Date, "%Y-%m-%d")
	
	unimpaired[[z]]$raw <- RemoveLeapDays(unimpaired[[z]]$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(unimpaired[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		unimpaired[[z]]$Index$Valley <- "SacV"
		unimpaired[[z]]$Index$Index <- yeartype_old$SVI
		unimpaired[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(unimpaired[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		unimpaired[[z]]$Index$Valley <- "SJV"
		unimpaired[[z]]$Index$Index <- yeartype_old$SJV
		unimpaired[[z]]$Index$Year <- yeartype_old$Year
	} else {
		unimpaired[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",unimpaired[[z]]$raw$site_no[[1]]))
	}
	
#	if(as.numeric(unimpaired$raw$site_no[[1]]) %in% SacV_gauges$site_no){
#		unimpaired$Index$Valley <- "SacV"
#		unimpaired$Index$Index <- YEARTYPEqdf$SacV_num
#		unimpaired$Index$Year <- YEARTYPEqdf$Year
#	} else if(as.numeric(unimpaired$raw$site_no[[1]]) %in% SJV_gauges$site_no){
#		unimpaired$Index$Valley <- "SJV"
#		unimpaired$Index$Index <- YEARTYPEqdf$SJV_num
#		unimpaired$Index$Year <- YEARTYPEqdf$Year
#	} else {
#		unimpaired$Index$Valley <- "ERROR"
#		print(paste("Error",unimpaired$raw$site_no[[1]]))
#	}
#	
	
	###DATA PROCESSING
	unimpaired[[z]]$prep <- prepdata(unimpaired[[z]]$raw)
	unimpaired[[z]]$Availability <- DataAvailability(unimpaired[[z]]$raw)
	unimpaired[[z]]$thresholds_maf <- thresholds(unimpaired[[z]]$prep)
	if(all(unimpaired[[z]]$thresholds_maf==0)){
	} else {
#	unimpaired$record_stats <- record_stats(unimpaired$prep, unimpaired$thresholds_maf)
	unimpaired[[z]]$Winter_3mon <- Split3Winter(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
	unimpaired[[z]]$Winter_6mon <- Split6Winter(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
	unimpaired[[z]]$Winter_monthly <- SplitWinterMonthly(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
	unimpaired[[z]]$HydroYear <- SplitHydroYear(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)	
	unimpaired[[z]]$HydroYear <- cleanupHY(unimpaired[[z]]$HydroYear)
	unimpaired[[z]]$Winter_6mon <- cleanup6MON(unimpaired[[z]]$Winter_6mon)
	unimpaired[[z]]$Winter_3mon <- cleanup3MON(unimpaired[[z]]$Winter_3mon)

	
	#rewrite below to write to a single output df
	unimpaired[[z]]$ThresholdFit6MON <- ThresholdFit(unimpaired[[z]]$Winter_6mon, 0.9)
	unimpaired[[z]]$ThresholdFit3MON <- ThresholdFit(unimpaired[[z]]$Winter_3mon, 0.9)
	unimpaired[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(unimpaired[[z]]$Winter_monthly, 0.9)
	unimpaired[[z]]$ThresholdFitHY <- ThresholdFit(unimpaired[[z]]$HydroYear, 0.9)
	
	
	unimpaired$MKT6MON <- MKT(unimpaired$ThresholdFit6MON)
	unimpaired$MKT3MON <- MKT(unimpaired$ThresholdFit3MON)
	unimpaired$MKTMON  <- MKT(unimpaired$ThresholdFitMON)
	unimpaired$MKTHY <- MKT(unimpaired$ThresholdFitHY)
	
	MKT6MONday$tau[[z]] <- unimpaired$MKT6MON$MKTday[[1]][[1]]
	MKT6MONday$pvalue[[z]] <- unimpaired$MKT6MON$MKTday[[2]][[1]]
	MKT6MONday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	MKT3MONday$tau[[z]] <- unimpaired$MKT3MON$MKTday[[1]][[1]]
	MKT3MONday$pvalue[[z]] <- unimpaired$MKT3MON$MKTday[[2]][[1]]
	MKT3MONday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	MKTHYday$tau[[z]] <- unimpaired$MKTHY$MKTday[[1]][[1]]
	MKTHYday$pvalue[[z]] <- unimpaired$MKTHY$MKTday[[2]][[1]]
	MKTHYday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	for(k in 1:6){
		MKTMONday[[k]]$tau[[z]] <- unimpaired$MKTMON[[k]]$MKTday[[1]][[1]]
		MKTMONday[[k]]$pvalue[[z]] <- unimpaired$MKTMON[[k]]$MKTday[[2]][[1]]
		MKTMONday[[k]]$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	}
	
	MKT6MONvol$tau[[z]] <- unimpaired$MKT6MON$MKTvol[[1]][[1]]
	MKT6MONvol$pvalue[[z]] <- unimpaired$MKT6MON$MKTvol[[2]][[1]]
	MKT6MONvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	MKT3MONvol$tau[[z]] <- unimpaired$MKT3MON$MKTvol[[1]][[1]]
	MKT3MONvol$pvalue[[z]] <- unimpaired$MKT3MON$MKTvol[[2]][[1]]
	MKT3MONvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	MKTHYvol$tau[[z]] <- unimpaired$MKTHY$MKTvol[[1]][[1]]
	MKTHYvol$pvalue[[z]] <- unimpaired$MKTHY$MKTvol[[2]][[1]]
	MKTHYvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	for(k in 1:6){
		MKTMONvol[[k]]$tau[[z]] <- unimpaired$MKTMON[[k]]$MKTvol[[1]][[1]]
		MKTMONvol[[k]]$pvalue[[z]] <- unimpaired$MKTMON[[k]]$MKTvol[[2]][[1]]
		MKTMONvol[[k]]$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	}
	
	
	##########################
	unimpaired$FracAbove6MON <- FracAboveExtract(unimpaired$ThresholdFit6MON)
	unimpaired$FracAbove3MON <- FracAboveExtract(unimpaired$ThresholdFit3MON)
	unimpaired$FracAboveMON  <- FracAboveExtract(unimpaired$ThresholdFitMON)
	unimpaired$FracAboveHY <- FracAboveExtract(unimpaired$ThresholdFitHY)
	
	FracAbove6MONday$FracAboveday[[z]] <- mean(unimpaired$FracAbove6MON$FracAboveday, na.rm=TRUE)
	FracAbove6MONday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	FracAbove3MONday$FracAboveday[[z]] <- mean(unimpaired$FracAbove3MON$FracAboveday, na.rm=TRUE)
	FracAbove3MONday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	FracAboveHYday$FracAboveday[[z]] <- mean(unimpaired$FracAboveHY$FracAboveday, na.rm=TRUE)
	FracAboveHYday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	for(k in 1:6){
		FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(unimpaired$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
		FracAboveMONday[[k]]$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	}
	
	FracAbove6MONvol$FracAbovevol[[z]] <- mean(unimpaired$FracAbove6MON$FracAbovevol, na.rm=TRUE)
	FracAbove6MONvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	FracAbove3MONvol$FracAbovevol[[z]] <- mean(unimpaired$FracAbove3MON$FracAbovevol, na.rm=TRUE)
	FracAbove3MONvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	FracAboveHYvol$FracAbovevol[[z]] <- mean(unimpaired$FracAboveHY$FracAbovevol, na.rm=TRUE)
	FracAboveHYvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	for(k in 1:6){
		FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(unimpaired$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
		FracAboveMONvol[[k]]$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	}
	###############################################################
	
	FracAbove6MONday$FracAboveday[[z]] <- gls(unimpaired$FracAbove6MON$FracAboveday, na.rm=TRUE)
	FracAbove6MONday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	FracAbove3MONday$FracAboveday[[z]] <- mean(unimpaired$FracAbove3MON$FracAboveday, na.rm=TRUE)
	FracAbove3MONday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	FracAboveHYday$FracAboveday[[z]] <- mean(unimpaired$FracAboveHY$FracAboveday, na.rm=TRUE)
	FracAboveHYday$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	for(k in 1:6){
		FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(unimpaired$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
		FracAboveMONday[[k]]$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	}
	
	FracAbove6MONvol$FracAbovevol[[z]] <- mean(unimpaired$FracAbove6MON$FracAbovevol, na.rm=TRUE)
	FracAbove6MONvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	FracAbove3MONvol$FracAbovevol[[z]] <- mean(unimpaired$FracAbove3MON$FracAbovevol, na.rm=TRUE)
	FracAbove3MONvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	FracAboveHYvol$FracAbovevol[[z]] <- mean(unimpaired$FracAboveHY$FracAbovevol, na.rm=TRUE)
	FracAboveHYvol$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	
	for(k in 1:6){
		FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(unimpaired$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
		FracAboveMONvol[[k]]$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	}
	
	#############################
	}
	
	if(any(z==seq(10,200,10))){
		save.image()
	}
}	

save.image()








