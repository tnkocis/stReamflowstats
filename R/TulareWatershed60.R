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




tulare60_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Streamflow\\Tulare\\TXT\\Y60_gauges_tulare.txt")
tulare60_g <- as.numeric(tulare60_g$SITENO)


MKTMONdaytulare60 <- vector("list",6)
names(MKTMONdaytulare60)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONdaytulare60[[k]] <- data.frame(tau=rep(NA, length(tulare60_g)), pvalue=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
}
MKT6MONdaytulare60 <- data.frame(tau=rep(NA, length(tulare60_g)), pvalue=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
MKT3MONdaytulare60 <- data.frame(tau=rep(NA, length(tulare60_g)), pvalue=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
MKTHYdaytulare60 <- data.frame(tau=rep(NA, length(tulare60_g)), pvalue=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))

MKTMONvoltulare60 <- vector("list",6)
names(MKTMONvoltulare60)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONvoltulare60[[k]] <- data.frame(tau=rep(NA, length(tulare60_g)), pvalue=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
}
MKT6MONvoltulare60 <- data.frame(tau=rep(NA, length(tulare60_g)), pvalue=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
MKT3MONvoltulare60 <- data.frame(tau=rep(NA, length(tulare60_g)), pvalue=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
MKTHYvoltulare60 <- data.frame(tau=rep(NA, length(tulare60_g)), pvalue=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))




######################
FracAboveMONdaytulare60 <- vector("list",6)
names(FracAboveMONdaytulare60)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONdaytulare60[[k]] <- data.frame(FracAboveday=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
}
FracAbove6MONdaytulare60 <- data.frame(FracAboveday=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
FracAbove3MONdaytulare60 <- data.frame(FracAboveday=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
FracAboveHYdaytulare60 <- data.frame(FracAboveday=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))

FracAboveMONvoltulare60 <- vector("list",6)
names(FracAboveMONvoltulare60)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONvoltulare60[[k]] <- data.frame(FracAbovevol=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
}
FracAbove6MONvoltulare60 <- data.frame(FracAbovevol=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
FracAbove3MONvoltulare60 <- data.frame(FracAbovevol=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
FracAboveHYvoltulare60 <- data.frame(FracAbovevol=rep(NA, length(tulare60_g)), gauge=rep(NA, length(tulare60_g)))
################################


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
tulare60_g <- tulare60_g[which(tulare60_g %in% txtgauges)]
#length(tulare60_g)
#tulare60 <- vector("list", 5)

tulare60 <- vector("list", length(tulare60_g))
for(z in 1:length(tulare60_g)){
#	tulare60 <- list()
	tulare60[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",tulare60_g[[z]],".csv",sep=""), header=TRUE)
#	tulare60$raw2$site_no <- as.numeric(tulare60$raw2$site_no)
#	tulare60$raw <- readNWISdv(tulare60_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	tulare60[[z]]$raw$Date <- as.Date(tulare60[[z]]$raw$Date, "%Y-%m-%d")
	
	tulare60[[z]]$raw <- RemoveLeapDays(tulare60[[z]]$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(tulare60[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		tulare60[[z]]$Index$Valley <- "SacV"
		tulare60[[z]]$Index$Index <- yeartype_old$SVI
		tulare60[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(tulare60[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		tulare60[[z]]$Index$Valley <- "SJV"
		tulare60[[z]]$Index$Index <- yeartype_old$SJV
		tulare60[[z]]$Index$Year <- yeartype_old$Year
	} else {
		tulare60[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",tulare60[[z]]$raw$site_no[[1]]))
	}
	
#	if(as.numeric(tulare60$raw$site_no[[1]]) %in% SacV_gauges$site_no){
#		tulare60$Index$Valley <- "SacV"
#		tulare60$Index$Index <- YEARTYPEqdf$SacV_num
#		tulare60$Index$Year <- YEARTYPEqdf$Year
#	} else if(as.numeric(tulare60$raw$site_no[[1]]) %in% SJV_gauges$site_no){
#		tulare60$Index$Valley <- "SJV"
#		tulare60$Index$Index <- YEARTYPEqdf$SJV_num
#		tulare60$Index$Year <- YEARTYPEqdf$Year
#	} else {
#		tulare60$Index$Valley <- "ERROR"
#		print(paste("Error",tulare60$raw$site_no[[1]]))
#	}
#	
	
	###DATA PROCESSING
	tulare60[[z]]$prep <- prepdata(tulare60[[z]]$raw)
	tulare60[[z]]$Availability <- DataAvailability(tulare60[[z]]$raw)
	tulare60[[z]]$thresholds_maf <- thresholds(tulare60[[z]]$prep)
	if(all(tulare60[[z]]$thresholds_maf==0)){
	} else {
#	tulare60$record_stats <- record_stats(tulare60$prep, tulare60$thresholds_maf)
		tulare60[[z]]$Winter_3mon <- Split3Winter(tulare60[[z]]$prep, tulare60[[z]]$Index, tulare60[[z]]$thresholds_maf)
		tulare60[[z]]$Winter_6mon <- Split6Winter(tulare60[[z]]$prep, tulare60[[z]]$Index, tulare60[[z]]$thresholds_maf)
		tulare60[[z]]$Winter_monthly <- SplitWinterMonthly(tulare60[[z]]$prep, tulare60[[z]]$Index, tulare60[[z]]$thresholds_maf)
		tulare60[[z]]$HydroYear <- SplitHydroYear(tulare60[[z]]$prep, tulare60[[z]]$Index, tulare60[[z]]$thresholds_maf)	
		tulare60[[z]]$HydroYear <- cleanupHY(tulare60[[z]]$HydroYear)
		tulare60[[z]]$Winter_6mon <- cleanup6MON(tulare60[[z]]$Winter_6mon)
		tulare60[[z]]$Winter_3mon <- cleanup3MON(tulare60[[z]]$Winter_3mon)
		
		
		#rewrite below to write to a single output df
		tulare60[[z]]$ThresholdFit6MON <- ThresholdFit(tulare60[[z]]$Winter_6mon, 0.9)
		tulare60[[z]]$ThresholdFit3MON <- ThresholdFit(tulare60[[z]]$Winter_3mon, 0.9)
		tulare60[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(tulare60[[z]]$Winter_monthly, 0.9)
		tulare60[[z]]$ThresholdFitHY <- ThresholdFit(tulare60[[z]]$HydroYear, 0.9)
		
		
		tulare60[[z]]$MKT6MON <- MKT(tulare60[[z]]$ThresholdFit6MON)
		tulare60[[z]]$MKT3MON <- MKT(tulare60[[z]]$ThresholdFit3MON)
		tulare60[[z]]$MKTMON  <- MKT(tulare60[[z]]$ThresholdFitMON)
		tulare60[[z]]$MKTHY <- MKT(tulare60[[z]]$ThresholdFitHY)
		
		MKT6MONdaytulare60$tau[[z]] <- tulare60[[z]]$MKT6MON$MKTday[[1]][[1]]
		MKT6MONdaytulare60$pvalue[[z]] <- tulare60[[z]]$MKT6MON$MKTday[[2]][[1]]
		MKT6MONdaytulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		MKT3MONdaytulare60$tau[[z]] <- tulare60[[z]]$MKT3MON$MKTday[[1]][[1]]
		MKT3MONdaytulare60$pvalue[[z]] <- tulare60[[z]]$MKT3MON$MKTday[[2]][[1]]
		MKT3MONdaytulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		MKTHYdaytulare60$tau[[z]] <- tulare60[[z]]$MKTHY$MKTday[[1]][[1]]
		MKTHYdaytulare60$pvalue[[z]] <- tulare60[[z]]$MKTHY$MKTday[[2]][[1]]
		MKTHYdaytulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONdaytulare60[[k]]$tau[[z]] <- tulare60[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
			MKTMONdaytulare60[[k]]$pvalue[[z]] <- tulare60[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
			MKTMONdaytulare60[[k]]$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		}
		
		MKT6MONvoltulare60$tau[[z]] <- tulare60[[z]]$MKT6MON$MKTvol[[1]][[1]]
		MKT6MONvoltulare60$pvalue[[z]] <- tulare60[[z]]$MKT6MON$MKTvol[[2]][[1]]
		MKT6MONvoltulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		MKT3MONvoltulare60$tau[[z]] <- tulare60[[z]]$MKT3MON$MKTvol[[1]][[1]]
		MKT3MONvoltulare60$pvalue[[z]] <- tulare60[[z]]$MKT3MON$MKTvol[[2]][[1]]
		MKT3MONvoltulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		MKTHYvoltulare60$tau[[z]] <- tulare60[[z]]$MKTHY$MKTvol[[1]][[1]]
		MKTHYvoltulare60$pvalue[[z]] <- tulare60[[z]]$MKTHY$MKTvol[[2]][[1]]
		MKTHYvoltulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONvoltulare60[[k]]$tau[[z]] <- tulare60[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
			MKTMONvoltulare60[[k]]$pvalue[[z]] <- tulare60[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
			MKTMONvoltulare60[[k]]$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		}
		
		
		##########################
		tulare60[[z]]$FracAbove6MON <- FracAboveExtract(tulare60[[z]]$ThresholdFit6MON)
		tulare60[[z]]$FracAbove3MON <- FracAboveExtract(tulare60[[z]]$ThresholdFit3MON)
		tulare60[[z]]$FracAboveMON  <- FracAboveExtract(tulare60[[z]]$ThresholdFitMON)
		tulare60[[z]]$FracAboveHY <- FracAboveExtract(tulare60[[z]]$ThresholdFitHY)
		
		FracAbove6MONdaytulare60$FracAboveday[[z]] <- mean(tulare60[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
		FracAbove6MONdaytulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		FracAbove3MONdaytulare60$FracAboveday[[z]] <- mean(tulare60[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
		FracAbove3MONdaytulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		FracAboveHYdaytulare60$FracAboveday[[z]] <- mean(tulare60[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
		FracAboveHYdaytulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONdaytulare60[[k]]$FracAboveday[[z]] <- mean(tulare60[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
			FracAboveMONdaytulare60[[k]]$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		}
		
		FracAbove6MONvoltulare60$FracAbovevol[[z]] <- mean(tulare60[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
		FracAbove6MONvoltulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		FracAbove3MONvoltulare60$FracAbovevol[[z]] <- mean(tulare60[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
		FracAbove3MONvoltulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		FracAboveHYvoltulare60$FracAbovevol[[z]] <- mean(tulare60[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
		FracAboveHYvoltulare60$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONvoltulare60[[k]]$FracAbovevol[[z]] <- mean(tulare60[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
			FracAboveMONvoltulare60[[k]]$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
		}
		###############################################################
		
#		FracAbove6MONday$FracAboveday[[z]] <- gls(tulare60[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONday$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONday$FracAboveday[[z]] <- mean(tulare60[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONday$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYday$FracAboveday[[z]] <- mean(tulare60[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYday$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(tulare60[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONday[[k]]$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvol$FracAbovevol[[z]] <- mean(tulare60[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvol$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvol$FracAbovevol[[z]] <- mean(tulare60[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvol$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvol$FracAbovevol[[z]] <- mean(tulare60[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvol$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(tulare60[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvol[[k]]$gauge[[z]] <- tulare60[[z]]$raw$site_no[[1]]
#		}
		
		#############################
	}
	
	if(any(z==seq(5,200,5))){
		save.image()
	}
}	

save.image()

names(tulare60) <- tulare60_g



