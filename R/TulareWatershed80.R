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




tulare80_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Streamflow\\Tulare\\TXT\\Y80_gauges_tulare.txt")
tulare80_g <- as.numeric(tulare80_g$SITENO)


MKTMONdaytulare80 <- vector("list",6)
names(MKTMONdaytulare80)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONdaytulare80[[k]] <- data.frame(tau=rep(NA, length(tulare80_g)), pvalue=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
}
MKT6MONdaytulare80 <- data.frame(tau=rep(NA, length(tulare80_g)), pvalue=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
MKT3MONdaytulare80 <- data.frame(tau=rep(NA, length(tulare80_g)), pvalue=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
MKTHYdaytulare80 <- data.frame(tau=rep(NA, length(tulare80_g)), pvalue=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))

MKTMONvoltulare80 <- vector("list",6)
names(MKTMONvoltulare80)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONvoltulare80[[k]] <- data.frame(tau=rep(NA, length(tulare80_g)), pvalue=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
}
MKT6MONvoltulare80 <- data.frame(tau=rep(NA, length(tulare80_g)), pvalue=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
MKT3MONvoltulare80 <- data.frame(tau=rep(NA, length(tulare80_g)), pvalue=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
MKTHYvoltulare80 <- data.frame(tau=rep(NA, length(tulare80_g)), pvalue=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))




######################
FracAboveMONdaytulare80 <- vector("list",6)
names(FracAboveMONdaytulare80)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONdaytulare80[[k]] <- data.frame(FracAboveday=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
}
FracAbove6MONdaytulare80 <- data.frame(FracAboveday=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
FracAbove3MONdaytulare80 <- data.frame(FracAboveday=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
FracAboveHYdaytulare80 <- data.frame(FracAboveday=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))

FracAboveMONvoltulare80 <- vector("list",6)
names(FracAboveMONvoltulare80)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONvoltulare80[[k]] <- data.frame(FracAbovevol=rep(NA, length(tulare80_g)), VolAbvMAF=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
}
FracAbove6MONvoltulare80 <- data.frame(FracAbovevol=rep(NA, length(tulare80_g)), VolAbvMAF=rep(NA, length(tulare80_g)), gauge=rep(NA, length(tulare80_g)))
FracAbove3MONvoltulare80 <- data.frame(FracAbovevol=rep(NA, length(tulare80_g)),  VolAbvMAF=rep(NA, length(tulare80_g)),gauge=rep(NA, length(tulare80_g)))
FracAboveHYvoltulare80 <- data.frame(FracAbovevol=rep(NA, length(tulare80_g)),  VolAbvMAF=rep(NA, length(tulare80_g)),gauge=rep(NA, length(tulare80_g)))

################################


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
tulare80_g <- tulare80_g[which(tulare80_g %in% txtgauges)]
#length(tulare80_g)
#tulare80 <- vector("list", 5)

tulare80 <- vector("list", length(tulare80_g))
for(z in 1:length(tulare80_g)){
#	tulare80 <- list()
	tulare80[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",tulare80_g[[z]],".csv",sep=""), header=TRUE)
#	tulare80$raw2$site_no <- as.numeric(tulare80$raw2$site_no)
#	tulare80$raw <- readNWISdv(tulare80_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	tulare80[[z]]$raw$Date <- as.Date(tulare80[[z]]$raw$Date, "%Y-%m-%d")
	
	tulare80[[z]]$raw <- RemoveLeapDays(tulare80[[z]]$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(tulare80[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		tulare80[[z]]$Index$Valley <- "SacV"
		tulare80[[z]]$Index$Index <- yeartype_old$SVI
		tulare80[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(tulare80[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		tulare80[[z]]$Index$Valley <- "SJV"
		tulare80[[z]]$Index$Index <- yeartype_old$SJI
		tulare80[[z]]$Index$Year <- yeartype_old$Year
	} else {
		tulare80[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",tulare80[[z]]$raw$site_no[[1]]))
	}
	
#	if(as.numeric(tulare80$raw$site_no[[1]]) %in% SacV_gauges$site_no){
#		tulare80$Index$Valley <- "SacV"
#		tulare80$Index$Index <- YEARTYPEqdf$SacV_num
#		tulare80$Index$Year <- YEARTYPEqdf$Year
#	} else if(as.numeric(tulare80$raw$site_no[[1]]) %in% SJV_gauges$site_no){
#		tulare80$Index$Valley <- "SJV"
#		tulare80$Index$Index <- YEARTYPEqdf$SJV_num
#		tulare80$Index$Year <- YEARTYPEqdf$Year
#	} else {
#		tulare80$Index$Valley <- "ERROR"
#		print(paste("Error",tulare80$raw$site_no[[1]]))
#	}
#	
	
	###DATA PROCESSING
	tulare80[[z]]$prep <- prepdata(tulare80[[z]]$raw)
	tulare80[[z]]$Availability <- DataAvailability(tulare80[[z]]$raw)
	tulare80[[z]]$thresholds_maf <- thresholds(tulare80[[z]]$prep)
	if(all(tulare80[[z]]$thresholds_maf==0)){
	} else {
#	tulare80$record_stats <- record_stats(tulare80$prep, tulare80$thresholds_maf)
		tulare80[[z]]$Winter_3mon <- Split3Winter(tulare80[[z]]$prep, tulare80[[z]]$Index, tulare80[[z]]$thresholds_maf)
		tulare80[[z]]$Winter_6mon <- Split6Winter(tulare80[[z]]$prep, tulare80[[z]]$Index, tulare80[[z]]$thresholds_maf)
		tulare80[[z]]$Winter_monthly <- SplitWinterMonthly(tulare80[[z]]$prep, tulare80[[z]]$Index, tulare80[[z]]$thresholds_maf)
		tulare80[[z]]$HydroYear <- SplitHydroYear(tulare80[[z]]$prep, tulare80[[z]]$Index, tulare80[[z]]$thresholds_maf)	
		tulare80[[z]]$HydroYear <- cleanupHY(tulare80[[z]]$HydroYear)
		tulare80[[z]]$Winter_6mon <- cleanup6MON(tulare80[[z]]$Winter_6mon)
		tulare80[[z]]$Winter_3mon <- cleanup3MON(tulare80[[z]]$Winter_3mon)
		
		
		#rewrite below to write to a single output df
		tulare80[[z]]$ThresholdFit6MON <- ThresholdFit(tulare80[[z]]$Winter_6mon, 0.9)
		tulare80[[z]]$ThresholdFit3MON <- ThresholdFit(tulare80[[z]]$Winter_3mon, 0.9)
		tulare80[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(tulare80[[z]]$Winter_monthly, 0.9)
		tulare80[[z]]$ThresholdFitHY <- ThresholdFit(tulare80[[z]]$HydroYear, 0.9)
		
		
		tulare80[[z]]$MKT6MON <- MKT(tulare80[[z]]$ThresholdFit6MON)
		tulare80[[z]]$MKT3MON <- MKT(tulare80[[z]]$ThresholdFit3MON)
		tulare80[[z]]$MKTMON  <- MKT(tulare80[[z]]$ThresholdFitMON)
		tulare80[[z]]$MKTHY <- MKT(tulare80[[z]]$ThresholdFitHY)
		
		MKT6MONdaytulare80$tau[[z]] <- tulare80[[z]]$MKT6MON$MKTday[[1]][[1]]
		MKT6MONdaytulare80$pvalue[[z]] <- tulare80[[z]]$MKT6MON$MKTday[[2]][[1]]
		MKT6MONdaytulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		MKT3MONdaytulare80$tau[[z]] <- tulare80[[z]]$MKT3MON$MKTday[[1]][[1]]
		MKT3MONdaytulare80$pvalue[[z]] <- tulare80[[z]]$MKT3MON$MKTday[[2]][[1]]
		MKT3MONdaytulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		MKTHYdaytulare80$tau[[z]] <- tulare80[[z]]$MKTHY$MKTday[[1]][[1]]
		MKTHYdaytulare80$pvalue[[z]] <- tulare80[[z]]$MKTHY$MKTday[[2]][[1]]
		MKTHYdaytulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONdaytulare80[[k]]$tau[[z]] <- tulare80[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
			MKTMONdaytulare80[[k]]$pvalue[[z]] <- tulare80[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
			MKTMONdaytulare80[[k]]$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		}
		
		MKT6MONvoltulare80$tau[[z]] <- tulare80[[z]]$MKT6MON$MKTvol[[1]][[1]]
		MKT6MONvoltulare80$pvalue[[z]] <- tulare80[[z]]$MKT6MON$MKTvol[[2]][[1]]
		MKT6MONvoltulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		MKT3MONvoltulare80$tau[[z]] <- tulare80[[z]]$MKT3MON$MKTvol[[1]][[1]]
		MKT3MONvoltulare80$pvalue[[z]] <- tulare80[[z]]$MKT3MON$MKTvol[[2]][[1]]
		MKT3MONvoltulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		MKTHYvoltulare80$tau[[z]] <- tulare80[[z]]$MKTHY$MKTvol[[1]][[1]]
		MKTHYvoltulare80$pvalue[[z]] <- tulare80[[z]]$MKTHY$MKTvol[[2]][[1]]
		MKTHYvoltulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONvoltulare80[[k]]$tau[[z]] <- tulare80[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
			MKTMONvoltulare80[[k]]$pvalue[[z]] <- tulare80[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
			MKTMONvoltulare80[[k]]$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		}
		
		
		##########################
		tulare80[[z]]$FracAbove6MON <- FracAboveExtract(tulare80[[z]]$ThresholdFit6MON)
		tulare80[[z]]$FracAbove3MON <- FracAboveExtract(tulare80[[z]]$ThresholdFit3MON)
		tulare80[[z]]$FracAboveMON  <- FracAboveExtract(tulare80[[z]]$ThresholdFitMON)
		tulare80[[z]]$FracAboveHY <- FracAboveExtract(tulare80[[z]]$ThresholdFitHY)
		
		FracAbove6MONdaytulare80$FracAboveday[[z]] <- mean(tulare80[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
		FracAbove6MONdaytulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		FracAbove3MONdaytulare80$FracAboveday[[z]] <- mean(tulare80[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
		FracAbove3MONdaytulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		FracAboveHYdaytulare80$FracAboveday[[z]] <- mean(tulare80[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
		FracAboveHYdaytulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONdaytulare80[[k]]$FracAboveday[[z]] <- mean(tulare80[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
			FracAboveMONdaytulare80[[k]]$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		}
		
		FracAbove6MONvoltulare80$FracAbovevol[[z]] <- mean(tulare80[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
		FracAbove6MONvoltulare80$VolAbvMAF[[z]] <- mean(tulare80[[z]]$FracAbove6MON$VolAbvMAF, na.rm=TRUE)
		FracAbove6MONvoltulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		FracAbove3MONvoltulare80$FracAbovevol[[z]] <- mean(tulare80[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
		FracAbove3MONvoltulare80$VolAbvMAF[[z]] <- mean(tulare80[[z]]$FracAbove3MON$VolAbvMAF, na.rm=TRUE)
		FracAbove3MONvoltulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		FracAboveHYvoltulare80$FracAbovevol[[z]] <- mean(tulare80[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
		FracAboveHYvoltulare80$VolAbvMAF[[z]] <- mean(tulare80[[z]]$FracAboveHY$VolAbvMAF, na.rm=TRUE)
		FracAboveHYvoltulare80$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONvoltulare80[[k]]$FracAbovevol[[z]] <- mean(tulare80[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
			FracAboveMONvoltulare80[[k]]$VolAbvMAF[[z]] <- mean(tulare80[[z]]$FracAboveMON[[k]]$VolAbvMAF, na.rm=TRUE)
			FracAboveMONvoltulare80[[k]]$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
		}
		###############################################################
		
#		FracAbove6MONday$FracAboveday[[z]] <- gls(tulare80[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONday$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONday$FracAboveday[[z]] <- mean(tulare80[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONday$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYday$FracAboveday[[z]] <- mean(tulare80[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYday$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(tulare80[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONday[[k]]$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvol$FracAbovevol[[z]] <- mean(tulare80[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvol$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvol$FracAbovevol[[z]] <- mean(tulare80[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvol$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvol$FracAbovevol[[z]] <- mean(tulare80[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvol$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(tulare80[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvol[[k]]$gauge[[z]] <- tulare80[[z]]$raw$site_no[[1]]
#		}
		
		#############################
	}
	
	if(any(z==seq(5,200,10))){
		save.image()
	}
}	

save.image()

names(tulare80) <- tulare80_g


for(i in 1:length(tulare80)){
	tulare80[[i]]$Winter_monthly <- cleanupMON(tulare80[[i]]$Winter_monthly)
}

