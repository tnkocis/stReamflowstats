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




saclower_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Streamflow\\Sacramento\\saclower\\TXT\\Y80_gauges_sacl.txt")
saclower_g <- as.numeric(saclower_g$SITENO)


MKTMONday <- vector("list",6)
names(MKTMONday)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONday[[k]] <- data.frame(tau=rep(NA, length(saclower_g)), pvalue=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
}
MKT6MONday <- data.frame(tau=rep(NA, length(saclower_g)), pvalue=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
MKT3MONday <- data.frame(tau=rep(NA, length(saclower_g)), pvalue=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
MKTHYday <- data.frame(tau=rep(NA, length(saclower_g)), pvalue=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))

MKTMONvol <- vector("list",6)
names(MKTMONvol)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONvol[[k]] <- data.frame(tau=rep(NA, length(saclower_g)), pvalue=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
}
MKT6MONvol <- data.frame(tau=rep(NA, length(saclower_g)), pvalue=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
MKT3MONvol <- data.frame(tau=rep(NA, length(saclower_g)), pvalue=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
MKTHYvol <- data.frame(tau=rep(NA, length(saclower_g)), pvalue=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))




######################
FracAboveMONday <- vector("list",6)
names(FracAboveMONday)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONday[[k]] <- data.frame(FracAboveday=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
}
FracAbove6MONday <- data.frame(FracAboveday=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
FracAbove3MONday <- data.frame(FracAboveday=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
FracAboveHYday <- data.frame(FracAboveday=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))

FracAboveMONvol <- vector("list",6)
names(FracAboveMONvol)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONvol[[k]] <- data.frame(FracAbovevol=rep(NA, length(saclower_g)), VolAbvMAF=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
}
FracAbove6MONvol <- data.frame(FracAbovevol=rep(NA, length(saclower_g)), VolAbvMAF=rep(NA, length(saclower_g)), gauge=rep(NA, length(saclower_g)))
FracAbove3MONvol <- data.frame(FracAbovevol=rep(NA, length(saclower_g)),  VolAbvMAF=rep(NA, length(saclower_g)),gauge=rep(NA, length(saclower_g)))
FracAboveHYvol <- data.frame(FracAbovevol=rep(NA, length(saclower_g)),  VolAbvMAF=rep(NA, length(saclower_g)),gauge=rep(NA, length(saclower_g)))
################################


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
saclower_g <- saclower_g[which(saclower_g %in% txtgauges)]
#length(saclower_g)
#saclower <- vector("list", 5)

saclower <- vector("list", length(saclower_g))
for(z in 1:length(saclower_g)){
#	saclower <- list()
	saclower[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",saclower_g[[z]],".csv",sep=""), header=TRUE)
#	saclower$raw2$site_no <- as.numeric(saclower$raw2$site_no)
#	saclower$raw <- readNWISdv(saclower_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	saclower[[z]]$raw$Date <- as.Date(saclower[[z]]$raw$Date, "%Y-%m-%d")
	
	saclower[[z]]$raw <- RemoveLeapDays(saclower[[z]]$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(saclower[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		saclower[[z]]$Index$Valley <- "SacV"
		saclower[[z]]$Index$Index <- yeartype_old$SVI
		saclower[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(saclower[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		saclower[[z]]$Index$Valley <- "SJV"
		saclower[[z]]$Index$Index <- yeartype_old$SJI
		saclower[[z]]$Index$Year <- yeartype_old$Year
	} else {
		saclower[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",saclower[[z]]$raw$site_no[[1]]))
	}
	
#	if(as.numeric(saclower$raw$site_no[[1]]) %in% SacV_gauges$site_no){
#		saclower$Index$Valley <- "SacV"
#		saclower$Index$Index <- YEARTYPEqdf$SacV_num
#		saclower$Index$Year <- YEARTYPEqdf$Year
#	} else if(as.numeric(saclower$raw$site_no[[1]]) %in% SJV_gauges$site_no){
#		saclower$Index$Valley <- "SJV"
#		saclower$Index$Index <- YEARTYPEqdf$SJV_num
#		saclower$Index$Year <- YEARTYPEqdf$Year
#	} else {
#		saclower$Index$Valley <- "ERROR"
#		print(paste("Error",saclower$raw$site_no[[1]]))
#	}
#	
	
	###DATA PROCESSING
	saclower[[z]]$prep <- prepdata(saclower[[z]]$raw)
	saclower[[z]]$Availability <- DataAvailability(saclower[[z]]$raw)
	saclower[[z]]$thresholds_maf <- thresholds(saclower[[z]]$prep)
	if(all(saclower[[z]]$thresholds_maf==0)){
	} else {
#	saclower$record_stats <- record_stats(saclower$prep, saclower$thresholds_maf)
		saclower[[z]]$Winter_3mon <- Split3Winter(saclower[[z]]$prep, saclower[[z]]$Index, saclower[[z]]$thresholds_maf)
		saclower[[z]]$Winter_6mon <- Split6Winter(saclower[[z]]$prep, saclower[[z]]$Index, saclower[[z]]$thresholds_maf)
		saclower[[z]]$Winter_monthly <- SplitWinterMonthly(saclower[[z]]$prep, saclower[[z]]$Index, saclower[[z]]$thresholds_maf)
		saclower[[z]]$HydroYear <- SplitHydroYear(saclower[[z]]$prep, saclower[[z]]$Index, saclower[[z]]$thresholds_maf)	
		saclower[[z]]$HydroYear <- cleanupHY(saclower[[z]]$HydroYear)
		saclower[[z]]$Winter_6mon <- cleanup6MON(saclower[[z]]$Winter_6mon)
		saclower[[z]]$Winter_3mon <- cleanup3MON(saclower[[z]]$Winter_3mon)
		
		
		#rewrite below to write to a single output df
		saclower[[z]]$ThresholdFit6MON <- ThresholdFit(saclower[[z]]$Winter_6mon, 0.9)
		saclower[[z]]$ThresholdFit3MON <- ThresholdFit(saclower[[z]]$Winter_3mon, 0.9)
		saclower[[z]]$ThresholdFitMON  <- ThresholdFitMonthly(saclower[[z]]$Winter_monthly, 0.9)
		saclower[[z]]$ThresholdFitHY <- ThresholdFit(saclower[[z]]$HydroYear, 0.9)
		
		
		saclower[[z]]$MKT6MON <- MKT(saclower[[z]]$ThresholdFit6MON)
		saclower[[z]]$MKT3MON <- MKT(saclower[[z]]$ThresholdFit3MON)
		saclower[[z]]$MKTMON  <- MKT(saclower[[z]]$ThresholdFitMON)
		saclower[[z]]$MKTHY <- MKT(saclower[[z]]$ThresholdFitHY)
		
		MKT6MONday$tau[[z]] <- saclower[[z]]$MKT6MON$MKTday[[1]][[1]]
		MKT6MONday$pvalue[[z]] <- saclower[[z]]$MKT6MON$MKTday[[2]][[1]]
		MKT6MONday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		MKT3MONday$tau[[z]] <- saclower[[z]]$MKT3MON$MKTday[[1]][[1]]
		MKT3MONday$pvalue[[z]] <- saclower[[z]]$MKT3MON$MKTday[[2]][[1]]
		MKT3MONday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		MKTHYday$tau[[z]] <- saclower[[z]]$MKTHY$MKTday[[1]][[1]]
		MKTHYday$pvalue[[z]] <- saclower[[z]]$MKTHY$MKTday[[2]][[1]]
		MKTHYday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONday[[k]]$tau[[z]] <- saclower[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
			MKTMONday[[k]]$pvalue[[z]] <- saclower[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
			MKTMONday[[k]]$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		}
		
		MKT6MONvol$tau[[z]] <- saclower[[z]]$MKT6MON$MKTvol[[1]][[1]]
		MKT6MONvol$pvalue[[z]] <- saclower[[z]]$MKT6MON$MKTvol[[2]][[1]]
		MKT6MONvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		MKT3MONvol$tau[[z]] <- saclower[[z]]$MKT3MON$MKTvol[[1]][[1]]
		MKT3MONvol$pvalue[[z]] <- saclower[[z]]$MKT3MON$MKTvol[[2]][[1]]
		MKT3MONvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		MKTHYvol$tau[[z]] <- saclower[[z]]$MKTHY$MKTvol[[1]][[1]]
		MKTHYvol$pvalue[[z]] <- saclower[[z]]$MKTHY$MKTvol[[2]][[1]]
		MKTHYvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONvol[[k]]$tau[[z]] <- saclower[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
			MKTMONvol[[k]]$pvalue[[z]] <- saclower[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
			MKTMONvol[[k]]$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		}
		
		
		##########################
		saclower[[z]]$FracAbove6MON <- FracAboveExtract(saclower[[z]]$ThresholdFit6MON)
		saclower[[z]]$FracAbove3MON <- FracAboveExtract(saclower[[z]]$ThresholdFit3MON)
		saclower[[z]]$FracAboveMON  <- FracAboveExtract(saclower[[z]]$ThresholdFitMON)
		saclower[[z]]$FracAboveHY <- FracAboveExtract(saclower[[z]]$ThresholdFitHY)
		
		FracAbove6MONday$FracAboveday[[z]] <- mean(saclower[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
		FracAbove6MONday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		FracAbove3MONday$FracAboveday[[z]] <- mean(saclower[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
		FracAbove3MONday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		FracAboveHYday$FracAboveday[[z]] <- mean(saclower[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
		FracAboveHYday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(saclower[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
			FracAboveMONday[[k]]$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		}
		
		FracAbove6MONvol$FracAbovevol[[z]] <- mean(saclower[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
		FracAbove6MONvol$VolAbvMAF[[z]] <- mean(saclower[[z]]$FracAbove6MON$VolAbvMAF, na.rm=TRUE)
		FracAbove6MONvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		FracAbove3MONvol$FracAbovevol[[z]] <- mean(saclower[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
		FracAbove3MONvol$VolAbvMAF[[z]] <- mean(saclower[[z]]$FracAbove3MON$VolAbvMAF, na.rm=TRUE)
		FracAbove3MONvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		FracAboveHYvol$FracAbovevol[[z]] <- mean(saclower[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
		FracAboveHYvol$VolAbvMAF[[z]] <- mean(saclower[[z]]$FracAboveHY$VolAbvMAF, na.rm=TRUE)
		FracAboveHYvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(saclower[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
			FracAboveMONvol[[k]]$VolAbvMAF[[z]] <- mean(saclower[[z]]$FracAboveMON[[k]]$VolAbvMAF, na.rm=TRUE)
			FracAboveMONvol[[k]]$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
		}
		###############################################################
		
#		FracAbove6MONday$FracAboveday[[z]] <- gls(saclower[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONday$FracAboveday[[z]] <- mean(saclower[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYday$FracAboveday[[z]] <- mean(saclower[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYday$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(saclower[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONday[[k]]$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvol$FracAbovevol[[z]] <- mean(saclower[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvol$FracAbovevol[[z]] <- mean(saclower[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvol$FracAbovevol[[z]] <- mean(saclower[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvol$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(saclower[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvol[[k]]$gauge[[z]] <- saclower[[z]]$raw$site_no[[1]]
#		}
		
		#############################
	}
	
	if(any(z==seq(5,200,10))){
		save.image()
	}
}	

save.image()

names(saclower) <- saclower_g

