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




unimpaired_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Streamflow\\Unimpaired_70.txt")
unimpaired_g <- as.numeric(unimpaired_g$site_no)


MKTMONdayunimpaired <- vector("list",6)
names(MKTMONdayunimpaired)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONdayunimpaired[[k]] <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
MKT6MONdayunimpaired <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
MKT3MONdayunimpaired <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
MKTHYdayunimpaired <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))

MKTMONvolunimpaired <- vector("list",6)
names(MKTMONvolunimpaired)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	MKTMONvolunimpaired[[k]] <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
MKT6MONvolunimpaired <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
MKT3MONvolunimpaired <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
MKTHYvolunimpaired <- data.frame(tau=rep(NA, length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))




######################
FracAboveMONdayunimpaired <- vector("list",6)
names(FracAboveMONdayunimpaired)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONdayunimpaired[[k]] <- data.frame(FracAboveday=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
FracAbove6MONdayunimpaired <- data.frame(FracAboveday=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
FracAbove3MONdayunimpaired <- data.frame(FracAboveday=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
FracAboveHYdayunimpaired <- data.frame(FracAboveday=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))

FracAboveMONvolunimpaired <- vector("list",6)
names(FracAboveMONvolunimpaired)<- c("NOV","DEC","JAN","FEB","MAR","APR")
for(k in 1:6){
	FracAboveMONvolunimpaired[[k]] <- data.frame(FracAbovevol=rep(NA, length(unimpaired_g)), VolAbvMAF=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
FracAbove6MONvolunimpaired <- data.frame(FracAbovevol=rep(NA, length(unimpaired_g)), VolAbvMAF=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
FracAbove3MONvolunimpaired <- data.frame(FracAbovevol=rep(NA, length(unimpaired_g)),  VolAbvMAF=rep(NA, length(unimpaired_g)),gauge=rep(NA, length(unimpaired_g)))
FracAboveHYvolunimpaired <- data.frame(FracAbovevol=rep(NA, length(unimpaired_g)),  VolAbvMAF=rep(NA, length(unimpaired_g)),gauge=rep(NA, length(unimpaired_g)))

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
		unimpaired[[z]]$Index$Index <- yeartype_old$SJI
		unimpaired[[z]]$Index$Year <- yeartype_old$Year
	} else {
		unimpaired[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",unimpaired[[z]]$raw$site_no[[1]]))
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
		
		
		unimpaired[[z]]$MKT6MON <- MKT(unimpaired[[z]]$ThresholdFit6MON)
		unimpaired[[z]]$MKT3MON <- MKT(unimpaired[[z]]$ThresholdFit3MON)
		unimpaired[[z]]$MKTMON  <- MKT(unimpaired[[z]]$ThresholdFitMON)
		unimpaired[[z]]$MKTHY <- MKT(unimpaired[[z]]$ThresholdFitHY)
		
		MKT6MONdayunimpaired$tau[[z]] <- unimpaired[[z]]$MKT6MON$MKTday[[1]][[1]]
		MKT6MONdayunimpaired$pvalue[[z]] <- unimpaired[[z]]$MKT6MON$MKTday[[2]][[1]]
		MKT6MONdayunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		MKT3MONdayunimpaired$tau[[z]] <- unimpaired[[z]]$MKT3MON$MKTday[[1]][[1]]
		MKT3MONdayunimpaired$pvalue[[z]] <- unimpaired[[z]]$MKT3MON$MKTday[[2]][[1]]
		MKT3MONdayunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		MKTHYdayunimpaired$tau[[z]] <- unimpaired[[z]]$MKTHY$MKTday[[1]][[1]]
		MKTHYdayunimpaired$pvalue[[z]] <- unimpaired[[z]]$MKTHY$MKTday[[2]][[1]]
		MKTHYdayunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONdayunimpaired[[k]]$tau[[z]] <- unimpaired[[z]]$MKTMON[[k]]$MKTday[[1]][[1]]
			MKTMONdayunimpaired[[k]]$pvalue[[z]] <- unimpaired[[z]]$MKTMON[[k]]$MKTday[[2]][[1]]
			MKTMONdayunimpaired[[k]]$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		}
		
		MKT6MONvolunimpaired$tau[[z]] <- unimpaired[[z]]$MKT6MON$MKTvol[[1]][[1]]
		MKT6MONvolunimpaired$pvalue[[z]] <- unimpaired[[z]]$MKT6MON$MKTvol[[2]][[1]]
		MKT6MONvolunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		MKT3MONvolunimpaired$tau[[z]] <- unimpaired[[z]]$MKT3MON$MKTvol[[1]][[1]]
		MKT3MONvolunimpaired$pvalue[[z]] <- unimpaired[[z]]$MKT3MON$MKTvol[[2]][[1]]
		MKT3MONvolunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		MKTHYvolunimpaired$tau[[z]] <- unimpaired[[z]]$MKTHY$MKTvol[[1]][[1]]
		MKTHYvolunimpaired$pvalue[[z]] <- unimpaired[[z]]$MKTHY$MKTvol[[2]][[1]]
		MKTHYvolunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			MKTMONvolunimpaired[[k]]$tau[[z]] <- unimpaired[[z]]$MKTMON[[k]]$MKTvol[[1]][[1]]
			MKTMONvolunimpaired[[k]]$pvalue[[z]] <- unimpaired[[z]]$MKTMON[[k]]$MKTvol[[2]][[1]]
			MKTMONvolunimpaired[[k]]$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		}
		
		
		##########################
		unimpaired[[z]]$FracAbove6MON <- FracAboveExtract(unimpaired[[z]]$ThresholdFit6MON)
		unimpaired[[z]]$FracAbove3MON <- FracAboveExtract(unimpaired[[z]]$ThresholdFit3MON)
		unimpaired[[z]]$FracAboveMON  <- FracAboveExtract(unimpaired[[z]]$ThresholdFitMON)
		unimpaired[[z]]$FracAboveHY <- FracAboveExtract(unimpaired[[z]]$ThresholdFitHY)
		
		FracAbove6MONdayunimpaired$FracAboveday[[z]] <- mean(unimpaired[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
		FracAbove6MONdayunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		FracAbove3MONdayunimpaired$FracAboveday[[z]] <- mean(unimpaired[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
		FracAbove3MONdayunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		FracAboveHYdayunimpaired$FracAboveday[[z]] <- mean(unimpaired[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
		FracAboveHYdayunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONdayunimpaired[[k]]$FracAboveday[[z]] <- mean(unimpaired[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
			FracAboveMONdayunimpaired[[k]]$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		}
		
		FracAbove6MONvolunimpaired$FracAbovevol[[z]] <- mean(unimpaired[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
		FracAbove6MONvolunimpaired$VolAbvMAF[[z]] <- mean(unimpaired[[z]]$FracAbove6MON$VolAbvMAF, na.rm=TRUE)
		FracAbove6MONvolunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		FracAbove3MONvolunimpaired$FracAbovevol[[z]] <- mean(unimpaired[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
		FracAbove3MONvolunimpaired$VolAbvMAF[[z]] <- mean(unimpaired[[z]]$FracAbove3MON$VolAbvMAF, na.rm=TRUE)
		FracAbove3MONvolunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		FracAboveHYvolunimpaired$FracAbovevol[[z]] <- mean(unimpaired[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
		FracAboveHYvolunimpaired$VolAbvMAF[[z]] <- mean(unimpaired[[z]]$FracAboveHY$VolAbvMAF, na.rm=TRUE)
		FracAboveHYvolunimpaired$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		
		for(k in 1:6){
			FracAboveMONvolunimpaired[[k]]$FracAbovevol[[z]] <- mean(unimpaired[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
			FracAboveMONvolunimpaired[[k]]$VolAbvMAF[[z]] <- mean(unimpaired[[z]]$FracAboveMON[[k]]$VolAbvMAF, na.rm=TRUE)
			FracAboveMONvolunimpaired[[k]]$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		}
		###############################################################
		
#		FracAbove6MONday$FracAboveday[[z]] <- gls(unimpaired[[z]]$FracAbove6MON$FracAboveday, na.rm=TRUE)
#		FracAbove6MONday$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONday$FracAboveday[[z]] <- mean(unimpaired[[z]]$FracAbove3MON$FracAboveday, na.rm=TRUE)
#		FracAbove3MONday$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYday$FracAboveday[[z]] <- mean(unimpaired[[z]]$FracAboveHY$FracAboveday, na.rm=TRUE)
#		FracAboveHYday$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONday[[k]]$FracAboveday[[z]] <- mean(unimpaired[[z]]$FracAboveMON[[k]]$FracAboveday, na.rm=TRUE)
#			FracAboveMONday[[k]]$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
#		}
#		
#		FracAbove6MONvol$FracAbovevol[[z]] <- mean(unimpaired[[z]]$FracAbove6MON$FracAbovevol, na.rm=TRUE)
#		FracAbove6MONvol$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
#		
#		FracAbove3MONvol$FracAbovevol[[z]] <- mean(unimpaired[[z]]$FracAbove3MON$FracAbovevol, na.rm=TRUE)
#		FracAbove3MONvol$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
#		
#		FracAboveHYvol$FracAbovevol[[z]] <- mean(unimpaired[[z]]$FracAboveHY$FracAbovevol, na.rm=TRUE)
#		FracAboveHYvol$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
#		
#		for(k in 1:6){
#			FracAboveMONvol[[k]]$FracAbovevol[[z]] <- mean(unimpaired[[z]]$FracAboveMON[[k]]$FracAbovevol, na.rm=TRUE)
#			FracAboveMONvol[[k]]$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
#		}
		
		#############################
	}
	
	if(any(z==seq(5,200,10))){
		save.image()
	}
}	

save.image()

names(unimpaired) <- unimpaired_g





