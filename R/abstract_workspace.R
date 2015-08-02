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

txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
unimpaired_g <- unimpaired_g[which(unimpaired_g %in% txtgauges)]
#length(unimpaired_g)
#unimpaired <- vector("list", 5)
for(z in 1:length(unimpaired_g)){
	unimpaired <- list()
	unimpaired$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",unimpaired_g[[z]],".csv",sep=""), header=TRUE)
#	unimpaired$raw2$site_no <- as.numeric(unimpaired$raw2$site_no)
#	unimpaired$raw <- readNWISdv(unimpaired_g[[z]],"00060", startDate="1945-10-01",
#			endDate=Sys.Date(), statCd="00003")
	unimpaired$raw$Date <- as.Date(unimpaired$raw$Date, "%Y-%m-%d")
	
	unimpaired$raw <- RemoveLeapDays(unimpaired$raw)
	
	yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(unimpaired$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		unimpaired$Index$Valley <- "SacV"
		unimpaired$Index$Index <- yeartype_old$SVI
		unimpaired$Index$Year <- yeartype_old$Year
	} else if(as.numeric(unimpaired$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		unimpaired$Index$Valley <- "SJV"
		unimpaired$Index$Index <- yeartype_old$SJV
		unimpaired$Index$Year <- yeartype_old$Year
	} else {
		unimpaired$Index$Valley <- "ERROR"
		print(paste("Error",unimpaired$raw$site_no[[1]]))
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
	unimpaired$prep <- prepdata(unimpaired$raw)
	unimpaired$Availability <- DataAvailability(unimpaired$raw)
	unimpaired$thresholds_maf <- thresholds(unimpaired$prep)
	if(all(unimpaired$thresholds_maf==0)){
	} else {
#	unimpaired$record_stats <- record_stats(unimpaired$prep, unimpaired$thresholds_maf)
	unimpaired$Winter_3mon <- Split3Winter(unimpaired$prep, unimpaired$Index, unimpaired$thresholds_maf)
	unimpaired$Winter_6mon <- Split6Winter(unimpaired$prep, unimpaired$Index, unimpaired$thresholds_maf)
	unimpaired$Winter_monthly <- SplitWinterMonthly(unimpaired$prep, unimpaired$Index, unimpaired$thresholds_maf)
	unimpaired$HydroYear <- SplitHydroYear(unimpaired$prep, unimpaired$Index, unimpaired$thresholds_maf)	
	unimpaired$HydroYear <- cleanupHY(unimpaired$HydroYear)
	unimpaired$Winter_6mon <- cleanup6MON(unimpaired$Winter_6mon)
	unimpaired$Winter_3mon <- cleanup3MON(unimpaired$Winter_3mon)

	
	#rewrite below to write to a single output df
	unimpaired$ThresholdFit6MON <- ThresholdFit(unimpaired$Winter_6mon, 0.95)
	unimpaired$ThresholdFit3MON <- ThresholdFit(unimpaired$Winter_3mon, 0.95)
	unimpaired$ThresholdFitMON  <- ThresholdFitMonthly(unimpaired$Winter_monthly, 0.95)
	unimpaired$ThresholdFitHY <- ThresholdFit(unimpaired$HydroYear, 0.95)
	
	
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
	}
}	

#names(unimpaired) <- unimpaired_g[1:2]


