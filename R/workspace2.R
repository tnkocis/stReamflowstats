# TODO: Add comment
# 
# Author: tiffnk
###############################################################################
library(dplyr)
library(hydroTSM)
library(dataRetrieval)

SacV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_svi.txt")
SJV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_sji.txt")
unimpaired_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\unimpaired_gauges.csv")
unimpaired_g <- as.numeric(unimpaired_g$Unimpaired)

unimpaired <- vector("list", length=length(unimpaired_g))
names(unimpaired) <- unimpaired_g
for(i in 1:length(unimpaired_g)){
	unimpaired[[i]]$raw <- readNWISdv(unimpaired_g[[i]],"00060", startDate="1900-01-01",
			endDate=Sys.Date(), statCd="00003")
}



USGS11377100 <- list()
USGS11377100$raw  <- readNWISdv(11377100,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
USGS11377100$raw <- RemoveLeapDays(USGS11377100$raw)

if(as.numeric(USGS11377100$raw$site_no[[1]]) %in% SacV_gauges$site_no){
	USGS11377100$Index$Valley <- "SacV"
	USGS11377100$Index$Index <- YEARTYPEdf$SacV_Round
	USGS11377100$Index$Year <- YEARTYPEdf$Year
} else if(as.numeric(USGS11377100$raw$site_no[[1]]) %in% SJV_gauges$site_no){
	USGS11377100$Index$Valley <- "SJV"
	USGS11377100$Index$Index <- YEARTYPEdf$SJV_Round
	USGS11377100$Index$Year <- YEARTYPEdf$Year
} else {
	USGS11377100$Index$Valley <- "ERROR"
	print(paste("Error",USGS11377100$raw$site_no[[1]]))
}

###DATA PROCESSING
USGS11377100$prep <- prepdata(USGS11377100$raw)
USGS11377100$thresholds_maf <- thresholds(USGS11377100$prep)
USGS11377100$record_stats <- record_stats(USGS11377100$prep, USGS11377100$thresholds_maf)
USGS11377100$Availability <- DataAvailability(USGS11377100$prep)
USGS11377100$Winter_3mon <- Split3Winter(USGS11377100$prep, USGS11377100$Index, USGS11377100$thresholds_maf)

USGS11377100$Winter_6mon <- Split6Winter(USGS11377100$prep, USGS11377100$Index)
USGS11377100$Winter_monthly <- SplitWinterMonthly(USGS11377100$prep, USGS11377100$Index)

for(n in 1:length(USGS11377100$Winter_3mon$Data)){
	USGS11377100$Winter_3mon$Stats[[n]]$Thresholds$Totals$Thresholds_maf <- as.numeric(USGS11377100$Winter_3mon$Stats[[n]]$Thresholds$Quantiles_acfte6)
}
for(n in 3:8){
	USGS11377100$Winter_3mon[[n]]$Stats$Thresholds$Totals$Thresholds_maf <- as.numeric(USGS11377100$Winter_3mon[[n]]$Stats$Thresholds$Quantiles_acfte6)
}

for(n in 1:length(USGS11377100$Winter_6mon$Data)){
	USGS11377100$Winter_6mon$Stats[[n]]$Thresholds$Totals$Thresholds_maf <- as.numeric(USGS11377100$Winter_6mon$Stats[[n]]$Thresholds$Quantiles_acfte6)
}
for(n in 3:8){
	USGS11377100$Winter_6mon[[n]]$Stats$Thresholds$Totals$Thresholds_maf <- as.numeric(USGS11377100$Winter_6mon[[n]]$Stats$Thresholds$Quantiles_acfte6)
}

for(n in 1:length(USGS11377100$Winter_monthly$Data)){
	for(m in 1:6){
	USGS11377100$Winter_monthly$Stats[[n]][[m]]$Thresholds$Totals$Thresholds_maf <- as.numeric(USGS11377100$Winter_monthly$Stats[[n]][[m]]$Thresholds$Quantiles_acfte6)
	}
}
for(n in 3:8){
	for(m in 1:6){
	USGS11377100$Winter_monthly[[n]][[m]]$Stats$Thresholds$Totals$Thresholds_maf <- as.numeric(USGS11377100$Winter_monthly[[n]][[m]]$Stats$Thresholds$Quantiles_acfte6)
	}
}


### WRITE TO FILES #####
maindir <- "C:\\Users\\tiffn_000\\Documents\\workspaces\\output"
gaugedir <- as.character(USGS11377100$raw$site_no[[1]])
prepdatadir <- "Data_prep"
availdir <- "Availability"
availname <- c("Yearly","Monthly","Decadal")
threemondir <- "3_Month_Winter"
sixmondir <- "6_Month_Winter"
mondir <- "Monthly_Winter"
monthlywinter <- c("Nov","Dec","Jan","Feb","Mar","Apr")
sub1dir <- c("Yearly","All","C","D","BN","AN","W")
subdir2 <- c("Data","General_Stats","Threshold_Stats","Threshold_Coded")
dir.create(file.path(maindir, gaugedir))
dir.create(file.path(maindir, gaugedir, prepdatadir))
write.csv(USGS11377100$prep, file=file.path(maindir, gaugedir, prepdatadir, paste("Data_", as.character(USGS11377100$raw$site_no[[1]]),".csv", sep="")))

dir.create(file.path(maindir, gaugedir, "Index"))
write.csv(as.data.frame(USGS11377100$Index),
		file=file.path(maindir, gaugedir, "Index",paste(as.character(USGS11377100$raw$site_no[[1]]),"_","Index.csv",sep="")))

dir.create(file.path(maindir, gaugedir, availdir))
for(n in 1:3){
	write.csv(USGS11377100$Availability[[n]], file=file.path(maindir,gaugedir, availdir, paste(availname[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv", sep="")))
}

############# three month
dir.create(file.path(maindir, gaugedir, threemondir))
for(n in 1:length(sub1dir)){
	dir.create(file.path(maindir, gaugedir, threemondir,sub1dir[[n]]))
}

for(n in 1:length(USGS11377100$Winter_3mon$Data)){
	dir.create(file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(USGS11377100$Winter_3mon$Data)[[n]]))
	write.csv(USGS11377100$Winter_3mon$Data[[n]], 
			file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(USGS11377100$Winter_3mon$Data)[[n]], 
					paste("3monData_",names(USGS11377100$Winter_3mon$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(as.data.frame(USGS11377100$Winter_3mon$Stats[[n]]$Values), 
			file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(USGS11377100$Winter_3mon$Data)[[n]],
					paste("3monStats_",names(USGS11377100$Winter_3mon$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(USGS11377100$Winter_3mon$Stats[[n]]$Thresholds$Totals, 
			file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(USGS11377100$Winter_3mon$Data)[[n]], 
					paste("3monThresholds_",names(USGS11377100$Winter_3mon$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(as.data.frame(USGS11377100$Winter_3mon$Stats[[n]]$Thresholds$coded), 
			file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(USGS11377100$Winter_3mon$Data)[[n]],
					paste("3monThresholdCodes_",names(USGS11377100$Winter_3mon$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
}

for(n in 2:7){
	write.csv(USGS11377100$Winter_3mon[[n+1]]$Data, 
			file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(USGS11377100$Winter_monthly)[[n+1]],"_","data_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(as.data.frame(USGS11377100$Winter_3mon[[n+1]]$Stats[1:7]), 
			file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(USGS11377100$Winter_monthly)[[n+1]],"_","stats_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(USGS11377100$Winter_3mon[[n+1]]$Stats$Thresholds$Totals, 
			file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(USGS11377100$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(as.data.frame(USGS11377100$Winter_3mon[[n+1]]$Stats$Thresholds$coded), 
			file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(USGS11377100$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
}



########################## six month
dir.create(file.path(maindir, gaugedir, sixmondir))
for(n in 1:length(sub1dir)){
	dir.create(file.path(maindir, gaugedir, sixmondir,sub1dir[[n]]))
}

for(n in 1:length(USGS11377100$Winter_6mon$Data)){
	dir.create(file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(USGS11377100$Winter_6mon$Data)[[n]]))

	write.csv(USGS11377100$Winter_6mon$Data[[n]], 
			file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(USGS11377100$Winter_6mon$Data)[[n]],
					paste("6monData_",names(USGS11377100$Winter_6mon$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(as.data.frame(USGS11377100$Winter_6mon$Stats[[n]]$Values), 
			file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(USGS11377100$Winter_6mon$Data)[[n]],
					paste("6monStats_",names(USGS11377100$Winter_6mon$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(USGS11377100$Winter_6mon$Stats[[n]]$Thresholds$Totals, 
			file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(USGS11377100$Winter_6mon$Data)[[n]],
					paste("6monThresholds_",names(USGS11377100$Winter_6mon$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(as.data.frame(USGS11377100$Winter_6mon$Stats[[n]]$Thresholds$coded), 
			file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(USGS11377100$Winter_6mon$Data)[[n]],
					paste("6monThresholdCodes_",names(USGS11377100$Winter_6mon$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
}

for(n in 2:7){
	write.csv(USGS11377100$Winter_6mon[[n+1]]$Data, 
			file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(USGS11377100$Winter_monthly)[[n+1]],"_","data_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(as.data.frame(USGS11377100$Winter_6mon[[n+1]]$Stats[1:7]), 
			file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]], paste(names(USGS11377100$Winter_monthly)[[n+1]],"_","stats_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(USGS11377100$Winter_6mon[[n+1]]$Stats$Thresholds$Totals, 
			file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(USGS11377100$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	write.csv(as.data.frame(USGS11377100$Winter_6mon[[n+1]]$Stats$Thresholds$coded), 
			file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(USGS11377100$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
}


################### monthly
dir.create(file.path(maindir, gaugedir, mondir))
for(n in 1:length(sub1dir)){
	dir.create(file.path(maindir, gaugedir, mondir,sub1dir[[n]]))
	for(m in 1:6){
		dir.create(file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]]))
	}
}

for(n in 1:length(USGS11377100$Winter_monthly$Data)){
	dir.create(file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(USGS11377100$Winter_monthly$Data)[[n]]))
	for(m in 1:6){
		dir.create(file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(USGS11377100$Winter_monthly$Data)[[n]],monthlywinter[[m]]))
		write.csv(USGS11377100$Winter_monthly$Data[[n]][[m]], 
			file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(USGS11377100$Winter_monthly$Data)[[n]],monthlywinter[[m]],
					paste("Data_",monthlywinter[[m]],"_",names(USGS11377100$Winter_monthly$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	
		write.csv(as.data.frame(USGS11377100$Winter_monthly$Stats[[n]][[m]]$Values), 
			file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(USGS11377100$Winter_monthly$Data)[[n]],monthlywinter[[m]],
					paste("Stats_",monthlywinter[[m]],"_",names(USGS11377100$Winter_monthly$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	
		write.csv(USGS11377100$Winter_monthly$Stats[[n]][[m]]$Thresholds$Totals, 
			file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(USGS11377100$Winter_monthly$Data)[[n]],monthlywinter[[m]],
					paste("Thresholds_",monthlywinter[[m]],"_",names(USGS11377100$Winter_monthly$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	
		write.csv(as.data.frame(USGS11377100$Winter_monthly$Stats[[n]][[m]]$Thresholds$coded), 
			file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(USGS11377100$Winter_monthly$Data)[[n]],monthlywinter[[m]],
					paste("ThresholdCodes_",monthlywinter[[m]],"_",names(USGS11377100$Winter_monthly$Data)[[n]],"_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	}
}

for(n in 2:7){
	for(m in 1:6){
		write.csv(USGS11377100$Winter_monthly[[n+1]][[m]]$Data, 
			file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(USGS11377100$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","data_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(USGS11377100$Winter_monthly[[n+1]][[m]]$Stats[1:7]), 
			file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]], paste(names(USGS11377100$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","stats_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
		write.csv(USGS11377100$Winter_monthly[[n+1]][[m]]$Stats$Thresholds$Totals, 
			file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(USGS11377100$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","threshold_stats_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(USGS11377100$Winter_monthly[[n+1]][[m]]$Stats$Thresholds$coded), 
			file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(USGS11377100$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","threshold_coded_",as.character(USGS11377100$raw$site_no[[1]]),".csv",sep="")))
	}
}
