# TODO: Add comment
# 
# Author: tiffn_000
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
	unimpaired[[i]]$raw <- RemoveLeapDays(unimpaired[[i]]$raw)
	
	if(as.numeric(unimpaired[[i]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		unimpaired[[i]]$Index$Valley <- "SacV"
		unimpaired[[i]]$Index$Index <- YEARTYPEdf$SacV_Round
		unimpaired[[i]]$Index$Year <- YEARTYPEdf$Year
	} else if(as.numeric(unimpaired[[i]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		unimpaired[[i]]$Index$Valley <- "SJV"
		unimpaired[[i]]$Index$Index <- YEARTYPEdf$SJV_Round
		unimpaired[[i]]$Index$Year <- YEARTYPEdf$Year
	} else {
		unimpaired[[i]]$Index$Valley <- "ERROR"
		print(paste("Error",unimpaired[[i]]$raw$site_no[[1]]))
	}
	
	###DATA PROCESSING
	unimpaired[[i]]$prep <- prepdata(unimpaired[[i]]$raw)
	unimpaired[[i]]$Availability <- DataAvailability(unimpaired[[i]]$prep)
	unimpaired[[i]]$Winter_6mon <- Split6Winter(unimpaired[[i]]$prep, unimpaired[[i]]$Index)
}	
	
	unimpaired[[i]]$Winter_3mon <- Split3Winter(unimpaired[[i]]$prep, unimpaired[[i]]$Index)
	unimpaired[[i]]$Winter_monthly <- SplitWinterMonthly(unimpaired[[i]]$prep, unimpaired[[i]]$Index)
	
	for(n in 1:length(unimpaired[[i]]$Winter_3mon$Data)){
		unimpaired[[i]]$Winter_3mon$Stats[[n]]$Thresholds$Totals$Thresholds_maf <- as.numeric(unimpaired[[i]]$Winter_3mon$Stats[[n]]$Thresholds$Quantiles_acfte6)
	}
	for(n in 3:8){
		unimpaired[[i]]$Winter_3mon[[n]]$Stats$Thresholds$Totals$Thresholds_maf <- as.numeric(unimpaired[[i]]$Winter_3mon[[n]]$Stats$Thresholds$Quantiles_acfte6)
	}
	
	for(n in 1:length(unimpaired[[i]]$Winter_6mon$Data)){
		unimpaired[[i]]$Winter_6mon$Stats[[n]]$Thresholds$Totals$Thresholds_maf <- as.numeric(unimpaired[[i]]$Winter_6mon$Stats[[n]]$Thresholds$Quantiles_acfte6)
	}
	for(n in 3:8){
		unimpaired[[i]]$Winter_6mon[[n]]$Stats$Thresholds$Totals$Thresholds_maf <- as.numeric(unimpaired[[i]]$Winter_6mon[[n]]$Stats$Thresholds$Quantiles_acfte6)
	}
	
	for(n in 1:length(unimpaired[[i]]$Winter_monthly$Data)){
		for(m in 1:6){
			unimpaired[[i]]$Winter_monthly$Stats[[n]][[m]]$Thresholds$Totals$Thresholds_maf <- as.numeric(unimpaired[[i]]$Winter_monthly$Stats[[n]][[m]]$Thresholds$Quantiles_acfte6)
		}
	}
	for(n in 3:8){
		for(m in 1:6){
			unimpaired[[i]]$Winter_monthly[[n]][[m]]$Stats$Thresholds$Totals$Thresholds_maf <- as.numeric(unimpaired[[i]]$Winter_monthly[[n]][[m]]$Stats$Thresholds$Quantiles_acfte6)
		}
	}
	
	
	### WRITE TO FILES #####
	maindir <- "C:\\Users\\tiffn_000\\Documents\\workspaces\\output"
	gaugedir <- as.character(unimpaired[[i]]$raw$site_no[[1]])
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
	write.csv(unimpaired[[i]]$prep, file=file.path(maindir, gaugedir, prepdatadir, paste("Data_", as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv", sep="")))
	
	dir.create(file.path(maindir, gaugedir, "Index"))
	write.csv(as.data.frame(unimpaired[[i]]$Index),
			file=file.path(maindir, gaugedir, "Index",paste(as.character(unimpaired[[i]]$raw$site_no[[1]]),"_","Index.csv",sep="")))
	
	dir.create(file.path(maindir, gaugedir, availdir))
	for(n in 1:3){
		write.csv(unimpaired[[i]]$Availability[[n]], file=file.path(maindir,gaugedir, availdir, paste(availname[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv", sep="")))
	}
	
	############# three month
	dir.create(file.path(maindir, gaugedir, threemondir))
	for(n in 1:length(sub1dir)){
		dir.create(file.path(maindir, gaugedir, threemondir,sub1dir[[n]]))
	}
	
	for(n in 1:length(unimpaired[[i]]$Winter_3mon$Data)){
		dir.create(file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_3mon$Data)[[n]]))
		write.csv(unimpaired[[i]]$Winter_3mon$Data[[n]], 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_3mon$Data)[[n]], 
						paste("3monData_",names(unimpaired[[i]]$Winter_3mon$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired[[i]]$Winter_3mon$Stats[[n]]$Values), 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_3mon$Data)[[n]],
						paste("3monStats_",names(unimpaired[[i]]$Winter_3mon$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(unimpaired[[i]]$Winter_3mon$Stats[[n]]$Thresholds$Totals, 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_3mon$Data)[[n]], 
						paste("3monThresholds_",names(unimpaired[[i]]$Winter_3mon$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired[[i]]$Winter_3mon$Stats[[n]]$Thresholds$coded), 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_3mon$Data)[[n]],
						paste("3monThresholdCodes_",names(unimpaired[[i]]$Winter_3mon$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
	}
	
	for(n in 2:7){
		write.csv(unimpaired[[i]]$Winter_3mon[[n+1]]$Data, 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_","data_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired[[i]]$Winter_3mon[[n+1]]$Stats[1:7]), 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_","stats_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(unimpaired[[i]]$Winter_3mon[[n+1]]$Stats$Thresholds$Totals, 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired[[i]]$Winter_3mon[[n+1]]$Stats$Thresholds$coded), 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
	}
	
	
	
	########################## six month
	dir.create(file.path(maindir, gaugedir, sixmondir))
	for(n in 1:length(sub1dir)){
		dir.create(file.path(maindir, gaugedir, sixmondir,sub1dir[[n]]))
	}
	
	for(n in 1:length(unimpaired[[i]]$Winter_6mon$Data)){
		dir.create(file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_6mon$Data)[[n]]))
		
		write.csv(unimpaired[[i]]$Winter_6mon$Data[[n]], 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_6mon$Data)[[n]],
						paste("6monData_",names(unimpaired[[i]]$Winter_6mon$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired[[i]]$Winter_6mon$Stats[[n]]$Values), 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_6mon$Data)[[n]],
						paste("6monStats_",names(unimpaired[[i]]$Winter_6mon$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(unimpaired[[i]]$Winter_6mon$Stats[[n]]$Thresholds$Totals, 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_6mon$Data)[[n]],
						paste("6monThresholds_",names(unimpaired[[i]]$Winter_6mon$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired[[i]]$Winter_6mon$Stats[[n]]$Thresholds$coded), 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_6mon$Data)[[n]],
						paste("6monThresholdCodes_",names(unimpaired[[i]]$Winter_6mon$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
	}
	
	for(n in 2:7){
		write.csv(unimpaired[[i]]$Winter_6mon[[n+1]]$Data, 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_","data_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired[[i]]$Winter_6mon[[n+1]]$Stats[1:7]), 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]], paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_","stats_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(unimpaired[[i]]$Winter_6mon[[n+1]]$Stats$Thresholds$Totals, 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired[[i]]$Winter_6mon[[n+1]]$Stats$Thresholds$coded), 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
	}
	
	
	################### monthly
	dir.create(file.path(maindir, gaugedir, mondir))
	for(n in 1:length(sub1dir)){
		dir.create(file.path(maindir, gaugedir, mondir,sub1dir[[n]]))
		for(m in 1:6){
			dir.create(file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]]))
		}
	}
	
	for(n in 1:length(unimpaired[[i]]$Winter_monthly$Data)){
		dir.create(file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_monthly$Data)[[n]]))
		for(m in 1:6){
			dir.create(file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_monthly$Data)[[n]],monthlywinter[[m]]))
			write.csv(unimpaired[[i]]$Winter_monthly$Data[[n]][[m]], 
					file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_monthly$Data)[[n]],monthlywinter[[m]],
							paste("Data_",monthlywinter[[m]],"_",names(unimpaired[[i]]$Winter_monthly$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
			
			write.csv(as.data.frame(unimpaired[[i]]$Winter_monthly$Stats[[n]][[m]]$Values), 
					file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_monthly$Data)[[n]],monthlywinter[[m]],
							paste("Stats_",monthlywinter[[m]],"_",names(unimpaired[[i]]$Winter_monthly$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
			
			write.csv(unimpaired[[i]]$Winter_monthly$Stats[[n]][[m]]$Thresholds$Totals, 
					file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_monthly$Data)[[n]],monthlywinter[[m]],
							paste("Thresholds_",monthlywinter[[m]],"_",names(unimpaired[[i]]$Winter_monthly$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
			
			write.csv(as.data.frame(unimpaired[[i]]$Winter_monthly$Stats[[n]][[m]]$Thresholds$coded), 
					file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired[[i]]$Winter_monthly$Data)[[n]],monthlywinter[[m]],
							paste("ThresholdCodes_",monthlywinter[[m]],"_",names(unimpaired[[i]]$Winter_monthly$Data)[[n]],"_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		}
	}
	
	for(n in 2:7){
		for(m in 1:6){
			write.csv(unimpaired[[i]]$Winter_monthly[[n+1]][[m]]$Data, 
					file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","data_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
			write.csv(as.data.frame(unimpaired[[i]]$Winter_monthly[[n+1]][[m]]$Stats[1:7]), 
					file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]], paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","stats_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
			write.csv(unimpaired[[i]]$Winter_monthly[[n+1]][[m]]$Stats$Thresholds$Totals, 
					file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","threshold_stats_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
			write.csv(as.data.frame(unimpaired[[i]]$Winter_monthly[[n+1]][[m]]$Stats$Thresholds$coded), 
					file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(unimpaired[[i]]$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","threshold_coded_",as.character(unimpaired[[i]]$raw$site_no[[1]]),".csv",sep="")))
		}
	}
	




}

