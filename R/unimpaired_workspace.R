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


unimpaired <- vector("list", 5)
for(z in 1:5){
	unimpaired[[z]] <- list()
	unimpaired[[z]]$raw <- readNWISdv(unimpaired_g[[z]],"00060", startDate="1900-01-01",
			endDate=Sys.Date(), statCd="00003")

	unimpaired[[z]]$raw <- RemoveLeapDays(unimpaired[[z]]$raw)
	
	if(as.numeric(unimpaired[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		unimpaired[[z]]$Index$Valley <- "SacV"
		unimpaired[[z]]$Index$Index <- YEARTYPEqdf$SacV_num
		unimpaired[[z]]$Index$Year <- YEARTYPEqdf$Year
	} else if(as.numeric(unimpaired[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		unimpaired[[z]]$Index$Valley <- "SJV"
		unimpaired[[z]]$Index$Index <- YEARTYPEqdf$SJV_num
		unimpaired[[z]]$Index$Year <- YEARTYPEqdf$Year
	} else {
		unimpaired[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",unimpaired[[z]]$raw$site_no[[1]]))
	}


	###DATA PROCESSING
	unimpaired[[z]]$prep <- prepdata(unimpaired[[z]]$raw)
	unimpaired[[z]]$Availability <- DataAvailability(unimpaired[[z]]$raw)
	unimpaired[[z]]$thresholds_maf <- thresholds(unimpaired[[z]]$prep)
#	unimpaired[[z]]$record_stats <- record_stats(unimpaired[[z]]$prep, unimpaired[[z]]$thresholds_maf)
#	unimpaired[[z]]$Winter_3mon <- Split3Winter(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
#	unimpaired[[z]]$Winter_6mon <- Split6Winter(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
#	unimpaired[[z]]$Winter_monthly <- SplitWinterMonthly(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
	unimpaired[[z]]$HydroYear <- SplitHydroYear(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)	
	unimpaired[[z]]$HydroYear <- cleanup(unimpaired[[z]]$HydroYear)
	unimpaired[[z]]$Daysmax <- FreqAnalysis(unimpaired[[z]]$HydroYear, c(1,3,7), unimpaired[[z]]$Index)
	unimpaired[[z]]$PearsonIII <- FitqPearsonIIIroll(unimpaired[[z]]$Daysmax$All$zoo$X3DayMaxQ_maf,10, probs=c(0.01, 1/50, 1/20, 1/10))	
}	
names(unimpaired) <- unimpaired_g[1:5]

for(z in 1:5){
	for(n in 1:length(unimpaired[[z]]$PearsonIII)){
		pdf(paste0("C:\\Users\\tiffn_000\\Desktop\\Plots6\\",names(unimpaired)[[z]],names(unimpaired[[z]]$PearsonIII)[[n]],".pdf"), width=11, height=8.5, useDingbats=FALSE)
		plot(x=unimpaired[[z]]$PearsonIII[[n]][[2]], y=unimpaired[[z]]$PearsonIII[[n]][[1]], type="b", xlab=NA, ylab=NA, xlim=c(as.Date("1900-01-01"), Sys.Date()))
		title(main=paste(names(unimpaired)[[z]],names(unimpaired[[z]]$PearsonIII)[[n]], "3-Day Max" ,sep=" "),
			xlab="Date", ylab="Q (maf)")
		dev.off()
	}
}
########################################


	### WRITE TO FILES #####
	maindir <- "C:\\Users\\tiffn_000\\Documents\\workspaces\\output"
	gaugedir <- as.character(unimpaired$raw$site_no[[1]])
	prepdatadir <- "Data_prep"
	availdir <- "Availability"
	availname <- c("Yearly","Monthly","Decadal")
	thresholdsdir <- "Thresholds"
	recordstatsdir <- "Period_of_Record_Stats"
	threemondir <- "3_Month_Winter"
	sixmondir <- "6_Month_Winter"
	mondir <- "Monthly_Winter"
	hydroyeardir <- "HydroYears"
	monthlywinter <- c("Nov","Dec","Jan","Feb","Mar","Apr")
	sub1dir <- c("Yearly","All","C","D","BN","AN","W")
	subdir2 <- c("Data","General_Stats","Threshold_Stats","Threshold_Coded")
	dir.create(file.path(maindir, gaugedir))
	dir.create(file.path(maindir, gaugedir, prepdatadir))
	write.csv(unimpaired$prep, file=file.path(maindir, gaugedir, prepdatadir, paste("Data_", as.character(unimpaired$raw$site_no[[1]]),".csv", sep="")))
	
	dir.create(file.path(maindir, gaugedir, "Index"))
	write.csv(as.data.frame(unimpaired$Index),
			file=file.path(maindir, gaugedir, "Index",paste(as.character(unimpaired$raw$site_no[[1]]),"_","Index.csv",sep="")))
	
	dir.create(file.path(maindir, gaugedir, availdir))
	for(n in 1:3){
		write.csv(unimpaired$Availability[[n]], file=file.path(maindir,gaugedir, availdir, paste(availname[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv", sep="")))
	}
	
	dir.create(file.path(maindir, gaugedir, thresholdsdir))
	write.csv(unimpaired$thresholds_maf, file=file.path(maindir, gaugedir, thresholdsdir, paste(as.character(unimpaired$raw$site_no[[1]]),"_","Thresholds_maf.csv",sep="")))
	
	dir.create(file.path(maindir, gaugedir, recordstatsdir))
	write.csv(paddf(unimpaired$record_stats[1:7]), file=file.path(maindir, gaugedir, recordstatsdir, paste(as.character(unimpaired$raw$site_no[[1]]),"_","POR_stats.csv",sep="")))
	write.csv(unimpaired$record_stats$Thresholds$Totals, file=file.path(maindir, gaugedir, recordstatsdir, paste(as.character(unimpaired$raw$site_no[[1]]),"_","Thresholds_stats.csv",sep="")))
	write.csv(as.data.frame(unimpaired$record_stats$Thresholds$coded), file=file.path(maindir, gaugedir, recordstatsdir, paste(as.character(unimpaired$raw$site_no[[1]]),"_","Thresholds_coded.csv",sep="")))
	
	
	############# three month
	dir.create(file.path(maindir, gaugedir, threemondir))
	for(n in 1:length(sub1dir)){
		dir.create(file.path(maindir, gaugedir, threemondir,sub1dir[[n]]))
	}
	
	for(n in 1:length(unimpaired$Winter_3mon$Data)){
		dir.create(file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired$Winter_3mon$Data)[[n]]))
		write.csv(unimpaired$Winter_3mon$Data[[n]], 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired$Winter_3mon$Data)[[n]], 
						paste("3monData_",names(unimpaired$Winter_3mon$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(paddf(unimpaired$Winter_3mon$Stats[[n]]$Values), 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired$Winter_3mon$Data)[[n]],
						paste("3monStats_",names(unimpaired$Winter_3mon$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(unimpaired$Winter_3mon$Stats[[n]]$Thresholds$Totals, 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired$Winter_3mon$Data)[[n]], 
						paste("3monThresholds_",names(unimpaired$Winter_3mon$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired$Winter_3mon$Stats[[n]]$Thresholds$coded), 
				file=file.path(maindir,gaugedir,threemondir, sub1dir[[1]], names(unimpaired$Winter_3mon$Data)[[n]],
						paste("3monThresholdCodes_",names(unimpaired$Winter_3mon$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
	}
	
	
	for(n in 2:7){
		if(class(unimpaired$Winter_3mon[[n+1]])== "list"){
			write.csv(unimpaired$Winter_3mon[[n+1]]$Data, 
					file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","data_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(paddf(unimpaired$Winter_3mon[[n+1]]$Stats[1:7]), 
					file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(unimpaired$Winter_3mon[[n+1]]$Stats$Thresholds$Totals, 
					file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(as.data.frame(unimpaired$Winter_3mon[[n+1]]$Stats$Thresholds$coded), 
					file=file.path(maindir,gaugedir,threemondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		} else {
			unlink(file.path(maindir, gaugedir, threemondir,sub1dir[[n]]), recursive=TRUE)
			dir.create(file.path(maindir, gaugedir, threemondir,paste("No",sub1dir[[n]],"Years_In_Input_Dataset",sep="_")))
		}
		}
	
	
	
	########################## six month
	dir.create(file.path(maindir, gaugedir, sixmondir))
	for(n in 1:length(sub1dir)){
		dir.create(file.path(maindir, gaugedir, sixmondir,sub1dir[[n]]))
	}
	
	for(n in 1:length(unimpaired$Winter_6mon$Data)){
		dir.create(file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired$Winter_6mon$Data)[[n]]))
		
		write.csv(unimpaired$Winter_6mon$Data[[n]], 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired$Winter_6mon$Data)[[n]],
						paste("6monData_",names(unimpaired$Winter_6mon$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(paddf(unimpaired$Winter_6mon$Stats[[n]]$Values), 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired$Winter_6mon$Data)[[n]],
						paste("6monStats_",names(unimpaired$Winter_6mon$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(unimpaired$Winter_6mon$Stats[[n]]$Thresholds$Totals, 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired$Winter_6mon$Data)[[n]],
						paste("6monThresholds_",names(unimpaired$Winter_6mon$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired$Winter_6mon$Stats[[n]]$Thresholds$coded), 
				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[1]], names(unimpaired$Winter_6mon$Data)[[n]],
						paste("6monThresholdCodes_",names(unimpaired$Winter_6mon$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
	}
	
#	for(n in 2:7){
#		write.csv(unimpaired$Winter_6mon[[n+1]]$Data, 
#				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_","data_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#		write.csv(paddf(unimpaired$Winter_6mon[[n+1]]$Stats[1:7]), 
#				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#		write.csv(unimpaired$Winter_6mon[[n+1]]$Stats$Thresholds$Totals, 
#				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#		write.csv(as.data.frame(unimpaired$Winter_6mon[[n+1]]$Stats$Thresholds$coded), 
#				file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#	}
	
	for(n in 2:7){
		if(class(unimpaired$Winter_6mon[[n+1]])== "list"){
			write.csv(unimpaired$Winter_6mon[[n+1]]$Data, 
					file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","data_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(paddf(unimpaired$Winter_6mon[[n+1]]$Stats[1:7]), 
					file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(unimpaired$Winter_6mon[[n+1]]$Stats$Thresholds$Totals, 
					file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(as.data.frame(unimpaired$Winter_6mon[[n+1]]$Stats$Thresholds$coded), 
					file=file.path(maindir,gaugedir,sixmondir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		} else {
			unlink(file.path(maindir, gaugedir, sixmondir,sub1dir[[n]]), recursive=TRUE)
			dir.create(file.path(maindir, gaugedir, sixmondir,paste("No",sub1dir[[n]],"Years_In_Input_Dataset",sep="_")))
		}
	}
	
	
	################### monthly
	dir.create(file.path(maindir, gaugedir, mondir))
	for(n in 1:length(sub1dir)){
		dir.create(file.path(maindir, gaugedir, mondir,sub1dir[[n]]))
		for(m in 1:6){
			dir.create(file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]]))
		}
	}
	
	for(n in 1:length(unimpaired$Winter_monthly$Data)){
		dir.create(file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired$Winter_monthly$Data)[[n]]))
		for(m in 1:6){
			dir.create(file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired$Winter_monthly$Data)[[n]],monthlywinter[[m]]))
			write.csv(unimpaired$Winter_monthly$Data[[n]][[m]], 
					file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired$Winter_monthly$Data)[[n]],monthlywinter[[m]],
							paste("Data_",monthlywinter[[m]],"_",names(unimpaired$Winter_monthly$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			
			write.csv(paddf(unimpaired$Winter_monthly$Stats[[n]][[m]]$Values), 
					file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired$Winter_monthly$Data)[[n]],monthlywinter[[m]],
							paste("Stats_",monthlywinter[[m]],"_",names(unimpaired$Winter_monthly$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			
			write.csv(unimpaired$Winter_monthly$Stats[[n]][[m]]$Thresholds$Totals, 
					file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired$Winter_monthly$Data)[[n]],monthlywinter[[m]],
							paste("Thresholds_",monthlywinter[[m]],"_",names(unimpaired$Winter_monthly$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			
			write.csv(as.data.frame(unimpaired$Winter_monthly$Stats[[n]][[m]]$Thresholds$coded), 
					file=file.path(maindir,gaugedir,mondir, sub1dir[[1]], names(unimpaired$Winter_monthly$Data)[[n]],monthlywinter[[m]],
							paste("ThresholdCodes_",monthlywinter[[m]],"_",names(unimpaired$Winter_monthly$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		}
	}
	
#	for(n in 2:7){
#		for(m in 1:6){
#			write.csv(unimpaired$Winter_monthly[[n+1]][[m]]$Data, 
#					file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","data_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#			write.csv(paddf(unimpaired$Winter_monthly[[n+1]][[m]]$Stats[1:7]), 
#					file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#			write.csv(unimpaired$Winter_monthly[[n+1]][[m]]$Stats$Thresholds$Totals, 
#					file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","threshold_stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#			write.csv(as.data.frame(unimpaired$Winter_monthly[[n+1]][[m]]$Stats$Thresholds$coded), 
#					file=file.path(maindir, gaugedir, mondir,sub1dir[[n]],monthlywinter[[m]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","threshold_coded_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#		}
#	}
#	
	for(n in 2:7){
		for(m in 1:6){
			if(class(unimpaired$Winter_monthly[[n+1]])== "list"){
				write.csv(unimpaired$Winter_monthly[[n+1]][[m]]$Data, 
						file=file.path(maindir,gaugedir,mondir, sub1dir[[n]],monthlywinter[[m]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","data_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
				write.csv(paddf(unimpaired$Winter_monthly[[n+1]][[m]]$Stats[1:7]), 
						file=file.path(maindir,gaugedir,mondir, sub1dir[[n]],monthlywinter[[m]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
				write.csv(unimpaired$Winter_monthly[[n+1]][[m]]$Stats$Thresholds$Totals, 
						file=file.path(maindir,gaugedir,mondir, sub1dir[[n]],monthlywinter[[m]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","threshold_stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
				write.csv(as.data.frame(unimpaired$Winter_monthly[[n+1]][[m]]$Stats$Thresholds$coded), 
						file=file.path(maindir,gaugedir,mondir, sub1dir[[n]],monthlywinter[[m]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_",monthlywinter[[m]],"_","threshold_coded_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			} else {
				unlink(file.path(maindir, gaugedir, mondir,sub1dir[[n]]), recursive=TRUE)
				dir.create(file.path(maindir, gaugedir, mondir, paste("No",sub1dir[[n]],"Years_In_Input_Dataset",sep="_")))
			}
		}
	}
	
	########################## hydro year
	dir.create(file.path(maindir, gaugedir, hydroyeardir))
	for(n in 1:length(sub1dir)){
		dir.create(file.path(maindir, gaugedir, hydroyeardir,sub1dir[[n]]))
	}
	
	for(n in 1:length(unimpaired$HydroYear$Data)){
		dir.create(file.path(maindir,gaugedir,hydroyeardir, sub1dir[[1]], names(unimpaired$HydroYear$Data)[[n]]))
		
		write.csv(unimpaired$HydroYear$Data[[n]], 
				file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[1]], names(unimpaired$HydroYear$Data)[[n]],
						paste("HydroYearData_",names(unimpaired$HydroYear$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(paddf(unimpaired$HydroYear$Stats[[n]]$Values), 
				file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[1]], names(unimpaired$HydroYear$Data)[[n]],
						paste("HydroYearStats_",names(unimpaired$HydroYear$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(unimpaired$HydroYear$Stats[[n]]$Thresholds$Totals, 
				file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[1]], names(unimpaired$HydroYear$Data)[[n]],
						paste("HydroYearThresholds_",names(unimpaired$HydroYear$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		write.csv(as.data.frame(unimpaired$HydroYear$Stats[[n]]$Thresholds$coded), 
				file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[1]], names(unimpaired$HydroYear$Data)[[n]],
						paste("HydroYearThresholdCodes_",names(unimpaired$HydroYear$Data)[[n]],"_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
	}
	
#	for(n in 2:7){
#		write.csv(unimpaired$HydroYear[[n+1]]$Data, 
#				file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[n]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_","data_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#		write.csv(paddf(unimpaired$HydroYear[[n+1]]$Stats[1:7]), 
#				file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#		write.csv(unimpaired$HydroYear[[n+1]]$Stats$Thresholds$Totals, 
#				file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[n]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#		write.csv(as.data.frame(unimpaired$HydroYear[[n+1]]$Stats$Thresholds$coded), 
#				file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[n]],paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
#	}
	
	for(n in 2:7){
		if(class(unimpaired$HydroYear[[n+1]])== "list"){
			write.csv(unimpaired$HydroYear[[n+1]]$Data, 
					file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","data_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(paddf(unimpaired$HydroYear[[n+1]]$Stats[1:7]), 
					file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(unimpaired$HydroYear[[n+1]]$Stats$Thresholds$Totals, 
					file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_stats_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
			write.csv(as.data.frame(unimpaired$HydroYear[[n+1]]$Stats$Thresholds$coded), 
					file=file.path(maindir,gaugedir,hydroyeardir, sub1dir[[n]], paste(names(unimpaired$Winter_monthly)[[n+1]],"_","threshold_coded_",as.character(unimpaired$raw$site_no[[1]]),".csv",sep="")))
		} else {
			unlink(file.path(maindir, gaugedir, hydroyeardir,sub1dir[[n]]), recursive=TRUE)
			dir.create(file.path(maindir, gaugedir, hydroyeardir,paste("No",sub1dir[[n]],"Years_In_Input_Dataset",sep="_")))
		}
	}
	

