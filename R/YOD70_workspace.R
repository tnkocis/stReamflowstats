# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


library(dplyr)
library(hydroTSM)
library(dataRetrieval)

SacV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_svi.txt")
SJV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_sji.txt")
YOD70_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\YOD70\\YOD70gauges.csv")
YOD70_g <- as.numeric(YOD70_g$YOD70)


YOD70 <- vector("list", 5)
for(z in 1:1){
	YOD70[[z]] <- list()
	YOD70[[z]]$raw <- readNWISdv(YOD70_g[[z]],"00060", startDate="1945-10-01",
			endDate=Sys.Date(), statCd="00003")
	
	YOD70[[z]]$raw <- RemoveLeapDays(YOD70[[z]]$raw)
	
	if(as.numeric(YOD70[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		YOD70[[z]]$Index$Valley <- "SacV"
		YOD70[[z]]$Index$Index <- YEARTYPEqdf$SacV_num
		YOD70[[z]]$Index$Year <- YEARTYPEqdf$Year
	} else if(as.numeric(YOD70[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		YOD70[[z]]$Index$Valley <- "SJV"
		YOD70[[z]]$Index$Index <- YEARTYPEqdf$SJV_num
		YOD70[[z]]$Index$Year <- YEARTYPEqdf$Year
	} else {
		YOD70[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",YOD70[[z]]$raw$site_no[[1]]))
	}
	
	
	###DATA PROCESSING
	YOD70[[z]]$prep <- prepdata(YOD70[[z]]$raw)
	YOD70[[z]]$Availability <- DataAvailability(YOD70[[z]]$raw)
	YOD70[[z]]$thresholds_maf <- thresholds(YOD70[[z]]$prep)
#	YOD70[[z]]$record_stats <- record_stats(YOD70[[z]]$prep, YOD70[[z]]$thresholds_maf)
#	YOD70[[z]]$Winter_3mon <- Split3Winter(YOD70[[z]]$prep, YOD70[[z]]$Index, YOD70[[z]]$thresholds_maf)
	YOD70[[z]]$Winter_6mon <- Split6Winter(YOD70[[z]]$prep, YOD70[[z]]$Index, YOD70[[z]]$thresholds_maf)
	YOD70[[z]]$Winter_monthly <- SplitWinterMonthly(YOD70[[z]]$prep, YOD70[[z]]$Index, YOD70[[z]]$thresholds_maf)
	YOD70[[z]]$HydroYear <- SplitHydroYear(YOD70[[z]]$prep, YOD70[[z]]$Index, YOD70[[z]]$thresholds_maf)	
	YOD70[[z]]$HydroYear <- cleanupHY(YOD70[[z]]$HydroYear)
	YOD70[[z]]$Winter_6mon <- cleanup6MON(YOD70[[z]]$Winter_6mon)
	YOD70[[z]]$DaysmaxHY <- FreqAnalysis(YOD70[[z]]$HydroYear, c(3), YOD70[[z]]$Index)
	YOD70[[z]]$Daysmax6MON <- FreqAnalysis(YOD70[[z]]$Winter_6mon, c(3), YOD70[[z]]$Index)
	## add loop here if daysmax vector length >1 (UPDATES BELOW AS OF 7/27/15)
	YOD70[[z]]$PearsonIIIrollHY <- vector("list", 6)
	YOD70[[z]]$PearsonIIIroll6MON <- vector("list", 6)
	for(k in 1:6){
		YOD70[[z]]$PearsonIIIrollHY[[k]] <- FitqPearsonIIIroll(YOD70[[z]]$DaysmaxHY[[k]]$zoo$X3DayMaxQ_maf, movewidth=10, probs=c(0.01, 1/50, 1/20, 1/10))	
		YOD70[[z]]$PearsonIIIroll6MON[[k]] <- FitqPearsonIIIroll(YOD70[[z]]$Daysmax6MON[[k]]$zoo$X3DayMaxQ_maf, movewidth=10, probs=c(0.01, 1/50, 1/20, 1/10))	
	}
	names(YOD70[[z]]$PearsonIIIrollHY) <- names(YOD70[[z]]$DaysmaxHY)
	names(YOD70[[z]]$PearsonIIIroll6MON) <- names(YOD70[[z]]$Daysma6MON)
	YOD70[[z]]$glsPIIIHY <- vector("list", 6)
	YOD70[[z]]$glsPIII6MON <- vector("list", 6)
	for(k in 1:6){
		YOD70[[z]]$glsPIIIHY[[k]] <- glsPIII(YOD70[[z]]$PearsonIIIrollHY[[k]])
		YOD70[[z]]$glsPIII6MON[[k]] <- glsPIII(YOD70[[z]]$PearsonIIIroll6MON[[k]])
	}
	names(YOD70[[z]]$glsPIIIHY) <- names(YOD70[[z]]$DaysmaxHY)
	names(YOD70[[z]]$glsPIII6MON) <- names(YOD70[[z]]$Daysma6MON)
	YOD70[[z]]$MonthlyThreshold <- MonthlyThresholdQ(YOD70[[z]]$Winter_monthly, prob=0.95)
	YOD70[[z]]$glsMonthly <- vector("list",6)
	for(k in 1:6){
		YOD70[[z]]$glsMonthly[[k]] <- vector("list",6)
		for (i in 1:6){
			YOD70[[z]]$glsMonthly[[k]][[i]] <- glsMonthly(YOD70[[z]]$MonthlyThreshold[[k]][[i]])
		}
		names(YOD70[[z]]$glsMonthly[[k]]) <- names(YOD70[[z]]$MonthlyThreshold[[k]])
	}
	names(YOD70[[z]]$glsMonthly) <- names(YOD70[[z]]$MonthlyThreshold)
	
	
}	
names(YOD70) <- YOD70_g[1:2]

######################




unimpaired_g <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\unimpaired_gauges.csv")
unimpaired_g <- as.numeric(unimpaired_g$Unimpaired)

glsHY <- vector("list", 4)
names(glsHY) <- c("NEP_0.01", "NEP_0.02", "NEP_0.05", "NEP_0.1")
for(i in 1:4){
	glsHY[[i]] <- data.frame(slope=rep(NA,length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
gls6MON <- vector("list", 4)
names(gls6MON) <- c("NEP_0.01", "NEP_0.02", "NEP_0.05", "NEP_0.1")
for(i in 1:4){
	gls6MON[[i]] <- data.frame(slope=rep(NA,length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}
gls3MON <- vector("list", 4)
names(gls3MON) <- c("NEP_0.01", "NEP_0.02", "NEP_0.05", "NEP_0.1")
for(i in 1:4){
	gls3MON[[i]] <- data.frame(slope=rep(NA,length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
}

glsMON <- vector("list",6)
names(glsMON)<- c("NOV","DEC","JAN","FEB","MAR","APR")
glsMONRelQ <- vector("list",6)
names(glsMONRelQ)<- c("NOV_RelQ","DEC_RelQ","JAN_RelQ","FEB_RelQ","MAR_RelQ","APR_RelQ")

for(k in 1:6){
	glsMON[[k]] <- vector("list",4)
	names(glsMON[[k]]) <- c("NEP_0.01", "NEP_0.02", "NEP_0.05", "NEP_0.1")
	glsMONRelQ[[k]] <- data.frame(slope=rep(NA,length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
	for(i in 1:4){
		glsMON[[k]][[i]] <- data.frame(slope=rep(NA,length(unimpaired_g)), pvalue=rep(NA, length(unimpaired_g)), gauge=rep(NA, length(unimpaired_g)))
	}
}
	

unimpaired <- vector("list", 5)
for(z in 1:1){
	unimpaired[[z]] <- list()
	unimpaired[[z]]$raw <- readNWISdv(unimpaired_g[[z]],"00060", startDate="1945-10-01",
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
	unimpaired[[z]]$Winter_3mon <- Split3Winter(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
	unimpaired[[z]]$Winter_6mon <- Split6Winter(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
	unimpaired[[z]]$Winter_monthly <- SplitWinterMonthly(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)
	unimpaired[[z]]$HydroYear <- SplitHydroYear(unimpaired[[z]]$prep, unimpaired[[z]]$Index, unimpaired[[z]]$thresholds_maf)	
	unimpaired[[z]]$HydroYear <- cleanupHY(unimpaired[[z]]$HydroYear)
	unimpaired[[z]]$Winter_6mon <- cleanup6MON(unimpaired[[z]]$Winter_6mon)
	unimpaired[[z]]$Winter_3mon <- cleanup3MON(unimpaired[[z]]$Winter_3mon)
	unimpaired[[z]]$Winter_monthly <- cleanupMON(unimpaired[[z]]$Winter_monthly)
	unimpaired[[z]]$DaysmaxHY <- FreqAnalysis(unimpaired[[z]]$HydroYear, c(3), unimpaired[[z]]$Index)
	unimpaired[[z]]$Daysmax6MON <- FreqAnalysis(unimpaired[[z]]$Winter_6mon, c(3), unimpaired[[z]]$Index)
	unimpaired[[z]]$Daysmax3MON <- FreqAnalysis(unimpaired[[z]]$Winter_3mon, c(3), unimpaired[[z]]$Index)
	unimpaired[[z]]$DaysmaxMON <- FreqAnalysisMonthly(unimpaired[[z]]$Winter_monthly, c(3), unimpaired[[z]]$Index)
	## add loop here if daysmax vector length >1 (UPDATES BELOW AS OF 7/27/15)
	unimpaired[[z]]$PearsonIIIrollHY <- vector("list", 6)
	unimpaired[[z]]$PearsonIIIroll6MON <- vector("list", 6)
	unimpaired[[z]]$PearsonIIIroll3MON <- vector("list", 6)
	for(k in 1:6){
		unimpaired[[z]]$PearsonIIIrollHY[[k]] <- FitqPearsonIIIroll(unimpaired[[z]]$DaysmaxHY[[k]]$zoo$X3DayMaxQ_maf, movewidth=10, probs=c(0.01, 1/50, 1/20, 1/10), npoints=10)	
		unimpaired[[z]]$PearsonIIIroll6MON[[k]] <- FitqPearsonIIIroll(unimpaired[[z]]$Daysmax6MON[[k]]$zoo$X3DayMaxQ_maf, movewidth=10, probs=c(0.01, 1/50, 1/20, 1/10), npoints=10)
		unimpaired[[z]]$PearsonIIIroll3MON[[k]] <- FitqPearsonIIIroll(unimpaired[[z]]$Daysmax3MON[[k]]$zoo$X3DayMaxQ_maf, movewidth=10, probs=c(0.01, 1/50, 1/20, 1/10), npoints=10)
	}
	names(unimpaired[[z]]$PearsonIIIrollHY) <- names(unimpaired[[z]]$DaysmaxHY)
	names(unimpaired[[z]]$PearsonIIIroll6MON) <- names(unimpaired[[z]]$Daysmax6MON)
	names(unimpaired[[z]]$PearsonIIIroll3MON) <- names(unimpaired[[z]]$Daysmax3MON)
	unimpaired[[z]]$PearsonIIIrollMON <- FitqPearsonIIIrollMonthly(unimpaired[[z]]$DaysmaxMON$All$zoo, movewidth=10,probs=c(0.01, 1/50, 1/20, 1/10), npoints=10)
#	unimpaired[[z]]$glsPIIIHY <- vector("list", 6)
#	unimpaired[[z]]$glsPIII6MON <- vector("list", 6)
#	for(k in 1:6){
#		unimpaired[[z]]$glsPIIIHY[[k]] <- glsPIII(unimpaired[[z]]$PearsonIIIrollHY[[k]])
#		unimpaired[[z]]$glsPIII6MON[[k]] <- glsPIII(unimpaired[[z]]$PearsonIIIroll6MON[[k]])
#	}
#	names(unimpaired[[z]]$glsPIIIHY) <- names(unimpaired[[z]]$DaysmaxHY)
#	names(unimpaired[[z]]$glsPIII6MON) <- names(unimpaired[[z]]$Daysma6MON)
	unimpaired[[z]]$glsPIIIHY <- glsPIII(unimpaired[[z]]$PearsonIIIrollHY[[1]])
	unimpaired[[z]]$glsPIII6MON <- glsPIII(unimpaired[[z]]$PearsonIIIroll6MON[[1]])
	unimpaired[[z]]$glsPIII3MON <- glsPIII(unimpaired[[z]]$PearsonIIIroll3MON[[1]])
	unimpaired[[z]]$glsPIIIMON <- glsPIIIMonthly(unimpaired[[z]]$PearsonIIIrollMON)
	unimpaired[[z]]$MonthlyThreshold <- MonthlyThresholdQ(unimpaired[[z]]$Winter_monthly, prob=0.50)
	unimpaired[[z]]$MonthlyRelative <- MonthlyRelativeTotalQ(unimpaired[[z]]$Winter_monthly)
	
	unimpaired[[z]]$glsMonthly <- vector("list",6)
#	for(k in 1:6){
#		unimpaired[[z]]$glsMonthly[[k]] <- vector("list",6)
#		for (i in 1:6){
#			unimpaired[[z]]$glsMonthly[[k]][[i]] <- glsMonthly(unimpaired[[z]]$MonthlyThreshold[[k]][[i]])
#		}
#		names(unimpaired[[z]]$glsMonthly[[k]]) <- names(unimpaired[[z]]$MonthlyThreshold[[k]])
#	}

	unimpaired[[z]]$glsMonthly[[1]] <- glsMonthly(unimpaired[[z]]$MonthlyThreshold[[1]])
	unimpaired[[z]]$glsMonthlyRelQ <- vector("list",6)
	unimpaired[[z]]$glsMonthlyRelQ[[1]] <- glsMonthlyRelQ(unimpaired[[z]]$MonthlyRelative[[1]])

	names(unimpaired[[z]]$glsMonthly) <- names(unimpaired[[z]]$MonthlyThreshold)
	names(unimpaired[[z]]$glsMonthlyRelQ) <- names(unimpaired[[z]]$MonthlyRelative)
	
	for(i in 1:4){
		glsHY[[i]]$slope[[z]] <-  unimpaired[[z]]$glsPIIIHY[[i]][[1]][[2]]
		glsHY[[i]]$pvalue[[z]] <-  unimpaired[[z]]$glsPIIIHY[[i]][[4]][[2]]
		glsHY[[i]]$gauge[[z]] <-  unimpaired[[z]]$raw$site_no[[1]]
		gls6MON[[i]]$slope[[z]] <-  unimpaired[[z]]$glsPIII6MON[[i]][[1]][[2]]
		gls6MON[[i]]$pvalue[[z]] <-  unimpaired[[z]]$glsPIII6MON[[i]][[4]][[2]]
		gls6MON[[i]]$gauge[[z]] <-  unimpaired[[z]]$raw$site_no[[1]]
		gls3MON[[i]]$slope[[z]] <-  unimpaired[[z]]$glsPIII3MON[[i]][[1]][[2]]
		gls3MON[[i]]$pvalue[[z]] <-  unimpaired[[z]]$glsPIII3MON[[i]][[4]][[2]]
		gls3MON[[i]]$gauge[[z]] <-  unimpaired[[z]]$raw$site_no[[1]]
		for(k in 1:6){
			glsMON[[k]][[i]]$slope[[z]] <- unimpaired[[z]]$glsPIIIMON[[k]][[i]][[1]][[2]]
			glsMON[[k]][[i]]$pvalue[[z]] <- unimpaired[[z]]$glsPIIIMON[[k]][[i]][[4]][[2]]
			glsMON[[k]][[i]]$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
		}
	}
	for(k in 1:6){
		glsMONRelQ[[k]]$slope[[z]] <- unimpaired[[z]]$glsMonthlyRelQ[[1]][[k]][[1]][[2]]
		glsMONRelQ[[k]]$pvalue[[z]] <- unimpaired[[z]]$glsMonthlyRelQ[[1]][[k]][[4]][[2]]
		glsMONRelQ[[k]]$gauge[[z]] <- unimpaired[[z]]$raw$site_no[[1]]
	}
	


}	
names(unimpaired) <- unimpaired_g[1:2]
