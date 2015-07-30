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


#unimpaired <- vector("list", 5)
for(z in 1:length(unimpaired_g)){
	unimpaired <- list()
	unimpaired$raw <- readNWISdv(unimpaired_g[[z]],"00060", startDate="1945-10-01",
			endDate=Sys.Date(), statCd="00003")
	
	unimpaired$raw <- RemoveLeapDays(unimpaired$raw)
	
	if(as.numeric(unimpaired$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		unimpaired$Index$Valley <- "SacV"
		unimpaired$Index$Index <- YEARTYPEqdf$SacV_num
		unimpaired$Index$Year <- YEARTYPEqdf$Year
	} else if(as.numeric(unimpaired$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		unimpaired$Index$Valley <- "SJV"
		unimpaired$Index$Index <- YEARTYPEqdf$SJV_num
		unimpaired$Index$Year <- YEARTYPEqdf$Year
	} else {
		unimpaired$Index$Valley <- "ERROR"
		print(paste("Error",unimpaired$raw$site_no[[1]]))
	}
	
	
	###DATA PROCESSING
	unimpaired$prep <- prepdata(unimpaired$raw)
	unimpaired$Availability <- DataAvailability(unimpaired$raw)
	unimpaired$thresholds_maf <- thresholds(unimpaired$prep)
#	unimpaired$record_stats <- record_stats(unimpaired$prep, unimpaired$thresholds_maf)
	unimpaired$Winter_3mon <- Split3Winter(unimpaired$prep, unimpaired$Index, unimpaired$thresholds_maf)
	unimpaired$Winter_6mon <- Split6Winter(unimpaired$prep, unimpaired$Index, unimpaired$thresholds_maf)
	unimpaired$Winter_monthly <- SplitWinterMonthly(unimpaired$prep, unimpaired$Index, unimpaired$thresholds_maf)
	unimpaired$HydroYear <- SplitHydroYear(unimpaired$prep, unimpaired$Index, unimpaired$thresholds_maf)	
	unimpaired$HydroYear <- cleanupHY(unimpaired$HydroYear)
	unimpaired$Winter_6mon <- cleanup6MON(unimpaired$Winter_6mon)
	unimpaired$Winter_3mon <- cleanup3MON(unimpaired$Winter_3mon)
	unimpaired$Winter_monthly <- cleanupMON(unimpaired$Winter_monthly)
	unimpaired$DaysmaxHY <- FreqAnalysis(unimpaired$HydroYear, c(3), unimpaired$Index)
	unimpaired$Daysmax6MON <- FreqAnalysis(unimpaired$Winter_6mon, c(3), unimpaired$Index)
	unimpaired$Daysmax3MON <- FreqAnalysis(unimpaired$Winter_3mon, c(3), unimpaired$Index)
	unimpaired$DaysmaxMON <- FreqAnalysisMonthly(unimpaired$Winter_monthly, c(3), unimpaired$Index)
	## add loop here if daysmax vector length >1 (UPDATES BELOW AS OF 7/27/15)
	unimpaired$PearsonIIIrollHY <- vector("list", 6)
	unimpaired$PearsonIIIroll6MON <- vector("list", 6)
	unimpaired$PearsonIIIroll3MON <- vector("list", 6)
	for(k in 1:1){
		unimpaired$PearsonIIIrollHY[[k]] <- FitqPearsonIIIroll(unimpaired$DaysmaxHY[[k]]$zoo$X3DayMaxQ_maf, movewidth=10, probs=c(0.01, 1/50, 1/20, 1/10), npoints=10)	
		unimpaired$PearsonIIIroll6MON[[k]] <- FitqPearsonIIIroll(unimpaired$Daysmax6MON[[k]]$zoo$X3DayMaxQ_maf, movewidth=10, probs=c(0.01, 1/50, 1/20, 1/10), npoints=10)
		unimpaired$PearsonIIIroll3MON[[k]] <- FitqPearsonIIIroll(unimpaired$Daysmax3MON[[k]]$zoo$X3DayMaxQ_maf, movewidth=10, probs=c(0.01, 1/50, 1/20, 1/10), npoints=10)
	}
	names(unimpaired$PearsonIIIrollHY) <- names(unimpaired$DaysmaxHY)
	names(unimpaired$PearsonIIIroll6MON) <- names(unimpaired$Daysmax6MON)
	names(unimpaired$PearsonIIIroll3MON) <- names(unimpaired$Daysmax3MON)
	unimpaired$PearsonIIIrollMON <- FitqPearsonIIIrollMonthly(unimpaired$DaysmaxMON$All$zoo, movewidth=10,probs=c(0.01, 1/50, 1/20, 1/10), npoints=10)
#	unimpaired$glsPIIIHY <- vector("list", 6)
#	unimpaired$glsPIII6MON <- vector("list", 6)
#	for(k in 1:6){
#		unimpaired$glsPIIIHY[[k]] <- glsPIII(unimpaired$PearsonIIIrollHY[[k]])
#		unimpaired$glsPIII6MON[[k]] <- glsPIII(unimpaired$PearsonIIIroll6MON[[k]])
#	}
#	names(unimpaired$glsPIIIHY) <- names(unimpaired$DaysmaxHY)
#	names(unimpaired$glsPIII6MON) <- names(unimpaired$Daysma6MON)
	unimpaired$glsPIIIHY <- glsPIII(unimpaired$PearsonIIIrollHY[[1]])
	unimpaired$glsPIII6MON <- glsPIII(unimpaired$PearsonIIIroll6MON[[1]])
	unimpaired$glsPIII3MON <- glsPIII(unimpaired$PearsonIIIroll3MON[[1]])
	unimpaired$glsPIIIMON <- glsPIIIMonthly(unimpaired$PearsonIIIrollMON)
	unimpaired$MonthlyThreshold <- MonthlyThresholdQ(unimpaired$Winter_monthly, prob=0.50)
	unimpaired$MonthlyRelative <- MonthlyRelativeTotalQ(unimpaired$Winter_monthly)
	
	unimpaired$glsMonthly <- vector("list",6)
#	for(k in 1:6){
#		unimpaired$glsMonthly[[k]] <- vector("list",6)
#		for (i in 1:6){
#			unimpaired$glsMonthly[[k]][[i]] <- glsMonthly(unimpaired$MonthlyThreshold[[k]][[i]])
#		}
#		names(unimpaired$glsMonthly[[k]]) <- names(unimpaired$MonthlyThreshold[[k]])
#	}
	
	unimpaired$glsMonthly[[1]] <- glsMonthly(unimpaired$MonthlyThreshold[[1]])
	unimpaired$glsMonthlyRelQ <- vector("list",6)
	unimpaired$glsMonthlyRelQ[[1]] <- glsMonthlyRelQ(unimpaired$MonthlyRelative[[1]])
	
	names(unimpaired$glsMonthly) <- names(unimpaired$MonthlyThreshold)
	names(unimpaired$glsMonthlyRelQ) <- names(unimpaired$MonthlyRelative)
	
	for(i in 1:4){
		glsHY[[i]]$slope[[z]] <-  unimpaired$glsPIIIHY[[i]][[1]][[2]]
		glsHY[[i]]$pvalue[[z]] <-  unimpaired$glsPIIIHY[[i]][[4]][[2]]
		glsHY[[i]]$gauge[[z]] <-  unimpaired$raw$site_no[[1]]
		gls6MON[[i]]$slope[[z]] <-  unimpaired$glsPIII6MON[[i]][[1]][[2]]
		gls6MON[[i]]$pvalue[[z]] <-  unimpaired$glsPIII6MON[[i]][[4]][[2]]
		gls6MON[[i]]$gauge[[z]] <-  unimpaired$raw$site_no[[1]]
		gls3MON[[i]]$slope[[z]] <-  unimpaired$glsPIII3MON[[i]][[1]][[2]]
		gls3MON[[i]]$pvalue[[z]] <-  unimpaired$glsPIII3MON[[i]][[4]][[2]]
		gls3MON[[i]]$gauge[[z]] <-  unimpaired$raw$site_no[[1]]
		for(k in 1:6){
			glsMON[[k]][[i]]$slope[[z]] <- unimpaired$glsPIIIMON[[k]][[i]][[1]][[2]]
			glsMON[[k]][[i]]$pvalue[[z]] <- unimpaired$glsPIIIMON[[k]][[i]][[4]][[2]]
			glsMON[[k]][[i]]$gauge[[z]] <- unimpaired$raw$site_no[[1]]
		}
	}
	for(k in 1:6){
		glsMONRelQ[[k]]$slope[[z]] <- unimpaired$glsMonthlyRelQ[[1]][[k]][[1]][[2]]
		glsMONRelQ[[k]]$pvalue[[z]] <- unimpaired$glsMonthlyRelQ[[1]][[k]][[4]][[2]]
		glsMONRelQ[[k]]$gauge[[z]] <- unimpaired$raw$site_no[[1]]
	}
}	
#names(unimpaired) <- unimpaired_g[1:2]

