# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

library(dplyr)
library(hydroTSM)
library(dataRetrieval)

SacV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_svi.txt")
SJV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_sji.txt")
yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")


load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\functions_5_13_16",".RData", sep=""))
load("C:\\Users\\tiffn_000\\Google Drive\\Kaweah\\data_for_thresholds.RData")

sites <- list.files("C:\\Users\\tiffn_000\\Google Drive\\Kaweah\\TXT\\", pattern=".csv")
sites <- unlist(strsplit(unlist(strsplit(sites,".csv")),"g"))
######################
spall <- sites[sites != ""]

for(i in 1:1){
	batchnum <- i
	spbatch <- spall[(15*i-14):(15*i)]
	spbatch <- spbatch[!is.na(spbatch)]
	
	spbatch_g <- spbatch
	
	
	txtgauges <- list.files("C:\\Users\\tiffn_000\\Google Drive\\Kaweah\\TXT\\")
	txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
	txtgauges <- txtgauges[txtgauges != ""]
	spbatch_g <- spbatch_g[which(spbatch_g %in% txtgauges)]
	
	
	spbatch <- vector("list", length(spbatch_g))
	for(z in 1:length(spbatch_g)){
		spbatch[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Google Drive\\Kaweah\\TXT\\","g",spbatch_g[[z]],".csv",sep=""), header=TRUE)
		spbatch[[z]]$raw$Date <- as.Date(spbatch[[z]]$raw$Date, "%Y-%m-%d")
		
		spbatch[[z]]$raw <- RemoveLeapDays(spbatch[[z]]$raw)
		
			spbatch[[z]]$Index$Valley <- "SJV"
			spbatch[[z]]$Index$Index <- yeartype_old$SJI
			spbatch[[z]]$Index$Year <- yeartype_old$Year
		
		###DATA PROCESSING
		spbatch[[z]]$prep <- prepdata(spbatch[[z]]$raw)
		spbatch[[z]]$Availability <- DataAvailability(spbatch[[z]]$raw)
		spbatch[[z]]$thresholds_maf <- thresholds(data_for_thresholds[[which(names(data_for_thresholds)==as.character(spbatch[[z]]$raw$site_no[[1]]))]])
		
		
		if(all(spbatch[[z]]$thresholds_maf==0)){
		} else {
			
			spbatch[[z]]$Winter_3mon <- Split3Winter(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholds_maf)
			spbatch[[z]]$Winter_6mon <- Split6Winter(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholds_maf)
			spbatch[[z]]$Winter_monthly <- SplitWinterMonthly(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholds_maf)
			spbatch[[z]]$HydroYear <- SplitHydroYear(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholds_maf)	
			spbatch[[z]]$HydroYear <- cleanupHY(spbatch[[z]]$HydroYear)
			spbatch[[z]]$Winter_6mon <- cleanup6MON(spbatch[[z]]$Winter_6mon)
			spbatch[[z]]$Winter_3mon <- cleanup3MON(spbatch[[z]]$Winter_3mon)			
		}
	}
	names(spbatch) <- spbatch_g
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\kaweah_full_record_spbatch_",batchnum,".RData",sep=""))
}

for(y in 90:99){	
	for(z in 1:1){
			batchnum <- z
			load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\kaweah_full_record_spbatch_",batchnum,".RData", sep=""))
	#		load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\functions_5_13_16",".RData", sep=""))
			
		thresholdchoice <- y
		thresholdtext <- paste(thresholdchoice,"%",sep="")
		
	#		for(k in 1:length(spbatch)){
	#			spbatch[[k]]$thresholds_maf <- thresholds(spbatch[[k]]$prep)
	#		}
			
			test_peakflows <- vector("list", length(spbatch))
			test_peakflowstats <- vector("list", length(spbatch))
			test_peakflowsummary <- vector("list", length(spbatch))
			test_peakflowmonthlystats <- vector("list", length(spbatch))
			test_peakflowsdf <- vector("list", length(spbatch))
			for(k in 1:length(spbatch)){
				test_peakflows[[k]] <- vector("list",1)
				names(test_peakflows[[k]]) <- c("peakflows")
				test_peakflows[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$HydroYear$Data))
				for(i in 1:length(spbatch[[k]]$HydroYear$Data)){
					test_peakflows[[k]]$peakflows[[i]] <- simplified_peakanalysis(input=spbatch[[k]]$HydroYear$Data[[i]],
							width=3, threshold=(eval(parse(text=paste("spbatch[[k]]$thresholds_maf$P",thresholdchoice,"maf/(86400*2.29568411e-5*1e-6)",sep="")))), 
							thresholdname=thresholdtext, mastertime="hy", Index=spbatch[[k]]$Index)

				}
				test_peakflowstats[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
				for(i in 1:length(test_peakflows[[k]]$peakflows))	{
					test_peakflowstats[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[2]]
				}
				test_peakflowsummary[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
				for(i in 1:length(test_peakflows[[k]]$peakflows))	{
					test_peakflowsummary[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[1]]
				}
				test_peakflowmonthlystats[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
				for(i in 1:length(test_peakflows[[k]]$peakflows))	{
					test_peakflowmonthlystats[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[3]]
				}
				test_peakflowsdf[[k]] <- vector("list",3)
				names(test_peakflowsdf[[k]]) <- c("pfstatsdf","pfsummarydf","pfmonthlystats")
				test_peakflowsdf[[k]]$pfstatsdf <- do.call(rbind.data.frame,test_peakflowstats[[k]])
				test_peakflowsdf[[k]]$pfsummarydf <- do.call(rbind.data.frame,test_peakflowsummary[[k]])
				test_peakflowsdf[[k]]$pfmonthlystats <- do.call(rbind.data.frame,test_peakflowmonthlystats[[k]])
				test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero <- rep(NA, length(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv))
				for(i in 1:length(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv)){
					if(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv[[i]]==0){
						test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 1
					}else{
						test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 0
					}
				}
				test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero_cumsum <- cumsum(test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero)	
			}
			names(test_peakflowsdf)<- names(spbatch)
			names(test_peakflows)<- names(spbatch)
			names(test_peakflowstats)<- names(spbatch)
			names(test_peakflowsummary)<- names(spbatch)
			names(test_peakflowmonthlystats)<- names(spbatch)
			test_split <- vector("list", length(spbatch))
			for(k in 1:length(spbatch)){
				test_split[[k]]<- peakflowanalysis_split(test_peakflowsdf[[k]]$pfmonthlystats)
			}
			names(test_split)<- names(spbatch)	
			
			test_peakflowmags_full <- vector("list", length(spbatch))
			for(k in 1:length(spbatch)){
				test_peakflowmags_full[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],1800)
				
			}
			names(test_peakflowmags_full) <- names(spbatch)
			
		test_peakflowmags_full_bind <- test_peakflowmags_full[[1]]
		for(k in 2:length(spbatch)){
			for(i in 1:6){
				for(l in 1:15){
					test_peakflowmags_full_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_full_bind[[i]][[l]],
							test_peakflowmags_full[[k]][[i]][[l]])
				}
			}
		}
			
	
	#		for(i in 1:15){
	#			write.csv(test_peakflowmags_full_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\all\\batch_",batchnum,"_",
	#							names(test_peakflowmags_full_bind$all)[[i]],".csv", sep=""))
	#			
	#			write.csv(test_peakflowmags_full_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\W\\batch_",batchnum,"_",
	#							names(test_peakflowmags_full_bind$W)[[i]],".csv", sep=""))
	#			
	#			write.csv(test_peakflowmags_full_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\AN\\batch_",batchnum,"_",
	#							names(test_peakflowmags_full_bind$AN)[[i]],".csv", sep=""))
	#			
	#			write.csv(test_peakflowmags_full_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\BN\\batch_",batchnum,"_",
	#							names(test_peakflowmags_full_bind$BN)[[i]],".csv", sep=""))
	#			
	#			write.csv(test_peakflowmags_full_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\D\\batch_",batchnum,"_",
	#							names(test_peakflowmags_full_bind$D)[[i]],".csv", sep=""))
	#			
	#			write.csv(test_peakflowmags_full_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\C\\batch_",batchnum,"_",
	#							names(test_peakflowmags_full_bind$C)[[i]],".csv", sep=""))
	#		}
	#		
	
			save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\kaweah_percentile_",thresholdchoice,"_spbatch_",batchnum,".RData", sep=""))
		
	}	
}
		
thresholdsvect <- c(90:99)	
gwmodel_data <- vector("list", length(thresholdsvect))
for(w in 1:length(thresholdsvect)){
	percnum <- w
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\kaweah_percentile_",thresholdsvect[[percnum]],"_spbatch_",1,".RData", sep=""))
	gwmodel_data[[percnum]] <- test_split
}
names(gwmodel_data)<- thresholdsvect
save(gwmodel_data, file="C:/Users/tiffn_000/Google Drive/Kaweah/vol_data_for_kaweah_gwmodel.RData")

