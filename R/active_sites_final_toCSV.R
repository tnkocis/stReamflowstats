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
yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")



######################
spall <- unique(impairmentsdf$downstreamgauge)

for(i in 2:7){
	batchnum <- i
	spbatch <- spall[(15*i-14):(15*i)]
	spbatch <- spbatch[!is.na(spbatch)]
	
	spbatch_g <- as.numeric(spbatch)
	
	
	txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
	txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
	txtgauges <- txtgauges[txtgauges != ""]
	spbatch_g <- spbatch_g[which(spbatch_g %in% txtgauges)]
	
	
	spbatch <- vector("list", length(spbatch_g))
	for(z in 1:length(spbatch_g)){
		spbatch[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",spbatch_g[[z]],".csv",sep=""), header=TRUE)
		spbatch[[z]]$raw$Date <- as.Date(spbatch[[z]]$raw$Date, "%Y-%m-%d")
		
		spbatch[[z]]$raw <- RemoveLeapDays(spbatch[[z]]$raw)
		
		if(as.numeric(spbatch[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
			spbatch[[z]]$Index$Valley <- "SacV"
			spbatch[[z]]$Index$Index <- yeartype_old$SVI
			spbatch[[z]]$Index$Year <- yeartype_old$Year
		} else if(as.numeric(spbatch[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
			spbatch[[z]]$Index$Valley <- "SJV"
			spbatch[[z]]$Index$Index <- yeartype_old$SJI
			spbatch[[z]]$Index$Year <- yeartype_old$Year
		} else {
			spbatch[[z]]$Index$Valley <- "ERROR"
			print(paste("Error",spbatch[[z]]$raw$site_no[[1]]))
		}
		
		###DATA PROCESSING
		spbatch[[z]]$prep <- prepdata(spbatch[[z]]$raw)
		spbatch[[z]]$Availability <- DataAvailability(spbatch[[z]]$raw)
		spbatch[[z]]$dams <- damsearch(spbatch[[z]], impairmentsdf)
		spbatch[[z]]$thresholdsdams_maf <- thresholdsdams(spbatch[[z]]$prep, spbatch[[z]]$dams)
		
		
		if(all(spbatch[[z]]$thresholdsdams_maf==0)){
		} else {
			
			spbatch[[z]]$Winter_3mon <- Split3Winter(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholdsdams_maf)
			spbatch[[z]]$Winter_6mon <- Split6Winter(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholdsdams_maf)
			spbatch[[z]]$Winter_monthly <- SplitWinterMonthly(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholdsdams_maf)
			spbatch[[z]]$HydroYear <- SplitHydroYear(spbatch[[z]]$prep, spbatch[[z]]$Index, spbatch[[z]]$thresholdsdams_maf)	
			spbatch[[z]]$HydroYear <- cleanupHY(spbatch[[z]]$HydroYear)
			spbatch[[z]]$Winter_6mon <- cleanup6MON(spbatch[[z]]$Winter_6mon)
			spbatch[[z]]$Winter_3mon <- cleanup3MON(spbatch[[z]]$Winter_3mon)
			spbatch[[z]]$thresholdsdams3mon_maf <- thresholdsdams3mon(spbatch[[z]]$Winter_3mon, spbatch[[z]]$dams)
			
		}
	}
	names(spbatch) <- spbatch_g
	
	spbatch_peakflows <- vector("list", length(spbatch))
	spbatch_peakflowstats <- vector("list", length(spbatch))
	spbatch_peakflowsummary <- vector("list", length(spbatch))
	spbatch_peakflowmonthlystats <- vector("list", length(spbatch))
	spbatch_peakflowsdf <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		spbatch_peakflows[[k]] <- vector("list",1)
		names(spbatch_peakflows[[k]]) <- c("peakflows")
		spbatch_peakflows[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$HydroYear$Data))
		for(i in 1:length(spbatch[[k]]$HydroYear$Data)){
			spbatch_peakflows[[k]]$peakflows[[i]] <- peakanalysis(input=spbatch[[k]]$HydroYear$Data[[i]],
					width=3, threshold=(spbatch[[k]]$thresholdsdams_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="hy", Index=spbatch[[k]]$Index)
		}
		spbatch_peakflowstats[[k]] <- vector("list",length(spbatch_peakflows[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows[[k]]$peakflows))	{
			spbatch_peakflowstats[[k]][[i]] <- spbatch_peakflows[[k]]$peakflows[[i]][[2]]
		}
		spbatch_peakflowsummary[[k]] <- vector("list",length(spbatch_peakflows[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows[[k]]$peakflows))	{
			spbatch_peakflowsummary[[k]][[i]] <- spbatch_peakflows[[k]]$peakflows[[i]][[1]]
		}
		spbatch_peakflowmonthlystats[[k]] <- vector("list",length(spbatch_peakflows[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows[[k]]$peakflows))	{
			spbatch_peakflowmonthlystats[[k]][[i]] <- spbatch_peakflows[[k]]$peakflows[[i]][[3]]
		}
		spbatch_peakflowsdf[[k]] <- vector("list",3)
		names(spbatch_peakflowsdf[[k]]) <- c("pfstatsdf","pfsummarydf","pfmonthlystats")
		spbatch_peakflowsdf[[k]]$pfstatsdf <- do.call(rbind.data.frame,spbatch_peakflowstats[[k]])
		spbatch_peakflowsdf[[k]]$pfsummarydf <- do.call(rbind.data.frame,spbatch_peakflowsummary[[k]])
		spbatch_peakflowsdf[[k]]$pfmonthlystats <- do.call(rbind.data.frame,spbatch_peakflowmonthlystats[[k]])
		spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero <- rep(NA, length(spbatch_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv))
		for(i in 1:length(spbatch_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv)){
			if(spbatch_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv[[i]]==0){
				spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 1
			}else{
				spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 0
			}
		}
		spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero_cumsum <- cumsum(spbatch_peakflowsdf[[k]]$pfstatsdf$volday_is_zero)	
	}
	names(spbatch_peakflowsdf)<- names(spbatch)
	names(spbatch_peakflows)<- names(spbatch)
	names(spbatch_peakflowstats)<- names(spbatch)
	names(spbatch_peakflowsummary)<- names(spbatch)
	names(spbatch_peakflowmonthlystats)<- names(spbatch)
	
	for(k in 1:length(spbatch)){
		damsunique <- data.frame(year=unique(spbatch[[k]]$dams$YEAR_BUILT), totalcapacity_yr = rep(NA,length(unique(spbatch[[k]]$dams$YEAR_BUILT))))
		for(i in 1:length(damsunique$year)){
			damsunique$totalcapacity_yr[[i]] <- sum(spbatch[[k]]$dams$CAPACITY__[which(spbatch[[k]]$dams$YEAR_BUILT==damsunique$year[[i]])])	
		}
		spbatch[[k]]$damsunique <- damsunique
	}
	
	active_basins <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\active_basins.csv", header=TRUE)
	active_basins <- active_basins[which(active_basins$site_no%in%unique(impairmentsdf$downstreamgauge)),]
	impairments_basins <- merge(impairmentsdf,active_basins, by.x="downstreamgauge", by.y="site_no", all=TRUE, sort=FALSE)
	write.csv(impairments_basins, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\impairments.csv")
	impairments_tulare<- impairments_basins[which(impairments_basins$basin=="tulare"),]
	impairments_sac <- impairments_basins[which(impairments_basins$basin=="sac"),]
	impairments_sj <- impairments_basins[which(impairments_basins$basin=="sj"),]
	unimp_gauges <- impairments_basins[which(is.na(impairments_basins$DSTR_GAUGE)),]
	
	analysis_year <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\analysis_years.csv", header=TRUE)
	analysis_year <- unique(analysis_year)
	
	
	spbatch_peakflows3 <- vector("list", length(spbatch))
	spbatch_peakflowstats3 <- vector("list", length(spbatch))
	spbatch_peakflowsummary3 <- vector("list", length(spbatch))
	spbatch_peakflowmonthlystats3 <- vector("list", length(spbatch))
	spbatch_peakflowsdf3 <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		spbatch_peakflows3[[k]] <- vector("list",1)
		names(spbatch_peakflows3[[k]]) <- c("peakflows")
		spbatch_peakflows3[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$Winter_6mon$Data))
		for(i in 1:length(spbatch[[k]]$Winter_6mon$Data)){
			spbatch_peakflows3[[k]]$peakflows[[i]] <- peakanalysis(input=spbatch[[k]]$Winter_6mon$Data[[i]],
					width=3, threshold=(spbatch[[k]]$thresholdsdams_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="3mon", Index=spbatch[[k]]$Index)
		}
		spbatch_peakflowstats3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowstats3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[2]]
		}
		spbatch_peakflowsummary3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowsummary3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[1]]
		}
		spbatch_peakflowmonthlystats3[[k]] <- vector("list",length(spbatch_peakflows3[[k]]$peakflows))
		for(i in 1:length(spbatch_peakflows3[[k]]$peakflows))	{
			spbatch_peakflowmonthlystats3[[k]][[i]] <- spbatch_peakflows3[[k]]$peakflows[[i]][[3]]
		}
		spbatch_peakflowsdf3[[k]] <- vector("list",3)
		names(spbatch_peakflowsdf3[[k]]) <- c("pfstatsdf3","pfsummarydf3","pfmonthlystats3")
		spbatch_peakflowsdf3[[k]]$pfstatsdf3 <- do.call(rbind.data.frame,spbatch_peakflowstats3[[k]])
		spbatch_peakflowsdf3[[k]]$pfsummarydf3 <- do.call(rbind.data.frame,spbatch_peakflowsummary3[[k]])
		spbatch_peakflowsdf3[[k]]$pfmonthlystats3 <- do.call(rbind.data.frame,spbatch_peakflowmonthlystats3[[k]])
		spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero <- rep(NA, length(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv))
		for(i in 1:length(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv)){
			if(spbatch_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv[[i]]==0){
				spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero[[i]] <- 1
			}else{
				spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero[[i]] <- 0
			}
		}
		spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero_cumsum <- cumsum(spbatch_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero)	
	}
	names(spbatch_peakflowsdf3)<- names(spbatch)
	names(spbatch_peakflows3)<- names(spbatch)
	names(spbatch_peakflowstats3)<- names(spbatch)
	names(spbatch_peakflowsummary3)<- names(spbatch)
	names(spbatch_peakflowmonthlystats3)<- names(spbatch)
	
	spbatch_pktrends3mon <- vector("list",length(spbatch))
	for(i in 1:length(spbatch_peakflowsdf3)){
		spbatch_pktrends3mon[[i]] <- peakflowtrends(spbatch_peakflowsdf3[[i]]$pfstatsdf3, names(spbatch_peakflowsdf3)[[i]],analysis_year$analysis_year[which(spbatch[[i]]$raw$site_no[[1]]==analysis_year$downstreamgauge)] )
	}
	names(spbatch_pktrends3mon)<-names(spbatch_peakflowsdf3)
	
	spbatch_pktrendshy <- vector("list",length(spbatch))
	for(i in 1:length(spbatch_peakflowsdf)){
		spbatch_pktrendshy[[i]] <- peakflowtrends(spbatch_peakflowsdf[[i]]$pfstatsdf, names(spbatch_peakflowsdf)[[i]],analysis_year$analysis_year[which(spbatch[[i]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
	}
	names(spbatch_pktrendshy)<-names(spbatch_peakflowsdf)
	
	for(i in 1:length(spbatch_pktrends3mon)){
		write.csv(spbatch_pktrends3mon[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\mon3\\pktrends3mon_dam",names(spbatch_pktrends3mon)[[i]],".csv",sep=""))
		write.csv(spbatch_pktrends3mon[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\mon3\\pktrends3mon_full",names(spbatch_pktrends3mon)[[i]],".csv",sep=""))
		
	}
	
	for(i in 1:length(spbatch_pktrendshy)){
		write.csv(spbatch_pktrendshy[[i]][[1]] , file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\full\\pktrendshy_dam",names(spbatch_pktrendshy)[[i]],".csv",sep=""))
		write.csv(spbatch_pktrendshy[[i]][[2]] , file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\full\\pktrendshy_full",names(spbatch_pktrendshy)[[i]],".csv",sep=""))
		
	}
	
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\spbatch_",batchnum,".RData", sep=""))
}

for(i in 1:7){
	batchnum <- i
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\spbatch_",batchnum,".RData", sep=""))
	
	
	spbatch_peakflowmags_hy <- vector("list", length(spbatch_peakflowsdf))
	for(i in 1:length(spbatch_peakflowsdf)){
		spbatch_peakflowmags_hy[[i]] <- peakflowmags(spbatch_peakflowsdf[[i]]$pfstatsdf, names(spbatch_peakflowsdf)[[i]],1800)
	}
	spbatch_peakflowmagsdf_hy <- do.call(rbind.data.frame, spbatch_peakflowmags_hy)
	write.csv(spbatch_peakflowmagsdf_hy, file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\hy\\spbatch_peakflowmagsdf_hy_",batchnum,".csv",sep=""))
	
	
	
	spbatch_peakflowmags_3mon <- vector("list", length(spbatch_peakflowsdf3))
	for(i in 1:length(spbatch_peakflowsdf3)){
		spbatch_peakflowmags_3mon[[i]] <- peakflowmags(spbatch_peakflowsdf3[[i]]$pfstatsdf3, names(spbatch_peakflowsdf3)[[i]],1800)
	}
	spbatch_peakflowmagsdf_3mon <- do.call(rbind.data.frame, spbatch_peakflowmags_3mon)
	write.csv(spbatch_peakflowmagsdf_3mon, file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\mon3\\spbatch_peakflowmagsdf_mon3_",batchnum,".csv",sep=""))
	
	
	
	spbatch_peakflowmags_hydams <- vector("list", length(spbatch_peakflowsdf))
	for(i in 1:length(spbatch_peakflowsdf)){
		spbatch_peakflowmags_hydams[[i]] <- peakflowmags(spbatch_peakflowsdf[[i]]$pfstatsdf, names(spbatch_peakflowsdf)[[i]],year=
						analysis_year$analysis_year[which(spbatch[[i]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
	}
	spbatch_peakflowmagsdf_hydams <- do.call(rbind.data.frame, spbatch_peakflowmags_hydams)
	write.csv(spbatch_peakflowmagsdf_hydams, file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\hy\\spbatch_peakflowmagsdf_hy_dams_",batchnum,".csv",sep=""))
	
	
	spbatch_peakflowmags_3mondams <- vector("list", length(spbatch_peakflowsdf3))
	for(i in 1:length(spbatch_peakflowsdf3)){
		spbatch_peakflowmags_3mondams[[i]] <- peakflowmags(spbatch_peakflowsdf3[[i]]$pfstatsdf3, names(spbatch_peakflowsdf3)[[i]],year=
						analysis_year$analysis_year[which(spbatch[[i]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
	}
	spbatch_peakflowmagsdf_3mondams <- do.call(rbind.data.frame, spbatch_peakflowmags_3mondams)
	write.csv(spbatch_peakflowmagsdf_3mondams, file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\mon3\\spbatch_peakflowmagsdf_mon3_dams_",batchnum,".csv",sep=""))
	
	
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\spbatch_",batchnum,".RData", sep=""))
}


pkmagshy_full <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\hy")
pkmagshy_full_list <- vector("list", length(pkmagshy_full))
for(i in 1:length(pkmagshy_full)){
	pkmagshy_full_list[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\hy\\",pkmagshy_full[[i]],sep=""), header=TRUE, sep=",")
}
pkmagshy_full_df <- do.call(rbind.data.frame,pkmagshy_full_list)
write.csv(pkmagshy_full_df,"C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_hy_full.csv")

pkmagshy_dams <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\hy")
pkmagshy_dams_list <- vector("list", length(pkmagshy_dams))
for(i in 1:length(pkmagshy_dams)){
	pkmagshy_dams_list[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\hy\\",pkmagshy_dams[[i]],sep=""), header=TRUE, sep=",")
}
pkmagshy_dams_df <- do.call(rbind.data.frame,pkmagshy_dams_list)
write.csv(pkmagshy_dams_df,"C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_hy_dams.csv")

pkmagsmon3_full <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\mon3")
pkmagsmon3_full_list <- vector("list", length(pkmagsmon3_full))
for(i in 1:length(pkmagsmon3_full)){
	pkmagsmon3_full_list[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\full\\mon3\\",pkmagsmon3_full[[i]],sep=""), header=TRUE, sep=",")
}
pkmagsmon3_full_df <- do.call(rbind.data.frame,pkmagsmon3_full_list)
write.csv(pkmagsmon3_full_df,"C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_mon3_full.csv")

pkmagsmon3_dams <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\mon3")
pkmagsmon3_dams_list <- vector("list", length(pkmagsmon3_dams))
for(i in 1:length(pkmagsmon3_dams)){
	pkmagsmon3_dams_list[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\dams\\mon3\\",pkmagsmon3_dams[[i]],sep=""), header=TRUE, sep=",")
}
pkmagsmon3_dams_df <- do.call(rbind.data.frame,pkmagsmon3_dams_list)
write.csv(pkmagsmon3_dams_df,"C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_mon3_dams.csv")




active_basins <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\active_basins.csv", header=TRUE)
active_basins <- active_basins[which(active_basins$site_no%in%unique(impairmentsdf$downstreamgauge)),]
impairments_basins <- merge(impairmentsdf,active_basins, by.x="downstreamgauge", by.y="site_no", all=TRUE, sort=FALSE)
write.csv(impairments_basins, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\impairments.csv")
impairments_tulare<- impairments_basins[which(impairments_basins$basin=="tulare"),]
impairments_sac <- impairments_basins[which(impairments_basins$basin=="sac"),]
impairments_sj <- impairments_basins[which(impairments_basins$basin=="sj"),]
unimp_gauges <- impairments_basins[which(is.na(impairments_basins$DSTR_GAUGE)),]

analysis_year <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\analysis_years.csv", header=TRUE)
analysis_year <- unique(analysis_year)



rlmnvol <- rollmean(spbatch_peakflowsdf[[3]]$pfstatsdf$TotVolAbv_acft,5, na.pad=TRUE)
pval <- summary(lm(rlmnvol[which(spbatch_peakflowsdf[[3]]$pfstatsdf$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))] ~ spbatch_peakflowsdf[[3]]$pfstatsdf$year[which(spbatch_peakflowsdf[[3]]$pfstatsdf$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))]) )$coefficients[2,4]
pval <- data.frame(pval=round(pval,3))
qplot(spbatch_peakflowsdf[[3]]$pfstatsdf$year,rlmnvol, geom="line")+geom_smooth(method="lm",se=FALSE)+
		geom_smooth(aes(x=spbatch_peakflowsdf[[3]]$pfstatsdf$year[which(spbatch_peakflowsdf[[3]]$pfstatsdf$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))], y=rlmnvol[which(spbatch_peakflowsdf[[3]]$pfstatsdf$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))]),
				method="lm",se=FALSE, color="red")+
		xlab("Year")+
		ylab("Volume Above 90% (acft)")+
		ggtitle(paste(spbatch[[3]]$raw$site_no[[1]],", ", analysis_year$basin[which(analysis_year$downstreamgauge==spbatch[[3]]$raw$site_no[[1]])], ", pval=", pval$pval, sep=""))

rlmnvol <- rollmean(spbatch_peakflowsdf3[[3]]$pfstatsdf3$TotVolAbv_acft,1, na.pad=TRUE)
pval <- summary(lm(rlmnvol[which(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))] ~ spbatch_peakflowsdf3[[3]]$pfstatsdf3$year[which(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))]) )$coefficients[2,4]
pval <- data.frame(pval=round(pval,3))
qplot(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year,rlmnvol, geom="point")+geom_smooth(method="lm",se=FALSE)+
		geom_smooth(aes(x=spbatch_peakflowsdf3[[3]]$pfstatsdf3$year[which(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))], y=rlmnvol[which(spbatch_peakflowsdf3[[3]]$pfstatsdf3$year>(analysis_year$analysis_year[which(spbatch[[3]]$raw$site_no[[1]]==analysis_year$downstreamgauge)]))]),
				method="lm",se=FALSE, color="red")+
		xlab("Year")+
		ylab("Volume Above 90% (acft)")+
		ggtitle(paste(spbatch[[3]]$raw$site_no[[1]],", ", analysis_year$basin[which(analysis_year$downstreamgauge==spbatch[[3]]$raw$site_no[[1]])], ", pval=", pval$pval, sep=""))


trendvol <- read.csv(file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_totvol.csv", header=TRUE)
trenddays<- read.csv(file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_totdays.csv", header=TRUE)
trendnum <- read.csv(file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_numpks.csv", header=TRUE)
mags <- read.csv(file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\mags\\grouped\\pkmags_mon3_dams.csv", header=TRUE)

merged <- merge(mags,trendvol, by="gauge")
merged <- merge(merged,trenddays, by="gauge")
merged <- merge(merged,trendnum, by="gauge")
write.csv(merged, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\merged_3mon_dams.csv")



testarea <- hypeakplotsstats(spbatch_peakflowsdf[[3]]$pfmonthlystats, names(spbatch_peakflowsdf)[[3]])











for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\post_agu_spbatch_",batchnum,".RData", sep=""))
	for(k in 1:length(spbatch)){
		spbatch[[k]]$thresholds_maf <- thresholds(spbatch[[k]]$prep)
	}
	
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
					width=3, threshold=(spbatch[[k]]$thresholds_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="hy", Index=spbatch[[k]]$Index)
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
	
	test_peakflowmags_1980 <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_1980[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],1980)
		
	}
	names(test_peakflowmags_1980) <- names(spbatch)
	
	test_peakflowmags_dams <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_dams[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],year=
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
		
	}
	names(test_peakflowmags_dams) <- names(spbatch)
	
	test_peakflowmags_full_bind <- test_peakflowmags_full[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_full_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_full_bind[[i]][[l]],
						test_peakflowmags_full[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_1980_bind <- test_peakflowmags_1980[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_1980_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_1980_bind[[i]][[l]],
						test_peakflowmags_1980[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_dams_bind <- test_peakflowmags_dams[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_dams_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_dams_bind[[i]][[l]],
						test_peakflowmags_dams[[k]][[i]][[l]])
			}
		}
	}
	
	
	for(i in 1:15){
		write.csv(test_peakflowmags_full_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\full\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\full\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\full\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\full\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\full\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\full\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_dams_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\dams\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\dams\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\dams\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\dams\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\dams\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\dams\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_1980_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\1980\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\1980\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\1980\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\1980\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\1980\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_mags\\1980\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$C)[[i]],".csv", sep=""))
	}


	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\post_agu_spbatch_",batchnum,".RData", sep=""))
}






### #######
for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\post_agu_spbatch_",batchnum,".RData", sep=""))
		
	test_peakflowtrends <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowtrends[[k]] <- vector("list", 1)
		names(test_peakflowtrends[[k]]) <- names(test_split[[k]])[[1]]
		for(i in 1:1){
			for(l in 1:15){
				test_peakflowtrends[[k]][[i]][[l]] <- simplified_peakflowtrends(test_split[[k]][[i]][[l]], names(test_split)[[k]],
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
			}
			names(test_peakflowtrends[[k]][[i]]) <- names(test_split[[k]][[i]])
		}
	}
	names(test_peakflowtrends) <- names(spbatch)
	
	
	test_peakflowtrends_bind <- test_peakflowtrends[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:1){
			for(l in 1:15){
				for(m in 1:3){
					test_peakflowtrends_bind[[i]][[l]][[m]] <- rbind.data.frame(test_peakflowtrends_bind[[i]][[l]][[m]],
							test_peakflowtrends[[k]][[i]][[l]][[m]])	
				}
			}
		}
	}
	
	for(i in 1:15){
		write.csv(test_peakflowtrends_bind$all[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_trends\\all\\dams\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[1]],".csv", sep=""))

		write.csv(test_peakflowtrends_bind$all[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_trends\\all\\full\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[2]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_agu\\peakflow_trends\\all\\1980\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[3]],".csv", sep=""))
		
	}

	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\post_agu_spbatch_",batchnum,".RData", sep=""))
}











################preimp
#####################
#########################

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\post_agu_spbatch_",batchnum,".RData", sep=""))
	for(k in 1:length(spbatch)){
		spbatch[[k]]$thresholds_preimp_maf <- thresholds_pre_imp(spbatch[[k]]$prep, spbatch[[k]]$dams)
	}
	
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
					width=3, threshold=(spbatch[[k]]$thresholds_preimp_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="hy", Index=spbatch[[k]]$Index)
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
	
	test_peakflowmags_1980 <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_1980[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],1980)
		
	}
	names(test_peakflowmags_1980) <- names(spbatch)
	
	test_peakflowmags_dams <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_dams[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],year=
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
		
	}
	names(test_peakflowmags_dams) <- names(spbatch)
	
	test_peakflowmags_full_bind <- test_peakflowmags_full[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_full_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_full_bind[[i]][[l]],
						test_peakflowmags_full[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_1980_bind <- test_peakflowmags_1980[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_1980_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_1980_bind[[i]][[l]],
						test_peakflowmags_1980[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_dams_bind <- test_peakflowmags_dams[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_dams_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_dams_bind[[i]][[l]],
						test_peakflowmags_dams[[k]][[i]][[l]])
			}
		}
	}
	
	
	for(i in 1:15){
		write.csv(test_peakflowmags_full_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\full\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\full\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\full\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\full\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\full\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\full\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_dams_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\dams\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\dams\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\dams\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\dams\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\dams\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\dams\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_1980_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\1980\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\1980\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\1980\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\1980\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\1980\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_mags\\1980\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$C)[[i]],".csv", sep=""))
	}
	
	
	test_peakflowtrends <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowtrends[[k]] <- vector("list", 1)
		names(test_peakflowtrends[[k]]) <- names(test_split[[k]])[[1]]
		for(i in 1:1){
			for(l in 1:15){
				test_peakflowtrends[[k]][[i]][[l]] <- simplified_peakflowtrends(test_split[[k]][[i]][[l]], names(test_split)[[k]],
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
			}
			names(test_peakflowtrends[[k]][[i]]) <- names(test_split[[k]][[i]])
		}
	}
	names(test_peakflowtrends) <- names(spbatch)
	
	
	test_peakflowtrends_bind <- test_peakflowtrends[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:1){
			for(l in 1:15){
				for(m in 1:3){
					test_peakflowtrends_bind[[i]][[l]][[m]] <- rbind.data.frame(test_peakflowtrends_bind[[i]][[l]][[m]],
							test_peakflowtrends[[k]][[i]][[l]][[m]])	
				}
			}
		}
	}
	
	for(i in 1:15){
		write.csv(test_peakflowtrends_bind$all[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_trends\\all\\dams\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[1]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_trends\\all\\full\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[2]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\pre_impairment\\peakflow_trends\\all\\1980\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[3]],".csv", sep=""))
		
	}
	
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\pre_impariment_spbatch_",batchnum,".RData", sep=""))
}








################postimp
#####################
#########################

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\post_agu_spbatch_",batchnum,".RData", sep=""))

	
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
					width=3, threshold=(spbatch[[k]]$thresholdsdams_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="hy", Index=spbatch[[k]]$Index)
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
	
	test_peakflowmags_1980 <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_1980[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],1980)
		
	}
	names(test_peakflowmags_1980) <- names(spbatch)
	
	test_peakflowmags_dams <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_dams[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],year=
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
		
	}
	names(test_peakflowmags_dams) <- names(spbatch)
	
	test_peakflowmags_full_bind <- test_peakflowmags_full[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_full_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_full_bind[[i]][[l]],
						test_peakflowmags_full[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_1980_bind <- test_peakflowmags_1980[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_1980_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_1980_bind[[i]][[l]],
						test_peakflowmags_1980[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_dams_bind <- test_peakflowmags_dams[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_dams_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_dams_bind[[i]][[l]],
						test_peakflowmags_dams[[k]][[i]][[l]])
			}
		}
	}
	
	
	for(i in 1:15){
		write.csv(test_peakflowmags_full_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\full\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\full\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\full\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\full\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\full\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\full\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_dams_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\dams\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\dams\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\dams\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\dams\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\dams\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\dams\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_1980_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\1980\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\1980\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\1980\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\1980\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\1980\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_mags\\1980\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$C)[[i]],".csv", sep=""))
	}
	
	
	test_peakflowtrends <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowtrends[[k]] <- vector("list", 1)
		names(test_peakflowtrends[[k]]) <- names(test_split[[k]])[[1]]
		for(i in 1:1){
			for(l in 1:15){
				test_peakflowtrends[[k]][[i]][[l]] <- simplified_peakflowtrends(test_split[[k]][[i]][[l]], names(test_split)[[k]],
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
			}
			names(test_peakflowtrends[[k]][[i]]) <- names(test_split[[k]][[i]])
		}
	}
	names(test_peakflowtrends) <- names(spbatch)
	
	
	test_peakflowtrends_bind <- test_peakflowtrends[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:1){
			for(l in 1:15){
				for(m in 1:3){
					test_peakflowtrends_bind[[i]][[l]][[m]] <- rbind.data.frame(test_peakflowtrends_bind[[i]][[l]][[m]],
							test_peakflowtrends[[k]][[i]][[l]][[m]])	
				}
			}
		}
	}
	
	for(i in 1:15){
		write.csv(test_peakflowtrends_bind$all[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_trends\\all\\dams\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[1]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_trends\\all\\full\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[2]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\post_impairment\\peakflow_trends\\all\\1980\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[3]],".csv", sep=""))
		
	}
	
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\post_impariment_spbatch_",batchnum,".RData", sep=""))
}






################full record
#####################
#########################

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\post_agu_spbatch_",batchnum,".RData", sep=""))
	
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
					width=3, threshold=(spbatch[[k]]$thresholds_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%", mastertime="hy", Index=spbatch[[k]]$Index)
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
	
	test_peakflowmags_1980 <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_1980[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],1980)
		
	}
	names(test_peakflowmags_1980) <- names(spbatch)
	
	test_peakflowmags_dams <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_dams[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],year=
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
		
	}
	names(test_peakflowmags_dams) <- names(spbatch)
	
	test_peakflowmags_full_bind <- test_peakflowmags_full[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_full_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_full_bind[[i]][[l]],
						test_peakflowmags_full[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_1980_bind <- test_peakflowmags_1980[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_1980_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_1980_bind[[i]][[l]],
						test_peakflowmags_1980[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_dams_bind <- test_peakflowmags_dams[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_dams_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_dams_bind[[i]][[l]],
						test_peakflowmags_dams[[k]][[i]][[l]])
			}
		}
	}
	
	
	for(i in 1:15){
		write.csv(test_peakflowmags_full_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\full\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\full\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\full\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\full\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\full\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\full\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_dams_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\dams\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\dams\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\dams\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\dams\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\dams\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\dams\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_1980_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\1980\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\1980\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\1980\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\1980\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\1980\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_mags\\1980\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$C)[[i]],".csv", sep=""))
	}
	
	
	test_peakflowtrends <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowtrends[[k]] <- vector("list", 1)
		names(test_peakflowtrends[[k]]) <- names(test_split[[k]])[[1]]
		for(i in 1:1){
			for(l in 1:15){
				test_peakflowtrends[[k]][[i]][[l]] <- simplified_peakflowtrends(test_split[[k]][[i]][[l]], names(test_split)[[k]],
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
			}
			names(test_peakflowtrends[[k]][[i]]) <- names(test_split[[k]][[i]])
		}
	}
	names(test_peakflowtrends) <- names(spbatch)
	
	
	test_peakflowtrends_bind <- test_peakflowtrends[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:1){
			for(l in 1:15){
				for(m in 1:3){
					test_peakflowtrends_bind[[i]][[l]][[m]] <- rbind.data.frame(test_peakflowtrends_bind[[i]][[l]][[m]],
							test_peakflowtrends[[k]][[i]][[l]][[m]])	
				}
			}
		}
	}
	
	for(i in 1:15){
		write.csv(test_peakflowtrends_bind$all[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_trends\\all\\dams\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[1]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_trends\\all\\full\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[2]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_trends\\all\\1980\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[3]],".csv", sep=""))
		
	}
	
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
}



























#durations stats#
test_durations <- vector("list",length(spbatch[[1]]$HydroYear$Data)) 
for(i in 1:length(spbatch[[1]]$HydroYear$Data)){
	test_durations[[i]] <- durations_stats(spbatch[[1]]$HydroYear$Data[[i]], width=3, threshold=(spbatch[[1]]$thresholdsdams_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
					thresholdname="90%",Index=spbatch[[1]]$Index, gauge=names(spbatch)[[1]])
}
test_durationsdfbind <- vector("list",16)
test_durationsdfbind <- test_durations[[1]]
for(i in 2:length(test_durations)){
	for(j in 1:16){
		test_durationsdfbind[[j]] <- rbind.data.frame(test_durationsdfbind[[j]],test_durations[[i]][[j]])
	}
}
library(ggplot2)
hydf <- test_durationsdfbind[[1]][which(test_durationsdfbind[[1]]$period=="hy"),]
test_durationsplot <- ggplot(hydf, aes(sthyyear))+
		geom_ribbon(aes(ymin=min,ymax=max),fill="grey80")+
		geom_ribbon(aes(ymin=q25,ymax=q75),fill="grey60")+
		geom_line(aes(y=median))
ggsave(filename="C:\\Users\\tiffn_000\\Desktop\\test_durations\\testdurationsplot1.pdf",plot=test_durationsplot,width=11,height=8.5,units="in",dpi=300)


hyevents <- test_durationsdfbind$hyevents
hyevents$stdecade <- NA
for(i in 1:length(hyevents$year)){
	if(hyevents$year[[i]]<1910){
		hyevents$stdecade[[i]] <-1900
	}else if(hyevents$year[[i]]<1920&hyevents$year[[i]]>=1910){
		hyevents$stdecade[[i]] <-1910
	}else if(hyevents$year[[i]]<1930&hyevents$year[[i]]>=1920){
		hyevents$stdecade[[i]] <-1920
	}else if(hyevents$year[[i]]<1940&hyevents$year[[i]]>=1930){
		hyevents$stdecade[[i]] <-1930
	}else if(hyevents$year[[i]]<1950&hyevents$year[[i]]>=1940){
		hyevents$stdecade[[i]] <-1940
	}else if(hyevents$year[[i]]<1960&hyevents$year[[i]]>=1950){
		hyevents$stdecade[[i]] <-1950
	}else if(hyevents$year[[i]]<1970&hyevents$year[[i]]>=1960){
		hyevents$stdecade[[i]] <-1960
	}else if(hyevents$year[[i]]<1980&hyevents$year[[i]]>=1970){
		hyevents$stdecade[[i]] <-1970
	}else if(hyevents$year[[i]]<1990&hyevents$year[[i]]>=1980){
		hyevents$stdecade[[i]] <-1980
	}else if(hyevents$year[[i]]<2000&hyevents$year[[i]]>=1990){
		hyevents$stdecade[[i]] <-1990
	}else if(hyevents$year[[i]]<2010&hyevents$year[[i]]>=2000){
		hyevents$stdecade[[i]] <-2000
	}else if(hyevents$year[[i]]<2020&hyevents$year[[i]]>=2010){
		hyevents$stdecade[[i]] <-2010
	}
}

decadalviolin <- ggplot(hyevents,aes(factor(stdecade),log(events))) +geom_violin()
ggsave(filename="C:\\Users\\tiffn_000\\Desktop\\test_durations\\decadalviolin3.pdf",plot=decadalviolin,width=11,height=8.5,units="in",dpi=300)


monthlyfullrecord <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\figures\\SGMA_meeting\\monthly_fullrecord_numbers.csv")
monthlyfullrecord$month <- factor(monthlyfullrecord$month, levels=c("nov","dec","jan","feb","mar","apr"))
monthlyfullrecord$yeartype <- factor(monthlyfullrecord$yeartype, levels=c("C","D","BN","AN","W"))
monthlyfullrecord$volume_maf <- monthlyfullrecord$volume_taf/1000
monthlyfullrecord_sac <- monthlyfullrecord[which(monthlyfullrecord$gauge==11447650),]
monthlyfullrecord_sj <- monthlyfullrecord[which(monthlyfullrecord$gauge==11303500),]


monthlybarplot_sac <- ggplot(monthlyfullrecord_sac, aes(x = yeartype, y = volume_maf, width=0.9, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~month, ncol=6)+ 
		theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		labs(title="Total Flows Above 90th Percentile For Average Year Type With Non-Zero Flow
						Sacramento USGS 11447650 (45 years of Data, 1970-2014)
", 
				x="Year Type", y="Magnitude of Average Year Type Total Flow (MAF)")+
		theme(axis.text=element_text(size=14),
				axis.title=element_text(size=16),
				title=element_text(size=16.5))

ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\SGMA_meeting\\sac_monthlybarplot_fullrecordthresold.png", monthlybarplot_sac, width=11, height=8.5, units="in")

monthlybarplot_sj <- ggplot(monthlyfullrecord_sj, aes(x = yeartype, y = volume_maf, width=0.9, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~month, ncol=6)+ 
		theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		labs(title="Total Flows Above 90th Percentile For Average Year Type With Non-Zero Flow
						San Joaquin USGS 11303500 (25 years of Data, 1989-2014)
						", 
				x="Year Type", y="Magnitude of Average Year Type Total Flow (MAF)")+
		theme(axis.text=element_text(size=14),
				axis.title=element_text(size=16),
				title=element_text(size=16.5))

ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\SGMA_meeting\\sj_monthlybarplot_fullrecordthresold.png", monthlybarplot_sj, width=11, height=8.5, units="in")

periodfullrecord <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\figures\\SGMA_meeting\\barplots_fullrecord_threshold\\data_for_barplots\\period_fullrecord_numbers.csv")

pe <- ggplot(monthlyfullrecord_sj, aes(x = yeartype, y = volume_maf, width=0.9, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~month, ncol=6)+ 
		theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		labs(title="Total Flows Above 90th Percentile For Average Year Type With Non-Zero Flow
						San Joaquin USGS 11303500 (25 years of Data, 1989-2014)
						", 
				x="Year Type", y="Magnitude of Average Year Type Total Flow (MAF)")+
		theme(axis.text=element_text(size=14),
				axis.title=element_text(size=16),
				title=element_text(size=16.5))

#see work computer scratch for more








################zero_threshold
#####################
#########################

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
	
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
					width=3, threshold=0, 
					thresholdname="zero", mastertime="hy", Index=spbatch[[k]]$Index)
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
	
	test_peakflowmags_1980 <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_1980[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],1980)
		
	}
	names(test_peakflowmags_1980) <- names(spbatch)
	
	test_peakflowmags_dams <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowmags_dams[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],year=
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
		
	}
	names(test_peakflowmags_dams) <- names(spbatch)
	
	test_peakflowmags_full_bind <- test_peakflowmags_full[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_full_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_full_bind[[i]][[l]],
						test_peakflowmags_full[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_1980_bind <- test_peakflowmags_1980[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_1980_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_1980_bind[[i]][[l]],
						test_peakflowmags_1980[[k]][[i]][[l]])
			}
		}
	}
	
	test_peakflowmags_dams_bind <- test_peakflowmags_dams[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:6){
			for(l in 1:15){
				test_peakflowmags_dams_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_dams_bind[[i]][[l]],
						test_peakflowmags_dams[[k]][[i]][[l]])
			}
		}
	}
	
	
	for(i in 1:15){
		write.csv(test_peakflowmags_full_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\full\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\full\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\full\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\full\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\full\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_full_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\full\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_full_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_dams_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\dams\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\dams\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\dams\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\dams\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\dams\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_dams_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\dams\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_dams_bind$C)[[i]],".csv", sep=""))
	}
	
	for(i in 1:15){
		write.csv(test_peakflowmags_1980_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\1980\\all\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$all)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\1980\\W\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$W)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\1980\\AN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$AN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\1980\\BN\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$BN)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\1980\\D\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$D)[[i]],".csv", sep=""))
		
		write.csv(test_peakflowmags_1980_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_mags\\1980\\C\\batch_",batchnum,"_",
						names(test_peakflowmags_1980_bind$C)[[i]],".csv", sep=""))
	}
	
	
	test_peakflowtrends <- vector("list", length(spbatch))
	for(k in 1:length(spbatch)){
		test_peakflowtrends[[k]] <- vector("list", 1)
		names(test_peakflowtrends[[k]]) <- names(test_split[[k]])[[1]]
		for(i in 1:1){
			for(l in 1:15){
				test_peakflowtrends[[k]][[i]][[l]] <- simplified_peakflowtrends(test_split[[k]][[i]][[l]], names(test_split)[[k]],
						analysis_year$analysis_year[which(spbatch[[k]]$raw$site_no[[1]]==analysis_year$downstreamgauge)])
			}
			names(test_peakflowtrends[[k]][[i]]) <- names(test_split[[k]][[i]])
		}
	}
	names(test_peakflowtrends) <- names(spbatch)
	
	
	test_peakflowtrends_bind <- test_peakflowtrends[[1]]
	for(k in 2:length(spbatch)){
		for(i in 1:1){
			for(l in 1:15){
				for(m in 1:3){
					test_peakflowtrends_bind[[i]][[l]][[m]] <- rbind.data.frame(test_peakflowtrends_bind[[i]][[l]][[m]],
							test_peakflowtrends[[k]][[i]][[l]][[m]])	
				}
			}
		}
	}
	
	
	for(i in 1:15){
		write.csv(test_peakflowtrends_bind$all[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\all\\dams\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[1]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\all\\full\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[2]],".csv", sep=""))
		
		write.csv(test_peakflowtrends_bind$all[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\all\\1980\\batch_",batchnum,"_",
						names(test_peakflowtrends_bind$all)[[i]],"_",names(test_peakflowtrends_bind$all[[i]])[[3]],".csv", sep=""))
		
#		write.csv(test_peakflowtrends_bind$W[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\W\\dams\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$W)[[i]],"_",names(test_peakflowtrends_bind$W[[i]])[[1]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$W[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\W\\full\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$W)[[i]],"_",names(test_peakflowtrends_bind$W[[i]])[[2]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$W[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\W\\1980\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$W)[[i]],"_",names(test_peakflowtrends_bind$W[[i]])[[3]],".csv", sep=""))
#		
#		
#		write.csv(test_peakflowtrends_bind$AN[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\AN\\dams\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$AN)[[i]],"_",names(test_peakflowtrends_bind$AN[[i]])[[1]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$AN[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\AN\\full\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$AN)[[i]],"_",names(test_peakflowtrends_bind$AN[[i]])[[2]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$AN[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\AN\\1980\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$AN)[[i]],"_",names(test_peakflowtrends_bind$AN[[i]])[[3]],".csv", sep=""))
#		
#		
#		write.csv(test_peakflowtrends_bind$BN[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\BN\\dams\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$BN)[[i]],"_",names(test_peakflowtrends_bind$BN[[i]])[[1]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$BN[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\BN\\full\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$BN)[[i]],"_",names(test_peakflowtrends_bind$BN[[i]])[[2]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$BN[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\BN\\1980\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$BN)[[i]],"_",names(test_peakflowtrends_bind$BN[[i]])[[3]],".csv", sep=""))
#		
#		
#		write.csv(test_peakflowtrends_bind$D[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\D\\dams\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$D)[[i]],"_",names(test_peakflowtrends_bind$D[[i]])[[1]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$D[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\D\\full\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$D)[[i]],"_",names(test_peakflowtrends_bind$D[[i]])[[2]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$D[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\D\\1980\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$D)[[i]],"_",names(test_peakflowtrends_bind$D[[i]])[[3]],".csv", sep=""))
#		
#		
#		
#		write.csv(test_peakflowtrends_bind$C[[i]][[1]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\C\\dams\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$C)[[i]],"_",names(test_peakflowtrends_bind$C[[i]])[[1]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$C[[i]][[2]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\C\\full\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$C)[[i]],"_",names(test_peakflowtrends_bind$C[[i]])[[2]],".csv", sep=""))
#		
#		write.csv(test_peakflowtrends_bind$C[[i]][[3]], file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\zero_threshold\\peakflow_trends\\C\\1980\\batch_",batchnum,"_",
#						names(test_peakflowtrends_bind$C)[[i]],"_",names(test_peakflowtrends_bind$C[[i]])[[3]],".csv", sep=""))
	}
	
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\zero_threshold_spbatch_",batchnum,".RData", sep=""))
}


### merge files####

keyvec <- c("nov","dec","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","hy","mon3","mon6")
masterlist <- vector("list",length(keyvec))
for(k in 1:length(keyvec)){
	masterlist[[k]] <- vector("list",7)
	for(i in 1:7){
		masterlist[[k]][[i]] <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\GIS\\full_record\\peakflow_trends\\all\\full\\batch_",
						i,"_",keyvec[[k]],"_trend_full.csv", sep=""))
		
	}
	masterlist[[k]] <- do.call(rbind.data.frame,masterlist[[k]])
	masterlist[[k]]$period <- keyvec[[k]]
}
names(masterlist)<-keyvec
masterdf <- do.call(rbind.data.frame,masterlist)

test <- masterdf[which(masterdf$measure=="totvolabv" & masterdf$window==10),]
write.csv(masterdf,"C:\\Users\\tiffn_000\\Google Drive\\data\\full_record\\full_record_trends_full.csv")

test2 <- data.frame(date=spbatch$`11447650`$prep$Date, discharge_TAF=spbatch$`11447650`$prep$Discharge_maf*1000)
test2 <- test2[which(test2$date >= as.Date("1976-02-01", "%Y-%m-%d")),]
write.csv(test2,"C:\\Users\\tiffn_000\\Google Drive\\data\\sac11447650_delta_dates.csv" )