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


scatter_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\scatter_sites\\scatter_sites.txt")
scatter_gauges <- scatter_gauges[which(scatter_gauges$site_no %in% unique(impairmentsdf$downstreamgauge)),]
scatter_g <- as.numeric(scatter_gauges$site_no)


txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
scatter_g <- scatter_g[which(scatter_g %in% txtgauges)]


scatter <- vector("list", length(scatter_g))
for(z in 1:length(scatter_g)){
	scatter[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",scatter_g[[z]],".csv",sep=""), header=TRUE)
	scatter[[z]]$raw$Date <- as.Date(scatter[[z]]$raw$Date, "%Y-%m-%d")
	
	scatter[[z]]$raw <- RemoveLeapDays(scatter[[z]]$raw)
	
	##yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")
	if(as.numeric(scatter[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		scatter[[z]]$Index$Valley <- "SacV"
		scatter[[z]]$Index$Index <- yeartype_old$SVI
		scatter[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(scatter[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		scatter[[z]]$Index$Valley <- "SJV"
		scatter[[z]]$Index$Index <- yeartype_old$SJI
		scatter[[z]]$Index$Year <- yeartype_old$Year
	} else {
		scatter[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",scatter[[z]]$raw$site_no[[1]]))
	}
	
	###DATA PROCESSING
	scatter[[z]]$prep <- prepdata(scatter[[z]]$raw)
	scatter[[z]]$Availability <- DataAvailability(scatter[[z]]$raw)
	scatter[[z]]$thresholds_maf <- thresholds(scatter[[z]]$prep)
	scatter[[z]]$dams <- damsearch(scatter[[z]], impairmentsdf)
	if(all(scatter[[z]]$thresholds_maf==0)){
	} else {

#		scatter[[z]]$Winter_3mon <- Split3Winter(scatter[[z]]$prep, scatter[[z]]$Index, scatter[[z]]$thresholds_maf)
#		scatter[[z]]$Winter_6mon <- Split6Winter(scatter[[z]]$prep, scatter[[z]]$Index, scatter[[z]]$thresholds_maf)
#		scatter[[z]]$Winter_monthly <- SplitWinterMonthly(scatter[[z]]$prep, scatter[[z]]$Index, scatter[[z]]$thresholds_maf)
		scatter[[z]]$HydroYear <- SplitHydroYear(scatter[[z]]$prep, scatter[[z]]$Index, scatter[[z]]$thresholds_maf)	
		scatter[[z]]$HydroYear <- cleanupHY(scatter[[z]]$HydroYear)
#		scatter[[z]]$Winter_6mon <- cleanup6MON(scatter[[z]]$Winter_6mon)
#		scatter[[z]]$Winter_3mon <- cleanup3MON(scatter[[z]]$Winter_3mon)
		
	}
}

scatter_peakflows <- vector("list", length(scatter))
scatter_peakflowstats <- vector("list", length(scatter))
scatter_peakflowsummary <- vector("list", length(scatter))
scatter_peakflowmonthlystats <- vector("list", length(scatter))
scatter_peakflowsdf <- vector("list", length(scatter))
for(k in 1:length(scatter)){
	scatter_peakflows[[k]] <- vector("list",1)
	names(scatter_peakflows[[k]]) <- c("peakflows")
	scatter_peakflows[[k]]$peakflows <- vector("list", length=length(scatter[[k]]$HydroYear$Data))
	for(i in 1:length(scatter[[k]]$HydroYear$Data)){
		scatter_peakflows[[k]]$peakflows[[i]] <- peakanalysis(input=scatter[[k]]$HydroYear$Data[[i]],
				width=3, threshold=(scatter[[k]]$thresholds_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
				thresholdname="90%", mastertime="hy", Index=scatter[[k]]$Index)
	}
	scatter_peakflowstats[[k]] <- vector("list",length(scatter_peakflows[[k]]$peakflows))
	for(i in 1:length(scatter_peakflows[[k]]$peakflows))	{
		scatter_peakflowstats[[k]][[i]] <- scatter_peakflows[[k]]$peakflows[[i]][[2]]
	}
	scatter_peakflowsummary[[k]] <- vector("list",length(scatter_peakflows[[k]]$peakflows))
	for(i in 1:length(scatter_peakflows[[k]]$peakflows))	{
		scatter_peakflowsummary[[k]][[i]] <- scatter_peakflows[[k]]$peakflows[[i]][[1]]
	}
	scatter_peakflowmonthlystats[[k]] <- vector("list",length(scatter_peakflows[[k]]$peakflows))
	for(i in 1:length(scatter_peakflows[[k]]$peakflows))	{
		scatter_peakflowmonthlystats[[k]][[i]] <- scatter_peakflows[[k]]$peakflows[[i]][[3]]
	}
	scatter_peakflowsdf[[k]] <- vector("list",3)
	names(scatter_peakflowsdf[[k]]) <- c("pfstatsdf","pfsummarydf","pfmonthlystats")
	scatter_peakflowsdf[[k]]$pfstatsdf <- do.call(rbind.data.frame,scatter_peakflowstats[[k]])
	scatter_peakflowsdf[[k]]$pfsummarydf <- do.call(rbind.data.frame,scatter_peakflowsummary[[k]])
	scatter_peakflowsdf[[k]]$pfmonthlystats <- do.call(rbind.data.frame,scatter_peakflowmonthlystats[[k]])
	scatter_peakflowsdf[[k]]$pfstatsdf$volday_is_zero <- rep(NA, length(scatter_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv))
	for(i in 1:length(scatter_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv)){
		if(scatter_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv[[i]]==0){
			scatter_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 1
		}else{
			scatter_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 0
		}
	}
	scatter_peakflowsdf[[k]]$pfstatsdf$volday_is_zero_cumsum <- cumsum(scatter_peakflowsdf[[k]]$pfstatsdf$volday_is_zero)	
}
names(scatter_peakflowsdf)<- names(scatter)
names(scatter_peakflows)<- names(scatter)
names(scatter_peakflowstats)<- names(scatter)
names(scatter_peakflowsummary)<- names(scatter)
names(scatter_peakflowmonthlystats)<- names(scatter)

for(k in 1:length(scatter)){
	damsunique <- data.frame(year=unique(scatter[[k]]$dams$YEAR_BUILT), totalcapacity_yr = rep(NA,length(unique(scatter[[k]]$dams$YEAR_BUILT))))
	for(i in 1:length(damsunique$year)){
		damsunique$totalcapacity_yr[[i]] <- sum(scatter[[k]]$dams$CAPACITY__[which(scatter[[k]]$dams$YEAR_BUILT==damsunique$year[[i]])])	
	}
	scatter[[k]]$damsunique <- damsunique
}

for(z in 1:length(scatter)){
	png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\zeroyears\\",names(scatter)[[z]],"_cumzero.png",sep=""), width=11, height=8.5, units="in", res=600)
	plot(scatter_peakflowsdf[[z]]$pfstatsdf$year,scatter_peakflowsdf[[z]]$pfstatsdf$volday_is_zero_cumsum,
			ylab="Cumulative Number of Years With Zero Flow Above 90%", xlab="Year")
	for(i in 1:length(scatter[[z]]$damsunique$year)){
		abline(v=scatter[[z]]$damsunique$year[[i]], col="red")
		text(x=scatter[[z]]$damsunique$year[[i]], y = max(scatter_peakflowsdf[[z]]$pfstatsdf$volday_is_zero_cumsum),labels=as.character(scatter[[z]]$damsunique$totalcapacity_yr[[i]]))
	}
	title(main=paste(scatter[[z]]$raw$site_no[[1]], "\nRed Line is year of dam, \nnumber is total capacity of dams added in year", sep=""))
	dev.off()
}


for(z in 1:length(scatter)){
	png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\volabv\\",names(scatter)[[z]],"_volabv.png",sep=""), width=11, height=8.5, units="in", res=600)
	plot(scatter_peakflowsdf[[z]]$pfstatsdf$year,scatter_peakflowsdf[[z]]$pfstatsdf$TotVolAbv_acft,
			ylab="Vol Above 90% acft", xlab="Year")
	for(i in 1:length(scatter[[z]]$damsunique$year)){
		abline(v=scatter[[z]]$damsunique$year[[i]], col="red")
		text(x=scatter[[z]]$damsunique$year[[i]], y = max(scatter_peakflowsdf[[z]]$pfstatsdf$TotVolAbv_acft),labels=as.character(scatter[[z]]$damsunique$totalcapacity_yr[[i]]))
	}
	title(main=paste(scatter[[z]]$raw$site_no[[1]], "\nRed Line is year of dam, \nnumber is total capacity of dams added in year", sep=""))
	dev.off()
}

for(z in 1:length(scatter)){
	png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\daysabv\\",names(scatter)[[z]],"_daysabv.png",sep=""), width=11, height=8.5, units="in", res=600)
	plot(scatter_peakflowsdf[[z]]$pfstatsdf$year,scatter_peakflowsdf[[z]]$pfstatsdf$TotDaysAbv,
			ylab="Days Above 90% acft", xlab="Year")
	for(i in 1:length(scatter[[z]]$damsunique$year)){
		abline(v=scatter[[z]]$damsunique$year[[i]], col="red")
		text(x=scatter[[z]]$damsunique$year[[i]], y = max(scatter_peakflowsdf[[z]]$pfstatsdf$TotDaysAbv),labels=as.character(scatter[[z]]$damsunique$totalcapacity_yr[[i]]))
	}
	title(main=paste(scatter[[z]]$raw$site_no[[1]], "\nRed Line is year of dam, \nnumber is total capacity of dams added in year", sep=""))
	dev.off()
}


unimp <- c(11189500,
11230500,
11231500,
11237000,
11237500,
11244000,
11264500,
11266500,
11315000,
11367500,
11374000,
11381500,
11383500,
11402000,
11318500,
11413000,
11317000)

unimp_edit <- read.csv("C:\\Users\\tiffn_000\\Documents\\GIS\\scatter_sites\\scatter_sites_unimp_edited2.csv")

unimp2 <- c(
		11189500,
		11230500,
		11231500,
		11237000,
		11237500,
		11264500,
		11266500,
		11315000,
		11317000,
		11318500,
		11367500,
		11374000,
		11381500,
		11383500,
		11402000,
		11413000
		)
###try 3-mon peakflows###

for(z in 1:length(scatter)){
 		scatter[[z]]$Winter_3mon <- Split3Winter(scatter[[z]]$prep, scatter[[z]]$Index, scatter[[z]]$thresholds_maf)
		scatter[[z]]$Winter_6mon <- Split6Winter(scatter[[z]]$prep, scatter[[z]]$Index, scatter[[z]]$thresholds_maf)
		scatter[[z]]$Winter_6mon <- cleanup6MON(scatter[[z]]$Winter_6mon)
		scatter[[z]]$Winter_3mon <- cleanup3MON(scatter[[z]]$Winter_3mon)
}


scatter_peakflows3 <- vector("list", length(scatter))
scatter_peakflowstats3 <- vector("list", length(scatter))
scatter_peakflowsummary3 <- vector("list", length(scatter))
scatter_peakflowmonthlystats3 <- vector("list", length(scatter))
scatter_peakflowsdf3 <- vector("list", length(scatter))
for(k in 1:length(scatter)){
	scatter_peakflows3[[k]] <- vector("list",1)
	names(scatter_peakflows3[[k]]) <- c("peakflows")
	scatter_peakflows3[[k]]$peakflows <- vector("list", length=length(scatter[[k]]$Winter_3mon$Data))
	for(i in 1:length(scatter[[k]]$Winter_3mon$Data)){
		scatter_peakflows3[[k]]$peakflows[[i]] <- peakanalysis(input=scatter[[k]]$Winter_3mon$Data[[i]],
				width=3, threshold=(scatter[[k]]$thresholds_maf$P90maf/(86400*2.29568411e-5*1e-6)), 
				thresholdname="90%", mastertime="3mon", Index=scatter[[k]]$Index)
	}
	scatter_peakflowstats3[[k]] <- vector("list",length(scatter_peakflows3[[k]]$peakflows))
	for(i in 1:length(scatter_peakflows3[[k]]$peakflows))	{
		scatter_peakflowstats3[[k]][[i]] <- scatter_peakflows3[[k]]$peakflows[[i]][[2]]
	}
	scatter_peakflowsummary3[[k]] <- vector("list",length(scatter_peakflows3[[k]]$peakflows))
	for(i in 1:length(scatter_peakflows3[[k]]$peakflows))	{
		scatter_peakflowsummary3[[k]][[i]] <- scatter_peakflows3[[k]]$peakflows[[i]][[1]]
	}
	scatter_peakflowmonthlystats3[[k]] <- vector("list",length(scatter_peakflows3[[k]]$peakflows))
	for(i in 1:length(scatter_peakflows3[[k]]$peakflows))	{
		scatter_peakflowmonthlystats3[[k]][[i]] <- scatter_peakflows3[[k]]$peakflows[[i]][[3]]
	}
	scatter_peakflowsdf3[[k]] <- vector("list",3)
	names(scatter_peakflowsdf3[[k]]) <- c("pfstatsdf3","pfsummarydf3","pfmonthlystats3")
	scatter_peakflowsdf3[[k]]$pfstatsdf3 <- do.call(rbind.data.frame,scatter_peakflowstats3[[k]])
	scatter_peakflowsdf3[[k]]$pfsummarydf3 <- do.call(rbind.data.frame,scatter_peakflowsummary3[[k]])
	scatter_peakflowsdf3[[k]]$pfmonthlystats3 <- do.call(rbind.data.frame,scatter_peakflowmonthlystats3[[k]])
	scatter_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero <- rep(NA, length(scatter_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv))
	for(i in 1:length(scatter_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv)){
		if(scatter_peakflowsdf3[[k]]$pfstatsdf3$TotDaysAbv[[i]]==0){
			scatter_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero[[i]] <- 1
		}else{
			scatter_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero[[i]] <- 0
		}
	}
	scatter_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero_cumsum <- cumsum(scatter_peakflowsdf3[[k]]$pfstatsdf3$volday_is_zero)	
}
names(scatter_peakflowsdf3)<- names(scatter)
names(scatter_peakflows3)<- names(scatter)
names(scatter_peakflowstats3)<- names(scatter)
names(scatter_peakflowsummary3)<- names(scatter)
names(scatter_peakflowmonthlystats3)<- names(scatter)

for(z in 1:length(scatter)){
	png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures3\\zeroyears\\",names(scatter)[[z]],"_cumzero.png",sep=""), width=11, height=8.5, units="in", res=600)
	plot(scatter_peakflowsdf3[[z]]$pfstatsdf3$year,scatter_peakflowsdf3[[z]]$pfstatsdf3$volday_is_zero_cumsum,
			ylab="Cumulative Number of Years With Zero Flow Above 90%", xlab="Year")
	for(i in 1:length(scatter[[z]]$damsunique$year)){
		abline(v=scatter[[z]]$damsunique$year[[i]], col="red")
		text(x=scatter[[z]]$damsunique$year[[i]], y = max(scatter_peakflowsdf3[[z]]$pfstatsdf3$volday_is_zero_cumsum),labels=as.character(scatter[[z]]$damsunique$totalcapacity_yr[[i]]))
	}
	title(main=paste(scatter[[z]]$raw$site_no[[1]], "\nRed Line is year of dam, \nnumber is total capacity of dams added in year", sep=""))
	dev.off()
}


for(z in 1:length(scatter)){
	png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures3\\volabv\\",names(scatter)[[z]],"_volabv.png",sep=""), width=11, height=8.5, units="in", res=600)
	plot(scatter_peakflowsdf3[[z]]$pfstatsdf3$year,scatter_peakflowsdf3[[z]]$pfstatsdf3$TotVolAbv_acft,
			ylab="Vol Above 90% acft", xlab="Year")
	for(i in 1:length(scatter[[z]]$damsunique$year)){
		abline(v=scatter[[z]]$damsunique$year[[i]], col="red")
		text(x=scatter[[z]]$damsunique$year[[i]], y = max(scatter_peakflowsdf3[[z]]$pfstatsdf3$TotVolAbv_acft),labels=as.character(scatter[[z]]$damsunique$totalcapacity_yr[[i]]))
	}
	title(main=paste(scatter[[z]]$raw$site_no[[1]], "\nRed Line is year of dam, \nnumber is total capacity of dams added in year", sep=""))
	dev.off()
}

for(z in 1:length(scatter)){
	png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures3\\daysabv\\",names(scatter)[[z]],"_daysabv.png",sep=""), width=11, height=8.5, units="in", res=600)
	plot(scatter_peakflowsdf3[[z]]$pfstatsdf3$year,scatter_peakflowsdf3[[z]]$pfstatsdf3$TotDaysAbv,
			ylab="Days Above 90% acft", xlab="Year")
	for(i in 1:length(scatter[[z]]$damsunique$year)){
		abline(v=scatter[[z]]$damsunique$year[[i]], col="red")
		text(x=scatter[[z]]$damsunique$year[[i]], y = max(scatter_peakflowsdf3[[z]]$pfstatsdf3$TotDaysAbv),labels=as.character(scatter[[z]]$damsunique$totalcapacity_yr[[i]]))
	}
	title(main=paste(scatter[[z]]$raw$site_no[[1]], "\nRed Line is year of dam, \nnumber is total capacity of dams added in year", sep=""))
	dev.off()
}