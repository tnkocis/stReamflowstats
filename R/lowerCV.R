# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


lowerCV_gauges <-  c(11185500,11186000,11186001,11187500,11189500,11192500,11192501,
		11194000,11202000,11202001,11203500,11204500,11206500,11206501,11208000,11208001,
		11209500,11210500,11213500,11214600,11215000,11216500,11218400,11222000,11224500,11229500,11230000,
		11230500,11231500,11234760,11235500,11237000,11237500,11239000,11242000,11243500,11244000,11246500,
		11249500,11250000,11251000,11257500,11259000,11261500)

library(dplyr)
library(hydroTSM)
library(dataRetrieval)
library(Kendall)

SacV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_svi.txt")
SJV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_sji.txt")
yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")


lowerCV_g <- as.numeric(lowerCV_gauges)
txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
lowerCV_g <- lowerCV_g[which(lowerCV_g %in% txtgauges)]


lowerCV <- vector("list", length(lowerCV_g))
names(lowerCV) <- lowerCV_g

for(z in 1:length(lowerCV_g)){
	lowerCV[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",lowerCV_g[[z]],".csv",sep=""), header=TRUE)
	lowerCV[[z]]$raw$Date <- as.Date(lowerCV[[z]]$raw$Date, "%Y-%m-%d")
	lowerCV[[z]]$raw <- RemoveLeapDays(lowerCV[[z]]$raw)
	
	if(as.numeric(lowerCV[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		lowerCV[[z]]$Index$Valley <- "SacV"
		lowerCV[[z]]$Index$Index <- yeartype_old$SVI
		lowerCV[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(lowerCV[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		lowerCV[[z]]$Index$Valley <- "SJV"
		lowerCV[[z]]$Index$Index <- yeartype_old$SJI
		lowerCV[[z]]$Index$Year <- yeartype_old$Year
	} else {
		lowerCV[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",lowerCV[[z]]$raw$site_no[[1]]))
	}
	
	###DATA PROCESSING
	lowerCV[[z]]$prep <- prepdata(lowerCV[[z]]$raw)
	lowerCV[[z]]$Availability <- DataAvailability(lowerCV[[z]]$raw)
	lowerCV[[z]]$thresholds_maf <- thresholds(lowerCV[[z]]$prep)
	if(all(lowerCV[[z]]$thresholds_maf==0)){
	} else {
		lowerCV[[z]]$Winter_3mon <- Split3Winter(lowerCV[[z]]$prep, lowerCV[[z]]$Index, lowerCV[[z]]$thresholds_maf)
		lowerCV[[z]]$Winter_6mon <- Split6Winter(lowerCV[[z]]$prep, lowerCV[[z]]$Index, lowerCV[[z]]$thresholds_maf)
		lowerCV[[z]]$Winter_monthly <- SplitWinterMonthly(lowerCV[[z]]$prep, lowerCV[[z]]$Index, lowerCV[[z]]$thresholds_maf)
		lowerCV[[z]]$HydroYear <- SplitHydroYear(lowerCV[[z]]$prep, lowerCV[[z]]$Index, lowerCV[[z]]$thresholds_maf)
	}
}
for(z in 1:length(lowerCV_g)){
	lowerCV[[z]]$HydroYear <- cleanupHY(lowerCV[[z]]$HydroYear)
	lowerCV[[z]]$Winter_6mon <- cleanup6MON(lowerCV[[z]]$Winter_6mon)
	lowerCV[[z]]$Winter_3mon <- cleanup3MON(lowerCV[[z]]$Winter_3mon)
	lowerCV[[z]]$Winter_monthly <- cleanupMON(lowerCV[[z]]$Winter_monthly)
}

for(i in 9:length(lowerCV)){
	year <- as.numeric(format(lowerCV[[i]]$HydroYear$All$Data$Date[[1]],"%Y"))+1
#	year <- 1965
	MKTloop <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	
	MK <- MannKendall(lowerCV[[i]]$HydroYear$All$Data$Discharge_acfte6_day[which(format(lowerCV[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]:nablock(lowerCV[[i]]$HydroYear$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[1]] <- MK$tau
	MKTloop$p2s[[1]] <- MK$sl
	MKTloop$station[[1]] <- lowerCV[[i]]$raw$site_no[[1]]
	MKTloop$start[[1]] <- as.character(lowerCV[[i]]$HydroYear$All$Data$Date[[which(format(lowerCV[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]]])
	MKTloop$end_date[[1]] <- as.character(lowerCV[[i]]$HydroYear$All$Data$Date[[nablock(lowerCV[[i]]$HydroYear$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[1]] <- "HY"
	
	MK <- MannKendall(lowerCV[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(lowerCV[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]:nablock(lowerCV[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[2]] <- MK$tau
	MKTloop$p2s[[2]] <- MK$sl
	MKTloop$station[[2]] <- lowerCV[[i]]$raw$site_no[[1]]
	MKTloop$start[[2]] <- as.character(lowerCV[[i]]$Winter_3mon$All$Data$Date[[which(format(lowerCV[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]]])
	MKTloop$end_date[[2]] <- as.character(lowerCV[[i]]$Winter_3mon$All$Data$Date[[nablock(lowerCV[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[2]] <- "3MON"
	
	MK <- MannKendall(lowerCV[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(lowerCV[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]:nablock(lowerCV[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[3]] <- MK$tau
	MKTloop$p2s[[3]] <- MK$sl
	MKTloop$station[[3]] <- lowerCV[[i]]$raw$site_no[[1]]
	MKTloop$start[[3]] <- as.character(lowerCV[[i]]$Winter_6mon$All$Data$Date[[which(format(lowerCV[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]]])
	MKTloop$end_date[[3]] <- as.character(lowerCV[[i]]$Winter_6mon$All$Data$Date[[nablock(lowerCV[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[3]] <- "6MON"
	
	MK <- MannKendall(lowerCV[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(lowerCV[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]:nablock(lowerCV[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKTloop$tau[[4]] <- MK$tau
	MKTloop$p2s[[4]] <- MK$sl
	MKTloop$station[[4]] <- lowerCV[[i]]$raw$site_no[[1]]
	MKTloop$start[[4]] <- as.character(lowerCV[[i]]$Winter_monthly$All$DEC$Data$Date[[which(format(lowerCV[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]]])
	MKTloop$end_date[[4]] <- as.character(lowerCV[[i]]$Winter_monthly$All$DEC$Data$Date[[nablock(lowerCV[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)]])
	MKTloop$period[[4]] <- "DEC"
	
	MK <- MannKendall(lowerCV[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(lowerCV[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]:nablock(lowerCV[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKTloop$tau[[5]] <- MK$tau
	MKTloop$p2s[[5]] <- MK$sl
	MKTloop$station[[5]] <- lowerCV[[i]]$raw$site_no[[1]]
	MKTloop$start[[5]] <- as.character(lowerCV[[i]]$Winter_monthly$All$JAN$Data$Date[[which(format(lowerCV[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[5]] <- as.character(lowerCV[[i]]$Winter_monthly$All$JAN$Data$Date[[nablock(lowerCV[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)]])
	MKTloop$period[[5]] <- "JAN"
	
	MK <- MannKendall(lowerCV[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(lowerCV[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]:nablock(lowerCV[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKTloop$tau[[6]] <- MK$tau
	MKTloop$p2s[[6]] <- MK$sl
	MKTloop$station[[6]] <- lowerCV[[i]]$raw$site_no[[1]]
	MKTloop$start[[6]] <- as.character(lowerCV[[i]]$Winter_monthly$All$FEB$Data$Date[[which(format(lowerCV[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[6]] <- as.character(lowerCV[[i]]$Winter_monthly$All$FEB$Data$Date[[nablock(lowerCV[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)]])
	MKTloop$period[[6]] <- "FEB"
	
	MK <- MannKendall(lowerCV[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(lowerCV[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]:nablock(lowerCV[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[7]] <- MK$tau
	MKTloop$p2s[[7]] <- MK$sl
	MKTloop$station[[7]] <- lowerCV[[i]]$raw$site_no[[1]]
	MKTloop$start[[7]] <- as.character(lowerCV[[i]]$Winter_monthly$All$MAR$Data$Date[[which(format(lowerCV[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[7]] <- as.character(lowerCV[[i]]$Winter_monthly$All$MAR$Data$Date[[nablock(lowerCV[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[7]] <- "MAR"
	
	MK <- MannKendall(lowerCV[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(lowerCV[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]:nablock(lowerCV[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[8]] <- MK$tau
	MKTloop$p2s[[8]] <- MK$sl
	MKTloop$station[[8]] <- lowerCV[[i]]$raw$site_no[[1]]
	MKTloop$start[[8]] <- as.character(lowerCV[[i]]$Winter_monthly$All$APR$Data$Date[[which(format(lowerCV[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[8]] <- as.character(lowerCV[[i]]$Winter_monthly$All$APR$Data$Date[[nablock(lowerCV[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[8]] <- "APR"
	
	MKTloop$start <- as.Date(MKTloop$start)
	MKTloop$end_date <- as.Date(MKTloop$end_date)
	write.csv(MKTloop, file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_stream\\full_records\\", names(lowerCV)[[i]],"_",year,".csv", sep=""))
}

######################
##PRECIPITATION#######
######################

library(sharpshootR)

lowerCV_precip_stations <- c("SRI","MRP","BUR","OWN","BUC","MGN","WST","BNR","OKH","HID","GRM","GRV","CHM","PSR","NTP","KSP","CRR","CRV","GTM",
"FLR","PCK","NFR","HNT","HTN","BGC","TMR","AGW","SAV","BDM","MTF","MIL","FRT","FGC","MBR","DKY","WSH","WSD","FEN","BLH","BAL","TRI","FCH","PNF",
"PFW","CGR","CRL","GRO","SGL","BIM","PNT","BRM","LDG","GNF","ASM","ATW","TRM","HCK","MNH","SPV","HSS","SGV","QUA","RGC","SCC","EGL","CBT","CHP","BCH","PSC",
"KR3","KPI","KP3","GLV","ISB","KR1","KCP","KPH","FRO","KTT","BFK","SPA","ZPC","MAP")

lowerCV_precip <- vector("list", length=length(lowerCV_precip_stations))
names(lowerCV_precip) <- lowerCV_precip_stations
for(i in 1:length(lowerCV_precip_stations)){
	lowerCV_precip[[i]] <- CDECquery(lowerCV_precip_stations[[i]] ,45, interval="D","1900-10-01","2015-10-01")
}


for(i in 1:length(lowerCV_precip)){
	write.csv(lowerCV_precip[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\Data\\precip\\",names(lowerCV_precip)[[i]],".csv",sep=""))
}

precipRMleapdays <- function(input){
	input[format(input$Date, "%m-%d")!="02-29",]
}




for(i in 1:length(lowerCV_precip)){
	lowerCV_precip[[i]] <- precipRMleapdays(lowerCV_precip[[i]])
}

lowerCV_precip_process <- vector("list", length(lowerCV_precip))
names(lowerCV_precip_process) <- names(lowerCV_precip)


for(z in 73:length(lowerCV_precip_process)){
	lowerCV_precip_process[[z]]$raw <- lowerCV_precip[[z]]
	lowerCV_precip_process[[z]]$raw$Date <- as.Date(lowerCV_precip_process[[z]]$raw$datetime, "%Y-%m-%d")
	lowerCV_precip_process[[z]]$raw$datetime <- NULL
	lowerCV_precip_process[[z]]$raw$site_no <- rep(names(lowerCV_precip_process)[[z]],length(lowerCV_precip_process[[z]]$raw$year))
	
	lowerCV_precip_process[[z]]$raw <- precipRMleapdays(lowerCV_precip_process[[z]]$raw)
	lowerCV_precip_process[[z]]$raw$X_00060_00003 <- lowerCV_precip_process[[z]]$raw$value
	lowerCV_precip_process[[z]]$raw$value <- NULL
	
	lowerCV_precip_process[[z]]$Index$Valley <- "SJV"
	lowerCV_precip_process[[z]]$Index$Index <- yeartype_old$SVI
	lowerCV_precip_process[[z]]$Index$Year <- yeartype_old$Year
	
	
	###DATA PROCESSING
	lowerCV_precip_process[[z]]$prep <- prepdata(lowerCV_precip_process[[z]]$raw)
	lowerCV_precip_process[[z]]$Availability <- DataAvailability(lowerCV_precip_process[[z]]$raw)
	lowerCV_precip_process[[z]]$thresholds_maf <- thresholds(lowerCV_precip_process[[z]]$prep)
	
	if(all(lowerCV_precip_process[[z]]$thresholds_maf==0)){
	} else {
#	lowerCV_precip_process$record_stats <- record_stats(lowerCV_precip_process$prep, lowerCV_precip_process$thresholds_maf)
		lowerCV_precip_process[[z]]$Winter_3mon <- Split3Winter(lowerCV_precip_process[[z]]$prep, lowerCV_precip_process[[z]]$Index, lowerCV_precip_process[[z]]$thresholds_maf)
		lowerCV_precip_process[[z]]$Winter_6mon <- Split6Winter(lowerCV_precip_process[[z]]$prep, lowerCV_precip_process[[z]]$Index, lowerCV_precip_process[[z]]$thresholds_maf)
		lowerCV_precip_process[[z]]$Winter_monthly <- SplitWinterMonthly(lowerCV_precip_process[[z]]$prep, lowerCV_precip_process[[z]]$Index, lowerCV_precip_process[[z]]$thresholds_maf)
		lowerCV_precip_process[[z]]$HydroYear <- SplitHydroYear(lowerCV_precip_process[[z]]$prep, lowerCV_precip_process[[z]]$Index, lowerCV_precip_process[[z]]$thresholds_maf)	
		lowerCV_precip_process[[z]]$HydroYear <- cleanupHY(lowerCV_precip_process[[z]]$HydroYear)
		lowerCV_precip_process[[z]]$Winter_6mon <- cleanup6MON(lowerCV_precip_process[[z]]$Winter_6mon)
		lowerCV_precip_process[[z]]$Winter_3mon <- cleanup3MON(lowerCV_precip_process[[z]]$Winter_3mon)
		lowerCV_precip_process[[z]]$Winter_monthly <- cleanupMON(lowerCV_precip_process[[z]]$Winter_monthly)
		
	}
}

lowerCV_precip_process$KP3 <- NULL
lowerCV_precip_process$KPI <- NULL


nablock <- function(x){
	length(x) - (which(duplicated(cumsum(rev(is.na(x)))))[1] - 1)
}

#######################################
#######################################
#######################################
#QCY1988
#SRC2006
#CGT2006
#RDH2003

for(i in 68:length(lowerCV_precip_process)){
	year <- as.numeric(format(lowerCV_precip_process[[i]]$HydroYear$All$Data$Date[[1]],"%Y"))+1
	MKTloop <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	
	MK <- MannKendall(lowerCV_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day[which(format(lowerCV_precip_process[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]:nablock(lowerCV_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[1]] <- MK$tau
	MKTloop$p2s[[1]] <- MK$sl
	MKTloop$station[[1]] <- lowerCV_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[1]] <- as.character(lowerCV_precip_process[[i]]$HydroYear$All$Data$Date[[which(format(lowerCV_precip_process[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]]])
	MKTloop$end_date[[1]] <- as.character(lowerCV_precip_process[[i]]$HydroYear$All$Data$Date[[nablock(lowerCV_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[1]] <- "HY"
	
	MK <- MannKendall(lowerCV_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(lowerCV_precip_process[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]:nablock(lowerCV_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[2]] <- MK$tau
	MKTloop$p2s[[2]] <- MK$sl
	MKTloop$station[[2]] <- lowerCV_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[2]] <- as.character(lowerCV_precip_process[[i]]$Winter_3mon$All$Data$Date[[which(format(lowerCV_precip_process[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]]])
	MKTloop$end_date[[2]] <- as.character(lowerCV_precip_process[[i]]$Winter_3mon$All$Data$Date[[nablock(lowerCV_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[2]] <- "3MON"
	
	MK <- MannKendall(lowerCV_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(lowerCV_precip_process[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]:nablock(lowerCV_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[3]] <- MK$tau
	MKTloop$p2s[[3]] <- MK$sl
	MKTloop$station[[3]] <- lowerCV_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[3]] <- as.character(lowerCV_precip_process[[i]]$Winter_6mon$All$Data$Date[[which(format(lowerCV_precip_process[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]]])
	MKTloop$end_date[[3]] <- as.character(lowerCV_precip_process[[i]]$Winter_6mon$All$Data$Date[[nablock(lowerCV_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[3]] <- "6MON"
	
	MK <- MannKendall(lowerCV_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]:nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKTloop$tau[[4]] <- MK$tau
	MKTloop$p2s[[4]] <- MK$sl
	MKTloop$station[[4]] <- lowerCV_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[4]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date[[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]]])
	MKTloop$end_date[[4]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date[[nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)]])
	MKTloop$period[[4]] <- "DEC"
	
	MK <- MannKendall(lowerCV_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]:nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKTloop$tau[[5]] <- MK$tau
	MKTloop$p2s[[5]] <- MK$sl
	MKTloop$station[[5]] <- lowerCV_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[5]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date[[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[5]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date[[nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)]])
	MKTloop$period[[5]] <- "JAN"
	
	MK <- MannKendall(lowerCV_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]:nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKTloop$tau[[6]] <- MK$tau
	MKTloop$p2s[[6]] <- MK$sl
	MKTloop$station[[6]] <- lowerCV_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[6]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date[[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[6]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date[[nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)]])
	MKTloop$period[[6]] <- "FEB"
	
	MK <- MannKendall(lowerCV_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]:nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[7]] <- MK$tau
	MKTloop$p2s[[7]] <- MK$sl
	MKTloop$station[[7]] <- lowerCV_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[7]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date[[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[7]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date[[nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[7]] <- "MAR"
	
	MK <- MannKendall(lowerCV_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]:nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[8]] <- MK$tau
	MKTloop$p2s[[8]] <- MK$sl
	MKTloop$station[[8]] <- lowerCV_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[8]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$APR$Data$Date[[which(format(lowerCV_precip_process[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[8]] <- as.character(lowerCV_precip_process[[i]]$Winter_monthly$All$APR$Data$Date[[nablock(lowerCV_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[8]] <- "APR"
	
	MKTloop$start <- as.Date(MKTloop$start)
	MKTloop$end_date <- as.Date(MKTloop$end_date)
	write.csv(MKTloop, file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_precip\\", names(lowerCV_precip_process)[[i]],"_",year,".csv", sep=""))
}

#################
######SNOW#######
#################
library(sharpshootR)
#monthly only lowerCV_snow <- c("VLC","AGP","BSH","BCB","UBC","FRW","CBT","CSV","CHM","DPO")

#daily only		lowerCV_snow <- c("KSP","GRM","TMR","HNT","GRV","PSR","CRL","STL","MTM","WWC","BIM","QUA","UTY","CHP","PSC","WTM","TUN","BCH","GNF")
#ALL 
lowerCV_snow <- c("VLC","AGP","BSH","BCB","UBC","FRW","GNF","CBT","CSV","CHM","DPO","KSP","GRM","TMR","HNT","GRV","PSR","CRL","STL","MTM","WWC","BIM","QUA","UTY","CHP","PSC","WTM","TUN","BCH")

LlowerCV <- vector("list", length=length(lowerCV_snow))
names(LlowerCV) <- lowerCV_snow
for(i in 1:length(lowerCV_snow)){
	LlowerCV[[i]] <- 
			CDECquery(lowerCV_snow[[i]], 3, interval = "D", "1900-01-01", "2015-10-01")
			
#			CDECsnowQuery(lowerCV_snow[[i]],1900,2015)
}

###monthly snow#####
MKTSnowlowerCV_DEC <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "December"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$SWE[which(LlowerCV[[i]]$month == "December")])
		MKTSnowlowerCV_DEC$tau[[i]] <- a$tau
		MKTSnowlowerCV_DEC$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_DEC$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_DEC$n[[i]] <- n
		MKTSnowlowerCV_DEC$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "December")[[1]]]]
		MKTSnowlowerCV_DEC$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "December")[[length(which(LlowerCV[[i]]$month == "December"))]]]]
	} else {
		MKTSnowlowerCV_DEC$n[[i]] <- n
	}
}


MKTSnowlowerCV_JAN <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "January"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$SWE[which(LlowerCV[[i]]$month == "January")])
		MKTSnowlowerCV_JAN$tau[[i]] <- a$tau
		MKTSnowlowerCV_JAN$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_JAN$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_JAN$n[[i]] <- n
		MKTSnowlowerCV_JAN$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "January")[[1]]]]
		MKTSnowlowerCV_JAN$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "January")[[length(which(LlowerCV[[i]]$month == "January"))]]]]
	} else {
		MKTSnowlowerCV_JAN$n[[i]] <- n
	}
}

MKTSnowlowerCV_FEB <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "February"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$SWE[which(LlowerCV[[i]]$month == "February")])
		MKTSnowlowerCV_FEB$tau[[i]] <- a$tau
		MKTSnowlowerCV_FEB$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_FEB$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_FEB$n[[i]] <- n
		MKTSnowlowerCV_FEB$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "February")[[1]]]]
		MKTSnowlowerCV_FEB$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "February")[[length(which(LlowerCV[[i]]$month == "February"))]]]]
	} else {
		MKTSnowlowerCV_FEB$n[[i]] <- n
	}
}

MKTSnowlowerCV_MAR <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "March"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$SWE[which(LlowerCV[[i]]$month == "March")])
		MKTSnowlowerCV_MAR$tau[[i]] <- a$tau
		MKTSnowlowerCV_MAR$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_MAR$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_MAR$n[[i]] <- n
		MKTSnowlowerCV_MAR$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "March")[[1]]]]
		MKTSnowlowerCV_MAR$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "March")[[length(which(LlowerCV[[i]]$month == "March"))]]]]
	} else {
		MKTSnowlowerCV_MAR$n[[i]] <- n
	}
}


MKTSnowlowerCV_APR <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "April"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$SWE[which(LlowerCV[[i]]$month == "April")])
		MKTSnowlowerCV_APR$tau[[i]] <- a$tau
		MKTSnowlowerCV_APR$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_APR$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_APR$n[[i]] <- n
		MKTSnowlowerCV_APR$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "April")[[1]]]]
		MKTSnowlowerCV_APR$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "April")[[length(which(LlowerCV[[i]]$month == "April"))]]]]
	} else {
		MKTSnowlowerCV_APR$n[[i]] <- n
	}
}


MKTSnowlowerCV_MAY <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "May"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$SWE[which(LlowerCV[[i]]$month == "May")])
		MKTSnowlowerCV_MAY$tau[[i]] <- a$tau
		MKTSnowlowerCV_MAY$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_MAY$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_MAY$n[[i]] <- n
		MKTSnowlowerCV_MAY$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "May")[[1]]]]
		MKTSnowlowerCV_MAY$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "May")[[length(which(LlowerCV[[i]]$month == "May"))]]]]
	} else {
		MKTSnowlowerCV_MAY$n[[i]] <- n
	}
}
write.csv(MKTSnowlowerCV_JAN,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\monthly\\MKTSnowlowerCV_JAN.csv")
write.csv(MKTSnowlowerCV_DEC,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\monthly\\MKTSnowlowerCV_DEC.csv")
write.csv(MKTSnowlowerCV_FEB,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\monthly\\MKTSnowlowerCV_FEB.csv")
write.csv(MKTSnowlowerCV_MAR,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\monthly\\MKTSnowlowerCV_MAR.csv")
write.csv(MKTSnowlowerCV_APR,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\monthly\\MKTSnowlowerCV_APR.csv")
write.csv(MKTSnowlowerCV_MAY,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\monthly\\MKTSnowlowerCV_MAY.csv")

for(i in 1:length(LlowerCV)){
	write.csv(LlowerCV[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\Data\\snow\\monthly\\",names(LlowerCV)[[i]],".csv",sep=""))
}

##daily snow###

MKTSnowlowerCV_DEC <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "December"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$value[which(LlowerCV[[i]]$month == "December")])
		MKTSnowlowerCV_DEC$tau[[i]] <- a$tau
		MKTSnowlowerCV_DEC$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_DEC$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_DEC$n[[i]] <- n
		MKTSnowlowerCV_DEC$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "December")[[1]]]]
		MKTSnowlowerCV_DEC$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "December")[[length(which(LlowerCV[[i]]$month == "December"))]]]]
	} else {
		MKTSnowlowerCV_DEC$n[[i]] <- n
	}
}


MKTSnowlowerCV_JAN <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "January"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$value[which(LlowerCV[[i]]$month == "January")])
		MKTSnowlowerCV_JAN$tau[[i]] <- a$tau
		MKTSnowlowerCV_JAN$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_JAN$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_JAN$n[[i]] <- n
		MKTSnowlowerCV_JAN$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "January")[[1]]]]
		MKTSnowlowerCV_JAN$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "January")[[length(which(LlowerCV[[i]]$month == "January"))]]]]
	} else {
		MKTSnowlowerCV_JAN$n[[i]] <- n
	}
}

MKTSnowlowerCV_FEB <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "February"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$value[which(LlowerCV[[i]]$month == "February")])
		MKTSnowlowerCV_FEB$tau[[i]] <- a$tau
		MKTSnowlowerCV_FEB$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_FEB$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_FEB$n[[i]] <- n
		MKTSnowlowerCV_FEB$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "February")[[1]]]]
		MKTSnowlowerCV_FEB$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "February")[[length(which(LlowerCV[[i]]$month == "February"))]]]]
	} else {
		MKTSnowlowerCV_FEB$n[[i]] <- n
	}
}

MKTSnowlowerCV_MAR <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "March"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$value[which(LlowerCV[[i]]$month == "March")])
		MKTSnowlowerCV_MAR$tau[[i]] <- a$tau
		MKTSnowlowerCV_MAR$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_MAR$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_MAR$n[[i]] <- n
		MKTSnowlowerCV_MAR$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "March")[[1]]]]
		MKTSnowlowerCV_MAR$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "March")[[length(which(LlowerCV[[i]]$month == "March"))]]]]
	} else {
		MKTSnowlowerCV_MAR$n[[i]] <- n
	}
}


MKTSnowlowerCV_APR <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "April"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$value[which(LlowerCV[[i]]$month == "April")])
		MKTSnowlowerCV_APR$tau[[i]] <- a$tau
		MKTSnowlowerCV_APR$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_APR$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_APR$n[[i]] <- n
		MKTSnowlowerCV_APR$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "April")[[1]]]]
		MKTSnowlowerCV_APR$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "April")[[length(which(LlowerCV[[i]]$month == "April"))]]]]
	} else {
		MKTSnowlowerCV_APR$n[[i]] <- n
	}
}


MKTSnowlowerCV_MAY <- data.frame(tau=rep(NA, length(LlowerCV)), pvalue=rep(NA, length(LlowerCV)), gauge=rep(NA, length(LlowerCV)), n=rep(NA, length(LlowerCV)), st_yr=rep(NA, length(LlowerCV)), end_yr=rep(NA, length(LlowerCV)))
for (i in 1:length(LlowerCV)){
	n <- length(which(LlowerCV[[i]]$month == "May"))
	if( n >5){
		a <- MannKendall(LlowerCV[[i]]$value[which(LlowerCV[[i]]$month == "May")])
		MKTSnowlowerCV_MAY$tau[[i]] <- a$tau
		MKTSnowlowerCV_MAY$pvalue[[i]] <- a$sl
		MKTSnowlowerCV_MAY$gauge[[i]] <- names(LlowerCV)[[i]]
		MKTSnowlowerCV_MAY$n[[i]] <- n
		MKTSnowlowerCV_MAY$st_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "May")[[1]]]]
		MKTSnowlowerCV_MAY$end_yr[[i]] <- LlowerCV[[i]]$year[[which(LlowerCV[[i]]$month == "May")[[length(which(LlowerCV[[i]]$month == "May"))]]]]
	} else {
		MKTSnowlowerCV_MAY$n[[i]] <- n
	}
}
write.csv(MKTSnowlowerCV_JAN,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\daily\\MKTSnowlowerCV_JAN.csv")
write.csv(MKTSnowlowerCV_DEC,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\daily\\MKTSnowlowerCV_DEC.csv")
write.csv(MKTSnowlowerCV_FEB,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\daily\\MKTSnowlowerCV_FEB.csv")
write.csv(MKTSnowlowerCV_MAR,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\daily\\MKTSnowlowerCV_MAR.csv")
write.csv(MKTSnowlowerCV_APR,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\daily\\MKTSnowlowerCV_APR.csv")
write.csv(MKTSnowlowerCV_MAY,file="C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\MKT\\MKT_snow\\daily\\MKTSnowlowerCV_MAY.csv")

for(i in 1:length(LlowerCV)){
	write.csv(LlowerCV[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\lowerCV\\Data\\snow\\daily\\",names(LlowerCV)[[i]],".csv",sep=""))
}
