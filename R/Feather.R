# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


feather_gauges <-  c(11390000,11392500,11395030,11395200,11396000,11397000,11397500,11399500,11400500,
		11401500,11402000,11404500,11407000,11409500,11413000,11413300,11414000,11416100,11416500,11417500,
		11418000,11418500,11421000,11422500,11424000,11425500)

library(dplyr)
library(hydroTSM)
library(dataRetrieval)
library(Kendall)

SacV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_svi.txt")
SJV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\sites_for_sji.txt")
yeartype_old <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\SVISJI\\Index.csv")


feather_g <- as.numeric(feather_gauges)
txtgauges <- list.files("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\")
txtgauges <- unlist(strsplit(unlist(strsplit(txtgauges,".csv")),"g"))
txtgauges <- txtgauges[txtgauges != ""]
feather_g <- feather_g[which(feather_g %in% txtgauges)]


feather <- vector("list", length(feather_g))
names(feather) <- feather_g

for(z in 1:length(feather_g)){
	feather[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\TXT\\TXT\\","g",feather_g[[z]],".csv",sep=""), header=TRUE)
	feather[[z]]$raw$Date <- as.Date(feather[[z]]$raw$Date, "%Y-%m-%d")
	feather[[z]]$raw <- RemoveLeapDays(feather[[z]]$raw)
	
	if(as.numeric(feather[[z]]$raw$site_no[[1]]) %in% SacV_gauges$site_no){
		feather[[z]]$Index$Valley <- "SacV"
		feather[[z]]$Index$Index <- yeartype_old$SVI
		feather[[z]]$Index$Year <- yeartype_old$Year
	} else if(as.numeric(feather[[z]]$raw$site_no[[1]]) %in% SJV_gauges$site_no){
		feather[[z]]$Index$Valley <- "SJV"
		feather[[z]]$Index$Index <- yeartype_old$SJI
		feather[[z]]$Index$Year <- yeartype_old$Year
	} else {
		feather[[z]]$Index$Valley <- "ERROR"
		print(paste("Error",feather[[z]]$raw$site_no[[1]]))
	}

	###DATA PROCESSING
	feather[[z]]$prep <- prepdata(feather[[z]]$raw)
	feather[[z]]$Availability <- DataAvailability(feather[[z]]$raw)
	feather[[z]]$thresholds_maf <- thresholds(feather[[z]]$prep)
	if(all(feather[[z]]$thresholds_maf==0)){
	} else {
		feather[[z]]$Winter_3mon <- Split3Winter(feather[[z]]$prep, feather[[z]]$Index, feather[[z]]$thresholds_maf)
		feather[[z]]$Winter_6mon <- Split6Winter(feather[[z]]$prep, feather[[z]]$Index, feather[[z]]$thresholds_maf)
		feather[[z]]$Winter_monthly <- SplitWinterMonthly(feather[[z]]$prep, feather[[z]]$Index, feather[[z]]$thresholds_maf)
		feather[[z]]$HydroYear <- SplitHydroYear(feather[[z]]$prep, feather[[z]]$Index, feather[[z]]$thresholds_maf)
	}
}
for(z in 1:length(feather_g)){
		feather[[z]]$HydroYear <- cleanupHY(feather[[z]]$HydroYear)
		feather[[z]]$Winter_6mon <- cleanup6MON(feather[[z]]$Winter_6mon)
		feather[[z]]$Winter_3mon <- cleanup3MON(feather[[z]]$Winter_3mon)
		feather[[z]]$Winter_monthly <- cleanupMON(feather[[z]]$Winter_monthly)
}

for(i in 1:length(feather)){
#	year <- as.numeric(format(feather[[i]]$HydroYear$All$Data$Date[[1]],"%Y"))
	year <- 1970
	MKTloop <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	
	MK <- MannKendall(feather[[i]]$HydroYear$All$Data$Discharge_acfte6_day[which(format(feather[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]:nablock(feather[[i]]$HydroYear$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[1]] <- MK$tau
	MKTloop$p2s[[1]] <- MK$sl
	MKTloop$station[[1]] <- feather[[i]]$raw$site_no[[1]]
	MKTloop$start[[1]] <- as.character(feather[[i]]$HydroYear$All$Data$Date[[which(format(feather[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]]])
	MKTloop$end_date[[1]] <- as.character(feather[[i]]$HydroYear$All$Data$Date[[nablock(feather[[i]]$HydroYear$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[1]] <- "HY"
	
	MK <- MannKendall(feather[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(feather[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]:nablock(feather[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[2]] <- MK$tau
	MKTloop$p2s[[2]] <- MK$sl
	MKTloop$station[[2]] <- feather[[i]]$raw$site_no[[1]]
	MKTloop$start[[2]] <- as.character(feather[[i]]$Winter_3mon$All$Data$Date[[which(format(feather[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]]])
	MKTloop$end_date[[2]] <- as.character(feather[[i]]$Winter_3mon$All$Data$Date[[nablock(feather[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[2]] <- "3MON"
	
	MK <- MannKendall(feather[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(feather[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]:nablock(feather[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[3]] <- MK$tau
	MKTloop$p2s[[3]] <- MK$sl
	MKTloop$station[[3]] <- feather[[i]]$raw$site_no[[1]]
	MKTloop$start[[3]] <- as.character(feather[[i]]$Winter_6mon$All$Data$Date[[which(format(feather[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]]])
	MKTloop$end_date[[3]] <- as.character(feather[[i]]$Winter_6mon$All$Data$Date[[nablock(feather[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[3]] <- "6MON"
	
	MK <- MannKendall(feather[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(feather[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]:nablock(feather[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKTloop$tau[[4]] <- MK$tau
	MKTloop$p2s[[4]] <- MK$sl
	MKTloop$station[[4]] <- feather[[i]]$raw$site_no[[1]]
	MKTloop$start[[4]] <- as.character(feather[[i]]$Winter_monthly$All$DEC$Data$Date[[which(format(feather[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]]])
	MKTloop$end_date[[4]] <- as.character(feather[[i]]$Winter_monthly$All$DEC$Data$Date[[nablock(feather[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)]])
	MKTloop$period[[4]] <- "DEC"
	
	MK <- MannKendall(feather[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(feather[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]:nablock(feather[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKTloop$tau[[5]] <- MK$tau
	MKTloop$p2s[[5]] <- MK$sl
	MKTloop$station[[5]] <- feather[[i]]$raw$site_no[[1]]
	MKTloop$start[[5]] <- as.character(feather[[i]]$Winter_monthly$All$JAN$Data$Date[[which(format(feather[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[5]] <- as.character(feather[[i]]$Winter_monthly$All$JAN$Data$Date[[nablock(feather[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)]])
	MKTloop$period[[5]] <- "JAN"
	
	MK <- MannKendall(feather[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(feather[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]:nablock(feather[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKTloop$tau[[6]] <- MK$tau
	MKTloop$p2s[[6]] <- MK$sl
	MKTloop$station[[6]] <- feather[[i]]$raw$site_no[[1]]
	MKTloop$start[[6]] <- as.character(feather[[i]]$Winter_monthly$All$FEB$Data$Date[[which(format(feather[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[6]] <- as.character(feather[[i]]$Winter_monthly$All$FEB$Data$Date[[nablock(feather[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)]])
	MKTloop$period[[6]] <- "FEB"
	
	MK <- MannKendall(feather[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(feather[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]:nablock(feather[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[7]] <- MK$tau
	MKTloop$p2s[[7]] <- MK$sl
	MKTloop$station[[7]] <- feather[[i]]$raw$site_no[[1]]
	MKTloop$start[[7]] <- as.character(feather[[i]]$Winter_monthly$All$MAR$Data$Date[[which(format(feather[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[7]] <- as.character(feather[[i]]$Winter_monthly$All$MAR$Data$Date[[nablock(feather[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[7]] <- "MAR"
	
	MK <- MannKendall(feather[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(feather[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]:nablock(feather[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[8]] <- MK$tau
	MKTloop$p2s[[8]] <- MK$sl
	MKTloop$station[[8]] <- feather[[i]]$raw$site_no[[1]]
	MKTloop$start[[8]] <- as.character(feather[[i]]$Winter_monthly$All$APR$Data$Date[[which(format(feather[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[8]] <- as.character(feather[[i]]$Winter_monthly$All$APR$Data$Date[[nablock(feather[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[8]] <- "APR"
	
	MKTloop$start <- as.Date(MKTloop$start)
	MKTloop$end_date <- as.Date(MKTloop$end_date)
	write.csv(MKTloop, file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\MKT\\MKT_stream\\1970\\", names(feather)[[i]],"_",year,".csv", sep=""))
}

######################
##PRECIPITATION#######
######################

library(sharpshootR)

feather_precip_stations <- c("DES","DSB","PRD","CHI","HRF","WWD","CHS","PVL","ANT","CNY","KTL","GRE",
"GNV","RTL","DOY","HMB","CBO","TAY","JDP","CSH","TVL","QNC","QRD","QRM","QCY","BKP","BUP","GRZ","DAV","FRD","BKL","FOR","PRT",
"VNT","PLP","EWS","PDE","JAR","BCM","BRS","BCR","LAP","GOL","SRR","SVL","SVM","STV","SBY","ORO","FBS","SRC","DWV","DNV","CHL",
"PKC","ALY","CMV","CAM","BOL","OHD","BUD","BUL","BGR","CLG","CGT","LSP","SPN","RDH","DRC","NVD","DMF","ENG","DPH","BRE","CFW")

feather_precip <- vector("list", length=length(feather_precip_stations))
names(feather_precip) <- feather_precip_stations
for(i in 22:length(feather_precip_stations)){
	feather_precip[[i]] <- CDECquery(feather_precip_stations[[i]] ,45, interval="D","1900-10-01","2015-10-01")
}

feather_precip$JDP <- NULL
feather_precip$TVL <- NULL

for(i in 1:length(feather_precip)){
	write.csv(feather_precip[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\precip\\",names(feather_precip)[[i]],".csv",sep=""))
}

precipRMleapdays <- function(input){
	input[format(input$Date, "%m-%d")!="02-29",]
}




for(i in 1:length(feather_precip)){
	feather_precip[[i]] <- precipRMleapdays(feather_precip[[i]])
}

feather_precip_process <- vector("list", length(feather_precip))
names(feather_precip_process) <- names(feather_precip)


for(z in 60:length(feather_precip_process)){
	feather_precip_process[[z]]$raw <- feather_precip[[z]]
	feather_precip_process[[z]]$raw$Date <- as.Date(feather_precip_process[[z]]$raw$datetime, "%Y-%m-%d")
	feather_precip_process[[z]]$raw$datetime <- NULL
	feather_precip_process[[z]]$raw$site_no <- rep(names(feather_precip_process)[[z]],length(feather_precip_process[[z]]$raw$year))
	
	feather_precip_process[[z]]$raw <- precipRMleapdays(feather_precip_process[[z]]$raw)
	feather_precip_process[[z]]$raw$X_00060_00003 <- feather_precip_process[[z]]$raw$value
	feather_precip_process[[z]]$raw$value <- NULL
	
	feather_precip_process[[z]]$Index$Valley <- "SacV"
	feather_precip_process[[z]]$Index$Index <- yeartype_old$SVI
	feather_precip_process[[z]]$Index$Year <- yeartype_old$Year
	
	
	###DATA PROCESSING
	feather_precip_process[[z]]$prep <- prepdata(feather_precip_process[[z]]$raw)
	feather_precip_process[[z]]$Availability <- DataAvailability(feather_precip_process[[z]]$raw)
	feather_precip_process[[z]]$thresholds_maf <- thresholds(feather_precip_process[[z]]$prep)
	
	if(all(feather_precip_process[[z]]$thresholds_maf==0)){
	} else {
#	feather_precip_process$record_stats <- record_stats(feather_precip_process$prep, feather_precip_process$thresholds_maf)
		feather_precip_process[[z]]$Winter_3mon <- Split3Winter(feather_precip_process[[z]]$prep, feather_precip_process[[z]]$Index, feather_precip_process[[z]]$thresholds_maf)
		feather_precip_process[[z]]$Winter_6mon <- Split6Winter(feather_precip_process[[z]]$prep, feather_precip_process[[z]]$Index, feather_precip_process[[z]]$thresholds_maf)
		feather_precip_process[[z]]$Winter_monthly <- SplitWinterMonthly(feather_precip_process[[z]]$prep, feather_precip_process[[z]]$Index, feather_precip_process[[z]]$thresholds_maf)
		feather_precip_process[[z]]$HydroYear <- SplitHydroYear(feather_precip_process[[z]]$prep, feather_precip_process[[z]]$Index, feather_precip_process[[z]]$thresholds_maf)	
		feather_precip_process[[z]]$HydroYear <- cleanupHY(feather_precip_process[[z]]$HydroYear)
		feather_precip_process[[z]]$Winter_6mon <- cleanup6MON(feather_precip_process[[z]]$Winter_6mon)
		feather_precip_process[[z]]$Winter_3mon <- cleanup3MON(feather_precip_process[[z]]$Winter_3mon)
		feather_precip_process[[z]]$Winter_monthly <- cleanupMON(feather_precip_process[[z]]$Winter_monthly)
		
	}
}

feather_precip_process$QRM <- NULL
feather_precip_process$BUD <- NULL
feather_precip_process$SVM <- NULL
feather_precip_process$HRF <- NULL
feather_precip_process$OHD <- NULL

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
feather_precip_process$OHD <- NULL

for(i in 1:length(feather_precip_process)){
	year <- as.numeric(format(feather_precip_process[[i]]$HydroYear$All$Data$Date[[1]],"%Y"))
	MKTloop <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	
	MK <- MannKendall(feather_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day[which(format(feather_precip_process[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]:nablock(feather_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[1]] <- MK$tau
	MKTloop$p2s[[1]] <- MK$sl
	MKTloop$station[[1]] <- feather_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[1]] <- as.character(feather_precip_process[[i]]$HydroYear$All$Data$Date[[which(format(feather_precip_process[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]]])
	MKTloop$end_date[[1]] <- as.character(feather_precip_process[[i]]$HydroYear$All$Data$Date[[nablock(feather_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[1]] <- "HY"
	
	MK <- MannKendall(feather_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(feather_precip_process[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]:nablock(feather_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[2]] <- MK$tau
	MKTloop$p2s[[2]] <- MK$sl
	MKTloop$station[[2]] <- feather_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[2]] <- as.character(feather_precip_process[[i]]$Winter_3mon$All$Data$Date[[which(format(feather_precip_process[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]]])
	MKTloop$end_date[[2]] <- as.character(feather_precip_process[[i]]$Winter_3mon$All$Data$Date[[nablock(feather_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[2]] <- "3MON"
	
	MK <- MannKendall(feather_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(feather_precip_process[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]:nablock(feather_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[3]] <- MK$tau
	MKTloop$p2s[[3]] <- MK$sl
	MKTloop$station[[3]] <- feather_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[3]] <- as.character(feather_precip_process[[i]]$Winter_6mon$All$Data$Date[[which(format(feather_precip_process[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]]])
	MKTloop$end_date[[3]] <- as.character(feather_precip_process[[i]]$Winter_6mon$All$Data$Date[[nablock(feather_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[3]] <- "6MON"
	
	MK <- MannKendall(feather_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(feather_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]:nablock(feather_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKTloop$tau[[4]] <- MK$tau
	MKTloop$p2s[[4]] <- MK$sl
	MKTloop$station[[4]] <- feather_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[4]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date[[which(format(feather_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]]])
	MKTloop$end_date[[4]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date[[nablock(feather_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)]])
	MKTloop$period[[4]] <- "DEC"
	
	MK <- MannKendall(feather_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(feather_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]:nablock(feather_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKTloop$tau[[5]] <- MK$tau
	MKTloop$p2s[[5]] <- MK$sl
	MKTloop$station[[5]] <- feather_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[5]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date[[which(format(feather_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[5]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date[[nablock(feather_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)]])
	MKTloop$period[[5]] <- "JAN"
	
	MK <- MannKendall(feather_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(feather_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]:nablock(feather_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKTloop$tau[[6]] <- MK$tau
	MKTloop$p2s[[6]] <- MK$sl
	MKTloop$station[[6]] <- feather_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[6]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date[[which(format(feather_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[6]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date[[nablock(feather_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)]])
	MKTloop$period[[6]] <- "FEB"
	
	MK <- MannKendall(feather_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(feather_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]:nablock(feather_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[7]] <- MK$tau
	MKTloop$p2s[[7]] <- MK$sl
	MKTloop$station[[7]] <- feather_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[7]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date[[which(format(feather_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[7]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date[[nablock(feather_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[7]] <- "MAR"
	
	MK <- MannKendall(feather_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(feather_precip_process[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]:nablock(feather_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[8]] <- MK$tau
	MKTloop$p2s[[8]] <- MK$sl
	MKTloop$station[[8]] <- feather_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[8]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$APR$Data$Date[[which(format(feather_precip_process[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[8]] <- as.character(feather_precip_process[[i]]$Winter_monthly$All$APR$Data$Date[[nablock(feather_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[8]] <- "APR"
	
	MKTloop$start <- as.Date(MKTloop$start)
	MKTloop$end_date <- as.Date(MKTloop$end_date)
	write.csv(MKTloop, file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\MKT\\MKT_precip\\", names(feather_precip_process)[[i]],"_",year,".csv", sep=""))
}

#################
######SNOW#######
#################

feather_snow <- c(
		"LLP",
		"KTL",
		"GRZ",
		"PLP"
#		"GOL",
#		"HMB",
#		"HRK",
#		"RTL",
#		"BKL",
#		"FOR",
#		"MDW",
#		"CSL",
#		"RCC"
				)

Lfeather <- vector("list", length=length(feather_snow))
names(Lfeather) <- feather_snow
for(i in 1:length(feather_snow)){
	Lfeather[[i]] <- 
#			CDECquery(feather_snow[[i]], 3, interval = "D", "1900-01-01", "2015-10-01")
	
			CDECsnowQuery(feather_snow[[i]],1900,2015)
}

MKTSnowfeather_DEC <- data.frame(tau=rep(NA, length(Lfeather)), pvalue=rep(NA, length(Lfeather)), gauge=rep(NA, length(Lfeather)), n=rep(NA, length(Lfeather)), st_yr=rep(NA, length(Lfeather)), end_yr=rep(NA, length(Lfeather)))
for (i in 1:length(Lfeather)){
	n <- length(which(Lfeather[[i]]$month == "December"))
	if( n >5){
		a <- MannKendall(Lfeather[[i]]$SWE[which(Lfeather[[i]]$month == "December")])
		MKTSnowfeather_DEC$tau[[i]] <- a$tau
		MKTSnowfeather_DEC$pvalue[[i]] <- a$sl
		MKTSnowfeather_DEC$gauge[[i]] <- names(Lfeather)[[i]]
		MKTSnowfeather_DEC$n[[i]] <- n
		MKTSnowfeather_DEC$st_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "December")[[1]]]]
		MKTSnowfeather_DEC$end_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "December")[[length(which(Lfeather[[i]]$month == "December"))]]]]
	} else {
		MKTSnowfeather_DEC$n[[i]] <- n
	}
}


MKTSnowfeather_JAN <- data.frame(tau=rep(NA, length(Lfeather)), pvalue=rep(NA, length(Lfeather)), gauge=rep(NA, length(Lfeather)), n=rep(NA, length(Lfeather)), st_yr=rep(NA, length(Lfeather)), end_yr=rep(NA, length(Lfeather)))
for (i in 1:length(Lfeather)){
	n <- length(which(Lfeather[[i]]$month == "January"))
	if( n >5){
		a <- MannKendall(Lfeather[[i]]$SWE[which(Lfeather[[i]]$month == "January")])
		MKTSnowfeather_JAN$tau[[i]] <- a$tau
		MKTSnowfeather_JAN$pvalue[[i]] <- a$sl
		MKTSnowfeather_JAN$gauge[[i]] <- names(Lfeather)[[i]]
		MKTSnowfeather_JAN$n[[i]] <- n
		MKTSnowfeather_JAN$st_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "January")[[1]]]]
		MKTSnowfeather_JAN$end_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "January")[[length(which(Lfeather[[i]]$month == "January"))]]]]
	} else {
		MKTSnowfeather_JAN$n[[i]] <- n
	}
}

MKTSnowfeather_FEB <- data.frame(tau=rep(NA, length(Lfeather)), pvalue=rep(NA, length(Lfeather)), gauge=rep(NA, length(Lfeather)), n=rep(NA, length(Lfeather)), st_yr=rep(NA, length(Lfeather)), end_yr=rep(NA, length(Lfeather)))
for (i in 1:length(Lfeather)){
	n <- length(which(Lfeather[[i]]$month == "February"))
	if( n >5){
		a <- MannKendall(Lfeather[[i]]$SWE[which(Lfeather[[i]]$month == "February")])
		MKTSnowfeather_FEB$tau[[i]] <- a$tau
		MKTSnowfeather_FEB$pvalue[[i]] <- a$sl
		MKTSnowfeather_FEB$gauge[[i]] <- names(Lfeather)[[i]]
		MKTSnowfeather_FEB$n[[i]] <- n
		MKTSnowfeather_FEB$st_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "February")[[1]]]]
		MKTSnowfeather_FEB$end_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "February")[[length(which(Lfeather[[i]]$month == "February"))]]]]
	} else {
		MKTSnowfeather_FEB$n[[i]] <- n
	}
}

MKTSnowfeather_MAR <- data.frame(tau=rep(NA, length(Lfeather)), pvalue=rep(NA, length(Lfeather)), gauge=rep(NA, length(Lfeather)), n=rep(NA, length(Lfeather)), st_yr=rep(NA, length(Lfeather)), end_yr=rep(NA, length(Lfeather)))
for (i in 1:length(Lfeather)){
	n <- length(which(Lfeather[[i]]$month == "March"))
	if( n >5){
		a <- MannKendall(Lfeather[[i]]$SWE[which(Lfeather[[i]]$month == "March")])
		MKTSnowfeather_MAR$tau[[i]] <- a$tau
		MKTSnowfeather_MAR$pvalue[[i]] <- a$sl
		MKTSnowfeather_MAR$gauge[[i]] <- names(Lfeather)[[i]]
		MKTSnowfeather_MAR$n[[i]] <- n
		MKTSnowfeather_MAR$st_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "March")[[1]]]]
		MKTSnowfeather_MAR$end_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "March")[[length(which(Lfeather[[i]]$month == "March"))]]]]
	} else {
		MKTSnowfeather_MAR$n[[i]] <- n
	}
}


MKTSnowfeather_APR <- data.frame(tau=rep(NA, length(Lfeather)), pvalue=rep(NA, length(Lfeather)), gauge=rep(NA, length(Lfeather)), n=rep(NA, length(Lfeather)), st_yr=rep(NA, length(Lfeather)), end_yr=rep(NA, length(Lfeather)))
for (i in 1:length(Lfeather)){
	n <- length(which(Lfeather[[i]]$month == "April"))
	if( n >5){
		a <- MannKendall(Lfeather[[i]]$SWE[which(Lfeather[[i]]$month == "April")])
		MKTSnowfeather_APR$tau[[i]] <- a$tau
		MKTSnowfeather_APR$pvalue[[i]] <- a$sl
		MKTSnowfeather_APR$gauge[[i]] <- names(Lfeather)[[i]]
		MKTSnowfeather_APR$n[[i]] <- n
		MKTSnowfeather_APR$st_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "April")[[1]]]]
		MKTSnowfeather_APR$end_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "April")[[length(which(Lfeather[[i]]$month == "April"))]]]]
	} else {
		MKTSnowfeather_APR$n[[i]] <- n
	}
}


MKTSnowfeather_MAY <- data.frame(tau=rep(NA, length(Lfeather)), pvalue=rep(NA, length(Lfeather)), gauge=rep(NA, length(Lfeather)), n=rep(NA, length(Lfeather)), st_yr=rep(NA, length(Lfeather)), end_yr=rep(NA, length(Lfeather)))
for (i in 1:length(Lfeather)){
	n <- length(which(Lfeather[[i]]$month == "May"))
	if( n >5){
		a <- MannKendall(Lfeather[[i]]$SWE[which(Lfeather[[i]]$month == "May")])
		MKTSnowfeather_MAY$tau[[i]] <- a$tau
		MKTSnowfeather_MAY$pvalue[[i]] <- a$sl
		MKTSnowfeather_MAY$gauge[[i]] <- names(Lfeather)[[i]]
		MKTSnowfeather_MAY$n[[i]] <- n
		MKTSnowfeather_MAY$st_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "May")[[1]]]]
		MKTSnowfeather_MAY$end_yr[[i]] <- Lfeather[[i]]$year[[which(Lfeather[[i]]$month == "May")[[length(which(Lfeather[[i]]$month == "May"))]]]]
	} else {
		MKTSnowfeather_MAY$n[[i]] <- n
	}
}

write.csv(MKTSnowfeather_JAN,file="C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\snow\\monthly\\MKTSnowfeather_JAN.csv")
write.csv(MKTSnowfeather_DEC,file="C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\snow\\monthly\\MKTSnowfeather_DEC.csv")
write.csv(MKTSnowfeather_FEB,file="C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\snow\\monthly\\MKTSnowfeather_FEB.csv")
write.csv(MKTSnowfeather_MAR,file="C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\snow\\monthly\\MKTSnowfeather_MAR.csv")
write.csv(MKTSnowfeather_APR,file="C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\snow\\monthly\\MKTSnowfeather_APR.csv")
write.csv(MKTSnowfeather_MAY,file="C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\snow\\monthly\\MKTSnowfeather_MAY.csv")

for(i in 1:length(Lfeather)){
	write.csv(Lfeather[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\Feather\\snow\\monthly\\",names(Lfeather)[[i]],".csv",sep=""))
}