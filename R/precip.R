# TODO: Add comment
# 
# Author: tnkocis
###############################################################################

AMERICAN_precip_stations <- c('BL2',
				'BYM',
'HYS',
'BLC',
'SGP',
'CLF',
'GKS',
'HLH',
'FRH',
'LON',
'VVL',
'GRG',
'GTW',
'RBB',
'RBP',
'ADR',
'NCS',
'FRN',
'ALP',
'PCF',
'PHM',
'PFH',
'EDI',
'SCN',
'CAP',
'CPT',
'PCV',
'PWS',
'FLD',
'FOL',
'SIL',
'CHG',
'RNC',
'ARW',
'CSU'
)
AMERICAN_precip <- vector("list", length=length(AMERICAN_precip_stations))
names(AMERICAN_precip) <- AMERICAN_precip_stations
for(i in 1:length(AMERICAN_precip_stations)){
	AMERICAN_precip[[i]] <- CDECquery(AMERICAN_precip_stations[[i]] ,45, interval="D","1900-10-01","2015-10-01")
}

for(i in 1:length(AMERICAN_precip_stations)){
	write.csv(AMERICAN_precip[[i]], file=paste("C:\\Users\\tiffn_000\\Google Drive\\precip\\",AMERICAN_precip_stations[[i]],".csv",sep=""))
}

precipRMleapdays <- function(input){
	input[format(input$Date, "%m-%d")!="02-29",]
}
for(i in 1:length(AMERICAN_precip)){
	AMERICAN_precip[[i]] <- precipRMleapdays(AMERICAN_precip[[i]])
}

american_precip_process <- vector("list", length(AMERICAN_precip))
names(american_precip_process) <- names(AMERICAN_precip)
american_precip_process$LON <- NULL
american_precip_process$PHM <- NULL
american_precip_process$PWS <- NULL
american_precip_process$RNC <- NULL

for(z in 1:length(american_precip_process)){
	american_precip_process[[z]]$raw <- AMERICAN_precip[[z]]
	american_precip_process[[z]]$raw$Date <- as.Date(american_precip_process[[z]]$raw$datetime, "%Y-%m-%d")
	american_precip_process[[z]]$raw$datetime <- NULL
	american_precip_process[[z]]$raw$site_no <- rep(names(american_precip_process)[[z]],length(american_precip_process[[z]]$raw$year))
	
	american_precip_process[[z]]$raw <- precipRMleapdays(american_precip_process[[z]]$raw)
	american_precip_process[[z]]$raw$X_00060_00003 <- american_precip_process[[z]]$raw$value
	american_precip_process[[z]]$raw$value <- NULL
	
	american_precip_process[[z]]$Index$Valley <- "SacV"
	american_precip_process[[z]]$Index$Index <- yeartype_old$SVI
	american_precip_process[[z]]$Index$Year <- yeartype_old$Year


	###DATA PROCESSING
	american_precip_process[[z]]$prep <- prepdata(american_precip_process[[z]]$raw)
	american_precip_process[[z]]$Availability <- DataAvailability(american_precip_process[[z]]$raw)
	american_precip_process[[z]]$thresholds_maf <- thresholds(american_precip_process[[z]]$prep)

	if(all(american_precip_process[[z]]$thresholds_maf==0)){
	} else {
#	american_precip_process$record_stats <- record_stats(american_precip_process$prep, american_precip_process$thresholds_maf)
		american_precip_process[[z]]$Winter_3mon <- Split3Winter(american_precip_process[[z]]$prep, american_precip_process[[z]]$Index, american_precip_process[[z]]$thresholds_maf)
		american_precip_process[[z]]$Winter_6mon <- Split6Winter(american_precip_process[[z]]$prep, american_precip_process[[z]]$Index, american_precip_process[[z]]$thresholds_maf)
		american_precip_process[[z]]$Winter_monthly <- SplitWinterMonthly(american_precip_process[[z]]$prep, american_precip_process[[z]]$Index, american_precip_process[[z]]$thresholds_maf)
		american_precip_process[[z]]$HydroYear <- SplitHydroYear(american_precip_process[[z]]$prep, american_precip_process[[z]]$Index, american_precip_process[[z]]$thresholds_maf)	
		american_precip_process[[z]]$HydroYear <- cleanupHY(american_precip_process[[z]]$HydroYear)
		american_precip_process[[z]]$Winter_6mon <- cleanup6MON(american_precip_process[[z]]$Winter_6mon)
		american_precip_process[[z]]$Winter_3mon <- cleanup3MON(american_precip_process[[z]]$Winter_3mon)
		american_precip_process[[z]]$Winter_monthly <- cleanupMON(american_precip_process[[z]]$Winter_monthly)
		
	}
}


nablock <- function(x){
	length(x) - (which(duplicated(cumsum(rev(is.na(x)))))[1] - 1)
}

#######################################
#######################################
#######################################

for(i in 1:length(american_precip_process)){
	year <- as.numeric(format(american_precip_process[[i]]$HydroYear$All$Data$Date[[1]],"%Y"))
	MKTloop <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	
	MK <- MannKendall(american_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day[which(format(american_precip_process[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]:nablock(american_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[1]] <- MK$tau
	MKTloop$p2s[[1]] <- MK$sl
	MKTloop$station[[1]] <- american_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[1]] <- as.character(american_precip_process[[i]]$HydroYear$All$Data$Date[[which(format(american_precip_process[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]]])
	MKTloop$end_date[[1]] <- as.character(american_precip_process[[i]]$HydroYear$All$Data$Date[[nablock(american_precip_process[[i]]$HydroYear$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[1]] <- "HY"
	
	MK <- MannKendall(american_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american_precip_process[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]:nablock(american_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[2]] <- MK$tau
	MKTloop$p2s[[2]] <- MK$sl
	MKTloop$station[[2]] <- american_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[2]] <- as.character(american_precip_process[[i]]$Winter_3mon$All$Data$Date[[which(format(american_precip_process[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]]])
	MKTloop$end_date[[2]] <- as.character(american_precip_process[[i]]$Winter_3mon$All$Data$Date[[nablock(american_precip_process[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[2]] <- "3MON"
	
	MK <- MannKendall(american_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american_precip_process[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]:nablock(american_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[3]] <- MK$tau
	MKTloop$p2s[[3]] <- MK$sl
	MKTloop$station[[3]] <- american_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[3]] <- as.character(american_precip_process[[i]]$Winter_6mon$All$Data$Date[[which(format(american_precip_process[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]]])
	MKTloop$end_date[[3]] <- as.character(american_precip_process[[i]]$Winter_6mon$All$Data$Date[[nablock(american_precip_process[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)]])
	MKTloop$period[[3]] <- "6MON"
	
	MK <- MannKendall(american_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]:nablock(american_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKTloop$tau[[4]] <- MK$tau
	MKTloop$p2s[[4]] <- MK$sl
	MKTloop$station[[4]] <- american_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[4]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date[[which(format(american_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==(year))[[1]]]])
	MKTloop$end_date[[4]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$DEC$Data$Date[[nablock(american_precip_process[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)]])
	MKTloop$period[[4]] <- "DEC"
	
	MK <- MannKendall(american_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]:nablock(american_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKTloop$tau[[5]] <- MK$tau
	MKTloop$p2s[[5]] <- MK$sl
	MKTloop$station[[5]] <- american_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[5]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date[[which(format(american_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[5]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$JAN$Data$Date[[nablock(american_precip_process[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)]])
	MKTloop$period[[5]] <- "JAN"
	
	MK <- MannKendall(american_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]:nablock(american_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKTloop$tau[[6]] <- MK$tau
	MKTloop$p2s[[6]] <- MK$sl
	MKTloop$station[[6]] <- american_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[6]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date[[which(format(american_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[6]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$FEB$Data$Date[[nablock(american_precip_process[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)]])
	MKTloop$period[[6]] <- "FEB"
	
	MK <- MannKendall(american_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]:nablock(american_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[7]] <- MK$tau
	MKTloop$p2s[[7]] <- MK$sl
	MKTloop$station[[7]] <- american_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[7]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date[[which(format(american_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[7]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$MAR$Data$Date[[nablock(american_precip_process[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[7]] <- "MAR"
	
	MK <- MannKendall(american_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american_precip_process[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]:nablock(american_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[8]] <- MK$tau
	MKTloop$p2s[[8]] <- MK$sl
	MKTloop$station[[8]] <- american_precip_process[[i]]$raw$site_no[[1]]
	MKTloop$start[[8]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$APR$Data$Date[[which(format(american_precip_process[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[8]] <- as.character(american_precip_process[[i]]$Winter_monthly$All$APR$Data$Date[[nablock(american_precip_process[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)]])
	MKTloop$period[[8]] <- "APR"
	
	MKTloop$start <- as.Date(MKTloop$start)
	MKTloop$end_date <- as.Date(MKTloop$end_date)
	write.csv(MKTloop, file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT_precip\\full_record\\MKT_precip", names(american_precip_process)[[i]],"_",year,".csv", sep=""))
}



test = c(rep(NA, 3), rnorm(10), rep(NA, 5))
test2 = c(rep(NA, 3), rnorm(5), NA, rnorm(3), rep(NA, 5))
na1 = length(test) -(which(duplicated(cumsum(rev(is.na(test)))))[1] - 2)
na2 = length(test2) -(which(duplicated(cumsum(rev(is.na(test2)))))[1] - 2)

na1
na2

which(is.na(test))
which(is.na(test2))