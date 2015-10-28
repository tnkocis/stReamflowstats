# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


######################
#######################
#######################
year <- 1981

for(i in 1:length(american)){
	MKTloop <- data.frame(period = rep(NA, 8), tau = rep(NA, 8), p2s = rep(NA, 8), station=rep(NA,8),start = rep(NA, 8), end_date = rep(NA, 8))
	
	MK <- MannKendall(american[[i]]$HydroYear$All$Data$Discharge_acfte6_day[which(format(american[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]:length(american[[i]]$HydroYear$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[1]] <- MK$tau
	MKTloop$p2s[[1]] <- MK$sl
	MKTloop$station[[1]] <- american[[i]]$raw$site_no[[1]]
	MKTloop$start[[1]] <- as.character(american[[i]]$HydroYear$All$Data$Date[[which(format(american[[i]]$HydroYear$All$Data$Date,"%Y-%m")==paste(year,"-10", sep=""))[[1]]]])
	MKTloop$end_date[[1]] <- as.character(tail(american[[i]]$HydroYear$All$Data$Date,1))
	MKTloop$period[[1]] <- "HY"
	
	MK <- MannKendall(american[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]:length(american[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[2]] <- MK$tau
	MKTloop$p2s[[2]] <- MK$sl
	MKTloop$station[[2]] <- american[[i]]$raw$site_no[[1]]
	MKTloop$start[[2]] <- as.character(american[[i]]$Winter_3mon$All$Data$Date[[which(format(american[[i]]$Winter_3mon$All$Data$Date,"%Y-%m")==paste(year,"-12", sep=""))[[1]]]])
	MKTloop$end_date[[2]] <- as.character(tail(american[[i]]$Winter_3mon$All$Data$Date,1))
	MKTloop$period[[2]] <- "3MON"
	
	MK <- MannKendall(american[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]:length(american[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day)])
	MKTloop$tau[[3]] <- MK$tau
	MKTloop$p2s[[3]] <- MK$sl
	MKTloop$station[[3]] <- american[[i]]$raw$site_no[[1]]
	MKTloop$start[[3]] <- as.character(american[[i]]$Winter_6mon$All$Data$Date[[which(format(american[[i]]$Winter_6mon$All$Data$Date,"%Y-%m")==paste(year,"-11", sep=""))[[1]]]])
	MKTloop$end_date[[3]] <- as.character(tail(american[[i]]$Winter_6mon$All$Data$Date,1))
	MKTloop$period[[3]] <- "6MON"
	
	
	MK <- MannKendall(american[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==year)[[1]]:length(american[[i]]$Winter_monthly$All$DEC$Data$Discharge_acfte6_day)])
	MKTloop$tau[[4]] <- MK$tau
	MKTloop$p2s[[4]] <- MK$sl
	MKTloop$station[[4]] <- american[[i]]$raw$site_no[[1]]
	MKTloop$start[[4]] <- as.character(american[[i]]$Winter_monthly$All$DEC$Data$Date[[which(format(american[[i]]$Winter_monthly$All$DEC$Data$Date,"%Y")==year)[[1]]]])
	MKTloop$end_date[[4]] <- as.character(tail(american[[i]]$Winter_monthly$All$DEC$Data$Date,1))
	MKTloop$period[[4]] <- "DEC"
	
	MK <- MannKendall(american[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]:length(american[[i]]$Winter_monthly$All$JAN$Data$Discharge_acfte6_day)])
	MKTloop$tau[[5]] <- MK$tau
	MKTloop$p2s[[5]] <- MK$sl
	MKTloop$station[[5]] <- american[[i]]$raw$site_no[[1]]
	MKTloop$start[[5]] <- as.character(american[[i]]$Winter_monthly$All$JAN$Data$Date[[which(format(american[[i]]$Winter_monthly$All$JAN$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[5]] <- as.character(tail(american[[i]]$Winter_monthly$All$JAN$Data$Date,1))
	MKTloop$period[[5]] <- "JAN"
	
	MK <- MannKendall(american[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]:length(american[[i]]$Winter_monthly$All$FEB$Data$Discharge_acfte6_day)])
	MKTloop$tau[[6]] <- MK$tau
	MKTloop$p2s[[6]] <- MK$sl
	MKTloop$station[[6]] <- american[[i]]$raw$site_no[[1]]
	MKTloop$start[[6]] <- as.character(american[[i]]$Winter_monthly$All$FEB$Data$Date[[which(format(american[[i]]$Winter_monthly$All$FEB$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[6]] <- as.character(tail(american[[i]]$Winter_monthly$All$FEB$Data$Date,1))
	MKTloop$period[[6]] <- "FEB"
	
	MK <- MannKendall(american[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]:length(american[[i]]$Winter_monthly$All$MAR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[7]] <- MK$tau
	MKTloop$p2s[[7]] <- MK$sl
	MKTloop$station[[7]] <- american[[i]]$raw$site_no[[1]]
	MKTloop$start[[7]] <- as.character(american[[i]]$Winter_monthly$All$MAR$Data$Date[[which(format(american[[i]]$Winter_monthly$All$MAR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[7]] <- as.character(tail(american[[i]]$Winter_monthly$All$MAR$Data$Date,1))
	MKTloop$period[[7]] <- "MAR"
	
	MK <- MannKendall(american[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day[which(format(american[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]:length(american[[i]]$Winter_monthly$All$APR$Data$Discharge_acfte6_day)])
	MKTloop$tau[[8]] <- MK$tau
	MKTloop$p2s[[8]] <- MK$sl
	MKTloop$station[[8]] <- american[[i]]$raw$site_no[[1]]
	MKTloop$start[[8]] <- as.character(american[[i]]$Winter_monthly$All$APR$Data$Date[[which(format(american[[i]]$Winter_monthly$All$APR$Data$Date,"%Y")==(year+1))[[1]]]])
	MKTloop$end_date[[8]] <- as.character(tail(american[[i]]$Winter_monthly$All$APR$Data$Date,1))
	MKTloop$period[[8]] <- "APR"
	
	MKTloop$start <- as.Date(MKTloop$start)
	MKTloop$end_date <- as.Date(MKTloop$end_date)
	write.csv(MKTloop, file=paste("C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKT_stream_sites\\",year,"\\MKT_", names(american)[[i]],"_",year,".csv", sep=""))
}













