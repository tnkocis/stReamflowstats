# TODO: remove leap days, 
# 		split into functions,
#		see notes
# 
# Author: tnkocis
###############################################################################
library(dplyr)
library(hydroTSM)
library(dataRetrieval)

gauges <- read.table("D:\\eclipse_workspace\\R_workspace\\gauge.txt",
				header=FALSE, sep="", stringsAsFactors=FALSE)
gauges <- gauges$V1

#use if doing all of gauges
#gauges_list <- vector("list", length=length(gauges))

#use if doing partial of gauges
gauges_list <- list()

for(i in 1:30){
	tryCatch({
		gauges_list[[i]] <- readNWISdv(gauges[i],"00060", startDate="1900-01-01",
			endDate=Sys.Date(), statCd="00003")}, 
		error=function(e){cat("ERROR :", conditionMessage(e),"\n")})
}

names(gauges_list) <- gauges[1:length(gauges_list)]
gauges_list <- Filter(function(x) length(x)[[1]]>0, gauges_list)

gauge_availability <- list()
for(i in 1:length(gauges_list)){
	gauge_availability[[i]] <- DataAvailability(gauges_list[[i]])
}
names(gauge_availability) <- names(gauges_list)


##FOR USGS 11519500 SCOTT R NR FORT JONES CA 

USGS11519500 <- list()
USGS11519500$raw  <- readNWISdv(11519500,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
USGS11519500$raw$Discharge_cfs <- USGS11519500$raw$X_00060_00003
USGS11519500$raw$X_00060_00003 <- NULL
USGS11519500$raw <- RemoveLeapDays(USGS11519500)
USGS11519500$Availability <- DataAvailability(USGS11519500$raw)
USGS11519500$Winter_Nov_Apr_6mon <- Split6Winter(USGS11519500)
USGS11519500$Winter_DEC_FEB_3mon <- Split3Winter(USGS11519500)
USGS11519500$Winter_Monthly <- SplitWinterMonthly(USGS11519500)

pdf(file="//Users//tiffnk//Documents//UCDAVIS//Lab//Streamflow_Analysis//Plots//USGS11519500_41_42_cont.pdf")
	par(mfrow=c(3,1))
	plot(USGS11519500$raw$Date,USGS11519500$raw$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main= "Entire Record")
	plot(USGS11519500$Winter_Nov_Apr_6mon$Data$`1941 - 1942`$Date,USGS11519500$Winter_Nov_Apr_6mon$Data$`1941 - 1942`$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="November to April")
	plot(USGS11519500$Winter_DEC_FEB$Data$`1941 - 1942`$Date,USGS11519500$Winter_DEC_FEB$Data$`1941 - 1942`$Discharge_cfs,  xlab="", ylab="Discharge (cfs)", main="December to February")
dev.off()

pdf(file="//Users//tiffnk//Documents//UCDAVIS//Lab//Streamflow_Analysis//Plots//USGS11519500_41_42_monthly.pdf")
	par(mfrow=c(3,2))
	plot(USGS11519500$Winter_Monthly$Data$`1941 - 1942`$NOV$Date,USGS11519500$Winter_Monthly$Data$`1941 - 1942`$NOV$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="November")
	plot(USGS11519500$Winter_Monthly$Data$`1941 - 1942`$DEC$Date,USGS11519500$Winter_Monthly$Data$`1941 - 1942`$DEC$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="December")
	plot(USGS11519500$Winter_Monthly$Data$`1941 - 1942`$JAN$Date,USGS11519500$Winter_Monthly$Data$`1941 - 1942`$JAN$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="January")
	plot(USGS11519500$Winter_Monthly$Data$`1941 - 1942`$FEB$Date,USGS11519500$Winter_Monthly$Data$`1941 - 1942`$FEB$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="February")
	plot(USGS11519500$Winter_Monthly$Data$`1941 - 1942`$MAR$Date,USGS11519500$Winter_Monthly$Data$`1941 - 1942`$MAR$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="March")
	plot(USGS11519500$Winter_Monthly$Data$`1941 - 1942`$APR$Date,USGS11519500$Winter_Monthly$Data$`1941 - 1942`$APR$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="April")
dev.off()