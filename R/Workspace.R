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

