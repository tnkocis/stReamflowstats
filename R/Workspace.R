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
USGS11519500$data  <- readNWISdv(11519500,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")

## remove leap days -- see michael
for(i in 1:length(USGS11519500$data)){
	pos_removal <- which((format(USGS11519500$data$Date, format="%m-%d")=="02-29"))
	USGS11519500$data[i] <- USGS11519500$data[i][[1]][!]
}


USGS11519500$Availability <- DataAvailability(USGS11519500$data)