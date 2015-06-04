# TODO: Add comment
# 
# Author: tiffnk
###############################################################################
library(dplyr)
library(hydroTSM)
library(dataRetrieval)

SacV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\Sacramento_Valley.csv")
SJV_gauges <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\Stream_Gauges_Raw_Data\\Sites_list_huc\\SanJoaquin_Valley.csv")

USGS11377100 <- list()
USGS11377100$raw  <- readNWISdv(11377100,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
USGS11377100$raw <- RemoveLeapDays(USGS11377100$raw)

if(as.numeric(USGS11377100$raw$site_no[[1]]) %in% SacV_gauges$site_no){
	USGS11377100$Index$Valley <- "SacV"
	USGS11377100$Index$Index <- YEARTYPEdf$SacV_Round
	USGS11377100$Index$Year <- YEARTYPEdf$Year
} else if(as.numeric(USGS11377100$raw$site_no[[1]]) %in% SJV_gauges$site_no){
	USGS11377100$Index$Valley <- "SJV"
	USGS11377100$Index$Index <- YEARTYPEdf$SJV_Round
	USGS11377100$Index$Year <- YEARTYPEdf$Year
} else {
	USGS11377100$Index$Valley <- "ERROR"
}

USGS11377100$prep <- prepdata(USGS11377100$raw)
USGS11377100$Availability <- DataAvailability(USGS11377100$prep)
USGS11377100$Winter_NOV_APR_6mon <- Split6Winter(USGS11377100$prep, USGS11377100$Index)
USGS11377100$Winter_DEC_FEB_3mon <- Split3Winter(USGS11377100$prep, USGS11377100$Index)
USGS11377100$Winter_Monthly <- SplitWinterMonthly(USGS11377100$prep, USGS11377100$Index)


C:\Users\tiffn_000\Documents\workspaces\eclipse_workspace\TXT\TXT