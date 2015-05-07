# TODO: Add comment
# 
# Author: tnkocis
###############################################################################


#finding percentile interval of SVI
# g1 USGS 11377100
# g2 USGS 11407000
# g3 USGS 11418000
# g4 USGS 11446500

gauges_yeartype <- read.csv("//Users//tiffnk//eclipse_workspace//list_for_yeartype.csv")
yeartype <- list()
for (i in 1:length(gauges_yeartype$SiteNumber)){
	yeartype[[i]] <- list()
}
	names(yeartype) <- gauges_yeartype$SiteNumber

for (i in 1:length(yeartype)){
		yeartype[[i]]$raw <- readNWISdv(paste(gauges_yeartype$SiteNumber[[i]]),"00060", startDate="1900-01-01",
				endDate=Sys.Date(), statCd="00003")
	}
for (i in 1:length(yeartype)){	
		yeartype[[i]]$raw <- RemoveLeapDays(yeartype[[i]]$raw)
	}
for (i in 1:length(yeartype)){	
		yeartype[[i]]$prep <- prepdata(yeartype[[i]]$raw)
	}
for (i in 1:length(yeartype)){	
		yeartype[[i]]$Yearly <- SplitHydroYear(yeartype[[i]]$prep)
}



	USGS11519500$raw  <- readNWISdv(11519500,"00060", startDate="1900-01-01",
			endDate=Sys.Date(), statCd="00003")
	USGS11519500$raw <- RemoveLeapDays(USGS11519500)
	USGS11519500$prep <- prepdata(USGS11519500$raw)
	USGS11519500$Availability <- DataAvailability(USGS11519500$prep)
	USGS11519500$Winter_Nov_Apr_6mon <- Split6Winter(USGS11519500$prep)
	USGS11519500$Winter_DEC_FEB_3mon <- Split3Winter(USGS11519500$prep)
	USGS11519500$Winter_Monthly <- SplitWinterMonthly(USGS11519500$prep)

