# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


USGS11519500 <- list()
USGS11519500$raw  <- readNWISdv(11519500,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
USGS11519500$raw <- RemoveLeapDays(USGS11519500$raw)
USGS11519500$prep <- prepdata(USGS11519500$raw)
USGS11519500$Availability <- DataAvailability(USGS11519500$prep)
USGS11519500$Winter_Nov_Apr_6mon <- Split6Winter(USGS11519500$prep)
USGS11519500$Winter_DEC_FEB_3mon <- Split3Winter(USGS11519500$prep)
USGS11519500$Winter_Monthly <- SplitWinterMonthly(USGS11519500$prep)


