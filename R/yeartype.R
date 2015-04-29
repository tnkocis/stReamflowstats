# TODO: Add comment
# 
# Author: tnkocis
###############################################################################


#finding percentile interval of SVI
# g1 USGS 11377100
# g2 USGS 11407000
# g3 USGS 11418000
# g4 USGS 11446500

Index_gauges <- list()
Index_gauges$g1$raw <- read.csv("D:\\GB_Project\\Streamflow_Analysis\\TXT\\g11377100.csv")
Index_gauges$g1$raw$Date <- as.Date(Index_gauges$g1$raw$Date)
Index_gauges$g2$raw <- read.csv("D:\\GB_Project\\Streamflow_Analysis\\TXT\\g11407000.csv")
Index_gauges$g2$raw$Date <- as.Date(Index_gauges$g2$raw$Date)
Index_gauges$g3$raw <- read.csv("D:\\GB_Project\\Streamflow_Analysis\\TXT\\g11418000.csv")
Index_gauges$g3$raw$Date <- as.Date(Index_gauges$g3$raw$Date)
Index_gauges$g4$raw <- read.csv("D:\\GB_Project\\Streamflow_Analysis\\TXT\\g11446500.csv")
Index_gauges$g4$raw$Date <- as.Date(Index_gauges$g4$raw$Date)

for (i in 1:length(Index_gauges)){
	Index_gauges[[i]]$raw <- RemoveLeapDays(Index_gauges[[i]])
}


for (i in 1:length(Index_gauges)){
	Index_gauges[[i]]$raw$Discharge_cfs <- Index_gauges[[i]]$raw$X_00060_00003
	Index_gauges[[i]]$raw$X_00060_00003 <- NULL
}

for (i in 1:length(Index_gauges)){
	Index_gauges[[i]]$by_year <- SplitHydroYear(Index_gauges[[i]])
}


b <- DataAvailability(Index_gauges[[3]])
