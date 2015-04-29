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

year_type <- function(input){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	year <- as.numeric(format(input$raw$Date,"%Y"))
	month <- as.numeric(format(input$raw$Date,"%m"))
	day <- as.numeric(format(input$raw$Date,"%d"))
	years <- seq(min(year), max(year), by=1)
	
	year_type <- list()
	for (i in seq(1,length(years)-1,1)){
		year_type[[i]] <- filter(input, (month >9 & year == years[i])| (month <10 & year == years[i]+1))
		names(year_type)[i] <- paste(years[i],"-",years[i]+1)
		year_type[[i]]["Discharge_ft3_day"] <- as.numeric(year_type[[i]]$Discharge_cfs)*86400
		year_type[[i]]["Discharge_acft_day"] <- year_type[[i]]$Discharge_ft3_day*(2.29568411e-5)
		year_type[[i]]["Discharge_acfte6_day"] <- year_type[[i]]$Discharge_acft_day*1e-6
	}
	
## stopped here
	summary <- list()
	summary$TotalQ
}