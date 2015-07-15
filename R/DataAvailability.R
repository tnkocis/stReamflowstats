# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


DataAvailability <- function(input){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#

	cont_date <- seq(from=as.Date("1900-01-01"), to=as.Date(Sys.Date()), by="day")
	cont_date <- cont_date[!(format(cont_date, format="%m-%d")=="02-29")]
	date_column_location <- which(names(input)=="Date")
	dates_present <- as.numeric(cont_date %in% input[[date_column_location]])	

	
	cont_year <- format(seq(from=as.Date("1900-01-01"),
					to=as.Date(Sys.Date()),by="year"), format="%Y")
	cont_month <- format(seq(from=as.Date("1900-01-01"), 
					to=as.Date(Sys.Date()), by="month"), format="%Y-%m")
	
	dec <- rep(1, length.out=round(length(cont_year)/10))
	dec_name <- rep("year", length.out=round(length(cont_year)/10))
	for (i in 1:length(dec)){
		dec[i] <- length(which(dates_present[((365*(i-1)*10)+1):(365*i*10)]==1))
		dec_name[i] <- paste((1900+((i-1)*10)),"-",(1900+(i*10)))
		
	}
	dec_name <- as.character(dec_name)
	decadal <- data.frame(decade=dec_name, available_days=dec, fraction_available=dec/(365*10))
	
	yr <- rep(1,length.out=length(cont_year))
	for(i in 1:length(cont_year)){
		yr[i] <- length(which(dates_present[((365*(i-1))+1):(365*i)]==1))
	}
	cont_year <- format(as.Date(cont_year, format="%Y"), "%Y")
	yearly <- data.frame(year=cont_year, available_days=yr,fraction_available=(yr/365))
	
	leng_mon <- cumsum(c(0, rep(c(31,28,31,30,31,30,31,31,30,31,30,31), length.out=length(cont_month))))
	month_length <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31), length.out=length(cont_month))
	mon <- rep(1,length.out=length(cont_month))
	for(i in 1:length(cont_month)){
		mon[i] <- length(which(dates_present[(leng_mon[i]+1):leng_mon[i+1]]==1))
	}
#	cont_month <- strptime(as.character(cont_month), format="%Y-%m")
	monthly <- data.frame(month=cont_month,available_days=mon, fraction_available=(mon/month_length))
	available <- list(yearly=yearly, monthly=monthly, decadal=decadal)
	return(available)
	
}

