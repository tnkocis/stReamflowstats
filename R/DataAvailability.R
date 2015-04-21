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
	available <- list(date=cont_date, present=dates_present)
	cont_year <- format(seq(from=as.Date("1900-01-01"),
					to=as.Date(Sys.Date()),by="year"), format="%Y")
	cont_month <- format(seq(from=as.Date("1900-01-01"), 
					to=as.Date(Sys.Date()), by="month"), format="%Y-%m")

	dec.1900 <- length(which(dates_present[1:(365*10)]==1))
	dec.1910 <- length(which(dates_present[((365*10)+1):(365*20)]==1))
	dec.1920 <- length(which(dates_present[((365*20)+1):(365*30)]==1))
	dec.1930 <- length(which(dates_present[((365*30)+1):(365*40)]==1))
	dec.1940 <- length(which(dates_present[((365*40)+1):(365*50)]==1))
	dec.1950 <- length(which(dates_present[((365*50)+1):(365*60)]==1))
	dec.1960 <- length(which(dates_present[((365*60)+1):(365*70)]==1))
	dec.1970 <- length(which(dates_present[((365*70)+1):(365*80)]==1))
	dec.1980 <- length(which(dates_present[((365*80)+1):(365*90)]==1))
	dec.1990 <- length(which(dates_present[((365*90)+1):(365*100)]==1))
	dec.2000 <- length(which(dates_present[((365*100)+1):(365*110)]==1))
	dec.2010 <- length(which(dates_present[((365*110)+1):length(dates_present)]==1))
	decade <- data.frame(decade=c("1900-1910","1910-1920","1920-1930","1930-1940","1940-1950",
					"1950-1960","1960-1970","1970-1980","1980-1990","1990-2000",
					"2000-2010","2010-present"),available_days=c(dec.1900,dec.1910,
					dec.1920,dec.1930,dec.1940,dec.1950,dec.1960,dec.1970,dec.1980,
					dec.1990,dec.2000,dec.2010), fraction_available=(c(dec.1900,dec.1910,
					dec.1920,dec.1930,dec.1940,dec.1950,dec.1960,dec.1970,dec.1980,
					dec.1990,dec.2000,dec.2010)/(365*10)))
	
	yr <- rep(1,length.out=length(cont_year))
	for(i in 1:length(cont_year)){
		yr[i] <- length(which(dates.present[((365*(i-1))+1):(365*i)]==1))
	}
	yearly <- data.frame(year=cont_year,available_days=yr,fraction_available=(yr/365))
	
	leng_mon <- cumsum(c(0, rep(c(31,28,31,30,31,30,31,31,30,31,30,31), length.out=length(cont_month))))
	month_length <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31), length.out=length(cont_month))
	mon <- rep(1,length.out=length(cont_month))
	for(i in 1:length(cont_month)){
		mon[i] <- length(which(dates_present[(leng_mon[i]+1):leng_mon[i+1]]==1))
	}
	monthly <- data.frame(month=cont_month,available_days=mon, fraction_available=(mon/month_length))
	available <- list(decade=decade, yearly=yearly, monthly=monthly)
	return(available)
	
}

