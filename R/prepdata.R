# TODO: Add comment
# adds continuous discharge column with NA's
# Author: tiffnk
###############################################################################


prepdata <- function(raw){
	#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
	
	cont_date <- seq(from=as.Date("1900-01-01"), to=as.Date(Sys.Date()), by="day")
	cont_date <- cont_date[!(format(cont_date, format="%m-%d")=="02-29")]
	date_column_location <- which(names(raw)=="Date")
	dates_present <- as.numeric(cont_date %in% raw[[date_column_location]])

#	available <- list(date=cont_date, present=dates_present)
	discharge_column_location <- which(names(raw)=="X_00060_00003")
	addNA <- rep(NA, length(cont_date))
	
	dates_present[which(is.na(raw[[discharge_column_location]]))] <- 0
	
#	for(i in 1:length(cont_date)){
#		if(dates_present[[i]]==1){
#			addNA[[i]] <- raw[[discharge_column_location]][[which(raw[[date_column_location]]==cont_date[[i]])]]
#		}
#	}
	cont_date_zoo <- zoo(addNA, cont_date)
	raw_zoo <- zoo(raw[[discharge_column_location]],raw[[date_column_location]])
	merge_zoo <- merge(cont_date_zoo,raw_zoo)
	merge_zoo_df <- as.data.frame(merge_zoo)
	first_date <- which(dates_present==1)[[1]]
	prep <- data.frame(Date=cont_date,Discharge_cfs=merge_zoo_df[[2]],Available=dates_present, Discharge_maf=(merge_zoo_df[[2]]*86400*2.29568411e-5*1e-6))
	prep <- prep[first_date:nrow(prep),]
	return(prep)
}
