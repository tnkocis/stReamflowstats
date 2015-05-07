# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


# write to text 
library(dplyr)
library(hydroTSM)
library(dataRetrieval)

gauges <- read.table("D:\\eclipse_workspace\\R_workspace\\gauge.txt",
		header=FALSE, sep="", stringsAsFactors=FALSE)

gauges <- gauges$V1

for (i in 2080:length(gauges)){
tryCatch({
			file_name <- paste("D:\\GB_Project\\Streamflow_Analysis\\TXT\\g",gauges[[i]],".csv", sep="")
			write.csv(readNWISdv(gauges[i],"00060", startDate="1900-01-01",
							endDate=Sys.Date(), statCd="00003"), file=file_name, row.names=FALSE)}, 
		error=function(e){cat("ERROR :", conditionMessage(e),"\n")})
}

# check modified date of text file 
# download from dataRetrieval
# append to text file 

last_date <- format(file.info(paste("D:\\GB_Project\\Streamflow_Analysis\\TXT\\g",gauges[[i]],".csv", sep=""))$mtime, "%Y-%d-%m")
file_name <- paste("D:\\GB_Project\\Streamflow_Analysis\\TXT\\g",gauges[[i]],".csv", sep="")
write.csv(readNWISdv(gauges[i],"00060", startDate=last_date,
				endDate=Sys.Date(), statCd="00003"), file=file_name, row.names=FALSE, append=TRUE)

# rerun stats