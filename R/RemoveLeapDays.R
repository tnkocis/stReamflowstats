# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


RemoveLeapDays <- function(input){
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
			filtered <- filter(input$raw, !(day==29 & month==2))
		return(filtered)
}

