# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


cleanupHY <- function(input){
	if (missing(input))
		stop("Input data to clean is required.")
	for(i in length(input$Data):1){
		if(sum(input$Data[[i]]$Available, na.rm=TRUE)<365){
			input$Data[[i]] <- NULL
		}
	}	
	return(input)
}
cleanup6MON <- function(input){
	if (missing(input))
		stop("Input data to clean is required.")
	for(i in length(input$Data):1){
		if(sum(input$Data[[i]]$Available, na.rm=TRUE)<170){
			input$Data[[i]] <- NULL
		}
	}	
	return(input)
}