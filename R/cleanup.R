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

cleanup3MON <- function(input){
	if (missing(input))
		stop("Input data to clean is required.")
	for(i in length(input$Data):1){
		if(sum(input$Data[[i]]$Available, na.rm=TRUE)<85){
			input$Data[[i]] <- NULL
		}
	}	
	return(input)
}

cleanupMON <- function(input){
	if (missing(input))
		stop("Input data to clean is required.")
	
	for(i in length(input$Data):1){
		for(k in 6:1){
			if(sum(input$Data[[i]][[k]]$Available, na.rm=TRUE)<20){
			input$Data[[i]][[k]] <- NULL
			}
		}
	}	
	return(input)
}