# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


totalflow <- function(data, timestep){
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(data))
		stop("Input data is required.")
	if (missing(timestep))
		stop("Timestep to total over is required.")

	if (timestep="monthly"){
		total <- data.frame(total_flow_cfs = rep(NA, length(data)), wy_st = rep(NA, length(data)), wy_end = rep(NA, length(data)))
		for(i in 1:length(data)){
			total$total_flow_cfs[[i]] <- sum(data[[i]]$Discharge_cfs)
			total$wy_st <- format(data[[i]]$Date[[1]],"%Y")
			total$wy_end <- format(data[[i]]$Date[[length(data[[i]]$Date)]],"%Y")
		}
	}
	if (timestep="3mon"){
		total <- data.frame(total_flow_cfs = rep(NA, length(data)), wy_st = rep(NA, length(data)), wy_end = rep(NA, length(data)))
		for(i in 1:length(data)){
			total$total_flow_cfs[[i]] <- sum(data[[i]]$Discharge_cfs)
			total$wy_st <- format(data[[i]]$Date[[1]],"%Y")
			total$wy_end <- format(data[[i]]$Date[[length(data[[i]]$Date)]],"%Y")
		}
	
	}
	if (timestep="6mon"){
		
	}
	if (timestep="yearly"){
		
	}
}
