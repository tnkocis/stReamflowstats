# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


totalflow <- function(data, timestep){ 
	
	if (missing(data))
		stop("Input data is required.")
	if (missing(timestep))
		stop("Timestep to total over is required (monthly,3mon,6mon,yearly).")

	if (timestep=="monthly"){
		total <- vector("list", length=6)
		for(n in 1:6){
			total[[n]] <- data.frame(total_flow_cfs = rep(NA, length(data)), wy_st = rep(NA, length(data)), wy_end = rep(NA, length(data)),center_date = rep(NA, length(data)))
		}
		for(i in 1:length(data)){
			for(n in 1:6){
				total[[n]]$total_flow_cfs[[i]] <- sum(data[[i]][[n]]$Discharge_cfs)
				total[[n]]$wy_st[[i]] <- format(data[[i]][[n]]$Date[[1]],"%Y")
				total[[n]]$wy_end[[i]] <- format(data[[i]][[n]]$Date[[length(data[[i]][[n]]$Date)]],"%Y")
				total[[n]]$center_date[[i]] <- median(data[[i]][[n]]$Date)
			}
		}
		for(n in 1:6){
			total[[n]]$center_date <- as.Date(total[[n]]$center_date)
		}
		names(total) <- names(data[[1]])
	}
	else {
		total <- data.frame(total_flow_cfs = rep(NA, length(data)), wy_st = rep(NA, length(data)), wy_end = rep(NA, length(data)), center_date = rep(NA, length(data)))
		for(i in 1:length(data)){
			total$total_flow_cfs[[i]] <- sum(data[[i]]$Discharge_cfs)
			total$wy_st[[i]] <- format(data[[i]]$Date[[1]],"%Y")
			total$wy_end[[i]] <- format(data[[i]]$Date[[length(data[[i]]$Date)]],"%Y")
			total$center_date[[i]] <- median(data[[i]]$Date)
		}
		total$center_date <- as.Date(total$center_date)
	}

	return(total)
}
