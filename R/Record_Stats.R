# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


record_stats <- function(input, percentiles){
	stats <- list()
	discharge_acfte6 <- input$Discharge_cfs*86400*2.29568411e-5*1e-6
	stats[["Total_Q_acfte6"]] <- sum(discharge_acfte6, na.rm=TRUE)
	stats[["Mean_acfte6"]] <- mean(discharge_acfte6, na.rm=TRUE)
	stats[["Median_acfte6"]] <- median(discharge_acfte6, na.rm=TRUE)
	stats[["min_acfte6"]] <- min(discharge_acfte6, na.rm=TRUE)
	stats[["max_acfte6"]] <- max(discharge_acfte6, na.rm=TRUE)
	stats[["max_date"]] <- input[["Date"]][which(discharge_acfte6==stats["max_acfte6"])]
	stats[["min_date"]] <- input[["Date"]][which(discharge_acfte6==stats["min_acfte6"])]
	stats[["Thresholds"]] <- list()
	stats[["Thresholds"]][["coded"]] <- list()
	for (k in 1:length(percentiles)){
		for (n in 1:length(discharge_acfte6)){
			if(is.na(discharge_acfte6[[n]])){
				stats[["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
			} else if (discharge_acfte6[[n]] >= percentiles[[k]]){
				stats[["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 1
			} else if (discharge_acfte6[[n]] < percentiles[[k]]){
				stats[["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- 0	
			} else {
				stats[["Thresholds"]][["coded"]][[names(percentiles)[k]]][[n]] <- NA
			}
		}
	}
	stats[["Thresholds"]][["Totals"]] <- list()
	stats[["Thresholds"]][["Totals"]][["Thresholds"]] <- c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95)
	for (i in 1:length(stats[["Thresholds"]][["Totals"]][["Thresholds"]])){
		stats[["Thresholds"]][["Totals"]][["DaysAbove"]][[i]] <- sum(stats[["Thresholds"]][["coded"]][[i]], na.rm=TRUE)
		stats[["Thresholds"]][["Totals"]][["FracDaysAbove"]][[i]] <- sum(stats[["Thresholds"]][["coded"]][[i]], na.rm=TRUE)/(length(stats[["Thresholds"]][["coded"]][[i]])-sum(is.na(stats[["Thresholds"]][["coded"]][[i]])))
		stats[["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]] <- sum(discharge_acfte6[which(stats[["Thresholds"]][["coded"]][[i]]==1)], na.rm=TRUE)
		stats[["Thresholds"]][["Totals"]][["Total_Q_acfte6"]][[i]] <-stats[["Total_Q_acfte6"]]
		stats[["Thresholds"]][["Totals"]][["Frac_Abv"]][[i]] <- stats[["Thresholds"]][["Totals"]][["Volume_Abv_acfte6"]][[i]]/stats[["Total_Q_acfte6"]]
	}
	stats[["Thresholds"]][["Totals"]] <- as.data.frame(stats[["Thresholds"]][["Totals"]])
	
	return(record_stats=stats)
}