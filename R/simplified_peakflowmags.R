# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


simplified_peakflowmags <- function(pfstatsdf, gauge, year){
	top <- vector("list", 6)
	for(i in 1:6){
		for(k in 1:15){
			mean_totvol_TAF <- mean(pfstatsdf[[i]][[k]]$TotVolAbv_acft[which(pfstatsdf[[i]][[k]]$TotVolAbv_acft!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])/1000
			mean_totdays <- mean(pfstatsdf[[i]][[k]]$TotDaysAbv[which(pfstatsdf[[i]][[k]]$TotDaysAbv!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])
			mean_numpeaks <- mean(pfstatsdf[[i]][[k]]$numpeaks[which(pfstatsdf[[i]][[k]]$numpeaks!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])
			frac_zero <- length(which(pfstatsdf[[i]][[k]]$numpeaks==0&pfstatsdf[[i]][[k]]$sthyyear>year))/length(pfstatsdf[[i]][[k]]$numpeaks[which(pfstatsdf[[i]][[k]]$sthyyear>year)])
			frac_nonzero <- 1-frac_zero
			num_zero <-length(which(pfstatsdf[[i]][[k]]$numpeaks==0&pfstatsdf[[i]][[k]]$sthyyear>year))
			num_nonzero <-length(which(pfstatsdf[[i]][[k]]$numpeaks!=0&pfstatsdf[[i]][[k]]$sthyyear>year))
			
			meandf <- data.frame(mean_totvol_TAF =mean_totvol_TAF ,mean_totdays=mean_totdays,mean_numpeaks=mean_numpeaks,
					frac_zero=frac_zero,
					frac_nonzero=frac_nonzero,
					num_zero=num_zero,
					num_nonzero=num_nonzero,
					gauge=gauge,
					styear=year)
			
			top[[i]][[k]] <- meandf	
		}
		names(top[[i]]) <- names(pfstatsdf[[i]])
	}
	names(top)<- names(pfstatsdf)
	return(top)
}

