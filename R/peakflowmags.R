# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


peakflowmags <- function(pfstatsdf, gauge, year){
	mean_totvol_TAF <- mean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$TotVolAbv_acft!=0&pfstatsdf$year>year)])/1000
	mean_totdays <- mean(pfstatsdf$TotDaysAbv[which(pfstatsdf$TotDaysAbv!=0&pfstatsdf$year>year)])
	mean_numpeaks <- mean(pfstatsdf$numpeaks[which(pfstatsdf$numpeaks!=0&pfstatsdf$year>year)])
	frac_zero <- length(which(pfstatsdf$numpeaks==0&pfstatsdf$year>year))/length(pfstatsdf$numpeaks[which(pfstatsdf$year>year)])
	frac_nonzero <- 1-frac_zero
	num_zero <-length(which(pfstatsdf$numpeaks==0&pfstatsdf$year>year))
	num_nonzero <-length(which(pfstatsdf$numpeaks!=0&pfstatsdf$year>year))
	
	meandf <- data.frame(mean_totvol_TAF =mean_totvol_TAF ,mean_totdays=mean_totdays,mean_numpeaks=mean_numpeaks,
			frac_zero=frac_zero,
			frac_nonzero=frac_nonzero,
			num_zero=num_zero,
			num_nonzero=num_nonzero,
			gauge=gauge,
			styear=year)
	
	return(meandf)
}