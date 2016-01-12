# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

fdcts <- spbatch$`11452500`$Winter_6mon$Data$`1903 - 1904`
fdcts$thres90af <- spbatch$`11452500`$thresholdsdams_maf$P90maf*1e6
fdcts$thres95af <- spbatch$`11452500`$thresholdsdams_maf$P95maf*1e6
fdcts$dischaf_90 <- rep(NA, length(fdcts$Discharge_acft_day))
fdcts$dischaf_95 <- rep(NA, length(fdcts$Discharge_acft_day))
for(i in 1:length(fdcts$Discharge_acft_day)){
	if(fdcts$Discharge_acft_day[[i]] <fdcts$thres90af[[i]]){
		fdcts$dischaf_90[[i]] <- 0
	} else {
		fdcts$dischaf_90[[i]] <- fdcts$Discharge_acft_day[[i]]
	}
	if(fdcts$Discharge_acft_day[[i]] <fdcts$thres95af[[i]]){
		fdcts$dischaf_95[[i]] <- 0	
	} else {
		fdcts$dischaf_95[[i]] <- fdcts$Discharge_acft_day[[i]]
	}
}

fdcts_record <- spbatch$`11452500`$Winter_6mon$All$Data[which(format(spbatch$`11452500`$Winter_6mon$All$Data$Date,"%Y")>=1900),]
fdcts_record$thres90af <- spbatch$`11452500`$thresholdsdams_maf$P90maf*1e6
fdcts_record$thres95af <- spbatch$`11452500`$thresholdsdams_maf$P95maf*1e6
fdcts_record$Discharge_acft_day <- fdcts_record$Discharge_acfte6_day*1e6
fdcts_record$dischaf_90 <- rep(NA, length(fdcts_record$Discharge_acft_day))
fdcts_record$dischaf_95 <- rep(NA, length(fdcts_record$Discharge_acft_day))
fdcts_record$Discharge_acft_day[which(is.na(fdcts_record$Discharge_acft_day))] <-0
for(i in 1:length(fdcts_record$Discharge_acft_day)){
	if(fdcts_record$Discharge_acft_day[[i]] <fdcts_record$thres90af[[i]]){
		fdcts_record$dischaf_90[[i]] <- 0
	} else {
		fdcts_record$dischaf_90[[i]] <- fdcts_record$Discharge_acft_day[[i]]
	}
	if(fdcts_record$Discharge_acft_day[[i]] <fdcts_record$thres95af[[i]]){
		fdcts_record$dischaf_95[[i]] <- 0	
	} else {
		fdcts_record$dischaf_95[[i]] <- fdcts_record$Discharge_acft_day[[i]]
	}
}






meants <- mean(spbatch$`11452500`$Winter_6mon$All$Data$Discharge_acfte6_day[which(format(spbatch$`11452500`$Winter_6mon$All$Data$Date,"%Y")>=1980)], na.rm=TRUE)
