# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


simplified_peakflowtrends_510 <- function(pfstatsdf, gauge, year){
	library(zoo)
	library(Kendall)
	
	meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft,5,na.pad=TRUE)
	meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft,10,na.pad=TRUE)

	
	MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft)
	MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
	MKmeantotvol5 <- MannKendall(meantotvol5)
	MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
	MKmeantotvol10 <- MannKendall(meantotvol10)
	MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))

	MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10)
	
	
	############################
	meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv,5,na.pad=TRUE)
	meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv,10,na.pad=TRUE)
	
	MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv)
	MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
	MKmeantotdays5 <- MannKendall(meantotdays5)
	MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
	MKmeantotdays10 <- MannKendall(meantotdays10)
	MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
	MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10)
	
	####################
	
	meannumpks5 <- rollmean(pfstatsdf$numpeaks,5,na.pad=TRUE)
	meannumpks10 <- rollmean(pfstatsdf$numpeaks,10,na.pad=TRUE)

	
	MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks)
	MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
	MKmeannumpks5 <- MannKendall(meannumpks5)
	MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
	MKmeannumpks10 <- MannKendall(meannumpks10)
	MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
	MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10)
	
	####################
	
	
	meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow,5,na.pad=TRUE)
	meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow,10,na.pad=TRUE)

	
	MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow)
	MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
	MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
	MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
	MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
	MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
	MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10)
	
	
	####################
	
	meantotpks5 <- rollmean(pfstatsdf$total_peakflow,5,na.pad=TRUE)
	meantotpks10 <- rollmean(pfstatsdf$total_peakflow,10,na.pad=TRUE)

	
	MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow)
	MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
	MKmeantotpks5 <- MannKendall(meantotpks5)
	MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
	MKmeantotpks10 <- MannKendall(meantotpks10)
	MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
	MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10)
	
	####################
	
	MKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
	
	finaldf <- MKdfs
	finaldf$gauge <- gauge
	
	
	meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
	meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
	
	
	MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>year)])
	MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
	MKmeantotvol5 <- MannKendall(meantotvol5)
	MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
	MKmeantotvol10 <- MannKendall(meantotvol10)
	MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))
	MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10)
	
	
	############################
	meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
	meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
	
	
	MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>year)])
	MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
	MKmeantotdays5 <- MannKendall(meantotdays5)
	MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
	MKmeantotdays10 <- MannKendall(meantotdays10)
	MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
	MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10)
	
	####################
	
	meannumpks5 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
	meannumpks10 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
	
	MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>year)])
	MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
	MKmeannumpks5 <- MannKendall(meannumpks5)
	MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
	MKmeannumpks10 <- MannKendall(meannumpks10)
	MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
	MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10)
	
	
	####################
	
	
	meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
	meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
	
	
	MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>year)])
	MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
	MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
	MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
	MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
	MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
	MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10)
	
	
	####################
	
	meantotpks5 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
	meantotpks10 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
	
	MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>year)])
	MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
	MKmeantotpks5 <- MannKendall(meantotpks5)
	MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
	MKmeantotpks10 <- MannKendall(meantotpks10)
	MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
	MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10)
	
	
	####################
	
	impMKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
	
	impfinaldf <- impMKdfs
	impfinaldf$gauge <- gauge
	
	
	
	#####################
	meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>1979)],5,na.pad=TRUE)
	meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>1979)],10,na.pad=TRUE)
	
	
	MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>1979)])
	MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
	MKmeantotvol5 <- MannKendall(meantotvol5)
	MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
	MKmeantotvol10 <- MannKendall(meantotvol10)
	MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))
	MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10)
	
	
	############################
	meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>1979)],5,na.pad=TRUE)
	meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>1979)],10,na.pad=TRUE)
	
	
	MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>1979)])
	MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
	MKmeantotdays5 <- MannKendall(meantotdays5)
	MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
	MKmeantotdays10 <- MannKendall(meantotdays10)
	MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
	MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10)
	
	####################
	
	meannumpks5 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>1979)],5,na.pad=TRUE)
	meannumpks10 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>1979)],10,na.pad=TRUE)
	
	MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>1979)])
	MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
	MKmeannumpks5 <- MannKendall(meannumpks5)
	MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
	MKmeannumpks10 <- MannKendall(meannumpks10)
	MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
	MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10)
	
	
	####################
	
	
	meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>1979)],5,na.pad=TRUE)
	meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>1979)],10,na.pad=TRUE)
	
	
	MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>1979)])
	MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
	MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
	MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
	MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
	MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
	MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10)
	
	
	####################
	
	meantotpks5 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>1979)],5,na.pad=TRUE)
	meantotpks10 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>1979)],10,na.pad=TRUE)
	
	MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>1979)])
	MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
	MKmeantotpks5 <- MannKendall(meantotpks5)
	MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
	MKmeantotpks10 <- MannKendall(meantotpks10)
	MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
	MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10)
	
	
	####################
	
	MKdfs1980 <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
	
	finaldf1980 <- MKdfs1980
	finaldf1980$gauge <- gauge
	
	
	
	
	
	################
	finallist <- list(trend_dams=impfinaldf,trend_full=finaldf, trend_1980=finaldf1980)
	
	return(finallist)
}


