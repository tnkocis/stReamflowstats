# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


simplified_peakflowtrends <- function(pfstatsdf, gauge, year){
	library(zoo)
	library(Kendall)
	
	meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft,5,na.pad=TRUE)
	meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft,10,na.pad=TRUE)
	meantotvol20 <- rollmean(pfstatsdf$TotVolAbv_acft,20,na.pad=TRUE)
	meantotvol30 <- rollmean(pfstatsdf$TotVolAbv_acft,30,na.pad=TRUE)
	
	MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft)
	MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
	MKmeantotvol5 <- MannKendall(meantotvol5)
	MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
	MKmeantotvol10 <- MannKendall(meantotvol10)
	MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))
	MKmeantotvol20 <- MannKendall(meantotvol20)
	MKmeantotvol20 <- data.frame(tau=MKmeantotvol20$tau[[1]],p2=MKmeantotvol20$sl[[1]],window=c(20),measure=c("totvolabv"))
	MKmeantotvol30 <- MannKendall(meantotvol30)
	MKmeantotvol30 <- data.frame(tau=MKmeantotvol30$tau[[1]],p2=MKmeantotvol30$sl[[1]],window=c(30),measure=c("totvolabv"))
	MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10,MKmeantotvol20,MKmeantotvol30)
	
	
	############################
	meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv,5,na.pad=TRUE)
	meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv,10,na.pad=TRUE)
	meantotdays20 <- rollmean(pfstatsdf$TotDaysAbv,20,na.pad=TRUE)
	meantotdays30 <- rollmean(pfstatsdf$TotDaysAbv,30,na.pad=TRUE)
	
	MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv)
	MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
	MKmeantotdays5 <- MannKendall(meantotdays5)
	MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
	MKmeantotdays10 <- MannKendall(meantotdays10)
	MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
	MKmeantotdays20 <- MannKendall(meantotdays20)
	MKmeantotdays20 <- data.frame(tau=MKmeantotdays20$tau[[1]],p2=MKmeantotdays20$sl[[1]],window=c(20),measure=c("totdaysabv"))
	MKmeantotdays30 <- MannKendall(meantotdays30)
	MKmeantotdays30 <- data.frame(tau=MKmeantotdays30$tau[[1]],p2=MKmeantotdays30$sl[[1]],window=c(30),measure=c("totdaysabv"))
	MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10,MKmeantotdays20,MKmeantotdays30)
	
	####################
	
	meannumpks5 <- rollmean(pfstatsdf$numpeaks,5,na.pad=TRUE)
	meannumpks10 <- rollmean(pfstatsdf$numpeaks,10,na.pad=TRUE)
	meannumpks20 <- rollmean(pfstatsdf$numpeaks,20,na.pad=TRUE)
	meannumpks30 <- rollmean(pfstatsdf$numpeaks,30,na.pad=TRUE)
	
	MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks)
	MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
	MKmeannumpks5 <- MannKendall(meannumpks5)
	MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
	MKmeannumpks10 <- MannKendall(meannumpks10)
	MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
	MKmeannumpks20 <- MannKendall(meannumpks20)
	MKmeannumpks20 <- data.frame(tau=MKmeannumpks20$tau[[1]],p2=MKmeannumpks20$sl[[1]],window=c(20),measure=c("numpeaksabv"))
	MKmeannumpks30 <- MannKendall(meannumpks30)
	MKmeannumpks30 <- data.frame(tau=MKmeannumpks30$tau[[1]],p2=MKmeannumpks30$sl[[1]],window=c(30),measure=c("numpeaksabv"))
	MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10,MKmeannumpks20,MKmeannumpks30)
	
	####################
	
	
	meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow,5,na.pad=TRUE)
	meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow,10,na.pad=TRUE)
	meanmeanpks20 <- rollmean(pfstatsdf$mean_peakflow,20,na.pad=TRUE)
	meanmeanpks30 <- rollmean(pfstatsdf$mean_peakflow,30,na.pad=TRUE)
	
	MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow)
	MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
	MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
	MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
	MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
	MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
	MKmeanmeanpks20 <- MannKendall(meanmeanpks20)
	MKmeanmeanpks20 <- data.frame(tau=MKmeanmeanpks20$tau[[1]],p2=MKmeanmeanpks20$sl[[1]],window=c(20),measure=c("meanpeaksabv"))
	MKmeanmeanpks30 <- MannKendall(meanmeanpks30)
	MKmeanmeanpks30 <- data.frame(tau=MKmeanmeanpks30$tau[[1]],p2=MKmeanmeanpks30$sl[[1]],window=c(30),measure=c("meanpeaksabv"))
	MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10,MKmeanmeanpks20,MKmeanmeanpks30)
	
	
	####################
	
	meantotpks5 <- rollmean(pfstatsdf$total_peakflow,5,na.pad=TRUE)
	meantotpks10 <- rollmean(pfstatsdf$total_peakflow,10,na.pad=TRUE)
	meantotpks20 <- rollmean(pfstatsdf$total_peakflow,20,na.pad=TRUE)
	meantotpks30 <- rollmean(pfstatsdf$total_peakflow,30,na.pad=TRUE)
	
	MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow)
	MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
	MKmeantotpks5 <- MannKendall(meantotpks5)
	MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
	MKmeantotpks10 <- MannKendall(meantotpks10)
	MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
	MKmeantotpks20 <- MannKendall(meantotpks20)
	MKmeantotpks20 <- data.frame(tau=MKmeantotpks20$tau[[1]],p2=MKmeantotpks20$sl[[1]],window=c(20),measure=c("totpeakflwabv"))
	MKmeantotpks30 <- MannKendall(meantotpks30)
	MKmeantotpks30 <- data.frame(tau=MKmeantotpks30$tau[[1]],p2=MKmeantotpks30$sl[[1]],window=c(30),measure=c("totpeakflwabv"))
	MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10,MKmeantotpks20,MKmeantotpks30)
		
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

