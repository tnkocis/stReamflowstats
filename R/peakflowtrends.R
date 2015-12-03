# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


peakflowtrends <- function(pfstatsdf, gauge, year){
	library(zoo)
	
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
	
	SNmeantotvol1 <- lm(pfstatsdf$TotVolAbv_acft ~ pfstatsdf$year)
	SNmeantotvol1 <- data.frame(slope=SNmeantotvol1$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol1))/var(residuals(SNmeantotvol1))),window=c(1),measure=c("totvolabv"))
	SNmeantotvol5 <- lm(meantotvol5 ~ pfstatsdf$year)
	SNmeantotvol5 <- data.frame(slope=SNmeantotvol5$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol5))/var(residuals(SNmeantotvol5))),window=c(5),measure=c("totvolabv"))
	SNmeantotvol10 <- lm(meantotvol10 ~ pfstatsdf$year)
	SNmeantotvol10 <- data.frame(slope=SNmeantotvol10$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol10))/var(residuals(SNmeantotvol10))),window=c(10),measure=c("totvolabv"))
	SNmeantotvol20 <- lm(meantotvol20 ~ pfstatsdf$year)
	SNmeantotvol20 <- data.frame(slope=SNmeantotvol20$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol20))/var(residuals(SNmeantotvol20))),window=c(20),measure=c("totvolabv"))
	SNmeantotvol30 <- lm(meantotvol30 ~ pfstatsdf$year)
	SNmeantotvol30 <- data.frame(slope=SNmeantotvol30$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol30))/var(residuals(SNmeantotvol30))),window=c(30),measure=c("totvolabv"))
	SNmeantotvol <- rbind.data.frame(SNmeantotvol1,SNmeantotvol5,SNmeantotvol10,SNmeantotvol20,SNmeantotvol30)
	
	
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
	
	SNmeantotdays1 <- lm(pfstatsdf$TotDaysAbv ~ pfstatsdf$year)
	SNmeantotdays1 <- data.frame(slope=SNmeantotdays1$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays1))/var(residuals(SNmeantotdays1))),window=c(1),measure=c("totdaysabv"))
	SNmeantotdays5 <- lm(meantotdays5 ~ pfstatsdf$year)
	SNmeantotdays5 <- data.frame(slope=SNmeantotdays5$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays5))/var(residuals(SNmeantotdays5))),window=c(5),measure=c("totdaysabv"))
	SNmeantotdays10 <- lm(meantotdays10 ~ pfstatsdf$year)
	SNmeantotdays10 <- data.frame(slope=SNmeantotdays10$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays10))/var(residuals(SNmeantotdays10))),window=c(10),measure=c("totdaysabv"))
	SNmeantotdays20 <- lm(meantotdays20 ~ pfstatsdf$year)
	SNmeantotdays20 <- data.frame(slope=SNmeantotdays20$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays20))/var(residuals(SNmeantotdays20))),window=c(20),measure=c("totdaysabv"))
	SNmeantotdays30 <- lm(meantotdays30 ~ pfstatsdf$year)
	SNmeantotdays30 <- data.frame(slope=SNmeantotdays30$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays30))/var(residuals(SNmeantotdays30))),window=c(30),measure=c("totdaysabv"))
	SNmeantotdays <- rbind.data.frame(SNmeantotdays1,SNmeantotdays5,SNmeantotdays10,SNmeantotdays20,SNmeantotdays30)
	
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
	
	SNmeannumpks1 <- lm(pfstatsdf$TotDaysAbv ~ pfstatsdf$year)
	SNmeannumpks1 <- data.frame(slope=SNmeannumpks1$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks1))/var(residuals(SNmeannumpks1))),window=c(1),measure=c("numpeaksabv"))
	SNmeannumpks5 <- lm(meannumpks5 ~ pfstatsdf$year)
	SNmeannumpks5 <- data.frame(slope=SNmeannumpks5$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks5))/var(residuals(SNmeannumpks5))),window=c(5),measure=c("numpeaksabv"))
	SNmeannumpks10 <- lm(meannumpks10 ~ pfstatsdf$year)
	SNmeannumpks10 <- data.frame(slope=SNmeannumpks10$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks10))/var(residuals(SNmeannumpks10))),window=c(10),measure=c("numpeaksabv"))
	SNmeannumpks20 <- lm(meannumpks20 ~ pfstatsdf$year)
	SNmeannumpks20 <- data.frame(slope=SNmeannumpks20$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks20))/var(residuals(SNmeannumpks20))),window=c(20),measure=c("numpeaksabv"))
	SNmeannumpks30 <- lm(meannumpks30 ~ pfstatsdf$year)
	SNmeannumpks30 <- data.frame(slope=SNmeannumpks30$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks30))/var(residuals(SNmeannumpks30))),window=c(30),measure=c("numpeaksabv"))
	SNmeannumpks <- rbind.data.frame(SNmeannumpks1,SNmeannumpks5,SNmeannumpks10,SNmeannumpks20,SNmeannumpks30)
	
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
	
	SNmeanmeanpks1 <- lm(pfstatsdf$TotDaysAbv ~ pfstatsdf$year)
	SNmeanmeanpks1 <- data.frame(slope=SNmeanmeanpks1$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks1))/var(residuals(SNmeanmeanpks1))),window=c(1),measure=c("meanpeaksabv"))
	SNmeanmeanpks5 <- lm(meanmeanpks5 ~ pfstatsdf$year)
	SNmeanmeanpks5 <- data.frame(slope=SNmeanmeanpks5$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks5))/var(residuals(SNmeanmeanpks5))),window=c(5),measure=c("meanpeaksabv"))
	SNmeanmeanpks10 <- lm(meanmeanpks10 ~ pfstatsdf$year)
	SNmeanmeanpks10 <- data.frame(slope=SNmeanmeanpks10$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks10))/var(residuals(SNmeanmeanpks10))),window=c(10),measure=c("meanpeaksabv"))
	SNmeanmeanpks20 <- lm(meanmeanpks20 ~ pfstatsdf$year)
	SNmeanmeanpks20 <- data.frame(slope=SNmeanmeanpks20$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks20))/var(residuals(SNmeanmeanpks20))),window=c(20),measure=c("meanpeaksabv"))
	SNmeanmeanpks30 <- lm(meanmeanpks30 ~ pfstatsdf$year)
	SNmeanmeanpks30 <- data.frame(slope=SNmeanmeanpks30$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks30))/var(residuals(SNmeanmeanpks30))),window=c(30),measure=c("meanpeaksabv"))
	SNmeanmeanpks <- rbind.data.frame(SNmeanmeanpks1,SNmeanmeanpks5,SNmeanmeanpks10,SNmeanmeanpks20,SNmeanmeanpks30)
	
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
	
	SNmeantotpks1 <- lm(pfstatsdf$TotDaysAbv ~ pfstatsdf$year)
	SNmeantotpks1 <- data.frame(slope=SNmeantotpks1$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks1))/var(residuals(SNmeantotpks1))),window=c(1),measure=c("totpeakflwabv"))
	SNmeantotpks5 <- lm(meantotpks5 ~ pfstatsdf$year)
	SNmeantotpks5 <- data.frame(slope=SNmeantotpks5$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks5))/var(residuals(SNmeantotpks5))),window=c(5),measure=c("totpeakflwabv"))
	SNmeantotpks10 <- lm(meantotpks10 ~ pfstatsdf$year)
	SNmeantotpks10 <- data.frame(slope=SNmeantotpks10$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks10))/var(residuals(SNmeantotpks10))),window=c(10),measure=c("totpeakflwabv"))
	SNmeantotpks20 <- lm(meantotpks20 ~ pfstatsdf$year)
	SNmeantotpks20 <- data.frame(slope=SNmeantotpks20$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks20))/var(residuals(SNmeantotpks20))),window=c(20),measure=c("totpeakflwabv"))
	SNmeantotpks30 <- lm(meantotpks30 ~ pfstatsdf$year)
	SNmeantotpks30 <- data.frame(slope=SNmeantotpks30$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks30))/var(residuals(SNmeantotpks30))),window=c(30),measure=c("totpeakflwabv"))
	SNmeantotpks <- rbind.data.frame(SNmeantotpks1,SNmeantotpks5,SNmeantotpks10,SNmeantotpks20,SNmeantotpks30)
	
	####################
	
	MKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
	SNdfs <- rbind.data.frame(SNmeantotvol,SNmeantotdays,SNmeannumpks,SNmeanmeanpks,SNmeantotpks)
	
	finaldf <- merge(MKdfs,SNdfs,by=c("window","measure"))
	finaldf$gauge <- gauge

	
	meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$year>year)],5,na.pad=TRUE)
	meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$year>year)],10,na.pad=TRUE)

	
	MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$year>year)])
	MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
	MKmeantotvol5 <- MannKendall(meantotvol5)
	MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
	MKmeantotvol10 <- MannKendall(meantotvol10)
	MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))
	MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10)
	
	SNmeantotvol1 <- lm(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotvol1 <- data.frame(slope=SNmeantotvol1$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol1))/var(residuals(SNmeantotvol1))),window=c(1),measure=c("totvolabv"))
	SNmeantotvol5 <- lm(meantotvol5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotvol5 <- data.frame(slope=SNmeantotvol5$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol5))/var(residuals(SNmeantotvol5))),window=c(5),measure=c("totvolabv"))
	SNmeantotvol10 <- lm(meantotvol10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotvol10 <- data.frame(slope=SNmeantotvol10$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol10))/var(residuals(SNmeantotvol10))),window=c(10),measure=c("totvolabv"))
	SNmeantotvol <- rbind.data.frame(SNmeantotvol1,SNmeantotvol5,SNmeantotvol10)
	
	
	############################
	meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)],5,na.pad=TRUE)
	meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)],10,na.pad=TRUE)

	
	MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)])
	MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
	MKmeantotdays5 <- MannKendall(meantotdays5)
	MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
	MKmeantotdays10 <- MannKendall(meantotdays10)
	MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
	MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10)
	
	SNmeantotdays1 <- lm(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotdays1 <- data.frame(slope=SNmeantotdays1$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays1))/var(residuals(SNmeantotdays1))),window=c(1),measure=c("totdaysabv"))
	SNmeantotdays5 <- lm(meantotdays5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotdays5 <- data.frame(slope=SNmeantotdays5$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays5))/var(residuals(SNmeantotdays5))),window=c(5),measure=c("totdaysabv"))
	SNmeantotdays10 <- lm(meantotdays10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotdays10 <- data.frame(slope=SNmeantotdays10$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays10))/var(residuals(SNmeantotdays10))),window=c(10),measure=c("totdaysabv"))
	SNmeantotdays <- rbind.data.frame(SNmeantotdays1,SNmeantotdays5,SNmeantotdays10)
	
	####################
	
	meannumpks5 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$year>year)],5,na.pad=TRUE)
	meannumpks10 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$year>year)],10,na.pad=TRUE)

	MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks[which(pfstatsdf$year>year)])
	MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
	MKmeannumpks5 <- MannKendall(meannumpks5)
	MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
	MKmeannumpks10 <- MannKendall(meannumpks10)
	MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
	MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10)
	
	SNmeannumpks1 <- lm(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeannumpks1 <- data.frame(slope=SNmeannumpks1$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks1))/var(residuals(SNmeannumpks1))),window=c(1),measure=c("numpeaksabv"))
	SNmeannumpks5 <- lm(meannumpks5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeannumpks5 <- data.frame(slope=SNmeannumpks5$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks5))/var(residuals(SNmeannumpks5))),window=c(5),measure=c("numpeaksabv"))
	SNmeannumpks10 <- lm(meannumpks10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeannumpks10 <- data.frame(slope=SNmeannumpks10$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks10))/var(residuals(SNmeannumpks10))),window=c(10),measure=c("numpeaksabv"))
	SNmeannumpks <- rbind.data.frame(SNmeannumpks1,SNmeannumpks5,SNmeannumpks10)
	
	####################
	
	
	meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$year>year)],5,na.pad=TRUE)
	meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$year>year)],10,na.pad=TRUE)

	
	MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow[which(pfstatsdf$year>year)])
	MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
	MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
	MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
	MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
	MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
	MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10)
	
	SNmeanmeanpks1 <- lm(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeanmeanpks1 <- data.frame(slope=SNmeanmeanpks1$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks1))/var(residuals(SNmeanmeanpks1))),window=c(1),measure=c("meanpeaksabv"))
	SNmeanmeanpks5 <- lm(meanmeanpks5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeanmeanpks5 <- data.frame(slope=SNmeanmeanpks5$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks5))/var(residuals(SNmeanmeanpks5))),window=c(5),measure=c("meanpeaksabv"))
	SNmeanmeanpks10 <- lm(meanmeanpks10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeanmeanpks10 <- data.frame(slope=SNmeanmeanpks10$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks10))/var(residuals(SNmeanmeanpks10))),window=c(10),measure=c("meanpeaksabv"))
	SNmeanmeanpks <- rbind.data.frame(SNmeanmeanpks1,SNmeanmeanpks5,SNmeanmeanpks10)
	
	####################
	
	meantotpks5 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$year>year)],5,na.pad=TRUE)
	meantotpks10 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$year>year)],10,na.pad=TRUE)

	MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow[which(pfstatsdf$year>year)])
	MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
	MKmeantotpks5 <- MannKendall(meantotpks5)
	MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
	MKmeantotpks10 <- MannKendall(meantotpks10)
	MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
	MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10)
	
	SNmeantotpks1 <- lm(pfstatsdf$TotDaysAbv[which(pfstatsdf$year>year)] ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotpks1 <- data.frame(slope=SNmeantotpks1$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks1))/var(residuals(SNmeantotpks1))),window=c(1),measure=c("totpeakflwabv"))
	SNmeantotpks5 <- lm(meantotpks5 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotpks5 <- data.frame(slope=SNmeantotpks5$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks5))/var(residuals(SNmeantotpks5))),window=c(5),measure=c("totpeakflwabv"))
	SNmeantotpks10 <- lm(meantotpks10 ~ pfstatsdf$year[which(pfstatsdf$year>year)])
	SNmeantotpks10 <- data.frame(slope=SNmeantotpks10$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks10))/var(residuals(SNmeantotpks10))),window=c(10),measure=c("totpeakflwabv"))
	SNmeantotpks <- rbind.data.frame(SNmeantotpks1,SNmeantotpks5,SNmeantotpks10)
	
	####################
	
	impMKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
	impSNdfs <- rbind.data.frame(SNmeantotvol,SNmeantotdays,SNmeannumpks,SNmeanmeanpks,SNmeantotpks)
	
	impfinaldf <- merge(impMKdfs,impSNdfs,by=c("window","measure"))
	impfinaldf$gauge <- gauge
	finallist <- list(trend_dams=impfinaldf,trend_full=finaldf)
	
	return(finallist)
}
