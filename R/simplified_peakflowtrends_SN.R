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


simplified_peakflowtrends_SN <- function(pfstatsdf, gauge, year, commonyear){
	library(zoo)
	library(Kendall)
	library(car)
	
	commonyear <- commonyear-1
	
	if(length(pfstatsdf$TotVolAbv_acft)>=5){
		meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft,5,na.pad=TRUE)	
	} else {
		meantotvol5 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$TotVolAbv_acft)>=10){
		meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft,10,na.pad=TRUE)
	} else {
		meantotvol10 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$TotVolAbv_acft)>=20){
		meantotvol20 <- rollmean(pfstatsdf$TotVolAbv_acft,20,na.pad=TRUE)
	} else {
		meantotvol20 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$TotVolAbv_acft)>=30){
		meantotvol30 <- rollmean(pfstatsdf$TotVolAbv_acft,30,na.pad=TRUE)
	} else {
		meantotvol30 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$TotVolAbv_acft)>=3){
		meantotvol1 <- pfstatsdf$TotVolAbv_acft
	} else {
		meantotvol1 <- rep(NA,length(pfstatsdf$sthyyear))
	}

	MKmeantotvol1 <- MannKendall(meantotvol1)
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
	
	
	if(all(is.na(meantotvol1))){
		SNmeantotvol1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("totvolabv"))
	}else{
		SNmeantotvol1 <- lm(meantotvol1 ~ pfstatsdf$sthyyear)
		SNmeantotvol1 <- data.frame(slope=SNmeantotvol1$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol1))/var(residuals(SNmeantotvol1))),data_variance=var(meantotvol1, na.rm=TRUE),window=c(1),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol1)))
		
	}
	
	if(all(is.na(meantotvol5))){
		SNmeantotvol5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("totvolabv"))
	}else{
		SNmeantotvol5 <- lm(meantotvol5 ~ pfstatsdf$sthyyear)
		SNmeantotvol5 <- data.frame(slope=SNmeantotvol5$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol5))/var(residuals(SNmeantotvol5))),data_variance=var(meantotvol5, na.rm=TRUE),window=c(5),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol5)))
		
	}
	if(all(is.na(meantotvol10))){
		SNmeantotvol10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("totvolabv"))
	}else{
		SNmeantotvol10 <- lm(meantotvol10 ~ pfstatsdf$sthyyear)
		SNmeantotvol10 <- data.frame(slope=SNmeantotvol10$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol10))/var(residuals(SNmeantotvol10))),data_variance=var(meantotvol10, na.rm=TRUE),window=c(10),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol10)))
		
	}
	if(all(is.na(meantotvol20))){
		SNmeantotvol20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("totvolabv"))
	}else{
		SNmeantotvol20 <- lm(meantotvol20 ~ pfstatsdf$sthyyear)
		SNmeantotvol20 <- data.frame(slope=SNmeantotvol20$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol20))/var(residuals(SNmeantotvol20))),data_variance=var(meantotvol20, na.rm=TRUE),window=c(20),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol20)))
		
	}
	if(all(is.na(meantotvol30))){
		SNmeantotvol30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("totvolabv"))
	}else{
		SNmeantotvol30 <- lm(meantotvol30 ~ pfstatsdf$sthyyear)
		SNmeantotvol30 <- data.frame(slope=SNmeantotvol30$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol30))/var(residuals(SNmeantotvol30))),data_variance=var(meantotvol30, na.rm=TRUE),window=c(30),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol30)))
	
	}
	SNmeantotvol <- rbind.data.frame(SNmeantotvol1,SNmeantotvol5,SNmeantotvol10,SNmeantotvol20,SNmeantotvol30)
	
	
	
	############################
	if(length(pfstatsdf$TotDaysAbv)>=5){
		meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv,5,na.pad=TRUE)	
	} else {
		meantotdays5 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$TotDaysAbv)>=10){
		meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv,10,na.pad=TRUE)
	} else {
		meantotdays10 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$TotDaysAbv)>=20){
		meantotdays20 <- rollmean(pfstatsdf$TotDaysAbv,20,na.pad=TRUE)
	} else {
		meantotdays20 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$TotDaysAbv)>=30){
		meantotdays30 <- rollmean(pfstatsdf$TotDaysAbv,30,na.pad=TRUE)
	} else {
		meantotdays30 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$TotDaysAbv)>=3){
		meantotdays1 <- pfstatsdf$TotDaysAbv
	} else {
		meantotdays1 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	
	MKmeantotdays1 <- MannKendall(meantotdays1)
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
	
	if(all(is.na(meantotdays1))){
		SNmeantotdays1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("totdaysabv"))
	}else{
		SNmeantotdays1 <- lm(meantotdays1 ~ pfstatsdf$sthyyear)
		SNmeantotdays1 <- data.frame(slope=SNmeantotdays1$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays1))/var(residuals(SNmeantotdays1))),data_variance=var(meantotdays1, na.rm=TRUE),window=c(1),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays1)))
		
	}
	
	if(all(is.na(meantotdays5))){
		SNmeantotdays5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("totdaysabv"))
	}else{
		SNmeantotdays5 <- lm(meantotdays5 ~ pfstatsdf$sthyyear)
		SNmeantotdays5 <- data.frame(slope=SNmeantotdays5$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays5))/var(residuals(SNmeantotdays5))),data_variance=var(meantotdays5, na.rm=TRUE),window=c(5),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays5)))
		
	}
	if(all(is.na(meantotdays10))){
		SNmeantotdays10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("totdaysabv"))
	}else{
		SNmeantotdays10 <- lm(meantotdays10 ~ pfstatsdf$sthyyear)
		SNmeantotdays10 <- data.frame(slope=SNmeantotdays10$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays10))/var(residuals(SNmeantotdays10))),data_variance=var(meantotdays10, na.rm=TRUE),window=c(10),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays10)))
		
	}
	if(all(is.na(meantotdays20))){
		SNmeantotdays20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("totdaysabv"))
	}else{
		SNmeantotdays20 <- lm(meantotdays20 ~ pfstatsdf$sthyyear)
		SNmeantotdays20 <- data.frame(slope=SNmeantotdays20$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays20))/var(residuals(SNmeantotdays20))),data_variance=var(meantotdays20, na.rm=TRUE),window=c(20),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays20)))
		
	}
	if(all(is.na(meantotdays30))){
		SNmeantotdays30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("totdaysabv"))
	}else{
		SNmeantotdays30 <- lm(meantotdays30 ~ pfstatsdf$sthyyear)
		SNmeantotdays30 <- data.frame(slope=SNmeantotdays30$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays30))/var(residuals(SNmeantotdays30))),data_variance=var(meantotdays30, na.rm=TRUE),window=c(30),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays30)))
		
	}
	SNmeantotdays <- rbind.data.frame(SNmeantotdays1,SNmeantotdays5,SNmeantotdays10,SNmeantotdays20,SNmeantotdays30)
	
	####################
	
	if(length(pfstatsdf$numpeaks)>=5){
		meannumpks5 <- rollmean(pfstatsdf$numpeaks,5,na.pad=TRUE)	
	} else {
		meannumpks5 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$numpeaks)>=10){
		meannumpks10 <- rollmean(pfstatsdf$numpeaks,10,na.pad=TRUE)
	} else {
		meannumpks10 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$numpeaks)>=20){
		meannumpks20 <- rollmean(pfstatsdf$numpeaks,20,na.pad=TRUE)
	} else {
		meannumpks20 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$numpeaks)>=30){
		meannumpks30 <- rollmean(pfstatsdf$numpeaks,30,na.pad=TRUE)
	} else {
		meannumpks30 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$numpeaks)>=3){
		meannumpks1 <- pfstatsdf$numpeaks
	} else {
		meannumpks1 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	
	MKmeannumpks1 <- MannKendall(meannumpks1)
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
	
	if(all(is.na(meannumpks1))){
		SNmeannumpks1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks1 <- lm(meannumpks1 ~ pfstatsdf$sthyyear)
		SNmeannumpks1 <- data.frame(slope=SNmeannumpks1$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks1))/var(residuals(SNmeannumpks1))),data_variance=var(meannumpks1, na.rm=TRUE),window=c(1),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks1)))
		
	}
	
	if(all(is.na(meannumpks5))){
		SNmeannumpks5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks5 <- lm(meannumpks5 ~ pfstatsdf$sthyyear)
		SNmeannumpks5 <- data.frame(slope=SNmeannumpks5$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks5))/var(residuals(SNmeannumpks5))),data_variance=var(meannumpks5, na.rm=TRUE),window=c(5),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks5)))
		
	}
	if(all(is.na(meannumpks10))){
		SNmeannumpks10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks10 <- lm(meannumpks10 ~ pfstatsdf$sthyyear)
		SNmeannumpks10 <- data.frame(slope=SNmeannumpks10$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks10))/var(residuals(SNmeannumpks10))),data_variance=var(meannumpks10, na.rm=TRUE),window=c(10),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks10)))
		
	}
	if(all(is.na(meannumpks20))){
		SNmeannumpks20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks20 <- lm(meannumpks20 ~ pfstatsdf$sthyyear)
		SNmeannumpks20 <- data.frame(slope=SNmeannumpks20$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks20))/var(residuals(SNmeannumpks20))),data_variance=var(meannumpks20, na.rm=TRUE),window=c(20),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks20)))
		
	}
	if(all(is.na(meannumpks30))){
		SNmeannumpks30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks30 <- lm(meannumpks30 ~ pfstatsdf$sthyyear)
		SNmeannumpks30 <- data.frame(slope=SNmeannumpks30$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks30))/var(residuals(SNmeannumpks30))),data_variance=var(meannumpks30, na.rm=TRUE),window=c(30),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks30)))
		
	}
	SNmeannumpks <- rbind.data.frame(SNmeannumpks1,SNmeannumpks5,SNmeannumpks10,SNmeannumpks20,SNmeannumpks30)
	
	
	####################
	
	if(length(pfstatsdf$mean_peakflow)>=5){
		meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow,5,na.pad=TRUE)	
	} else {
		meanmeanpks5 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$mean_peakflow)>=10){
		meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow,10,na.pad=TRUE)
	} else {
		meanmeanpks10 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$mean_peakflow)>=20){
		meanmeanpks20 <- rollmean(pfstatsdf$mean_peakflow,20,na.pad=TRUE)
	} else {
		meanmeanpks20 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$mean_peakflow)>=30){
		meanmeanpks30 <- rollmean(pfstatsdf$mean_peakflow,30,na.pad=TRUE)
	} else {
		meanmeanpks30 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$mean_peakflow)>=3){
		meanmeanpks1 <- pfstatsdf$mean_peakflow
	} else {
		meanmeanpks1 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	
	MKmeanmeanpks1 <- MannKendall(meanmeanpks1)
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
	
	if(all(is.na(meanmeanpks1))){
		SNmeanmeanpks1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks1 <- lm(meanmeanpks1 ~ pfstatsdf$sthyyear)
		SNmeanmeanpks1 <- data.frame(slope=SNmeanmeanpks1$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks1))/var(residuals(SNmeanmeanpks1))),data_variance=var(meanmeanpks1, na.rm=TRUE),window=c(1),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks1)))
		
	}
	
	if(all(is.na(meanmeanpks5))){
		SNmeanmeanpks5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks5 <- lm(meanmeanpks5 ~ pfstatsdf$sthyyear)
		SNmeanmeanpks5 <- data.frame(slope=SNmeanmeanpks5$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks5))/var(residuals(SNmeanmeanpks5))),data_variance=var(meanmeanpks5, na.rm=TRUE),window=c(5),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks5)))
		
	}
	if(all(is.na(meanmeanpks10))){
		SNmeanmeanpks10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks10 <- lm(meanmeanpks10 ~ pfstatsdf$sthyyear)
		SNmeanmeanpks10 <- data.frame(slope=SNmeanmeanpks10$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks10))/var(residuals(SNmeanmeanpks10))),data_variance=var(meanmeanpks10, na.rm=TRUE),window=c(10),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks10)))
		
	}
	if(all(is.na(meanmeanpks20))){
		SNmeanmeanpks20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks20 <- lm(meanmeanpks20 ~ pfstatsdf$sthyyear)
		SNmeanmeanpks20 <- data.frame(slope=SNmeanmeanpks20$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks20))/var(residuals(SNmeanmeanpks20))),data_variance=var(meanmeanpks20, na.rm=TRUE),window=c(20),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks20)))
		
	}
	if(all(is.na(meanmeanpks30))){
		SNmeanmeanpks30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks30 <- lm(meanmeanpks30 ~ pfstatsdf$sthyyear)
		SNmeanmeanpks30 <- data.frame(slope=SNmeanmeanpks30$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks30))/var(residuals(SNmeanmeanpks30))),data_variance=var(meanmeanpks30, na.rm=TRUE),window=c(30),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks30)))
		
	}
	SNmeanmeanpks <- rbind.data.frame(SNmeanmeanpks1,SNmeanmeanpks5,SNmeanmeanpks10,SNmeanmeanpks20,SNmeanmeanpks30)
	
	####################
	
	if(length(pfstatsdf$total_peakflow)>=5){
		meantotpks5 <- rollmean(pfstatsdf$total_peakflow,5,na.pad=TRUE)	
	} else {
		meantotpks5 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$total_peakflow)>=10){
		meantotpks10 <- rollmean(pfstatsdf$total_peakflow,10,na.pad=TRUE)
	} else {
		meantotpks10 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$total_peakflow)>=20){
		meantotpks20 <- rollmean(pfstatsdf$total_peakflow,20,na.pad=TRUE)
	} else {
		meantotpks20 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$total_peakflow)>=30){
		meantotpks30 <- rollmean(pfstatsdf$total_peakflow,30,na.pad=TRUE)
	} else {
		meantotpks30 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	if(length(pfstatsdf$total_peakflow)>=3){
		meantotpks1 <- pfstatsdf$total_peakflow
	} else {
		meantotpks1 <- rep(NA,length(pfstatsdf$sthyyear))
	}
	
	MKmeantotpks1 <- MannKendall(meantotpks1)
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
	
	if(all(is.na(meantotpks1))){
		SNmeantotpks1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks1 <- lm(meantotpks1 ~ pfstatsdf$sthyyear)
		SNmeantotpks1 <- data.frame(slope=SNmeantotpks1$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks1))/var(residuals(SNmeantotpks1))),data_variance=var(meantotpks1, na.rm=TRUE),window=c(1),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks1)))
		
	}
	
	if(all(is.na(meantotpks5))){
		SNmeantotpks5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks5 <- lm(meantotpks5 ~ pfstatsdf$sthyyear)
		SNmeantotpks5 <- data.frame(slope=SNmeantotpks5$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks5))/var(residuals(SNmeantotpks5))),data_variance=var(meantotpks5, na.rm=TRUE),window=c(5),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks5)))
		
	}
	if(all(is.na(meantotpks10))){
		SNmeantotpks10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks10 <- lm(meantotpks10 ~ pfstatsdf$sthyyear)
		SNmeantotpks10 <- data.frame(slope=SNmeantotpks10$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks10))/var(residuals(SNmeantotpks10))),data_variance=var(meantotpks10, na.rm=TRUE),window=c(10),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks10)))
		
	}
	if(all(is.na(meantotpks20))){
		SNmeantotpks20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks20 <- lm(meantotpks20 ~ pfstatsdf$sthyyear)
		SNmeantotpks20 <- data.frame(slope=SNmeantotpks20$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks20))/var(residuals(SNmeantotpks20))),data_variance=var(meantotpks20, na.rm=TRUE),window=c(20),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks20)))
		
	}
	if(all(is.na(meantotpks30))){
		SNmeantotpks30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks30 <- lm(meantotpks30 ~ pfstatsdf$sthyyear)
		SNmeantotpks30 <- data.frame(slope=SNmeantotpks30$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks30))/var(residuals(SNmeantotpks30))),data_variance=var(meantotpks30, na.rm=TRUE),window=c(30),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks30)))
		
	}
	SNmeantotpks <- rbind.data.frame(SNmeantotpks1,SNmeantotpks5,SNmeantotpks10,SNmeantotpks20,SNmeantotpks30)
	
	####################
	
	MKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
	SNdfs <- rbind.data.frame(SNmeantotvol,SNmeantotdays,SNmeannumpks,SNmeanmeanpks,SNmeantotpks)
	
	finaldf <- MKdfs
	finaldf$gauge <- gauge
	finaldfSN <- SNdfs
	finaldfSN$gauge <- gauge
	
	
	
	
	
	#####
	#####
	#####
	#####
	
	
	yearvec <- pfstatsdf$sthyyear[which(pfstatsdf$sthyyear>year)]
	totvolyearvec <- pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>year)]
	totdaysyearvec <- pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>year)]
	numpeakyearvec <- pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>year)]
	meanpeakyearvec <- pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>year)]
	totpeakyearvec <- pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>year)]
	
	
	if(length(totvolyearvec)>=5){
		meantotvol5 <- rollmean(totvolyearvec,5,na.pad=TRUE)	
	} else {
		meantotvol5 <- rep(NA,length(yearvec))
	}
	if(length(totvolyearvec)>=10){
		meantotvol10 <- rollmean(totvolyearvec,10,na.pad=TRUE)
	} else {
		meantotvol10 <- rep(NA,length(yearvec))
	}
	if(length(totvolyearvec)>=20){
		meantotvol20 <- rollmean(totvolyearvec,20,na.pad=TRUE)
	} else {
		meantotvol20 <- rep(NA,length(yearvec))
	}
	if(length(totvolyearvec)>=30){
		meantotvol30 <- rollmean(totvolyearvec,30,na.pad=TRUE)
	} else {
		meantotvol30 <- rep(NA,length(yearvec))
	}
	if(length(totvolyearvec)>=3){
		meantotvol1 <- totvolyearvec
	} else {
		meantotvol1 <- rep(NA,length(yearvec))
	}
	
	MKmeantotvol1 <- MannKendall(meantotvol1)
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
	
	
	if(all(is.na(meantotvol1))){
		SNmeantotvol1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("totvolabv"))
	}else{
		SNmeantotvol1 <- lm(meantotvol1 ~ yearvec)
		SNmeantotvol1 <- data.frame(slope=SNmeantotvol1$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol1))/var(residuals(SNmeantotvol1))),data_variance=var(meantotvol1, na.rm=TRUE),window=c(1),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol1)))
		
	}
	
	if(all(is.na(meantotvol5))){
		SNmeantotvol5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("totvolabv"))
	}else{
		SNmeantotvol5 <- lm(meantotvol5 ~ yearvec)
		SNmeantotvol5 <- data.frame(slope=SNmeantotvol5$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol5))/var(residuals(SNmeantotvol5))),data_variance=var(meantotvol5, na.rm=TRUE),window=c(5),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol5)))
		
	}
	if(all(is.na(meantotvol10))){
		SNmeantotvol10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("totvolabv"))
	}else{
		SNmeantotvol10 <- lm(meantotvol10 ~ yearvec)
		SNmeantotvol10 <- data.frame(slope=SNmeantotvol10$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol10))/var(residuals(SNmeantotvol10))),data_variance=var(meantotvol10, na.rm=TRUE),window=c(10),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol10)))
		
	}
	if(all(is.na(meantotvol20))){
		SNmeantotvol20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("totvolabv"))
	}else{
		SNmeantotvol20 <- lm(meantotvol20 ~ yearvec)
		SNmeantotvol20 <- data.frame(slope=SNmeantotvol20$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol20))/var(residuals(SNmeantotvol20))),data_variance=var(meantotvol20, na.rm=TRUE),window=c(20),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol20)))
		
	}
	if(all(is.na(meantotvol30))){
		SNmeantotvol30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("totvolabv"))
	}else{
		SNmeantotvol30 <- lm(meantotvol30 ~ yearvec)
		SNmeantotvol30 <- data.frame(slope=SNmeantotvol30$coefficients[[2]],SN=(var(fitted.values(SNmeantotvol30))/var(residuals(SNmeantotvol30))),data_variance=var(meantotvol30, na.rm=TRUE),window=c(30),measure=c("totvolabv"), dwstat=dwt(residuals(SNmeantotvol30)))
		
	}
	SNmeantotvol <- rbind.data.frame(SNmeantotvol1,SNmeantotvol5,SNmeantotvol10,SNmeantotvol20,SNmeantotvol30)
	
	
	
	############################
	if(length(totdaysyearvec)>=5){
		meantotdays5 <- rollmean(totdaysyearvec,5,na.pad=TRUE)	
	} else {
		meantotdays5 <- rep(NA,length(yearvec))
	}
	if(length(totdaysyearvec)>=10){
		meantotdays10 <- rollmean(totdaysyearvec,10,na.pad=TRUE)
	} else {
		meantotdays10 <- rep(NA,length(yearvec))
	}
	if(length(totdaysyearvec)>=20){
		meantotdays20 <- rollmean(totdaysyearvec,20,na.pad=TRUE)
	} else {
		meantotdays20 <- rep(NA,length(yearvec))
	}
	if(length(totdaysyearvec)>=30){
		meantotdays30 <- rollmean(totdaysyearvec,30,na.pad=TRUE)
	} else {
		meantotdays30 <- rep(NA,length(yearvec))
	}
	if(length(totdaysyearvec)>=3){
		meantotdays1 <- totdaysyearvec
	} else {
		meantotdays1 <- rep(NA,length(yearvec))
	}
	
	MKmeantotdays1 <- MannKendall(meantotdays1)
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
	
	if(all(is.na(meantotdays1))){
		SNmeantotdays1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("totdaysabv"))
	}else{
		SNmeantotdays1 <- lm(meantotdays1 ~ yearvec)
		SNmeantotdays1 <- data.frame(slope=SNmeantotdays1$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays1))/var(residuals(SNmeantotdays1))),data_variance=var(meantotdays1, na.rm=TRUE),window=c(1),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays1)))
		
	}
	
	if(all(is.na(meantotdays5))){
		SNmeantotdays5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("totdaysabv"))
	}else{
		SNmeantotdays5 <- lm(meantotdays5 ~ yearvec)
		SNmeantotdays5 <- data.frame(slope=SNmeantotdays5$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays5))/var(residuals(SNmeantotdays5))),data_variance=var(meantotdays5, na.rm=TRUE),window=c(5),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays5)))
		
	}
	if(all(is.na(meantotdays10))){
		SNmeantotdays10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("totdaysabv"))
	}else{
		SNmeantotdays10 <- lm(meantotdays10 ~ yearvec)
		SNmeantotdays10 <- data.frame(slope=SNmeantotdays10$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays10))/var(residuals(SNmeantotdays10))),data_variance=var(meantotdays10, na.rm=TRUE),window=c(10),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays10)))
		
	}
	if(all(is.na(meantotdays20))){
		SNmeantotdays20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("totdaysabv"))
	}else{
		SNmeantotdays20 <- lm(meantotdays20 ~ yearvec)
		SNmeantotdays20 <- data.frame(slope=SNmeantotdays20$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays20))/var(residuals(SNmeantotdays20))),data_variance=var(meantotdays20, na.rm=TRUE),window=c(20),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays20)))
		
	}
	if(all(is.na(meantotdays30))){
		SNmeantotdays30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("totdaysabv"))
	}else{
		SNmeantotdays30 <- lm(meantotdays30 ~ yearvec)
		SNmeantotdays30 <- data.frame(slope=SNmeantotdays30$coefficients[[2]],SN=(var(fitted.values(SNmeantotdays30))/var(residuals(SNmeantotdays30))),data_variance=var(meantotdays30, na.rm=TRUE),window=c(30),measure=c("totdaysabv"), dwstat=dwt(residuals(SNmeantotdays30)))
		
	}
	SNmeantotdays <- rbind.data.frame(SNmeantotdays1,SNmeantotdays5,SNmeantotdays10,SNmeantotdays20,SNmeantotdays30)
	
	####################
	
	if(length(numpeakyearvec)>=5){
		meannumpks5 <- rollmean(numpeakyearvec,5,na.pad=TRUE)	
	} else {
		meannumpks5 <- rep(NA,length(yearvec))
	}
	if(length(numpeakyearvec)>=10){
		meannumpks10 <- rollmean(numpeakyearvec,10,na.pad=TRUE)
	} else {
		meannumpks10 <- rep(NA,length(yearvec))
	}
	if(length(numpeakyearvec)>=20){
		meannumpks20 <- rollmean(numpeakyearvec,20,na.pad=TRUE)
	} else {
		meannumpks20 <- rep(NA,length(yearvec))
	}
	if(length(numpeakyearvec)>=30){
		meannumpks30 <- rollmean(numpeakyearvec,30,na.pad=TRUE)
	} else {
		meannumpks30 <- rep(NA,length(yearvec))
	}
	if(length(numpeakyearvec)>=3){
		meannumpks1 <- numpeakyearvec
	} else {
		meannumpks1 <- rep(NA,length(yearvec))
	}
	
	MKmeannumpks1 <- MannKendall(meannumpks1)
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
	
	if(all(is.na(meannumpks1))){
		SNmeannumpks1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks1 <- lm(meannumpks1 ~ yearvec)
		SNmeannumpks1 <- data.frame(slope=SNmeannumpks1$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks1))/var(residuals(SNmeannumpks1))),data_variance=var(meannumpks1, na.rm=TRUE),window=c(1),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks1)))
		
	}
	
	if(all(is.na(meannumpks5))){
		SNmeannumpks5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks5 <- lm(meannumpks5 ~ yearvec)
		SNmeannumpks5 <- data.frame(slope=SNmeannumpks5$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks5))/var(residuals(SNmeannumpks5))),data_variance=var(meannumpks5, na.rm=TRUE),window=c(5),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks5)))
		
	}
	if(all(is.na(meannumpks10))){
		SNmeannumpks10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks10 <- lm(meannumpks10 ~ yearvec)
		SNmeannumpks10 <- data.frame(slope=SNmeannumpks10$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks10))/var(residuals(SNmeannumpks10))),data_variance=var(meannumpks10, na.rm=TRUE),window=c(10),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks10)))
		
	}
	if(all(is.na(meannumpks20))){
		SNmeannumpks20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks20 <- lm(meannumpks20 ~ yearvec)
		SNmeannumpks20 <- data.frame(slope=SNmeannumpks20$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks20))/var(residuals(SNmeannumpks20))),data_variance=var(meannumpks20, na.rm=TRUE),window=c(20),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks20)))
		
	}
	if(all(is.na(meannumpks30))){
		SNmeannumpks30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("numpeaksabv"))
	}else{
		SNmeannumpks30 <- lm(meannumpks30 ~ yearvec)
		SNmeannumpks30 <- data.frame(slope=SNmeannumpks30$coefficients[[2]],SN=(var(fitted.values(SNmeannumpks30))/var(residuals(SNmeannumpks30))),data_variance=var(meannumpks30, na.rm=TRUE),window=c(30),measure=c("numpeaksabv"), dwstat=dwt(residuals(SNmeannumpks30)))
		
	}
	SNmeannumpks <- rbind.data.frame(SNmeannumpks1,SNmeannumpks5,SNmeannumpks10,SNmeannumpks20,SNmeannumpks30)
	
	
	####################
	
	if(length(meanpeakyearvec)>=5){
		meanmeanpks5 <- rollmean(meanpeakyearvec,5,na.pad=TRUE)	
	} else {
		meanmeanpks5 <- rep(NA,length(yearvec))
	}
	if(length(meanpeakyearvec)>=10){
		meanmeanpks10 <- rollmean(meanpeakyearvec,10,na.pad=TRUE)
	} else {
		meanmeanpks10 <- rep(NA,length(yearvec))
	}
	if(length(meanpeakyearvec)>=20){
		meanmeanpks20 <- rollmean(meanpeakyearvec,20,na.pad=TRUE)
	} else {
		meanmeanpks20 <- rep(NA,length(yearvec))
	}
	if(length(meanpeakyearvec)>=30){
		meanmeanpks30 <- rollmean(meanpeakyearvec,30,na.pad=TRUE)
	} else {
		meanmeanpks30 <- rep(NA,length(yearvec))
	}
	if(length(meanpeakyearvec)>=3){
		meanmeanpks1 <- meanpeakyearvec
	} else {
		meanmeanpks1 <- rep(NA,length(yearvec))
	}
	
	MKmeanmeanpks1 <- MannKendall(meanmeanpks1)
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
	
	if(all(is.na(meanmeanpks1))){
		SNmeanmeanpks1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks1 <- lm(meanmeanpks1 ~ yearvec)
		SNmeanmeanpks1 <- data.frame(slope=SNmeanmeanpks1$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks1))/var(residuals(SNmeanmeanpks1))),data_variance=var(meanmeanpks1, na.rm=TRUE),window=c(1),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks1)))
		
	}
	
	if(all(is.na(meanmeanpks5))){
		SNmeanmeanpks5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks5 <- lm(meanmeanpks5 ~ yearvec)
		SNmeanmeanpks5 <- data.frame(slope=SNmeanmeanpks5$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks5))/var(residuals(SNmeanmeanpks5))),data_variance=var(meanmeanpks5, na.rm=TRUE),window=c(5),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks5)))
		
	}
	if(all(is.na(meanmeanpks10))){
		SNmeanmeanpks10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks10 <- lm(meanmeanpks10 ~ yearvec)
		SNmeanmeanpks10 <- data.frame(slope=SNmeanmeanpks10$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks10))/var(residuals(SNmeanmeanpks10))),data_variance=var(meanmeanpks10, na.rm=TRUE),window=c(10),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks10)))
		
	}
	if(all(is.na(meanmeanpks20))){
		SNmeanmeanpks20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks20 <- lm(meanmeanpks20 ~ yearvec)
		SNmeanmeanpks20 <- data.frame(slope=SNmeanmeanpks20$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks20))/var(residuals(SNmeanmeanpks20))),data_variance=var(meanmeanpks20, na.rm=TRUE),window=c(20),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks20)))
		
	}
	if(all(is.na(meanmeanpks30))){
		SNmeanmeanpks30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("meanpeaksabv"))
	}else{
		SNmeanmeanpks30 <- lm(meanmeanpks30 ~ yearvec)
		SNmeanmeanpks30 <- data.frame(slope=SNmeanmeanpks30$coefficients[[2]],SN=(var(fitted.values(SNmeanmeanpks30))/var(residuals(SNmeanmeanpks30))),data_variance=var(meanmeanpks30, na.rm=TRUE),window=c(30),measure=c("meanpeaksabv"), dwstat=dwt(residuals(SNmeanmeanpks30)))
		
	}
	SNmeanmeanpks <- rbind.data.frame(SNmeanmeanpks1,SNmeanmeanpks5,SNmeanmeanpks10,SNmeanmeanpks20,SNmeanmeanpks30)
	
	####################
	
	if(length(totpeakyearvec)>=5){
		meantotpks5 <- rollmean(totpeakyearvec,5,na.pad=TRUE)	
	} else {
		meantotpks5 <- rep(NA,length(yearvec))
	}
	if(length(totpeakyearvec)>=10){
		meantotpks10 <- rollmean(totpeakyearvec,10,na.pad=TRUE)
	} else {
		meantotpks10 <- rep(NA,length(yearvec))
	}
	if(length(totpeakyearvec)>=20){
		meantotpks20 <- rollmean(totpeakyearvec,20,na.pad=TRUE)
	} else {
		meantotpks20 <- rep(NA,length(yearvec))
	}
	if(length(totpeakyearvec)>=30){
		meantotpks30 <- rollmean(totpeakyearvec,30,na.pad=TRUE)
	} else {
		meantotpks30 <- rep(NA,length(yearvec))
	}
	if(length(totpeakyearvec)>=3){
		meantotpks1 <- totpeakyearvec
	} else {
		meantotpks1 <- rep(NA,length(yearvec))
	}
	
	MKmeantotpks1 <- MannKendall(meantotpks1)
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
	
	if(all(is.na(meantotpks1))){
		SNmeantotpks1 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(1),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks1 <- lm(meantotpks1 ~ yearvec)
		SNmeantotpks1 <- data.frame(slope=SNmeantotpks1$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks1))/var(residuals(SNmeantotpks1))),data_variance=var(meantotpks1, na.rm=TRUE),window=c(1),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks1)))
		
	}
	
	if(all(is.na(meantotpks5))){
		SNmeantotpks5 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(5),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks5 <- lm(meantotpks5 ~ yearvec)
		SNmeantotpks5 <- data.frame(slope=SNmeantotpks5$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks5))/var(residuals(SNmeantotpks5))),data_variance=var(meantotpks5, na.rm=TRUE),window=c(5),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks5)))
		
	}
	if(all(is.na(meantotpks10))){
		SNmeantotpks10 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(10),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks10 <- lm(meantotpks10 ~ yearvec)
		SNmeantotpks10 <- data.frame(slope=SNmeantotpks10$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks10))/var(residuals(SNmeantotpks10))),data_variance=var(meantotpks10, na.rm=TRUE),window=c(10),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks10)))
		
	}
	if(all(is.na(meantotpks20))){
		SNmeantotpks20 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(20),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks20 <- lm(meantotpks20 ~ yearvec)
		SNmeantotpks20 <- data.frame(slope=SNmeantotpks20$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks20))/var(residuals(SNmeantotpks20))),data_variance=var(meantotpks20, na.rm=TRUE),window=c(20),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks20)))
		
	}
	if(all(is.na(meantotpks30))){
		SNmeantotpks30 <- data.frame(slope=NA,SN=NA,dwstat=NA,,data_variance=NA,window=c(30),measure=c("totpeakflwabv"))
	}else{
		SNmeantotpks30 <- lm(meantotpks30 ~ yearvec)
		SNmeantotpks30 <- data.frame(slope=SNmeantotpks30$coefficients[[2]],SN=(var(fitted.values(SNmeantotpks30))/var(residuals(SNmeantotpks30))),data_variance=var(meantotpks30, na.rm=TRUE),window=c(30),measure=c("totpeakflwabv"), dwstat=dwt(residuals(SNmeantotpks30)))
		
	}
	SNmeantotpks <- rbind.data.frame(SNmeantotpks1,SNmeantotpks5,SNmeantotpks10,SNmeantotpks20,SNmeantotpks30)
	
	####################
	
	MKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
	SNdfs <- rbind.data.frame(SNmeantotvol,SNmeantotdays,SNmeannumpks,SNmeanmeanpks,SNmeantotpks)
	
	finaldf_imp <- MKdfs
	finaldf_imp$gauge <- gauge
	finaldfSN_imp <- SNdfs
	finaldfSN_imp$gauge <- gauge
	
	
	
	
	
	
	
#	meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
#	meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
#	
#	
#	MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>year)])
#	MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
#	MKmeantotvol5 <- MannKendall(meantotvol5)
#	MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
#	MKmeantotvol10 <- MannKendall(meantotvol10)
#	MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))
#	MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10)
#	
#	
#	############################
#	meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
#	meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
#	
#	
#	MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>year)])
#	MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
#	MKmeantotdays5 <- MannKendall(meantotdays5)
#	MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
#	MKmeantotdays10 <- MannKendall(meantotdays10)
#	MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
#	MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10)
#	
#	####################
#	
#	meannumpks5 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
#	meannumpks10 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
#	
#	MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>year)])
#	MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
#	MKmeannumpks5 <- MannKendall(meannumpks5)
#	MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
#	MKmeannumpks10 <- MannKendall(meannumpks10)
#	MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
#	MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10)
#	
#	
#	####################
#	
#	
#	meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
#	meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
#	
#	
#	MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>year)])
#	MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
#	MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
#	MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
#	MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
#	MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
#	MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10)
#	
#	
#	####################
#	
#	meantotpks5 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>year)],5,na.pad=TRUE)
#	meantotpks10 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>year)],10,na.pad=TRUE)
#	
#	MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>year)])
#	MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
#	MKmeantotpks5 <- MannKendall(meantotpks5)
#	MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
#	MKmeantotpks10 <- MannKendall(meantotpks10)
#	MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
#	MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10)
#	
#	
#	####################
#	
#	impMKdfs <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
#	
#	impfinaldf <- impMKdfs
#	impfinaldf$gauge <- gauge
#	
#	
#	
#	#####################
#	meantotvol5 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>commonyear)],5,na.pad=TRUE)
#	meantotvol10 <- rollmean(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>commonyear)],10,na.pad=TRUE)
#	
#	
#	MKmeantotvol1 <- MannKendall(pfstatsdf$TotVolAbv_acft[which(pfstatsdf$sthyyear>commonyear)])
#	MKmeantotvol1 <- data.frame(tau=MKmeantotvol1$tau[[1]],p2=MKmeantotvol1$sl[[1]],window=c(1),measure=c("totvolabv"))
#	MKmeantotvol5 <- MannKendall(meantotvol5)
#	MKmeantotvol5 <- data.frame(tau=MKmeantotvol5$tau[[1]],p2=MKmeantotvol5$sl[[1]],window=c(5),measure=c("totvolabv"))
#	MKmeantotvol10 <- MannKendall(meantotvol10)
#	MKmeantotvol10 <- data.frame(tau=MKmeantotvol10$tau[[1]],p2=MKmeantotvol10$sl[[1]],window=c(10),measure=c("totvolabv"))
#	MKmeantotvol <- rbind.data.frame(MKmeantotvol1,MKmeantotvol5,MKmeantotvol10)
#	
#	
#	############################
#	meantotdays5 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>commonyear)],5,na.pad=TRUE)
#	meantotdays10 <- rollmean(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>commonyear)],10,na.pad=TRUE)
#	
#	
#	MKmeantotdays1 <- MannKendall(pfstatsdf$TotDaysAbv[which(pfstatsdf$sthyyear>commonyear)])
#	MKmeantotdays1 <- data.frame(tau=MKmeantotdays1$tau[[1]],p2=MKmeantotdays1$sl[[1]],window=c(1),measure=c("totdaysabv"))
#	MKmeantotdays5 <- MannKendall(meantotdays5)
#	MKmeantotdays5 <- data.frame(tau=MKmeantotdays5$tau[[1]],p2=MKmeantotdays5$sl[[1]],window=c(5),measure=c("totdaysabv"))
#	MKmeantotdays10 <- MannKendall(meantotdays10)
#	MKmeantotdays10 <- data.frame(tau=MKmeantotdays10$tau[[1]],p2=MKmeantotdays10$sl[[1]],window=c(10),measure=c("totdaysabv"))
#	MKmeantotdays <- rbind.data.frame(MKmeantotdays1,MKmeantotdays5,MKmeantotdays10)
#	
#	####################
#	
#	meannumpks5 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>commonyear)],5,na.pad=TRUE)
#	meannumpks10 <- rollmean(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>commonyear)],10,na.pad=TRUE)
#	
#	MKmeannumpks1 <- MannKendall(pfstatsdf$numpeaks[which(pfstatsdf$sthyyear>commonyear)])
#	MKmeannumpks1 <- data.frame(tau=MKmeannumpks1$tau[[1]],p2=MKmeannumpks1$sl[[1]],window=c(1),measure=c("numpeaksabv"))
#	MKmeannumpks5 <- MannKendall(meannumpks5)
#	MKmeannumpks5 <- data.frame(tau=MKmeannumpks5$tau[[1]],p2=MKmeannumpks5$sl[[1]],window=c(5),measure=c("numpeaksabv"))
#	MKmeannumpks10 <- MannKendall(meannumpks10)
#	MKmeannumpks10 <- data.frame(tau=MKmeannumpks10$tau[[1]],p2=MKmeannumpks10$sl[[1]],window=c(10),measure=c("numpeaksabv"))
#	MKmeannumpks <- rbind.data.frame(MKmeannumpks1,MKmeannumpks5,MKmeannumpks10)
#	
#	
#	####################
#	
#	
#	meanmeanpks5 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>commonyear)],5,na.pad=TRUE)
#	meanmeanpks10 <- rollmean(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>commonyear)],10,na.pad=TRUE)
#	
#	
#	MKmeanmeanpks1 <- MannKendall(pfstatsdf$mean_peakflow[which(pfstatsdf$sthyyear>commonyear)])
#	MKmeanmeanpks1 <- data.frame(tau=MKmeanmeanpks1$tau[[1]],p2=MKmeanmeanpks1$sl[[1]],window=c(1),measure=c("meanpeaksabv"))
#	MKmeanmeanpks5 <- MannKendall(meanmeanpks5)
#	MKmeanmeanpks5 <- data.frame(tau=MKmeanmeanpks5$tau[[1]],p2=MKmeanmeanpks5$sl[[1]],window=c(5),measure=c("meanpeaksabv"))
#	MKmeanmeanpks10 <- MannKendall(meanmeanpks10)
#	MKmeanmeanpks10 <- data.frame(tau=MKmeanmeanpks10$tau[[1]],p2=MKmeanmeanpks10$sl[[1]],window=c(10),measure=c("meanpeaksabv"))
#	MKmeanmeanpks <- rbind.data.frame(MKmeanmeanpks1,MKmeanmeanpks5,MKmeanmeanpks10)
#	
#	
#	####################
#	
#	meantotpks5 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>commonyear)],5,na.pad=TRUE)
#	meantotpks10 <- rollmean(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>commonyear)],10,na.pad=TRUE)
#	
#	MKmeantotpks1 <- MannKendall(pfstatsdf$total_peakflow[which(pfstatsdf$sthyyear>commonyear)])
#	MKmeantotpks1 <- data.frame(tau=MKmeantotpks1$tau[[1]],p2=MKmeantotpks1$sl[[1]],window=c(1),measure=c("totpeakflwabv"))
#	MKmeantotpks5 <- MannKendall(meantotpks5)
#	MKmeantotpks5 <- data.frame(tau=MKmeantotpks5$tau[[1]],p2=MKmeantotpks5$sl[[1]],window=c(5),measure=c("totpeakflwabv"))
#	MKmeantotpks10 <- MannKendall(meantotpks10)
#	MKmeantotpks10 <- data.frame(tau=MKmeantotpks10$tau[[1]],p2=MKmeantotpks10$sl[[1]],window=c(10),measure=c("totpeakflwabv"))
#	MKmeantotpks <- rbind.data.frame(MKmeantotpks1,MKmeantotpks5,MKmeantotpks10)
#	
#	
#	####################
#	
#	MKdfs1980 <- rbind.data.frame(MKmeantotvol,MKmeantotdays,MKmeannumpks,MKmeanmeanpks,MKmeantotpks)
#	
#	finaldf1980 <- MKdfs1980
#	finaldf1980$gauge <- gauge
#	finaldf1980$commonyear <- commonyear+1
#	
#	
#	
	
	
	################
	trend_full_merge <- merge(finaldf, finaldfSN)
	finallist <- list(
#			trend_dams=impfinaldf,
			trend_full=trend_full_merge
#			trend_1980=finaldf1980
	)
	
	return(finallist)
}


