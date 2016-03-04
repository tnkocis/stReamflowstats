# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


SNtest <- function(x,t, movingwindow,dwt, gauge){
	library(car)
	library(zoo)
	if(missing(movingwindow)&dwt==TRUE){
		SN <- lm(x ~ t)
		SNdf <- data.frame(slope=SN$coefficients[[2]],SN=(var(fitted.values(SN))/var(residuals(SN))),data_variance=var(x, na.rm=TRUE), dwstat=dwt(residuals(SN), max.lag=1), windowsize=NA, gauge=gauge)
		
	}else if(missing(movingwindow)&dwt==FALSE){
			SN <- lm(x ~ t)
			SNdf <- data.frame(slope=SN$coefficients[[2]],SN=(var(fitted.values(SN))/var(residuals(SN))),data_variance=var(x, na.rm=TRUE), dwstat=NA, windowsize=NA, gauge=gauge)
			
	}else if(dwt==FALSE){
		x <- rollmean(x,movingwindow,na.pad=TRUE)
		t <- seq(1,length(x),1)
		SN <- lm(x ~ t)
		SNdf <- data.frame(slope=SN$coefficients[[2]],SN=(var(fitted.values(SN))/var(residuals(SN))),data_variance=var(x, na.rm=TRUE), dwstat=NA, windowsize=movingwindow, gauge=gauge)
		
	}else{
		x <- rollmean(x,movingwindow,na.pad=TRUE)
		t <- seq(1,length(x),1)
		SN <- lm(x ~ t)
		SNdf <- data.frame(slope=SN$coefficients[[2]],SN=(var(fitted.values(SN))/var(residuals(SN))),data_variance=var(x, na.rm=TRUE), dwstat=dwt(residuals(SN), max.lag=1), windowsize=movingwindow, gauge=gauge)
		
	}
	return(SNdf)
}








i <-length(test_split)*6*15
comp <- data.frame(period=NA,yeartype=NA,MKtau=NA,MKp=NA,
		SNslope=NA,SNratio=NA,gauge=NA, dwt=NA, data_variance=NA)
for(i in 1:length(test_split)){
	for(j in 1:length(test_split[[i]])){
		for(k in 1:length(test_split[[i]][[j]])){
			period <- names(test_split[[i]][[j]])[[k]]
			yeartype <- names(test_split[[i]])[[j]]
			MK <- MannKendall(test_split[[i]][[j]][[k]]$TotVolAbv_acft)
			gauge <- names(test_split)[[i]]
			SN <- SNtest(test_split[[i]][[j]][[k]]$TotVolAbv_acft, test_split[[i]][[j]][[k]]$sthyyear)
			
			blah <- data.frame(period=period, yeartype=yeartype,MKtau=MK$tau[[1]], MKp=MK$sl[[1]],
				SNslope=SN$slope, SNratio=SN$SN, gauge=gauge,dwt=SN$dwstat,data_variance=SN$data_variance)
			
			comp <- rbind.data.frame(blah,comp)
		}
	}
}

voldayz <- test_split$`11452500`$all$aug$TotVolAbv_acft
voldayz[which(voldayz!=0)] <- 1
voldayz_cumsum <- cumsum(voldayz)
voldayz_cumsum_set <- voldayz_cumsum[2:length(voldayz_cumsum)]
diff <-  voldayz_cumsum_set - voldayz_cumsum[1:(length(voldayz_cumsum)-1)]

rollcount <- function(x,n) {z <- cumsum(c(0, x>0)); tail(z,-n) - head(z,-n)}



comp <- data.frame(period=NA,yeartype=NA,MKtau=NA,MKp=NA,
		SNslope=NA,SNratio=NA,gauge=NA, dwt=NA, data_variance=NA)
for(i in 1:length(spbatch)){
	for(j in 1:length(spbatch[[i]])){
		for(k in 1:length(spbatch[[i]][[j]])){
			period <- names(spbatch[[i]][[j]])[[k]]
			yeartype <- names(spbatch[[i]])[[j]]
			MK <- MannKendall(spbatch[[i]][[j]][[k]]$TotVolAbv_acft)
			gauge <- names(spbatch)[[i]]
			SN <- SNtest(spbatch[[i]][[j]][[k]]$TotVolAbv_acft, spbatch[[i]][[j]][[k]]$sthyyear)
			
			blah <- data.frame(period=period, yeartype=yeartype,MKtau=MK$tau[[1]], MKp=MK$sl[[1]],
					SNslope=SN$slope, SNratio=SN$SN, gauge=gauge,dwt=SN$dwstat,data_variance=SN$data_variance)
			
			comp <- rbind.data.frame(blah,comp)
		}
	}
}


n <- 5
m <- 30
SNtimestart <- Sys.time()
SNtest(spbatch$`11452500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day*1e6,
		spbatch$`11452500`$HydroYear$All$Data$Date,n*m, dwt=FALSE)
SNtimeend <- Sys.time()
SNtimeend - SNtimestart
MKTFPWstart <- Sys.time()
MKTFPW(rollmean(spbatch$`11452500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day*1e6,n*m, na.pad=FALSE))
MKTFPWend <- Sys.time()
MKTFPWend - MKTFPWstart
MKstart <- Sys.time()
MannKendall(rollmean(spbatch$`11452500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day*1e6,n*m, na.pad=FALSE))
MKend <- Sys.time()
MKend - MKstart
plot(rollmean(spbatch$`11452500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day*1e6,n*m, na.pad=FALSE),type="l")


plot(rollmean(spbatch$`11452500`$Winter_monthly$All$JAN$Data$Discharge_acfte6_day*1e6,110*30, na.pad=FALSE),type="l")


mux <- mean(test_split$`11452500`$all$hy$TotVolAbv_acft, na.rm=TRUE)
varx <- var(test_split$`11452500`$all$hy$TotVolAbv_acft, na.rm=TRUE)
p1 <- acf(test_split$`11452500`$all$hy$TotVolAbv_acft, na.action=na.pass, plot=FALSE)$acf[2]