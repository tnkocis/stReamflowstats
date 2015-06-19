# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


#finding percentile interval of SVI
# g1 USGS 11377100
# g2 USGS 11407000
# g3 USGS 11418000
# g4 USGS 11446500

SVI_gauges <- list()
SVI_gauges$g1  <- readNWISdv(11377100,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
SVI_gauges$g2  <- readNWISdv(11407000,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
SVI_gauges$g3  <- readNWISdv(11418000,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
SVI_gauges$g4  <- readNWISdv(11446500,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")

#SVI <- read.csv("/Users/tiffnk/Documents/UCDAVIS/Lab/Streamflow_Analysis/svi.csv", header=TRUE)

#SVI_quant <- list()
#for(i in 1:9){
#	SVI_quant[[i]] <- quantile(SVI$Index, probs=seq(0,1,0.005), type=i)
#}

#SVI_quant <- quantile(SVI$Index, probs=seq(0,1,0.005), type=)
#SVI_quant_oct <- quantile(SVI$oct_mar, probs=seq(0,1,0.005))
#SVI_quant_apr <- quantile(SVI$apr_jul, probs=seq(0,1,0.005))
#SVI_quant_wy <- quantile(SVI$Wysum, probs=seq(0,1,0.005))

#hist(SVI$Index, breaks=40, xlim=c(0,20))

USGS11377100 <- list()
USGS11377100$raw  <- readNWISdv(11377100,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
USGS11377100$raw$Discharge_cfs <- USGS11377100$raw$X_00060_00003
USGS11377100$raw$X_00060_00003 <- NULL
USGS11377100$raw <- RemoveLeapDays(USGS11377100)
USGS11377100$Availability <- DataAvailability(USGS11377100$raw)
USGS11377100$SVI_Q_Oct<- SVI_Q_Oct(USGS11377100)
USGS11377100$SVI_Q_Apr <- SVI_Q_Apr(USGS11377100)
USGS11377100$SVI_Q_Aug <- SVI_Q_Aug(USGS11377100)

USGS11407000 <- list()
USGS11407000$raw  <- readNWISdv(11407000,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
USGS11407000$raw$Discharge_cfs <- USGS11407000$raw$X_00060_00003
USGS11407000$raw$X_00060_00003 <- NULL
USGS11407000$raw <- RemoveLeapDays(USGS11407000)
USGS11407000$Availability <- DataAvailability(USGS11407000$raw)
USGS11407000$SVI_Q_Oct<- SVI_Q_Oct(USGS11407000)
USGS11407000$SVI_Q_Apr <- SVI_Q_Apr(USGS11407000)
USGS11407000$SVI_Q_Aug <- SVI_Q_Aug(USGS11407000)

USGS11418000<- list()
USGS11418000$raw  <- readNWISdv(11418000,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
USGS11418000$raw$Discharge_cfs <- USGS11418000$raw$X_00060_00003
USGS11418000$raw$X_00060_00003 <- NULL
USGS11418000$raw <- RemoveLeapDays(USGS11418000)
USGS11418000$Availability <- DataAvailability(USGS11418000$raw)
USGS11418000$SVI_Q_Oct<- SVI_Q_Oct(USGS11418000)
USGS11418000$SVI_Q_Apr <- SVI_Q_Apr(USGS11418000)
USGS11418000$SVI_Q_Aug <- SVI_Q_Aug(USGS11418000)

USGS11446500 <- list()
USGS11446500$raw  <- readNWISdv(11446500,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
USGS11446500$raw$Discharge_cfs <- USGS11446500$raw$X_00060_00003
USGS11446500$raw$X_00060_00003 <- NULL
USGS11446500$raw <- RemoveLeapDays(USGS11446500)
USGS11446500$Availability <- DataAvailability(USGS11446500$raw)
USGS11446500$SVI_Q_Oct<- SVI_Q_Oct(USGS11446500)
USGS11446500$SVI_Q_Apr <- SVI_Q_Apr(USGS11446500)
USGS11446500$SVI_Q_Aug <- SVI_Q_Aug(USGS11446500)












pdf(file="//Users//tiffnk//Documents//UCDAVIS//Lab//Streamflow_Analysis//Plots//USGS11377100_41_42_cont.pdf")
par(mfrow=c(3,1))
plot(USGS11377100$raw$Date,USGS11377100$raw$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main= "Entire Record")
plot(USGS11377100$Winter_Nov_Apr_6mon$Data$`1941 - 1942`$Date,USGS11377100$Winter_Nov_Apr_6mon$Data$`1941 - 1942`$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="November to April")
plot(USGS11377100$Winter_DEC_FEB$Data$`1941 - 1942`$Date,USGS11377100$Winter_DEC_FEB$Data$`1941 - 1942`$Discharge_cfs,  xlab="", ylab="Discharge (cfs)", main="December to February")
dev.off()

pdf(file="//Users//tiffnk//Documents//UCDAVIS//Lab//Streamflow_Analysis//Plots//USGS11377100_41_42_monthly.pdf")
par(mfrow=c(3,2))
plot(USGS11377100$Winter_Monthly$Data$`1941 - 1942`$NOV$Date,USGS11377100$Winter_Monthly$Data$`1941 - 1942`$NOV$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="November")
plot(USGS11377100$Winter_Monthly$Data$`1941 - 1942`$DEC$Date,USGS11377100$Winter_Monthly$Data$`1941 - 1942`$DEC$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="December")
plot(USGS11377100$Winter_Monthly$Data$`1941 - 1942`$JAN$Date,USGS11377100$Winter_Monthly$Data$`1941 - 1942`$JAN$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="January")
plot(USGS11377100$Winter_Monthly$Data$`1941 - 1942`$FEB$Date,USGS11377100$Winter_Monthly$Data$`1941 - 1942`$FEB$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="February")
plot(USGS11377100$Winter_Monthly$Data$`1941 - 1942`$MAR$Date,USGS11377100$Winter_Monthly$Data$`1941 - 1942`$MAR$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="March")
plot(USGS11377100$Winter_Monthly$Data$`1941 - 1942`$APR$Date,USGS11377100$Winter_Monthly$Data$`1941 - 1942`$APR$Discharge_cfs, xlab="", ylab="Discharge (cfs)", main="April")
dev.off()