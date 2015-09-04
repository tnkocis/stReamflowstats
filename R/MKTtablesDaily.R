# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################
library(Kendall)
library(dplyr)
library(hydroTSM)
library(dataRetrieval)



MKTdaily <- vector("list",7)
names(MKTdaily) <- c("active","saclower","sacupper","SJ", "tulare60", "tulare80","unimpaired")
##########################################################################################################




MKTdaily$active <- vector("list",4)
names(MKTdaily$active) <- c("HY","W3MON","W6MON", "WMON")
MKTdaily$active$HY <- vector("list", 6)
names(MKTdaily$active$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$active$W3MON <- vector("list", 6)
names(MKTdaily$active$W3MON) <- c("All","C","D","BN","AN","W")
MKTdaily$active$HY <- vector("list", 6)
names(MKTdaily$active$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$active$W6MON <- vector("list", 6)
names(MKTdaily$active$W6MON) <- c("All","C","D","BN","AN","W")
MKTdaily$active$WMON <- vector("list", 6)
names(MKTdaily$active$WMON) <- c("NOV","DEC","JAN","FEB","MAR","APR")
MKTdaily$active$WMON$NOV <- vector("list", 6)
names(MKTdaily$active$WMON$NOV) <- c("All","C","D","BN","AN","W")
MKTdaily$active$WMON$DEC <- vector("list", 6)
names(MKTdaily$active$WMON$DEC) <- c("All","C","D","BN","AN","W")
MKTdaily$active$WMON$JAN <- vector("list", 6)
names(MKTdaily$active$WMON$JAN) <- c("All","C","D","BN","AN","W")
MKTdaily$active$WMON$FEB <- vector("list", 6)
names(MKTdaily$active$WMON$FEB) <- c("All","C","D","BN","AN","W")
MKTdaily$active$WMON$MAR <- vector("list", 6)
names(MKTdaily$active$WMON$MAR) <- c("All","C","D","BN","AN","W")
MKTdaily$active$WMON$APR <- vector("list", 6)
names(MKTdaily$active$WMON$APR) <- c("All","C","D","BN","AN","W")

start <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(active[[1]]$HydroYear$All$Data$Discharge_acfte6_day[(which(active[[1]]$HydroYear$All$Data$Date==start)):(which(active[[1]]$HydroYear$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$HY$All <- d
for(i in 2:length(active)){
	start <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(active[[i]]$HydroYear$All$Data$Discharge_acfte6_day[(which(active[[i]]$HydroYear$All$Data$Date==start)):(which(active[[i]]$HydroYear$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$HY$All <- rbind(MKTdaily$active$HY$All, d)
}

d <- MannKendall(active[[1]]$HydroYear$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_C_year <- active[[1]]$HydroYear$C$Data$Date[[1]]
d$last_C_year <- active[[1]]$HydroYear$C$Data$Date[[tail(which(is.na(active[[1]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$HY$C <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$HydroYear$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_C_year <- active[[i]]$HydroYear$C$Data$Date[[1]]
	d$last_C_year <- active[[i]]$HydroYear$C$Data$Date[[tail(which(is.na(active[[i]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$HY$C <- rbind(MKTdaily$active$HY$C, d)
}

d <- MannKendall(active[[1]]$HydroYear$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_D_year <- active[[1]]$HydroYear$D$Data$Date[[1]]
d$last_D_year <- active[[1]]$HydroYear$D$Data$Date[[tail(which(is.na(active[[1]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$HY$D <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$HydroYear$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_D_year <- active[[i]]$HydroYear$D$Data$Date[[1]]
	d$last_D_year <- active[[i]]$HydroYear$D$Data$Date[[tail(which(is.na(active[[i]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$HY$D <- rbind(MKTdaily$active$HY$D, d)
}

d <- MannKendall(active[[1]]$HydroYear$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_BN_year <- active[[1]]$HydroYear$BN$Data$Date[[1]]
d$last_BN_year <- active[[1]]$HydroYear$BN$Data$Date[[tail(which(is.na(active[[1]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$HY$BN <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$HydroYear$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_BN_year <- active[[i]]$HydroYear$BN$Data$Date[[1]]
	d$last_BN_year <- active[[i]]$HydroYear$BN$Data$Date[[tail(which(is.na(active[[i]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$HY$BN <- rbind(MKTdaily$active$HY$BN, d)
}

d <- MannKendall(active[[1]]$HydroYear$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_AN_year <- active[[1]]$HydroYear$AN$Data$Date[[1]]
d$last_AN_year <- active[[1]]$HydroYear$AN$Data$Date[[tail(which(is.na(active[[1]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$HY$AN <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$HydroYear$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_AN_year <- active[[i]]$HydroYear$AN$Data$Date[[1]]
	d$last_AN_year <- active[[i]]$HydroYear$AN$Data$Date[[tail(which(is.na(active[[i]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$HY$AN <- rbind(MKTdaily$active$HY$AN, d)
}

d <- MannKendall(active[[1]]$HydroYear$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_W_year <- active[[1]]$HydroYear$W$Data$Date[[1]]
d$last_W_year <- active[[1]]$HydroYear$W$Data$Date[[tail(which(is.na(active[[1]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$HY$W <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$HydroYear$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_W_year <- active[[i]]$HydroYear$W$Data$Date[[1]]
	d$last_W_year <- active[[i]]$HydroYear$W$Data$Date[[tail(which(is.na(active[[i]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$HY$W <- rbind(MKTdaily$active$HY$W, d)
}


###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
d <- MannKendall(active[[1]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(active[[1]]$Winter_3mon$All$Data$Date==start)):(which(active[[1]]$Winter_3mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W3MON$All <- d
for(i in 2:length(active)){
	start <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(active[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(active[[i]]$Winter_3mon$All$Data$Date==start)):(which(active[[i]]$Winter_3mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W3MON$All <- rbind(MKTdaily$active$W3MON$All, d)
}

d <- MannKendall(active[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_C_year <- active[[1]]$Winter_3mon$C$Data$Date[[1]]
d$last_C_year <- active[[1]]$Winter_3mon$C$Data$Date[[tail(which(is.na(active[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W3MON$C <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_C_year <- active[[i]]$Winter_3mon$C$Data$Date[[1]]
	d$last_C_year <- active[[i]]$Winter_3mon$C$Data$Date[[tail(which(is.na(active[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W3MON$C <- rbind(MKTdaily$active$W3MON$C, d)
}

d <- MannKendall(active[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_D_year <- active[[1]]$Winter_3mon$D$Data$Date[[1]]
d$last_D_year <- active[[1]]$Winter_3mon$D$Data$Date[[tail(which(is.na(active[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W3MON$D <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_D_year <- active[[i]]$Winter_3mon$D$Data$Date[[1]]
	d$last_D_year <- active[[i]]$Winter_3mon$D$Data$Date[[tail(which(is.na(active[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W3MON$D <- rbind(MKTdaily$active$W3MON$D, d)
}

d <- MannKendall(active[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_BN_year <- active[[1]]$Winter_3mon$BN$Data$Date[[1]]
d$last_BN_year <- active[[1]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(active[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W3MON$BN <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_BN_year <- active[[i]]$Winter_3mon$BN$Data$Date[[1]]
	d$last_BN_year <- active[[i]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(active[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W3MON$BN <- rbind(MKTdaily$active$W3MON$BN, d)
}

d <- MannKendall(active[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_AN_year <- active[[1]]$Winter_3mon$AN$Data$Date[[1]]
d$last_AN_year <- active[[1]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(active[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W3MON$AN <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_AN_year <- active[[i]]$Winter_3mon$AN$Data$Date[[1]]
	d$last_AN_year <- active[[i]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(active[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W3MON$AN <- rbind(MKTdaily$active$W3MON$AN, d)
}

d <- MannKendall(active[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_W_year <- active[[1]]$Winter_3mon$W$Data$Date[[1]]
d$last_W_year <- active[[1]]$Winter_3mon$W$Data$Date[[tail(which(is.na(active[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W3MON$W <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_W_year <- active[[i]]$Winter_3mon$W$Data$Date[[1]]
	d$last_W_year <- active[[i]]$Winter_3mon$W$Data$Date[[tail(which(is.na(active[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W3MON$W <- rbind(MKTdaily$active$W3MON$W, d)
}

###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(active[[1]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(active[[1]]$Winter_6mon$All$Data$Date==start)):(which(active[[1]]$Winter_6mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W6MON$All <- d
for(i in 2:length(active)){
	start <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(active[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(active[[i]]$Winter_6mon$All$Data$Date==start)):(which(active[[i]]$Winter_6mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W6MON$All <- rbind(MKTdaily$active$W6MON$All, d)
}

d <- MannKendall(active[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_C_year <- active[[1]]$Winter_6mon$C$Data$Date[[1]]
d$last_C_year <- active[[1]]$Winter_6mon$C$Data$Date[[tail(which(is.na(active[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W6MON$C <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_C_year <- active[[i]]$Winter_6mon$C$Data$Date[[1]]
	d$last_C_year <- active[[i]]$Winter_6mon$C$Data$Date[[tail(which(is.na(active[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W6MON$C <- rbind(MKTdaily$active$W6MON$C, d)
}

d <- MannKendall(active[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_D_year <- active[[1]]$Winter_6mon$D$Data$Date[[1]]
d$last_D_year <- active[[1]]$Winter_6mon$D$Data$Date[[tail(which(is.na(active[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W6MON$D <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_D_year <- active[[i]]$Winter_6mon$D$Data$Date[[1]]
	d$last_D_year <- active[[i]]$Winter_6mon$D$Data$Date[[tail(which(is.na(active[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W6MON$D <- rbind(MKTdaily$active$W6MON$D, d)
}

d <- MannKendall(active[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_BN_year <- active[[1]]$Winter_6mon$BN$Data$Date[[1]]
d$last_BN_year <- active[[1]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(active[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W6MON$BN <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_BN_year <- active[[i]]$Winter_6mon$BN$Data$Date[[1]]
	d$last_BN_year <- active[[i]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(active[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W6MON$BN <- rbind(MKTdaily$active$W6MON$BN, d)
}

d <- MannKendall(active[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_AN_year <- active[[1]]$Winter_6mon$AN$Data$Date[[1]]
d$last_AN_year <- active[[1]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(active[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W6MON$AN <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_AN_year <- active[[i]]$Winter_6mon$AN$Data$Date[[1]]
	d$last_AN_year <- active[[i]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(active[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W6MON$AN <- rbind(MKTdaily$active$W6MON$AN, d)
}

d <- MannKendall(active[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_W_year <- active[[1]]$Winter_6mon$W$Data$Date[[1]]
d$last_W_year <- active[[1]]$Winter_6mon$W$Data$Date[[tail(which(is.na(active[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W6MON$W <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_W_year <- active[[i]]$Winter_6mon$W$Data$Date[[1]]
	d$last_W_year <- active[[i]]$Winter_6mon$W$Data$Date[[tail(which(is.na(active[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$active$W6MON$W <- rbind(MKTdaily$active$W6MON$W, d)
}

###############################################################
###############################################################
###############################################################

for(n in 1:6){
	if(n == 1){
		start <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
	} else if(n==2){
		start <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
	} else if(n==3){
		start <- as.Date(paste(as.numeric(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
	} else if(n==4){
		start <- as.Date(paste(as.numeric(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
	} else if(n==5){
		start <- as.Date(paste(as.numeric(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
	} else if(n==6){
		start <- as.Date(paste(as.numeric(as.character(active[[1]]$Availability$yearly$year[[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(active[[1]]$Availability$yearly$year[[tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
	}
	d <- MannKendall(active[[1]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(active[[1]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(active[[1]]$Winter_monthly$All[[n]]$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[1]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$active$WMON[[n]]$All <- d
	for(i in 2:length(active)){
		if(n == 1){
			start <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
		} else if(n==2){
			start <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
		} else if(n==3){
			start <- as.Date(paste(as.numeric(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
		} else if(n==4){
			start <- as.Date(paste(as.numeric(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
		} else if(n==5){
			start <- as.Date(paste(as.numeric(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
		} else if(n==6){
			start <- as.Date(paste(as.numeric(as.character(active[[i]]$Availability$yearly$year[[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(active[[i]]$Availability$yearly$year[[tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
		}
		d <- MannKendall(active[[i]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(active[[i]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(active[[i]]$Winter_monthly$All[[n]]$Data$Date==end))])
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- active[[i]]$raw$site_no[[1]]
		d$start_date <- start
		d$end_date <- end
		d$gaps_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$active$WMON[[n]]$All <- rbind(MKTdaily$active$WMON[[n]]$All, d)
	}
	
	d <- MannKendall(active[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[1]]$raw$site_no[[1]]
	d$first_C_year <- active[[1]]$Winter_monthly$C[[n]]$Data$Date[[1]]
	d$last_C_year <- active[[1]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(active[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$active$WMON[[n]]$C <- d
	for(i in 2:length(active)){
		d <- MannKendall(active[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- active[[i]]$raw$site_no[[1]]
		d$first_C_year <- active[[i]]$Winter_monthly$C[[n]]$Data$Date[[1]]
		d$last_C_year <- active[[i]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(active[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$active$WMON[[n]]$C <- rbind(MKTdaily$active$WMON[[n]]$C, d)
	}
	
	d <- MannKendall(active[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[1]]$raw$site_no[[1]]
	d$first_D_year <- active[[1]]$Winter_monthly$D[[n]]$Data$Date[[1]]
	d$last_D_year <- active[[1]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(active[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$active$WMON[[n]]$D <- d
	for(i in 2:length(active)){
		d <- MannKendall(active[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- active[[i]]$raw$site_no[[1]]
		d$first_D_year <- active[[i]]$Winter_monthly$D[[n]]$Data$Date[[1]]
		d$last_D_year <- active[[i]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(active[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$active$WMON[[n]]$D <- rbind(MKTdaily$active$WMON[[n]]$D, d)
	}
	
	d <- MannKendall(active[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[1]]$raw$site_no[[1]]
	d$first_BN_year <- active[[1]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
	d$last_BN_year <- active[[1]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(active[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$active$WMON[[n]]$BN <- d
	for(i in 2:length(active)){
		d <- MannKendall(active[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- active[[i]]$raw$site_no[[1]]
		d$first_BN_year <- active[[i]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
		d$last_BN_year <- active[[i]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(active[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$active$WMON[[n]]$BN <- rbind(MKTdaily$active$WMON[[n]]$BN, d)
	}
	
	d <- MannKendall(active[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[1]]$raw$site_no[[1]]
	d$first_AN_year <- active[[1]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
	d$last_AN_year <- active[[1]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(active[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$active$WMON[[n]]$AN <- d
	for(i in 2:length(active)){
		d <- MannKendall(active[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- active[[i]]$raw$site_no[[1]]
		d$first_AN_year <- active[[i]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
		d$last_AN_year <- active[[i]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(active[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$active$WMON[[n]]$AN <- rbind(MKTdaily$active$WMON[[n]]$AN, d)
	}
	
	d <- MannKendall(active[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[1]]$raw$site_no[[1]]
	d$first_W_year <- active[[1]]$Winter_monthly$W[[n]]$Data$Date[[1]]
	d$last_W_year <- active[[1]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(active[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$active$WMON[[n]]$W <- d
	for(i in 2:length(active)){
		d <- MannKendall(active[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- active[[i]]$raw$site_no[[1]]
		d$first_W_year <- active[[i]]$Winter_monthly$W[[n]]$Data$Date[[1]]
		d$last_W_year <- active[[i]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(active[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(active[[i]]$Availability$yearly$fraction_available[head(which(active[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$active$WMON[[n]]$W <- rbind(MKTdaily$active$WMON[[n]]$W, d)
	}
}

######################################







MKTdaily$saclower <- vector("list",4)
names(MKTdaily$saclower) <- c("HY","W3MON","W6MON", "WMON")
MKTdaily$saclower$HY <- vector("list", 6)
names(MKTdaily$saclower$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$W3MON <- vector("list", 6)
names(MKTdaily$saclower$W3MON) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$HY <- vector("list", 6)
names(MKTdaily$saclower$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$W6MON <- vector("list", 6)
names(MKTdaily$saclower$W6MON) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$WMON <- vector("list", 6)
names(MKTdaily$saclower$WMON) <- c("NOV","DEC","JAN","FEB","MAR","APR")
MKTdaily$saclower$WMON$NOV <- vector("list", 6)
names(MKTdaily$saclower$WMON$NOV) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$WMON$DEC <- vector("list", 6)
names(MKTdaily$saclower$WMON$DEC) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$WMON$JAN <- vector("list", 6)
names(MKTdaily$saclower$WMON$JAN) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$WMON$FEB <- vector("list", 6)
names(MKTdaily$saclower$WMON$FEB) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$WMON$MAR <- vector("list", 6)
names(MKTdaily$saclower$WMON$MAR) <- c("All","C","D","BN","AN","W")
MKTdaily$saclower$WMON$APR <- vector("list", 6)
names(MKTdaily$saclower$WMON$APR) <- c("All","C","D","BN","AN","W")

start <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(saclower[[1]]$HydroYear$All$Data$Discharge_acfte6_day[(which(saclower[[1]]$HydroYear$All$Data$Date==start)):(which(saclower[[1]]$HydroYear$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$HY$All <- d
for(i in 2:length(saclower)){
	start <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(saclower[[i]]$HydroYear$All$Data$Discharge_acfte6_day[(which(saclower[[i]]$HydroYear$All$Data$Date==start)):(which(saclower[[i]]$HydroYear$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$HY$All <- rbind(MKTdaily$saclower$HY$All, d)
}

d <- MannKendall(saclower[[1]]$HydroYear$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_C_year <- saclower[[1]]$HydroYear$C$Data$Date[[1]]
d$last_C_year <- saclower[[1]]$HydroYear$C$Data$Date[[tail(which(is.na(saclower[[1]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$HY$C <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$HydroYear$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_C_year <- saclower[[i]]$HydroYear$C$Data$Date[[1]]
	d$last_C_year <- saclower[[i]]$HydroYear$C$Data$Date[[tail(which(is.na(saclower[[i]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$HY$C <- rbind(MKTdaily$saclower$HY$C, d)
}

d <- MannKendall(saclower[[1]]$HydroYear$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_D_year <- saclower[[1]]$HydroYear$D$Data$Date[[1]]
d$last_D_year <- saclower[[1]]$HydroYear$D$Data$Date[[tail(which(is.na(saclower[[1]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$HY$D <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$HydroYear$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_D_year <- saclower[[i]]$HydroYear$D$Data$Date[[1]]
	d$last_D_year <- saclower[[i]]$HydroYear$D$Data$Date[[tail(which(is.na(saclower[[i]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$HY$D <- rbind(MKTdaily$saclower$HY$D, d)
}

d <- MannKendall(saclower[[1]]$HydroYear$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_BN_year <- saclower[[1]]$HydroYear$BN$Data$Date[[1]]
d$last_BN_year <- saclower[[1]]$HydroYear$BN$Data$Date[[tail(which(is.na(saclower[[1]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$HY$BN <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$HydroYear$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_BN_year <- saclower[[i]]$HydroYear$BN$Data$Date[[1]]
	d$last_BN_year <- saclower[[i]]$HydroYear$BN$Data$Date[[tail(which(is.na(saclower[[i]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$HY$BN <- rbind(MKTdaily$saclower$HY$BN, d)
}

d <- MannKendall(saclower[[1]]$HydroYear$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_AN_year <- saclower[[1]]$HydroYear$AN$Data$Date[[1]]
d$last_AN_year <- saclower[[1]]$HydroYear$AN$Data$Date[[tail(which(is.na(saclower[[1]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$HY$AN <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$HydroYear$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_AN_year <- saclower[[i]]$HydroYear$AN$Data$Date[[1]]
	d$last_AN_year <- saclower[[i]]$HydroYear$AN$Data$Date[[tail(which(is.na(saclower[[i]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$HY$AN <- rbind(MKTdaily$saclower$HY$AN, d)
}

d <- MannKendall(saclower[[1]]$HydroYear$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_W_year <- saclower[[1]]$HydroYear$W$Data$Date[[1]]
d$last_W_year <- saclower[[1]]$HydroYear$W$Data$Date[[tail(which(is.na(saclower[[1]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$HY$W <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$HydroYear$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_W_year <- saclower[[i]]$HydroYear$W$Data$Date[[1]]
	d$last_W_year <- saclower[[i]]$HydroYear$W$Data$Date[[tail(which(is.na(saclower[[i]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$HY$W <- rbind(MKTdaily$saclower$HY$W, d)
}


###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
d <- MannKendall(saclower[[1]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(saclower[[1]]$Winter_3mon$All$Data$Date==start)):(which(saclower[[1]]$Winter_3mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W3MON$All <- d
for(i in 2:length(saclower)){
	start <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(saclower[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(saclower[[i]]$Winter_3mon$All$Data$Date==start)):(which(saclower[[i]]$Winter_3mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W3MON$All <- rbind(MKTdaily$saclower$W3MON$All, d)
}

d <- MannKendall(saclower[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_C_year <- saclower[[1]]$Winter_3mon$C$Data$Date[[1]]
d$last_C_year <- saclower[[1]]$Winter_3mon$C$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W3MON$C <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_C_year <- saclower[[i]]$Winter_3mon$C$Data$Date[[1]]
	d$last_C_year <- saclower[[i]]$Winter_3mon$C$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W3MON$C <- rbind(MKTdaily$saclower$W3MON$C, d)
}

d <- MannKendall(saclower[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_D_year <- saclower[[1]]$Winter_3mon$D$Data$Date[[1]]
d$last_D_year <- saclower[[1]]$Winter_3mon$D$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W3MON$D <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_D_year <- saclower[[i]]$Winter_3mon$D$Data$Date[[1]]
	d$last_D_year <- saclower[[i]]$Winter_3mon$D$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W3MON$D <- rbind(MKTdaily$saclower$W3MON$D, d)
}

d <- MannKendall(saclower[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_BN_year <- saclower[[1]]$Winter_3mon$BN$Data$Date[[1]]
d$last_BN_year <- saclower[[1]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W3MON$BN <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_BN_year <- saclower[[i]]$Winter_3mon$BN$Data$Date[[1]]
	d$last_BN_year <- saclower[[i]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W3MON$BN <- rbind(MKTdaily$saclower$W3MON$BN, d)
}

d <- MannKendall(saclower[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_AN_year <- saclower[[1]]$Winter_3mon$AN$Data$Date[[1]]
d$last_AN_year <- saclower[[1]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W3MON$AN <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_AN_year <- saclower[[i]]$Winter_3mon$AN$Data$Date[[1]]
	d$last_AN_year <- saclower[[i]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W3MON$AN <- rbind(MKTdaily$saclower$W3MON$AN, d)
}

d <- MannKendall(saclower[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_W_year <- saclower[[1]]$Winter_3mon$W$Data$Date[[1]]
d$last_W_year <- saclower[[1]]$Winter_3mon$W$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W3MON$W <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_W_year <- saclower[[i]]$Winter_3mon$W$Data$Date[[1]]
	d$last_W_year <- saclower[[i]]$Winter_3mon$W$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W3MON$W <- rbind(MKTdaily$saclower$W3MON$W, d)
}

###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(saclower[[1]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(saclower[[1]]$Winter_6mon$All$Data$Date==start)):(which(saclower[[1]]$Winter_6mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W6MON$All <- d
for(i in 2:length(saclower)){
	start <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(saclower[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(saclower[[i]]$Winter_6mon$All$Data$Date==start)):(which(saclower[[i]]$Winter_6mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W6MON$All <- rbind(MKTdaily$saclower$W6MON$All, d)
}

d <- MannKendall(saclower[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_C_year <- saclower[[1]]$Winter_6mon$C$Data$Date[[1]]
d$last_C_year <- saclower[[1]]$Winter_6mon$C$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W6MON$C <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_C_year <- saclower[[i]]$Winter_6mon$C$Data$Date[[1]]
	d$last_C_year <- saclower[[i]]$Winter_6mon$C$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W6MON$C <- rbind(MKTdaily$saclower$W6MON$C, d)
}

d <- MannKendall(saclower[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_D_year <- saclower[[1]]$Winter_6mon$D$Data$Date[[1]]
d$last_D_year <- saclower[[1]]$Winter_6mon$D$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W6MON$D <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_D_year <- saclower[[i]]$Winter_6mon$D$Data$Date[[1]]
	d$last_D_year <- saclower[[i]]$Winter_6mon$D$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W6MON$D <- rbind(MKTdaily$saclower$W6MON$D, d)
}

d <- MannKendall(saclower[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_BN_year <- saclower[[1]]$Winter_6mon$BN$Data$Date[[1]]
d$last_BN_year <- saclower[[1]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W6MON$BN <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_BN_year <- saclower[[i]]$Winter_6mon$BN$Data$Date[[1]]
	d$last_BN_year <- saclower[[i]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W6MON$BN <- rbind(MKTdaily$saclower$W6MON$BN, d)
}

d <- MannKendall(saclower[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_AN_year <- saclower[[1]]$Winter_6mon$AN$Data$Date[[1]]
d$last_AN_year <- saclower[[1]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W6MON$AN <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_AN_year <- saclower[[i]]$Winter_6mon$AN$Data$Date[[1]]
	d$last_AN_year <- saclower[[i]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W6MON$AN <- rbind(MKTdaily$saclower$W6MON$AN, d)
}

d <- MannKendall(saclower[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- saclower[[1]]$raw$site_no[[1]]
d$first_W_year <- saclower[[1]]$Winter_6mon$W$Data$Date[[1]]
d$last_W_year <- saclower[[1]]$Winter_6mon$W$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$saclower$W6MON$W <- d
for(i in 2:length(saclower)){
	d <- MannKendall(saclower[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[i]]$raw$site_no[[1]]
	d$first_W_year <- saclower[[i]]$Winter_6mon$W$Data$Date[[1]]
	d$last_W_year <- saclower[[i]]$Winter_6mon$W$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$saclower$W6MON$W <- rbind(MKTdaily$saclower$W6MON$W, d)
}

###############################################################
###############################################################
###############################################################

for(n in 1:6){
	if(n == 1){
		start <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
	} else if(n==2){
		start <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
	} else if(n==3){
		start <- as.Date(paste(as.numeric(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
	} else if(n==4){
		start <- as.Date(paste(as.numeric(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
	} else if(n==5){
		start <- as.Date(paste(as.numeric(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
	} else if(n==6){
		start <- as.Date(paste(as.numeric(as.character(saclower[[1]]$Availability$yearly$year[[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(saclower[[1]]$Availability$yearly$year[[tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
	}
	d <- MannKendall(saclower[[1]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(saclower[[1]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(saclower[[1]]$Winter_monthly$All[[n]]$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[1]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$saclower$WMON[[n]]$All <- d
	for(i in 2:length(saclower)){
		if(n == 1){
			start <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
		} else if(n==2){
			start <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
		} else if(n==3){
			start <- as.Date(paste(as.numeric(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
		} else if(n==4){
			start <- as.Date(paste(as.numeric(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
		} else if(n==5){
			start <- as.Date(paste(as.numeric(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
		} else if(n==6){
			start <- as.Date(paste(as.numeric(as.character(saclower[[i]]$Availability$yearly$year[[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(saclower[[i]]$Availability$yearly$year[[tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
		}
		d <- MannKendall(saclower[[i]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(saclower[[i]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(saclower[[i]]$Winter_monthly$All[[n]]$Data$Date==end))])
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- saclower[[i]]$raw$site_no[[1]]
		d$start_date <- start
		d$end_date <- end
		d$gaps_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$saclower$WMON[[n]]$All <- rbind(MKTdaily$saclower$WMON[[n]]$All, d)
	}
	
	d <- MannKendall(saclower[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[1]]$raw$site_no[[1]]
	d$first_C_year <- saclower[[1]]$Winter_monthly$C[[n]]$Data$Date[[1]]
	d$last_C_year <- saclower[[1]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$saclower$WMON[[n]]$C <- d
	for(i in 2:length(saclower)){
		d <- MannKendall(saclower[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- saclower[[i]]$raw$site_no[[1]]
		d$first_C_year <- saclower[[i]]$Winter_monthly$C[[n]]$Data$Date[[1]]
		d$last_C_year <- saclower[[i]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$saclower$WMON[[n]]$C <- rbind(MKTdaily$saclower$WMON[[n]]$C, d)
	}
	
	d <- MannKendall(saclower[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[1]]$raw$site_no[[1]]
	d$first_D_year <- saclower[[1]]$Winter_monthly$D[[n]]$Data$Date[[1]]
	d$last_D_year <- saclower[[1]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$saclower$WMON[[n]]$D <- d
	for(i in 2:length(saclower)){
		d <- MannKendall(saclower[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- saclower[[i]]$raw$site_no[[1]]
		d$first_D_year <- saclower[[i]]$Winter_monthly$D[[n]]$Data$Date[[1]]
		d$last_D_year <- saclower[[i]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$saclower$WMON[[n]]$D <- rbind(MKTdaily$saclower$WMON[[n]]$D, d)
	}
	
	d <- MannKendall(saclower[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[1]]$raw$site_no[[1]]
	d$first_BN_year <- saclower[[1]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
	d$last_BN_year <- saclower[[1]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$saclower$WMON[[n]]$BN <- d
	for(i in 2:length(saclower)){
		d <- MannKendall(saclower[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- saclower[[i]]$raw$site_no[[1]]
		d$first_BN_year <- saclower[[i]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
		d$last_BN_year <- saclower[[i]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$saclower$WMON[[n]]$BN <- rbind(MKTdaily$saclower$WMON[[n]]$BN, d)
	}
	
	d <- MannKendall(saclower[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[1]]$raw$site_no[[1]]
	d$first_AN_year <- saclower[[1]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
	d$last_AN_year <- saclower[[1]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$saclower$WMON[[n]]$AN <- d
	for(i in 2:length(saclower)){
		d <- MannKendall(saclower[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- saclower[[i]]$raw$site_no[[1]]
		d$first_AN_year <- saclower[[i]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
		d$last_AN_year <- saclower[[i]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$saclower$WMON[[n]]$AN <- rbind(MKTdaily$saclower$WMON[[n]]$AN, d)
	}
	
	d <- MannKendall(saclower[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- saclower[[1]]$raw$site_no[[1]]
	d$first_W_year <- saclower[[1]]$Winter_monthly$W[[n]]$Data$Date[[1]]
	d$last_W_year <- saclower[[1]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(saclower[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(saclower[[1]]$Availability$yearly$fraction_available[head(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$saclower$WMON[[n]]$W <- d
	for(i in 2:length(saclower)){
		d <- MannKendall(saclower[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- saclower[[i]]$raw$site_no[[1]]
		d$first_W_year <- saclower[[i]]$Winter_monthly$W[[n]]$Data$Date[[1]]
		d$last_W_year <- saclower[[i]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(saclower[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(saclower[[i]]$Availability$yearly$fraction_available[head(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(saclower[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$saclower$WMON[[n]]$W <- rbind(MKTdaily$saclower$WMON[[n]]$W, d)
	}
}


#####################################################################










MKTdaily$sacupper <- vector("list",4)
names(MKTdaily$sacupper) <- c("HY","W3MON","W6MON", "WMON")
MKTdaily$sacupper$HY <- vector("list", 6)
names(MKTdaily$sacupper$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$W3MON <- vector("list", 6)
names(MKTdaily$sacupper$W3MON) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$HY <- vector("list", 6)
names(MKTdaily$sacupper$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$W6MON <- vector("list", 6)
names(MKTdaily$sacupper$W6MON) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$WMON <- vector("list", 6)
names(MKTdaily$sacupper$WMON) <- c("NOV","DEC","JAN","FEB","MAR","APR")
MKTdaily$sacupper$WMON$NOV <- vector("list", 6)
names(MKTdaily$sacupper$WMON$NOV) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$WMON$DEC <- vector("list", 6)
names(MKTdaily$sacupper$WMON$DEC) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$WMON$JAN <- vector("list", 6)
names(MKTdaily$sacupper$WMON$JAN) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$WMON$FEB <- vector("list", 6)
names(MKTdaily$sacupper$WMON$FEB) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$WMON$MAR <- vector("list", 6)
names(MKTdaily$sacupper$WMON$MAR) <- c("All","C","D","BN","AN","W")
MKTdaily$sacupper$WMON$APR <- vector("list", 6)
names(MKTdaily$sacupper$WMON$APR) <- c("All","C","D","BN","AN","W")

start <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(sacupper[[1]]$HydroYear$All$Data$Discharge_acfte6_day[(which(sacupper[[1]]$HydroYear$All$Data$Date==start)):(which(sacupper[[1]]$HydroYear$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$HY$All <- d
for(i in 2:length(sacupper)){
	start <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(sacupper[[i]]$HydroYear$All$Data$Discharge_acfte6_day[(which(sacupper[[i]]$HydroYear$All$Data$Date==start)):(which(sacupper[[i]]$HydroYear$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$HY$All <- rbind(MKTdaily$sacupper$HY$All, d)
}

d <- MannKendall(sacupper[[1]]$HydroYear$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_C_year <- sacupper[[1]]$HydroYear$C$Data$Date[[1]]
d$last_C_year <- sacupper[[1]]$HydroYear$C$Data$Date[[tail(which(is.na(sacupper[[1]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$HY$C <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$HydroYear$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_C_year <- sacupper[[i]]$HydroYear$C$Data$Date[[1]]
	d$last_C_year <- sacupper[[i]]$HydroYear$C$Data$Date[[tail(which(is.na(sacupper[[i]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$HY$C <- rbind(MKTdaily$sacupper$HY$C, d)
}

d <- MannKendall(sacupper[[1]]$HydroYear$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_D_year <- sacupper[[1]]$HydroYear$D$Data$Date[[1]]
d$last_D_year <- sacupper[[1]]$HydroYear$D$Data$Date[[tail(which(is.na(sacupper[[1]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$HY$D <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$HydroYear$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_D_year <- sacupper[[i]]$HydroYear$D$Data$Date[[1]]
	d$last_D_year <- sacupper[[i]]$HydroYear$D$Data$Date[[tail(which(is.na(sacupper[[i]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$HY$D <- rbind(MKTdaily$sacupper$HY$D, d)
}

d <- MannKendall(sacupper[[1]]$HydroYear$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_BN_year <- sacupper[[1]]$HydroYear$BN$Data$Date[[1]]
d$last_BN_year <- sacupper[[1]]$HydroYear$BN$Data$Date[[tail(which(is.na(sacupper[[1]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$HY$BN <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$HydroYear$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_BN_year <- sacupper[[i]]$HydroYear$BN$Data$Date[[1]]
	d$last_BN_year <- sacupper[[i]]$HydroYear$BN$Data$Date[[tail(which(is.na(sacupper[[i]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$HY$BN <- rbind(MKTdaily$sacupper$HY$BN, d)
}

d <- MannKendall(sacupper[[1]]$HydroYear$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_AN_year <- sacupper[[1]]$HydroYear$AN$Data$Date[[1]]
d$last_AN_year <- sacupper[[1]]$HydroYear$AN$Data$Date[[tail(which(is.na(sacupper[[1]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$HY$AN <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$HydroYear$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_AN_year <- sacupper[[i]]$HydroYear$AN$Data$Date[[1]]
	d$last_AN_year <- sacupper[[i]]$HydroYear$AN$Data$Date[[tail(which(is.na(sacupper[[i]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$HY$AN <- rbind(MKTdaily$sacupper$HY$AN, d)
}

d <- MannKendall(sacupper[[1]]$HydroYear$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_W_year <- sacupper[[1]]$HydroYear$W$Data$Date[[1]]
d$last_W_year <- sacupper[[1]]$HydroYear$W$Data$Date[[tail(which(is.na(sacupper[[1]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$HY$W <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$HydroYear$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_W_year <- sacupper[[i]]$HydroYear$W$Data$Date[[1]]
	d$last_W_year <- sacupper[[i]]$HydroYear$W$Data$Date[[tail(which(is.na(sacupper[[i]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$HY$W <- rbind(MKTdaily$sacupper$HY$W, d)
}


###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
d <- MannKendall(sacupper[[1]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(sacupper[[1]]$Winter_3mon$All$Data$Date==start)):(which(sacupper[[1]]$Winter_3mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W3MON$All <- d
for(i in 2:length(sacupper)){
	start <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(sacupper[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(sacupper[[i]]$Winter_3mon$All$Data$Date==start)):(which(sacupper[[i]]$Winter_3mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W3MON$All <- rbind(MKTdaily$sacupper$W3MON$All, d)
}

d <- MannKendall(sacupper[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_C_year <- sacupper[[1]]$Winter_3mon$C$Data$Date[[1]]
d$last_C_year <- sacupper[[1]]$Winter_3mon$C$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W3MON$C <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_C_year <- sacupper[[i]]$Winter_3mon$C$Data$Date[[1]]
	d$last_C_year <- sacupper[[i]]$Winter_3mon$C$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W3MON$C <- rbind(MKTdaily$sacupper$W3MON$C, d)
}

d <- MannKendall(sacupper[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_D_year <- sacupper[[1]]$Winter_3mon$D$Data$Date[[1]]
d$last_D_year <- sacupper[[1]]$Winter_3mon$D$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W3MON$D <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_D_year <- sacupper[[i]]$Winter_3mon$D$Data$Date[[1]]
	d$last_D_year <- sacupper[[i]]$Winter_3mon$D$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W3MON$D <- rbind(MKTdaily$sacupper$W3MON$D, d)
}

d <- MannKendall(sacupper[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_BN_year <- sacupper[[1]]$Winter_3mon$BN$Data$Date[[1]]
d$last_BN_year <- sacupper[[1]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W3MON$BN <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_BN_year <- sacupper[[i]]$Winter_3mon$BN$Data$Date[[1]]
	d$last_BN_year <- sacupper[[i]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W3MON$BN <- rbind(MKTdaily$sacupper$W3MON$BN, d)
}

d <- MannKendall(sacupper[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_AN_year <- sacupper[[1]]$Winter_3mon$AN$Data$Date[[1]]
d$last_AN_year <- sacupper[[1]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W3MON$AN <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_AN_year <- sacupper[[i]]$Winter_3mon$AN$Data$Date[[1]]
	d$last_AN_year <- sacupper[[i]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W3MON$AN <- rbind(MKTdaily$sacupper$W3MON$AN, d)
}

d <- MannKendall(sacupper[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_W_year <- sacupper[[1]]$Winter_3mon$W$Data$Date[[1]]
d$last_W_year <- sacupper[[1]]$Winter_3mon$W$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W3MON$W <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_W_year <- sacupper[[i]]$Winter_3mon$W$Data$Date[[1]]
	d$last_W_year <- sacupper[[i]]$Winter_3mon$W$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W3MON$W <- rbind(MKTdaily$sacupper$W3MON$W, d)
}

###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(sacupper[[1]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(sacupper[[1]]$Winter_6mon$All$Data$Date==start)):(which(sacupper[[1]]$Winter_6mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W6MON$All <- d
for(i in 2:length(sacupper)){
	start <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(sacupper[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(sacupper[[i]]$Winter_6mon$All$Data$Date==start)):(which(sacupper[[i]]$Winter_6mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W6MON$All <- rbind(MKTdaily$sacupper$W6MON$All, d)
}

d <- MannKendall(sacupper[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_C_year <- sacupper[[1]]$Winter_6mon$C$Data$Date[[1]]
d$last_C_year <- sacupper[[1]]$Winter_6mon$C$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W6MON$C <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_C_year <- sacupper[[i]]$Winter_6mon$C$Data$Date[[1]]
	d$last_C_year <- sacupper[[i]]$Winter_6mon$C$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W6MON$C <- rbind(MKTdaily$sacupper$W6MON$C, d)
}

d <- MannKendall(sacupper[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_D_year <- sacupper[[1]]$Winter_6mon$D$Data$Date[[1]]
d$last_D_year <- sacupper[[1]]$Winter_6mon$D$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W6MON$D <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_D_year <- sacupper[[i]]$Winter_6mon$D$Data$Date[[1]]
	d$last_D_year <- sacupper[[i]]$Winter_6mon$D$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W6MON$D <- rbind(MKTdaily$sacupper$W6MON$D, d)
}

d <- MannKendall(sacupper[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_BN_year <- sacupper[[1]]$Winter_6mon$BN$Data$Date[[1]]
d$last_BN_year <- sacupper[[1]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W6MON$BN <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_BN_year <- sacupper[[i]]$Winter_6mon$BN$Data$Date[[1]]
	d$last_BN_year <- sacupper[[i]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W6MON$BN <- rbind(MKTdaily$sacupper$W6MON$BN, d)
}

d <- MannKendall(sacupper[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_AN_year <- sacupper[[1]]$Winter_6mon$AN$Data$Date[[1]]
d$last_AN_year <- sacupper[[1]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W6MON$AN <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_AN_year <- sacupper[[i]]$Winter_6mon$AN$Data$Date[[1]]
	d$last_AN_year <- sacupper[[i]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W6MON$AN <- rbind(MKTdaily$sacupper$W6MON$AN, d)
}

d <- MannKendall(sacupper[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- sacupper[[1]]$raw$site_no[[1]]
d$first_W_year <- sacupper[[1]]$Winter_6mon$W$Data$Date[[1]]
d$last_W_year <- sacupper[[1]]$Winter_6mon$W$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$sacupper$W6MON$W <- d
for(i in 2:length(sacupper)){
	d <- MannKendall(sacupper[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[i]]$raw$site_no[[1]]
	d$first_W_year <- sacupper[[i]]$Winter_6mon$W$Data$Date[[1]]
	d$last_W_year <- sacupper[[i]]$Winter_6mon$W$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$sacupper$W6MON$W <- rbind(MKTdaily$sacupper$W6MON$W, d)
}

###############################################################
###############################################################
###############################################################

for(n in 1:6){
	if(n == 1){
		start <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
	} else if(n==2){
		start <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
	} else if(n==3){
		start <- as.Date(paste(as.numeric(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
	} else if(n==4){
		start <- as.Date(paste(as.numeric(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
	} else if(n==5){
		start <- as.Date(paste(as.numeric(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
	} else if(n==6){
		start <- as.Date(paste(as.numeric(as.character(sacupper[[1]]$Availability$yearly$year[[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(sacupper[[1]]$Availability$yearly$year[[tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
	}
	d <- MannKendall(sacupper[[1]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(sacupper[[1]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(sacupper[[1]]$Winter_monthly$All[[n]]$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[1]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$sacupper$WMON[[n]]$All <- d
	for(i in 2:length(sacupper)){
		if(n == 1){
			start <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
		} else if(n==2){
			start <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
		} else if(n==3){
			start <- as.Date(paste(as.numeric(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
		} else if(n==4){
			start <- as.Date(paste(as.numeric(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
		} else if(n==5){
			start <- as.Date(paste(as.numeric(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
		} else if(n==6){
			start <- as.Date(paste(as.numeric(as.character(sacupper[[i]]$Availability$yearly$year[[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(sacupper[[i]]$Availability$yearly$year[[tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
		}
		d <- MannKendall(sacupper[[i]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(sacupper[[i]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(sacupper[[i]]$Winter_monthly$All[[n]]$Data$Date==end))])
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- sacupper[[i]]$raw$site_no[[1]]
		d$start_date <- start
		d$end_date <- end
		d$gaps_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$sacupper$WMON[[n]]$All <- rbind(MKTdaily$sacupper$WMON[[n]]$All, d)
	}
	
	d <- MannKendall(sacupper[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[1]]$raw$site_no[[1]]
	d$first_C_year <- sacupper[[1]]$Winter_monthly$C[[n]]$Data$Date[[1]]
	d$last_C_year <- sacupper[[1]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$sacupper$WMON[[n]]$C <- d
	for(i in 2:length(sacupper)){
		d <- MannKendall(sacupper[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- sacupper[[i]]$raw$site_no[[1]]
		d$first_C_year <- sacupper[[i]]$Winter_monthly$C[[n]]$Data$Date[[1]]
		d$last_C_year <- sacupper[[i]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$sacupper$WMON[[n]]$C <- rbind(MKTdaily$sacupper$WMON[[n]]$C, d)
	}
	
	d <- MannKendall(sacupper[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[1]]$raw$site_no[[1]]
	d$first_D_year <- sacupper[[1]]$Winter_monthly$D[[n]]$Data$Date[[1]]
	d$last_D_year <- sacupper[[1]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$sacupper$WMON[[n]]$D <- d
	for(i in 2:length(sacupper)){
		d <- MannKendall(sacupper[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- sacupper[[i]]$raw$site_no[[1]]
		d$first_D_year <- sacupper[[i]]$Winter_monthly$D[[n]]$Data$Date[[1]]
		d$last_D_year <- sacupper[[i]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$sacupper$WMON[[n]]$D <- rbind(MKTdaily$sacupper$WMON[[n]]$D, d)
	}
	
	d <- MannKendall(sacupper[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[1]]$raw$site_no[[1]]
	d$first_BN_year <- sacupper[[1]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
	d$last_BN_year <- sacupper[[1]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$sacupper$WMON[[n]]$BN <- d
	for(i in 2:length(sacupper)){
		d <- MannKendall(sacupper[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- sacupper[[i]]$raw$site_no[[1]]
		d$first_BN_year <- sacupper[[i]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
		d$last_BN_year <- sacupper[[i]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$sacupper$WMON[[n]]$BN <- rbind(MKTdaily$sacupper$WMON[[n]]$BN, d)
	}
	
	d <- MannKendall(sacupper[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[1]]$raw$site_no[[1]]
	d$first_AN_year <- sacupper[[1]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
	d$last_AN_year <- sacupper[[1]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$sacupper$WMON[[n]]$AN <- d
	for(i in 2:length(sacupper)){
		d <- MannKendall(sacupper[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- sacupper[[i]]$raw$site_no[[1]]
		d$first_AN_year <- sacupper[[i]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
		d$last_AN_year <- sacupper[[i]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$sacupper$WMON[[n]]$AN <- rbind(MKTdaily$sacupper$WMON[[n]]$AN, d)
	}
	
	d <- MannKendall(sacupper[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- sacupper[[1]]$raw$site_no[[1]]
	d$first_W_year <- sacupper[[1]]$Winter_monthly$W[[n]]$Data$Date[[1]]
	d$last_W_year <- sacupper[[1]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(sacupper[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(sacupper[[1]]$Availability$yearly$fraction_available[head(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$sacupper$WMON[[n]]$W <- d
	for(i in 2:length(sacupper)){
		d <- MannKendall(sacupper[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- sacupper[[i]]$raw$site_no[[1]]
		d$first_W_year <- sacupper[[i]]$Winter_monthly$W[[n]]$Data$Date[[1]]
		d$last_W_year <- sacupper[[i]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(sacupper[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(sacupper[[i]]$Availability$yearly$fraction_available[head(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(sacupper[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$sacupper$WMON[[n]]$W <- rbind(MKTdaily$sacupper$WMON[[n]]$W, d)
	}
}

##################################################################






MKTdaily$SJ <- vector("list",4)
names(MKTdaily$SJ) <- c("HY","W3MON","W6MON", "WMON")
MKTdaily$SJ$HY <- vector("list", 6)
names(MKTdaily$SJ$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$W3MON <- vector("list", 6)
names(MKTdaily$SJ$W3MON) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$HY <- vector("list", 6)
names(MKTdaily$SJ$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$W6MON <- vector("list", 6)
names(MKTdaily$SJ$W6MON) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$WMON <- vector("list", 6)
names(MKTdaily$SJ$WMON) <- c("NOV","DEC","JAN","FEB","MAR","APR")
MKTdaily$SJ$WMON$NOV <- vector("list", 6)
names(MKTdaily$SJ$WMON$NOV) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$WMON$DEC <- vector("list", 6)
names(MKTdaily$SJ$WMON$DEC) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$WMON$JAN <- vector("list", 6)
names(MKTdaily$SJ$WMON$JAN) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$WMON$FEB <- vector("list", 6)
names(MKTdaily$SJ$WMON$FEB) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$WMON$MAR <- vector("list", 6)
names(MKTdaily$SJ$WMON$MAR) <- c("All","C","D","BN","AN","W")
MKTdaily$SJ$WMON$APR <- vector("list", 6)
names(MKTdaily$SJ$WMON$APR) <- c("All","C","D","BN","AN","W")

start <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(SJ[[1]]$HydroYear$All$Data$Discharge_acfte6_day[(which(SJ[[1]]$HydroYear$All$Data$Date==start)):(which(SJ[[1]]$HydroYear$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$HY$All <- d
for(i in 2:length(SJ)){
	start <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(SJ[[i]]$HydroYear$All$Data$Discharge_acfte6_day[(which(SJ[[i]]$HydroYear$All$Data$Date==start)):(which(SJ[[i]]$HydroYear$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$HY$All <- rbind(MKTdaily$SJ$HY$All, d)
}

d <- MannKendall(SJ[[1]]$HydroYear$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_C_year <- SJ[[1]]$HydroYear$C$Data$Date[[1]]
d$last_C_year <- SJ[[1]]$HydroYear$C$Data$Date[[tail(which(is.na(SJ[[1]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$HY$C <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$HydroYear$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_C_year <- SJ[[i]]$HydroYear$C$Data$Date[[1]]
	d$last_C_year <- SJ[[i]]$HydroYear$C$Data$Date[[tail(which(is.na(SJ[[i]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$HY$C <- rbind(MKTdaily$SJ$HY$C, d)
}

d <- MannKendall(SJ[[1]]$HydroYear$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_D_year <- SJ[[1]]$HydroYear$D$Data$Date[[1]]
d$last_D_year <- SJ[[1]]$HydroYear$D$Data$Date[[tail(which(is.na(SJ[[1]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$HY$D <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$HydroYear$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_D_year <- SJ[[i]]$HydroYear$D$Data$Date[[1]]
	d$last_D_year <- SJ[[i]]$HydroYear$D$Data$Date[[tail(which(is.na(SJ[[i]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$HY$D <- rbind(MKTdaily$SJ$HY$D, d)
}

d <- MannKendall(SJ[[1]]$HydroYear$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_BN_year <- SJ[[1]]$HydroYear$BN$Data$Date[[1]]
d$last_BN_year <- SJ[[1]]$HydroYear$BN$Data$Date[[tail(which(is.na(SJ[[1]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$HY$BN <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$HydroYear$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_BN_year <- SJ[[i]]$HydroYear$BN$Data$Date[[1]]
	d$last_BN_year <- SJ[[i]]$HydroYear$BN$Data$Date[[tail(which(is.na(SJ[[i]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$HY$BN <- rbind(MKTdaily$SJ$HY$BN, d)
}

d <- MannKendall(SJ[[1]]$HydroYear$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_AN_year <- SJ[[1]]$HydroYear$AN$Data$Date[[1]]
d$last_AN_year <- SJ[[1]]$HydroYear$AN$Data$Date[[tail(which(is.na(SJ[[1]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$HY$AN <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$HydroYear$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_AN_year <- SJ[[i]]$HydroYear$AN$Data$Date[[1]]
	d$last_AN_year <- SJ[[i]]$HydroYear$AN$Data$Date[[tail(which(is.na(SJ[[i]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$HY$AN <- rbind(MKTdaily$SJ$HY$AN, d)
}

d <- MannKendall(SJ[[1]]$HydroYear$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_W_year <- SJ[[1]]$HydroYear$W$Data$Date[[1]]
d$last_W_year <- SJ[[1]]$HydroYear$W$Data$Date[[tail(which(is.na(SJ[[1]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$HY$W <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$HydroYear$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_W_year <- SJ[[i]]$HydroYear$W$Data$Date[[1]]
	d$last_W_year <- SJ[[i]]$HydroYear$W$Data$Date[[tail(which(is.na(SJ[[i]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$HY$W <- rbind(MKTdaily$SJ$HY$W, d)
}


###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
d <- MannKendall(SJ[[1]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(SJ[[1]]$Winter_3mon$All$Data$Date==start)):(which(SJ[[1]]$Winter_3mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W3MON$All <- d
for(i in 2:length(SJ)){
	start <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(SJ[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(SJ[[i]]$Winter_3mon$All$Data$Date==start)):(which(SJ[[i]]$Winter_3mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W3MON$All <- rbind(MKTdaily$SJ$W3MON$All, d)
}

d <- MannKendall(SJ[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_C_year <- SJ[[1]]$Winter_3mon$C$Data$Date[[1]]
d$last_C_year <- SJ[[1]]$Winter_3mon$C$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W3MON$C <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_C_year <- SJ[[i]]$Winter_3mon$C$Data$Date[[1]]
	d$last_C_year <- SJ[[i]]$Winter_3mon$C$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W3MON$C <- rbind(MKTdaily$SJ$W3MON$C, d)
}

d <- MannKendall(SJ[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_D_year <- SJ[[1]]$Winter_3mon$D$Data$Date[[1]]
d$last_D_year <- SJ[[1]]$Winter_3mon$D$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W3MON$D <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_D_year <- SJ[[i]]$Winter_3mon$D$Data$Date[[1]]
	d$last_D_year <- SJ[[i]]$Winter_3mon$D$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W3MON$D <- rbind(MKTdaily$SJ$W3MON$D, d)
}

d <- MannKendall(SJ[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_BN_year <- SJ[[1]]$Winter_3mon$BN$Data$Date[[1]]
d$last_BN_year <- SJ[[1]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W3MON$BN <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_BN_year <- SJ[[i]]$Winter_3mon$BN$Data$Date[[1]]
	d$last_BN_year <- SJ[[i]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W3MON$BN <- rbind(MKTdaily$SJ$W3MON$BN, d)
}

d <- MannKendall(SJ[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_AN_year <- SJ[[1]]$Winter_3mon$AN$Data$Date[[1]]
d$last_AN_year <- SJ[[1]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W3MON$AN <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_AN_year <- SJ[[i]]$Winter_3mon$AN$Data$Date[[1]]
	d$last_AN_year <- SJ[[i]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W3MON$AN <- rbind(MKTdaily$SJ$W3MON$AN, d)
}

d <- MannKendall(SJ[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_W_year <- SJ[[1]]$Winter_3mon$W$Data$Date[[1]]
d$last_W_year <- SJ[[1]]$Winter_3mon$W$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W3MON$W <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_W_year <- SJ[[i]]$Winter_3mon$W$Data$Date[[1]]
	d$last_W_year <- SJ[[i]]$Winter_3mon$W$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W3MON$W <- rbind(MKTdaily$SJ$W3MON$W, d)
}

###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(SJ[[1]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(SJ[[1]]$Winter_6mon$All$Data$Date==start)):(which(SJ[[1]]$Winter_6mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W6MON$All <- d
for(i in 2:length(SJ)){
	start <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(SJ[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(SJ[[i]]$Winter_6mon$All$Data$Date==start)):(which(SJ[[i]]$Winter_6mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W6MON$All <- rbind(MKTdaily$SJ$W6MON$All, d)
}

d <- MannKendall(SJ[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_C_year <- SJ[[1]]$Winter_6mon$C$Data$Date[[1]]
d$last_C_year <- SJ[[1]]$Winter_6mon$C$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W6MON$C <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_C_year <- SJ[[i]]$Winter_6mon$C$Data$Date[[1]]
	d$last_C_year <- SJ[[i]]$Winter_6mon$C$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W6MON$C <- rbind(MKTdaily$SJ$W6MON$C, d)
}

d <- MannKendall(SJ[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_D_year <- SJ[[1]]$Winter_6mon$D$Data$Date[[1]]
d$last_D_year <- SJ[[1]]$Winter_6mon$D$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W6MON$D <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_D_year <- SJ[[i]]$Winter_6mon$D$Data$Date[[1]]
	d$last_D_year <- SJ[[i]]$Winter_6mon$D$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W6MON$D <- rbind(MKTdaily$SJ$W6MON$D, d)
}

d <- MannKendall(SJ[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_BN_year <- SJ[[1]]$Winter_6mon$BN$Data$Date[[1]]
d$last_BN_year <- SJ[[1]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W6MON$BN <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_BN_year <- SJ[[i]]$Winter_6mon$BN$Data$Date[[1]]
	d$last_BN_year <- SJ[[i]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W6MON$BN <- rbind(MKTdaily$SJ$W6MON$BN, d)
}

d <- MannKendall(SJ[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_AN_year <- SJ[[1]]$Winter_6mon$AN$Data$Date[[1]]
d$last_AN_year <- SJ[[1]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W6MON$AN <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_AN_year <- SJ[[i]]$Winter_6mon$AN$Data$Date[[1]]
	d$last_AN_year <- SJ[[i]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W6MON$AN <- rbind(MKTdaily$SJ$W6MON$AN, d)
}

d <- MannKendall(SJ[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- SJ[[1]]$raw$site_no[[1]]
d$first_W_year <- SJ[[1]]$Winter_6mon$W$Data$Date[[1]]
d$last_W_year <- SJ[[1]]$Winter_6mon$W$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$SJ$W6MON$W <- d
for(i in 2:length(SJ)){
	d <- MannKendall(SJ[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[i]]$raw$site_no[[1]]
	d$first_W_year <- SJ[[i]]$Winter_6mon$W$Data$Date[[1]]
	d$last_W_year <- SJ[[i]]$Winter_6mon$W$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$SJ$W6MON$W <- rbind(MKTdaily$SJ$W6MON$W, d)
}

###############################################################
###############################################################
###############################################################

for(n in 1:6){
	if(n == 1){
		start <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
	} else if(n==2){
		start <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
	} else if(n==3){
		start <- as.Date(paste(as.numeric(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
	} else if(n==4){
		start <- as.Date(paste(as.numeric(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
	} else if(n==5){
		start <- as.Date(paste(as.numeric(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
	} else if(n==6){
		start <- as.Date(paste(as.numeric(as.character(SJ[[1]]$Availability$yearly$year[[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(SJ[[1]]$Availability$yearly$year[[tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
	}
	d <- MannKendall(SJ[[1]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(SJ[[1]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(SJ[[1]]$Winter_monthly$All[[n]]$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[1]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$SJ$WMON[[n]]$All <- d
	for(i in 2:length(SJ)){
		if(n == 1){
			start <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
		} else if(n==2){
			start <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
		} else if(n==3){
			start <- as.Date(paste(as.numeric(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
		} else if(n==4){
			start <- as.Date(paste(as.numeric(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
		} else if(n==5){
			start <- as.Date(paste(as.numeric(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
		} else if(n==6){
			start <- as.Date(paste(as.numeric(as.character(SJ[[i]]$Availability$yearly$year[[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(SJ[[i]]$Availability$yearly$year[[tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
		}
		d <- MannKendall(SJ[[i]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(SJ[[i]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(SJ[[i]]$Winter_monthly$All[[n]]$Data$Date==end))])
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- SJ[[i]]$raw$site_no[[1]]
		d$start_date <- start
		d$end_date <- end
		d$gaps_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$SJ$WMON[[n]]$All <- rbind(MKTdaily$SJ$WMON[[n]]$All, d)
	}
	
	d <- MannKendall(SJ[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[1]]$raw$site_no[[1]]
	d$first_C_year <- SJ[[1]]$Winter_monthly$C[[n]]$Data$Date[[1]]
	d$last_C_year <- SJ[[1]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$SJ$WMON[[n]]$C <- d
	for(i in 2:length(SJ)){
		d <- MannKendall(SJ[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- SJ[[i]]$raw$site_no[[1]]
		d$first_C_year <- SJ[[i]]$Winter_monthly$C[[n]]$Data$Date[[1]]
		d$last_C_year <- SJ[[i]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$SJ$WMON[[n]]$C <- rbind(MKTdaily$SJ$WMON[[n]]$C, d)
	}
	
	d <- MannKendall(SJ[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[1]]$raw$site_no[[1]]
	d$first_D_year <- SJ[[1]]$Winter_monthly$D[[n]]$Data$Date[[1]]
	d$last_D_year <- SJ[[1]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$SJ$WMON[[n]]$D <- d
	for(i in 2:length(SJ)){
		d <- MannKendall(SJ[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- SJ[[i]]$raw$site_no[[1]]
		d$first_D_year <- SJ[[i]]$Winter_monthly$D[[n]]$Data$Date[[1]]
		d$last_D_year <- SJ[[i]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$SJ$WMON[[n]]$D <- rbind(MKTdaily$SJ$WMON[[n]]$D, d)
	}
	
	d <- MannKendall(SJ[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[1]]$raw$site_no[[1]]
	d$first_BN_year <- SJ[[1]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
	d$last_BN_year <- SJ[[1]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$SJ$WMON[[n]]$BN <- d
	for(i in 2:length(SJ)){
		d <- MannKendall(SJ[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- SJ[[i]]$raw$site_no[[1]]
		d$first_BN_year <- SJ[[i]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
		d$last_BN_year <- SJ[[i]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$SJ$WMON[[n]]$BN <- rbind(MKTdaily$SJ$WMON[[n]]$BN, d)
	}
	
	d <- MannKendall(SJ[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[1]]$raw$site_no[[1]]
	d$first_AN_year <- SJ[[1]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
	d$last_AN_year <- SJ[[1]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$SJ$WMON[[n]]$AN <- d
	for(i in 2:length(SJ)){
		d <- MannKendall(SJ[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- SJ[[i]]$raw$site_no[[1]]
		d$first_AN_year <- SJ[[i]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
		d$last_AN_year <- SJ[[i]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$SJ$WMON[[n]]$AN <- rbind(MKTdaily$SJ$WMON[[n]]$AN, d)
	}
	
	d <- MannKendall(SJ[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- SJ[[1]]$raw$site_no[[1]]
	d$first_W_year <- SJ[[1]]$Winter_monthly$W[[n]]$Data$Date[[1]]
	d$last_W_year <- SJ[[1]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(SJ[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(SJ[[1]]$Availability$yearly$fraction_available[head(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$SJ$WMON[[n]]$W <- d
	for(i in 2:length(SJ)){
		d <- MannKendall(SJ[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- SJ[[i]]$raw$site_no[[1]]
		d$first_W_year <- SJ[[i]]$Winter_monthly$W[[n]]$Data$Date[[1]]
		d$last_W_year <- SJ[[i]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(SJ[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(SJ[[i]]$Availability$yearly$fraction_available[head(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(SJ[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$SJ$WMON[[n]]$W <- rbind(MKTdaily$SJ$WMON[[n]]$W, d)
	}
}

############################################################

MKTdaily$tulare60 <- vector("list",4)
names(MKTdaily$tulare60) <- c("HY","W3MON","W6MON", "WMON")
MKTdaily$tulare60$HY <- vector("list", 6)
names(MKTdaily$tulare60$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$W3MON <- vector("list", 6)
names(MKTdaily$tulare60$W3MON) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$HY <- vector("list", 6)
names(MKTdaily$tulare60$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$W6MON <- vector("list", 6)
names(MKTdaily$tulare60$W6MON) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$WMON <- vector("list", 6)
names(MKTdaily$tulare60$WMON) <- c("NOV","DEC","JAN","FEB","MAR","APR")
MKTdaily$tulare60$WMON$NOV <- vector("list", 6)
names(MKTdaily$tulare60$WMON$NOV) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$WMON$DEC <- vector("list", 6)
names(MKTdaily$tulare60$WMON$DEC) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$WMON$JAN <- vector("list", 6)
names(MKTdaily$tulare60$WMON$JAN) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$WMON$FEB <- vector("list", 6)
names(MKTdaily$tulare60$WMON$FEB) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$WMON$MAR <- vector("list", 6)
names(MKTdaily$tulare60$WMON$MAR) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare60$WMON$APR <- vector("list", 6)
names(MKTdaily$tulare60$WMON$APR) <- c("All","C","D","BN","AN","W")

start <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(tulare60[[1]]$HydroYear$All$Data$Discharge_acfte6_day[(which(tulare60[[1]]$HydroYear$All$Data$Date==start)):(which(tulare60[[1]]$HydroYear$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$HY$All <- d
for(i in 2:length(tulare60)){
	start <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(tulare60[[i]]$HydroYear$All$Data$Discharge_acfte6_day[(which(tulare60[[i]]$HydroYear$All$Data$Date==start)):(which(tulare60[[i]]$HydroYear$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$HY$All <- rbind(MKTdaily$tulare60$HY$All, d)
}

d <- MannKendall(tulare60[[1]]$HydroYear$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_C_year <- tulare60[[1]]$HydroYear$C$Data$Date[[1]]
d$last_C_year <- tulare60[[1]]$HydroYear$C$Data$Date[[tail(which(is.na(tulare60[[1]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$HY$C <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$HydroYear$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_C_year <- tulare60[[i]]$HydroYear$C$Data$Date[[1]]
	d$last_C_year <- tulare60[[i]]$HydroYear$C$Data$Date[[tail(which(is.na(tulare60[[i]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$HY$C <- rbind(MKTdaily$tulare60$HY$C, d)
}

d <- MannKendall(tulare60[[1]]$HydroYear$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_D_year <- tulare60[[1]]$HydroYear$D$Data$Date[[1]]
d$last_D_year <- tulare60[[1]]$HydroYear$D$Data$Date[[tail(which(is.na(tulare60[[1]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$HY$D <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$HydroYear$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_D_year <- tulare60[[i]]$HydroYear$D$Data$Date[[1]]
	d$last_D_year <- tulare60[[i]]$HydroYear$D$Data$Date[[tail(which(is.na(tulare60[[i]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$HY$D <- rbind(MKTdaily$tulare60$HY$D, d)
}

d <- MannKendall(tulare60[[1]]$HydroYear$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_BN_year <- tulare60[[1]]$HydroYear$BN$Data$Date[[1]]
d$last_BN_year <- tulare60[[1]]$HydroYear$BN$Data$Date[[tail(which(is.na(tulare60[[1]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$HY$BN <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$HydroYear$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_BN_year <- tulare60[[i]]$HydroYear$BN$Data$Date[[1]]
	d$last_BN_year <- tulare60[[i]]$HydroYear$BN$Data$Date[[tail(which(is.na(tulare60[[i]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$HY$BN <- rbind(MKTdaily$tulare60$HY$BN, d)
}

d <- MannKendall(tulare60[[1]]$HydroYear$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_AN_year <- tulare60[[1]]$HydroYear$AN$Data$Date[[1]]
d$last_AN_year <- tulare60[[1]]$HydroYear$AN$Data$Date[[tail(which(is.na(tulare60[[1]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$HY$AN <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$HydroYear$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_AN_year <- tulare60[[i]]$HydroYear$AN$Data$Date[[1]]
	d$last_AN_year <- tulare60[[i]]$HydroYear$AN$Data$Date[[tail(which(is.na(tulare60[[i]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$HY$AN <- rbind(MKTdaily$tulare60$HY$AN, d)
}

d <- MannKendall(tulare60[[1]]$HydroYear$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_W_year <- tulare60[[1]]$HydroYear$W$Data$Date[[1]]
d$last_W_year <- tulare60[[1]]$HydroYear$W$Data$Date[[tail(which(is.na(tulare60[[1]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$HY$W <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$HydroYear$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_W_year <- tulare60[[i]]$HydroYear$W$Data$Date[[1]]
	d$last_W_year <- tulare60[[i]]$HydroYear$W$Data$Date[[tail(which(is.na(tulare60[[i]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$HY$W <- rbind(MKTdaily$tulare60$HY$W, d)
}


###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
d <- MannKendall(tulare60[[1]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(tulare60[[1]]$Winter_3mon$All$Data$Date==start)):(which(tulare60[[1]]$Winter_3mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W3MON$All <- d
for(i in 2:length(tulare60)){
	start <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(tulare60[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(tulare60[[i]]$Winter_3mon$All$Data$Date==start)):(which(tulare60[[i]]$Winter_3mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W3MON$All <- rbind(MKTdaily$tulare60$W3MON$All, d)
}

d <- MannKendall(tulare60[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_C_year <- tulare60[[1]]$Winter_3mon$C$Data$Date[[1]]
d$last_C_year <- tulare60[[1]]$Winter_3mon$C$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W3MON$C <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_C_year <- tulare60[[i]]$Winter_3mon$C$Data$Date[[1]]
	d$last_C_year <- tulare60[[i]]$Winter_3mon$C$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W3MON$C <- rbind(MKTdaily$tulare60$W3MON$C, d)
}

d <- MannKendall(tulare60[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_D_year <- tulare60[[1]]$Winter_3mon$D$Data$Date[[1]]
d$last_D_year <- tulare60[[1]]$Winter_3mon$D$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W3MON$D <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_D_year <- tulare60[[i]]$Winter_3mon$D$Data$Date[[1]]
	d$last_D_year <- tulare60[[i]]$Winter_3mon$D$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W3MON$D <- rbind(MKTdaily$tulare60$W3MON$D, d)
}

d <- MannKendall(tulare60[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_BN_year <- tulare60[[1]]$Winter_3mon$BN$Data$Date[[1]]
d$last_BN_year <- tulare60[[1]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W3MON$BN <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_BN_year <- tulare60[[i]]$Winter_3mon$BN$Data$Date[[1]]
	d$last_BN_year <- tulare60[[i]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W3MON$BN <- rbind(MKTdaily$tulare60$W3MON$BN, d)
}

d <- MannKendall(tulare60[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_AN_year <- tulare60[[1]]$Winter_3mon$AN$Data$Date[[1]]
d$last_AN_year <- tulare60[[1]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W3MON$AN <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_AN_year <- tulare60[[i]]$Winter_3mon$AN$Data$Date[[1]]
	d$last_AN_year <- tulare60[[i]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W3MON$AN <- rbind(MKTdaily$tulare60$W3MON$AN, d)
}

d <- MannKendall(tulare60[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_W_year <- tulare60[[1]]$Winter_3mon$W$Data$Date[[1]]
d$last_W_year <- tulare60[[1]]$Winter_3mon$W$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W3MON$W <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_W_year <- tulare60[[i]]$Winter_3mon$W$Data$Date[[1]]
	d$last_W_year <- tulare60[[i]]$Winter_3mon$W$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W3MON$W <- rbind(MKTdaily$tulare60$W3MON$W, d)
}

###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(tulare60[[1]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(tulare60[[1]]$Winter_6mon$All$Data$Date==start)):(which(tulare60[[1]]$Winter_6mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W6MON$All <- d
for(i in 2:length(tulare60)){
	start <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(tulare60[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(tulare60[[i]]$Winter_6mon$All$Data$Date==start)):(which(tulare60[[i]]$Winter_6mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W6MON$All <- rbind(MKTdaily$tulare60$W6MON$All, d)
}

d <- MannKendall(tulare60[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_C_year <- tulare60[[1]]$Winter_6mon$C$Data$Date[[1]]
d$last_C_year <- tulare60[[1]]$Winter_6mon$C$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W6MON$C <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_C_year <- tulare60[[i]]$Winter_6mon$C$Data$Date[[1]]
	d$last_C_year <- tulare60[[i]]$Winter_6mon$C$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W6MON$C <- rbind(MKTdaily$tulare60$W6MON$C, d)
}

d <- MannKendall(tulare60[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_D_year <- tulare60[[1]]$Winter_6mon$D$Data$Date[[1]]
d$last_D_year <- tulare60[[1]]$Winter_6mon$D$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W6MON$D <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_D_year <- tulare60[[i]]$Winter_6mon$D$Data$Date[[1]]
	d$last_D_year <- tulare60[[i]]$Winter_6mon$D$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W6MON$D <- rbind(MKTdaily$tulare60$W6MON$D, d)
}

d <- MannKendall(tulare60[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_BN_year <- tulare60[[1]]$Winter_6mon$BN$Data$Date[[1]]
d$last_BN_year <- tulare60[[1]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W6MON$BN <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_BN_year <- tulare60[[i]]$Winter_6mon$BN$Data$Date[[1]]
	d$last_BN_year <- tulare60[[i]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W6MON$BN <- rbind(MKTdaily$tulare60$W6MON$BN, d)
}

d <- MannKendall(tulare60[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_AN_year <- tulare60[[1]]$Winter_6mon$AN$Data$Date[[1]]
d$last_AN_year <- tulare60[[1]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W6MON$AN <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_AN_year <- tulare60[[i]]$Winter_6mon$AN$Data$Date[[1]]
	d$last_AN_year <- tulare60[[i]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W6MON$AN <- rbind(MKTdaily$tulare60$W6MON$AN, d)
}

d <- MannKendall(tulare60[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare60[[1]]$raw$site_no[[1]]
d$first_W_year <- tulare60[[1]]$Winter_6mon$W$Data$Date[[1]]
d$last_W_year <- tulare60[[1]]$Winter_6mon$W$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare60$W6MON$W <- d
for(i in 2:length(tulare60)){
	d <- MannKendall(tulare60[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[i]]$raw$site_no[[1]]
	d$first_W_year <- tulare60[[i]]$Winter_6mon$W$Data$Date[[1]]
	d$last_W_year <- tulare60[[i]]$Winter_6mon$W$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare60$W6MON$W <- rbind(MKTdaily$tulare60$W6MON$W, d)
}

###############################################################
###############################################################
###############################################################

for(n in 1:6){
	if(n == 1){
		start <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
	} else if(n==2){
		start <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
	} else if(n==3){
		start <- as.Date(paste(as.numeric(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
	} else if(n==4){
		start <- as.Date(paste(as.numeric(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
	} else if(n==5){
		start <- as.Date(paste(as.numeric(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
	} else if(n==6){
		start <- as.Date(paste(as.numeric(as.character(tulare60[[1]]$Availability$yearly$year[[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(tulare60[[1]]$Availability$yearly$year[[tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
	}
	d <- MannKendall(tulare60[[1]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(tulare60[[1]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(tulare60[[1]]$Winter_monthly$All[[n]]$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[1]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare60$WMON[[n]]$All <- d
	for(i in 2:length(tulare60)){
		if(n == 1){
			start <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
		} else if(n==2){
			start <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
		} else if(n==3){
			start <- as.Date(paste(as.numeric(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
		} else if(n==4){
			start <- as.Date(paste(as.numeric(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
		} else if(n==5){
			start <- as.Date(paste(as.numeric(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
		} else if(n==6){
			start <- as.Date(paste(as.numeric(as.character(tulare60[[i]]$Availability$yearly$year[[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(tulare60[[i]]$Availability$yearly$year[[tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
		}
		d <- MannKendall(tulare60[[i]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(tulare60[[i]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(tulare60[[i]]$Winter_monthly$All[[n]]$Data$Date==end))])
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare60[[i]]$raw$site_no[[1]]
		d$start_date <- start
		d$end_date <- end
		d$gaps_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare60$WMON[[n]]$All <- rbind(MKTdaily$tulare60$WMON[[n]]$All, d)
	}
	
	d <- MannKendall(tulare60[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[1]]$raw$site_no[[1]]
	d$first_C_year <- tulare60[[1]]$Winter_monthly$C[[n]]$Data$Date[[1]]
	d$last_C_year <- tulare60[[1]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare60$WMON[[n]]$C <- d
	for(i in 2:length(tulare60)){
		d <- MannKendall(tulare60[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare60[[i]]$raw$site_no[[1]]
		d$first_C_year <- tulare60[[i]]$Winter_monthly$C[[n]]$Data$Date[[1]]
		d$last_C_year <- tulare60[[i]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare60$WMON[[n]]$C <- rbind(MKTdaily$tulare60$WMON[[n]]$C, d)
	}
	
	d <- MannKendall(tulare60[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[1]]$raw$site_no[[1]]
	d$first_D_year <- tulare60[[1]]$Winter_monthly$D[[n]]$Data$Date[[1]]
	d$last_D_year <- tulare60[[1]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare60$WMON[[n]]$D <- d
	for(i in 2:length(tulare60)){
		d <- MannKendall(tulare60[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare60[[i]]$raw$site_no[[1]]
		d$first_D_year <- tulare60[[i]]$Winter_monthly$D[[n]]$Data$Date[[1]]
		d$last_D_year <- tulare60[[i]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare60$WMON[[n]]$D <- rbind(MKTdaily$tulare60$WMON[[n]]$D, d)
	}
	
	d <- MannKendall(tulare60[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[1]]$raw$site_no[[1]]
	d$first_BN_year <- tulare60[[1]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
	d$last_BN_year <- tulare60[[1]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare60$WMON[[n]]$BN <- d
	for(i in 2:length(tulare60)){
		d <- MannKendall(tulare60[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare60[[i]]$raw$site_no[[1]]
		d$first_BN_year <- tulare60[[i]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
		d$last_BN_year <- tulare60[[i]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare60$WMON[[n]]$BN <- rbind(MKTdaily$tulare60$WMON[[n]]$BN, d)
	}
	
	d <- MannKendall(tulare60[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[1]]$raw$site_no[[1]]
	d$first_AN_year <- tulare60[[1]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
	d$last_AN_year <- tulare60[[1]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare60$WMON[[n]]$AN <- d
	for(i in 2:length(tulare60)){
		d <- MannKendall(tulare60[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare60[[i]]$raw$site_no[[1]]
		d$first_AN_year <- tulare60[[i]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
		d$last_AN_year <- tulare60[[i]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare60$WMON[[n]]$AN <- rbind(MKTdaily$tulare60$WMON[[n]]$AN, d)
	}
	
	d <- MannKendall(tulare60[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare60[[1]]$raw$site_no[[1]]
	d$first_W_year <- tulare60[[1]]$Winter_monthly$W[[n]]$Data$Date[[1]]
	d$last_W_year <- tulare60[[1]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(tulare60[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare60[[1]]$Availability$yearly$fraction_available[head(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare60$WMON[[n]]$W <- d
	for(i in 2:length(tulare60)){
		d <- MannKendall(tulare60[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare60[[i]]$raw$site_no[[1]]
		d$first_W_year <- tulare60[[i]]$Winter_monthly$W[[n]]$Data$Date[[1]]
		d$last_W_year <- tulare60[[i]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(tulare60[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare60[[i]]$Availability$yearly$fraction_available[head(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare60[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare60$WMON[[n]]$W <- rbind(MKTdaily$tulare60$WMON[[n]]$W, d)
	}
}

########################################





MKTdaily$tulare80 <- vector("list",4)
names(MKTdaily$tulare80) <- c("HY","W3MON","W6MON", "WMON")
MKTdaily$tulare80$HY <- vector("list", 6)
names(MKTdaily$tulare80$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$W3MON <- vector("list", 6)
names(MKTdaily$tulare80$W3MON) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$HY <- vector("list", 6)
names(MKTdaily$tulare80$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$W6MON <- vector("list", 6)
names(MKTdaily$tulare80$W6MON) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$WMON <- vector("list", 6)
names(MKTdaily$tulare80$WMON) <- c("NOV","DEC","JAN","FEB","MAR","APR")
MKTdaily$tulare80$WMON$NOV <- vector("list", 6)
names(MKTdaily$tulare80$WMON$NOV) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$WMON$DEC <- vector("list", 6)
names(MKTdaily$tulare80$WMON$DEC) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$WMON$JAN <- vector("list", 6)
names(MKTdaily$tulare80$WMON$JAN) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$WMON$FEB <- vector("list", 6)
names(MKTdaily$tulare80$WMON$FEB) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$WMON$MAR <- vector("list", 6)
names(MKTdaily$tulare80$WMON$MAR) <- c("All","C","D","BN","AN","W")
MKTdaily$tulare80$WMON$APR <- vector("list", 6)
names(MKTdaily$tulare80$WMON$APR) <- c("All","C","D","BN","AN","W")

start <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(tulare80[[1]]$HydroYear$All$Data$Discharge_acfte6_day[(which(tulare80[[1]]$HydroYear$All$Data$Date==start)):(which(tulare80[[1]]$HydroYear$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$HY$All <- d
for(i in 2:length(tulare80)){
	start <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(tulare80[[i]]$HydroYear$All$Data$Discharge_acfte6_day[(which(tulare80[[i]]$HydroYear$All$Data$Date==start)):(which(tulare80[[i]]$HydroYear$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$HY$All <- rbind(MKTdaily$tulare80$HY$All, d)
}

d <- MannKendall(tulare80[[1]]$HydroYear$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_C_year <- tulare80[[1]]$HydroYear$C$Data$Date[[1]]
d$last_C_year <- tulare80[[1]]$HydroYear$C$Data$Date[[tail(which(is.na(tulare80[[1]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$HY$C <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$HydroYear$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_C_year <- tulare80[[i]]$HydroYear$C$Data$Date[[1]]
	d$last_C_year <- tulare80[[i]]$HydroYear$C$Data$Date[[tail(which(is.na(tulare80[[i]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$HY$C <- rbind(MKTdaily$tulare80$HY$C, d)
}

d <- MannKendall(tulare80[[1]]$HydroYear$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_D_year <- tulare80[[1]]$HydroYear$D$Data$Date[[1]]
d$last_D_year <- tulare80[[1]]$HydroYear$D$Data$Date[[tail(which(is.na(tulare80[[1]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$HY$D <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$HydroYear$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_D_year <- tulare80[[i]]$HydroYear$D$Data$Date[[1]]
	d$last_D_year <- tulare80[[i]]$HydroYear$D$Data$Date[[tail(which(is.na(tulare80[[i]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$HY$D <- rbind(MKTdaily$tulare80$HY$D, d)
}

d <- MannKendall(tulare80[[1]]$HydroYear$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_BN_year <- tulare80[[1]]$HydroYear$BN$Data$Date[[1]]
d$last_BN_year <- tulare80[[1]]$HydroYear$BN$Data$Date[[tail(which(is.na(tulare80[[1]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$HY$BN <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$HydroYear$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_BN_year <- tulare80[[i]]$HydroYear$BN$Data$Date[[1]]
	d$last_BN_year <- tulare80[[i]]$HydroYear$BN$Data$Date[[tail(which(is.na(tulare80[[i]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$HY$BN <- rbind(MKTdaily$tulare80$HY$BN, d)
}

d <- MannKendall(tulare80[[1]]$HydroYear$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_AN_year <- tulare80[[1]]$HydroYear$AN$Data$Date[[1]]
d$last_AN_year <- tulare80[[1]]$HydroYear$AN$Data$Date[[tail(which(is.na(tulare80[[1]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$HY$AN <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$HydroYear$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_AN_year <- tulare80[[i]]$HydroYear$AN$Data$Date[[1]]
	d$last_AN_year <- tulare80[[i]]$HydroYear$AN$Data$Date[[tail(which(is.na(tulare80[[i]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$HY$AN <- rbind(MKTdaily$tulare80$HY$AN, d)
}

d <- MannKendall(tulare80[[1]]$HydroYear$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_W_year <- tulare80[[1]]$HydroYear$W$Data$Date[[1]]
d$last_W_year <- tulare80[[1]]$HydroYear$W$Data$Date[[tail(which(is.na(tulare80[[1]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$HY$W <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$HydroYear$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_W_year <- tulare80[[i]]$HydroYear$W$Data$Date[[1]]
	d$last_W_year <- tulare80[[i]]$HydroYear$W$Data$Date[[tail(which(is.na(tulare80[[i]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$HY$W <- rbind(MKTdaily$tulare80$HY$W, d)
}


###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
d <- MannKendall(tulare80[[1]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(tulare80[[1]]$Winter_3mon$All$Data$Date==start)):(which(tulare80[[1]]$Winter_3mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W3MON$All <- d
for(i in 2:length(tulare80)){
	start <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(tulare80[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(tulare80[[i]]$Winter_3mon$All$Data$Date==start)):(which(tulare80[[i]]$Winter_3mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W3MON$All <- rbind(MKTdaily$tulare80$W3MON$All, d)
}

d <- MannKendall(tulare80[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_C_year <- tulare80[[1]]$Winter_3mon$C$Data$Date[[1]]
d$last_C_year <- tulare80[[1]]$Winter_3mon$C$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W3MON$C <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_C_year <- tulare80[[i]]$Winter_3mon$C$Data$Date[[1]]
	d$last_C_year <- tulare80[[i]]$Winter_3mon$C$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W3MON$C <- rbind(MKTdaily$tulare80$W3MON$C, d)
}

d <- MannKendall(tulare80[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_D_year <- tulare80[[1]]$Winter_3mon$D$Data$Date[[1]]
d$last_D_year <- tulare80[[1]]$Winter_3mon$D$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W3MON$D <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_D_year <- tulare80[[i]]$Winter_3mon$D$Data$Date[[1]]
	d$last_D_year <- tulare80[[i]]$Winter_3mon$D$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W3MON$D <- rbind(MKTdaily$tulare80$W3MON$D, d)
}

d <- MannKendall(tulare80[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_BN_year <- tulare80[[1]]$Winter_3mon$BN$Data$Date[[1]]
d$last_BN_year <- tulare80[[1]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W3MON$BN <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_BN_year <- tulare80[[i]]$Winter_3mon$BN$Data$Date[[1]]
	d$last_BN_year <- tulare80[[i]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W3MON$BN <- rbind(MKTdaily$tulare80$W3MON$BN, d)
}

d <- MannKendall(tulare80[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_AN_year <- tulare80[[1]]$Winter_3mon$AN$Data$Date[[1]]
d$last_AN_year <- tulare80[[1]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W3MON$AN <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_AN_year <- tulare80[[i]]$Winter_3mon$AN$Data$Date[[1]]
	d$last_AN_year <- tulare80[[i]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W3MON$AN <- rbind(MKTdaily$tulare80$W3MON$AN, d)
}

d <- MannKendall(tulare80[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_W_year <- tulare80[[1]]$Winter_3mon$W$Data$Date[[1]]
d$last_W_year <- tulare80[[1]]$Winter_3mon$W$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W3MON$W <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_W_year <- tulare80[[i]]$Winter_3mon$W$Data$Date[[1]]
	d$last_W_year <- tulare80[[i]]$Winter_3mon$W$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W3MON$W <- rbind(MKTdaily$tulare80$W3MON$W, d)
}

###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(tulare80[[1]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(tulare80[[1]]$Winter_6mon$All$Data$Date==start)):(which(tulare80[[1]]$Winter_6mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W6MON$All <- d
for(i in 2:length(tulare80)){
	start <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(tulare80[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(tulare80[[i]]$Winter_6mon$All$Data$Date==start)):(which(tulare80[[i]]$Winter_6mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W6MON$All <- rbind(MKTdaily$tulare80$W6MON$All, d)
}

d <- MannKendall(tulare80[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_C_year <- tulare80[[1]]$Winter_6mon$C$Data$Date[[1]]
d$last_C_year <- tulare80[[1]]$Winter_6mon$C$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W6MON$C <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_C_year <- tulare80[[i]]$Winter_6mon$C$Data$Date[[1]]
	d$last_C_year <- tulare80[[i]]$Winter_6mon$C$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W6MON$C <- rbind(MKTdaily$tulare80$W6MON$C, d)
}

d <- MannKendall(tulare80[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_D_year <- tulare80[[1]]$Winter_6mon$D$Data$Date[[1]]
d$last_D_year <- tulare80[[1]]$Winter_6mon$D$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W6MON$D <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_D_year <- tulare80[[i]]$Winter_6mon$D$Data$Date[[1]]
	d$last_D_year <- tulare80[[i]]$Winter_6mon$D$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W6MON$D <- rbind(MKTdaily$tulare80$W6MON$D, d)
}

d <- MannKendall(tulare80[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_BN_year <- tulare80[[1]]$Winter_6mon$BN$Data$Date[[1]]
d$last_BN_year <- tulare80[[1]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W6MON$BN <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_BN_year <- tulare80[[i]]$Winter_6mon$BN$Data$Date[[1]]
	d$last_BN_year <- tulare80[[i]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W6MON$BN <- rbind(MKTdaily$tulare80$W6MON$BN, d)
}

d <- MannKendall(tulare80[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_AN_year <- tulare80[[1]]$Winter_6mon$AN$Data$Date[[1]]
d$last_AN_year <- tulare80[[1]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W6MON$AN <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_AN_year <- tulare80[[i]]$Winter_6mon$AN$Data$Date[[1]]
	d$last_AN_year <- tulare80[[i]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W6MON$AN <- rbind(MKTdaily$tulare80$W6MON$AN, d)
}

d <- MannKendall(tulare80[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- tulare80[[1]]$raw$site_no[[1]]
d$first_W_year <- tulare80[[1]]$Winter_6mon$W$Data$Date[[1]]
d$last_W_year <- tulare80[[1]]$Winter_6mon$W$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$tulare80$W6MON$W <- d
for(i in 2:length(tulare80)){
	d <- MannKendall(tulare80[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[i]]$raw$site_no[[1]]
	d$first_W_year <- tulare80[[i]]$Winter_6mon$W$Data$Date[[1]]
	d$last_W_year <- tulare80[[i]]$Winter_6mon$W$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$tulare80$W6MON$W <- rbind(MKTdaily$tulare80$W6MON$W, d)
}

###############################################################
###############################################################
###############################################################

for(n in 1:6){
	if(n == 1){
		start <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
	} else if(n==2){
		start <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
	} else if(n==3){
		start <- as.Date(paste(as.numeric(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
	} else if(n==4){
		start <- as.Date(paste(as.numeric(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
	} else if(n==5){
		start <- as.Date(paste(as.numeric(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
	} else if(n==6){
		start <- as.Date(paste(as.numeric(as.character(tulare80[[1]]$Availability$yearly$year[[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(tulare80[[1]]$Availability$yearly$year[[tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
	}
	d <- MannKendall(tulare80[[1]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(tulare80[[1]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(tulare80[[1]]$Winter_monthly$All[[n]]$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[1]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare80$WMON[[n]]$All <- d
	for(i in 2:length(tulare80)){
		if(n == 1){
			start <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
		} else if(n==2){
			start <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
		} else if(n==3){
			start <- as.Date(paste(as.numeric(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
		} else if(n==4){
			start <- as.Date(paste(as.numeric(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
		} else if(n==5){
			start <- as.Date(paste(as.numeric(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
		} else if(n==6){
			start <- as.Date(paste(as.numeric(as.character(tulare80[[i]]$Availability$yearly$year[[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(tulare80[[i]]$Availability$yearly$year[[tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
		}
		d <- MannKendall(tulare80[[i]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(tulare80[[i]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(tulare80[[i]]$Winter_monthly$All[[n]]$Data$Date==end))])
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare80[[i]]$raw$site_no[[1]]
		d$start_date <- start
		d$end_date <- end
		d$gaps_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare80$WMON[[n]]$All <- rbind(MKTdaily$tulare80$WMON[[n]]$All, d)
	}
	
	d <- MannKendall(tulare80[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[1]]$raw$site_no[[1]]
	d$first_C_year <- tulare80[[1]]$Winter_monthly$C[[n]]$Data$Date[[1]]
	d$last_C_year <- tulare80[[1]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare80$WMON[[n]]$C <- d
	for(i in 2:length(tulare80)){
		d <- MannKendall(tulare80[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare80[[i]]$raw$site_no[[1]]
		d$first_C_year <- tulare80[[i]]$Winter_monthly$C[[n]]$Data$Date[[1]]
		d$last_C_year <- tulare80[[i]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare80$WMON[[n]]$C <- rbind(MKTdaily$tulare80$WMON[[n]]$C, d)
	}
	
	d <- MannKendall(tulare80[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[1]]$raw$site_no[[1]]
	d$first_D_year <- tulare80[[1]]$Winter_monthly$D[[n]]$Data$Date[[1]]
	d$last_D_year <- tulare80[[1]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare80$WMON[[n]]$D <- d
	for(i in 2:length(tulare80)){
		d <- MannKendall(tulare80[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare80[[i]]$raw$site_no[[1]]
		d$first_D_year <- tulare80[[i]]$Winter_monthly$D[[n]]$Data$Date[[1]]
		d$last_D_year <- tulare80[[i]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare80$WMON[[n]]$D <- rbind(MKTdaily$tulare80$WMON[[n]]$D, d)
	}
	
	d <- MannKendall(tulare80[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[1]]$raw$site_no[[1]]
	d$first_BN_year <- tulare80[[1]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
	d$last_BN_year <- tulare80[[1]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare80$WMON[[n]]$BN <- d
	for(i in 2:length(tulare80)){
		d <- MannKendall(tulare80[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare80[[i]]$raw$site_no[[1]]
		d$first_BN_year <- tulare80[[i]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
		d$last_BN_year <- tulare80[[i]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare80$WMON[[n]]$BN <- rbind(MKTdaily$tulare80$WMON[[n]]$BN, d)
	}
	
	d <- MannKendall(tulare80[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[1]]$raw$site_no[[1]]
	d$first_AN_year <- tulare80[[1]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
	d$last_AN_year <- tulare80[[1]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare80$WMON[[n]]$AN <- d
	for(i in 2:length(tulare80)){
		d <- MannKendall(tulare80[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare80[[i]]$raw$site_no[[1]]
		d$first_AN_year <- tulare80[[i]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
		d$last_AN_year <- tulare80[[i]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare80$WMON[[n]]$AN <- rbind(MKTdaily$tulare80$WMON[[n]]$AN, d)
	}
	
	d <- MannKendall(tulare80[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- tulare80[[1]]$raw$site_no[[1]]
	d$first_W_year <- tulare80[[1]]$Winter_monthly$W[[n]]$Data$Date[[1]]
	d$last_W_year <- tulare80[[1]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(tulare80[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(tulare80[[1]]$Availability$yearly$fraction_available[head(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$tulare80$WMON[[n]]$W <- d
	for(i in 2:length(tulare80)){
		d <- MannKendall(tulare80[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- tulare80[[i]]$raw$site_no[[1]]
		d$first_W_year <- tulare80[[i]]$Winter_monthly$W[[n]]$Data$Date[[1]]
		d$last_W_year <- tulare80[[i]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(tulare80[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(tulare80[[i]]$Availability$yearly$fraction_available[head(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(tulare80[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$tulare80$WMON[[n]]$W <- rbind(MKTdaily$tulare80$WMON[[n]]$W, d)
	}
}

##################################################################





MKTdaily$unimpaired <- vector("list",4)
names(MKTdaily$unimpaired) <- c("HY","W3MON","W6MON", "WMON")
MKTdaily$unimpaired$HY <- vector("list", 6)
names(MKTdaily$unimpaired$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$W3MON <- vector("list", 6)
names(MKTdaily$unimpaired$W3MON) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$HY <- vector("list", 6)
names(MKTdaily$unimpaired$HY) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$W6MON <- vector("list", 6)
names(MKTdaily$unimpaired$W6MON) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$WMON <- vector("list", 6)
names(MKTdaily$unimpaired$WMON) <- c("NOV","DEC","JAN","FEB","MAR","APR")
MKTdaily$unimpaired$WMON$NOV <- vector("list", 6)
names(MKTdaily$unimpaired$WMON$NOV) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$WMON$DEC <- vector("list", 6)
names(MKTdaily$unimpaired$WMON$DEC) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$WMON$JAN <- vector("list", 6)
names(MKTdaily$unimpaired$WMON$JAN) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$WMON$FEB <- vector("list", 6)
names(MKTdaily$unimpaired$WMON$FEB) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$WMON$MAR <- vector("list", 6)
names(MKTdaily$unimpaired$WMON$MAR) <- c("All","C","D","BN","AN","W")
MKTdaily$unimpaired$WMON$APR <- vector("list", 6)
names(MKTdaily$unimpaired$WMON$APR) <- c("All","C","D","BN","AN","W")

start <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(unimpaired[[1]]$HydroYear$All$Data$Discharge_acfte6_day[(which(unimpaired[[1]]$HydroYear$All$Data$Date==start)):(which(unimpaired[[1]]$HydroYear$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$HY$All <- d
for(i in 2:length(unimpaired)){
	start <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"10-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"09-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(unimpaired[[i]]$HydroYear$All$Data$Discharge_acfte6_day[(which(unimpaired[[i]]$HydroYear$All$Data$Date==start)):(which(unimpaired[[i]]$HydroYear$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$HY$All <- rbind(MKTdaily$unimpaired$HY$All, d)
}

d <- MannKendall(unimpaired[[1]]$HydroYear$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_C_year <- unimpaired[[1]]$HydroYear$C$Data$Date[[1]]
d$last_C_year <- unimpaired[[1]]$HydroYear$C$Data$Date[[tail(which(is.na(unimpaired[[1]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$HY$C <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$HydroYear$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_C_year <- unimpaired[[i]]$HydroYear$C$Data$Date[[1]]
	d$last_C_year <- unimpaired[[i]]$HydroYear$C$Data$Date[[tail(which(is.na(unimpaired[[i]]$HydroYear$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$HY$C <- rbind(MKTdaily$unimpaired$HY$C, d)
}

d <- MannKendall(unimpaired[[1]]$HydroYear$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_D_year <- unimpaired[[1]]$HydroYear$D$Data$Date[[1]]
d$last_D_year <- unimpaired[[1]]$HydroYear$D$Data$Date[[tail(which(is.na(unimpaired[[1]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$HY$D <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$HydroYear$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_D_year <- unimpaired[[i]]$HydroYear$D$Data$Date[[1]]
	d$last_D_year <- unimpaired[[i]]$HydroYear$D$Data$Date[[tail(which(is.na(unimpaired[[i]]$HydroYear$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$HY$D <- rbind(MKTdaily$unimpaired$HY$D, d)
}

d <- MannKendall(unimpaired[[1]]$HydroYear$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_BN_year <- unimpaired[[1]]$HydroYear$BN$Data$Date[[1]]
d$last_BN_year <- unimpaired[[1]]$HydroYear$BN$Data$Date[[tail(which(is.na(unimpaired[[1]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$HY$BN <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$HydroYear$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_BN_year <- unimpaired[[i]]$HydroYear$BN$Data$Date[[1]]
	d$last_BN_year <- unimpaired[[i]]$HydroYear$BN$Data$Date[[tail(which(is.na(unimpaired[[i]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$HY$BN <- rbind(MKTdaily$unimpaired$HY$BN, d)
}

d <- MannKendall(unimpaired[[1]]$HydroYear$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_AN_year <- unimpaired[[1]]$HydroYear$AN$Data$Date[[1]]
d$last_AN_year <- unimpaired[[1]]$HydroYear$AN$Data$Date[[tail(which(is.na(unimpaired[[1]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$HY$AN <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$HydroYear$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_AN_year <- unimpaired[[i]]$HydroYear$AN$Data$Date[[1]]
	d$last_AN_year <- unimpaired[[i]]$HydroYear$AN$Data$Date[[tail(which(is.na(unimpaired[[i]]$HydroYear$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$HY$AN <- rbind(MKTdaily$unimpaired$HY$AN, d)
}

d <- MannKendall(unimpaired[[1]]$HydroYear$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_W_year <- unimpaired[[1]]$HydroYear$W$Data$Date[[1]]
d$last_W_year <- unimpaired[[1]]$HydroYear$W$Data$Date[[tail(which(is.na(unimpaired[[1]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$HY$W <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$HydroYear$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_W_year <- unimpaired[[i]]$HydroYear$W$Data$Date[[1]]
	d$last_W_year <- unimpaired[[i]]$HydroYear$W$Data$Date[[tail(which(is.na(unimpaired[[i]]$HydroYear$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$HY$W <- rbind(MKTdaily$unimpaired$HY$W, d)
}


###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
d <- MannKendall(unimpaired[[1]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(unimpaired[[1]]$Winter_3mon$All$Data$Date==start)):(which(unimpaired[[1]]$Winter_3mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W3MON$All <- d
for(i in 2:length(unimpaired)){
	start <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"02-28",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(unimpaired[[i]]$Winter_3mon$All$Data$Discharge_acfte6_day[(which(unimpaired[[i]]$Winter_3mon$All$Data$Date==start)):(which(unimpaired[[i]]$Winter_3mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W3MON$All <- rbind(MKTdaily$unimpaired$W3MON$All, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_C_year <- unimpaired[[1]]$Winter_3mon$C$Data$Date[[1]]
d$last_C_year <- unimpaired[[1]]$Winter_3mon$C$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W3MON$C <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_C_year <- unimpaired[[i]]$Winter_3mon$C$Data$Date[[1]]
	d$last_C_year <- unimpaired[[i]]$Winter_3mon$C$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_3mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W3MON$C <- rbind(MKTdaily$unimpaired$W3MON$C, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_D_year <- unimpaired[[1]]$Winter_3mon$D$Data$Date[[1]]
d$last_D_year <- unimpaired[[1]]$Winter_3mon$D$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W3MON$D <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_D_year <- unimpaired[[i]]$Winter_3mon$D$Data$Date[[1]]
	d$last_D_year <- unimpaired[[i]]$Winter_3mon$D$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_3mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W3MON$D <- rbind(MKTdaily$unimpaired$W3MON$D, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_BN_year <- unimpaired[[1]]$Winter_3mon$BN$Data$Date[[1]]
d$last_BN_year <- unimpaired[[1]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W3MON$BN <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_BN_year <- unimpaired[[i]]$Winter_3mon$BN$Data$Date[[1]]
	d$last_BN_year <- unimpaired[[i]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W3MON$BN <- rbind(MKTdaily$unimpaired$W3MON$BN, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_AN_year <- unimpaired[[1]]$Winter_3mon$AN$Data$Date[[1]]
d$last_AN_year <- unimpaired[[1]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W3MON$AN <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_AN_year <- unimpaired[[i]]$Winter_3mon$AN$Data$Date[[1]]
	d$last_AN_year <- unimpaired[[i]]$Winter_3mon$AN$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_3mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W3MON$AN <- rbind(MKTdaily$unimpaired$W3MON$AN, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_W_year <- unimpaired[[1]]$Winter_3mon$W$Data$Date[[1]]
d$last_W_year <- unimpaired[[1]]$Winter_3mon$W$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W3MON$W <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_W_year <- unimpaired[[i]]$Winter_3mon$W$Data$Date[[1]]
	d$last_W_year <- unimpaired[[i]]$Winter_3mon$W$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_3mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W3MON$W <- rbind(MKTdaily$unimpaired$W3MON$W, d)
}

###############################################################
###############################################################
###############################################################


start <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
end  <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
d <- MannKendall(unimpaired[[1]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(unimpaired[[1]]$Winter_6mon$All$Data$Date==start)):(which(unimpaired[[1]]$Winter_6mon$All$Data$Date==end))])
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$start_date <- start
d$end_date <- end
d$gaps_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W6MON$All <- d
for(i in 2:length(unimpaired)){
	start <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
	end  <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"04-30",sep="-"), format="%Y-%m-%d")
	d <- MannKendall(unimpaired[[i]]$Winter_6mon$All$Data$Discharge_acfte6_day[(which(unimpaired[[i]]$Winter_6mon$All$Data$Date==start)):(which(unimpaired[[i]]$Winter_6mon$All$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W6MON$All <- rbind(MKTdaily$unimpaired$W6MON$All, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_C_year <- unimpaired[[1]]$Winter_6mon$C$Data$Date[[1]]
d$last_C_year <- unimpaired[[1]]$Winter_6mon$C$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W6MON$C <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_C_year <- unimpaired[[i]]$Winter_6mon$C$Data$Date[[1]]
	d$last_C_year <- unimpaired[[i]]$Winter_6mon$C$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_6mon$C$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W6MON$C <- rbind(MKTdaily$unimpaired$W6MON$C, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_D_year <- unimpaired[[1]]$Winter_6mon$D$Data$Date[[1]]
d$last_D_year <- unimpaired[[1]]$Winter_6mon$D$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W6MON$D <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_D_year <- unimpaired[[i]]$Winter_6mon$D$Data$Date[[1]]
	d$last_D_year <- unimpaired[[i]]$Winter_6mon$D$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_6mon$D$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W6MON$D <- rbind(MKTdaily$unimpaired$W6MON$D, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_BN_year <- unimpaired[[1]]$Winter_6mon$BN$Data$Date[[1]]
d$last_BN_year <- unimpaired[[1]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W6MON$BN <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_BN_year <- unimpaired[[i]]$Winter_6mon$BN$Data$Date[[1]]
	d$last_BN_year <- unimpaired[[i]]$Winter_6mon$BN$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_6mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W6MON$BN <- rbind(MKTdaily$unimpaired$W6MON$BN, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_AN_year <- unimpaired[[1]]$Winter_6mon$AN$Data$Date[[1]]
d$last_AN_year <- unimpaired[[1]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W6MON$AN <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_AN_year <- unimpaired[[i]]$Winter_6mon$AN$Data$Date[[1]]
	d$last_AN_year <- unimpaired[[i]]$Winter_6mon$AN$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_6mon$AN$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W6MON$AN <- rbind(MKTdaily$unimpaired$W6MON$AN, d)
}

d <- MannKendall(unimpaired[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
d$first_W_year <- unimpaired[[1]]$Winter_6mon$W$Data$Date[[1]]
d$last_W_year <- unimpaired[[1]]$Winter_6mon$W$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$unimpaired$W6MON$W <- d
for(i in 2:length(unimpaired)){
	d <- MannKendall(unimpaired[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
	d$first_W_year <- unimpaired[[i]]$Winter_6mon$W$Data$Date[[1]]
	d$last_W_year <- unimpaired[[i]]$Winter_6mon$W$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_6mon$W$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
	
	MKTdaily$unimpaired$W6MON$W <- rbind(MKTdaily$unimpaired$W6MON$W, d)
}

###############################################################
###############################################################
###############################################################

for(n in 1:6){
	if(n == 1){
		start <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
	} else if(n==2){
		start <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
	} else if(n==3){
		start <- as.Date(paste(as.numeric(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
	} else if(n==4){
		start <- as.Date(paste(as.numeric(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
	} else if(n==5){
		start <- as.Date(paste(as.numeric(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
	} else if(n==6){
		start <- as.Date(paste(as.numeric(as.character(unimpaired[[1]]$Availability$yearly$year[[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
		end  <- as.Date(paste(as.numeric(as.character(unimpaired[[1]]$Availability$yearly$year[[tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
	}
	d <- MannKendall(unimpaired[[1]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(unimpaired[[1]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(unimpaired[[1]]$Winter_monthly$All[[n]]$Data$Date==end))])
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
	d$start_date <- start
	d$end_date <- end
	d$gaps_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$unimpaired$WMON[[n]]$All <- d
	for(i in 2:length(unimpaired)){
		if(n == 1){
			start <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"11-30",sep="-"), format="%Y-%m-%d")
		} else if(n==2){
			start <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]),"12-31",sep="-"), format="%Y-%m-%d")
		} else if(n==3){
			start <- as.Date(paste(as.numeric(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"01-31",sep="-"), format="%Y-%m-%d")
		} else if(n==4){
			start <- as.Date(paste(as.numeric(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"02-28",sep="-"), format="%Y-%m-%d")
		} else if(n==5){
			start <- as.Date(paste(as.numeric(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"03-31",sep="-"), format="%Y-%m-%d")
		} else if(n==6){
			start <- as.Date(paste(as.numeric(as.character(unimpaired[[i]]$Availability$yearly$year[[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-01",sep="-"), format="%Y-%m-%d")
			end  <- as.Date(paste(as.numeric(as.character(unimpaired[[i]]$Availability$yearly$year[[tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)]]))+1,"04-30",sep="-"), format="%Y-%m-%d")
		}
		d <- MannKendall(unimpaired[[i]]$Winter_monthly$All[[n]]$Data$Discharge_acfte6_day[(which(unimpaired[[i]]$Winter_monthly$All[[n]]$Data$Date==start)):(which(unimpaired[[i]]$Winter_monthly$All[[n]]$Data$Date==end))])
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
		d$start_date <- start
		d$end_date <- end
		d$gaps_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$unimpaired$WMON[[n]]$All <- rbind(MKTdaily$unimpaired$WMON[[n]]$All, d)
	}
	
	d <- MannKendall(unimpaired[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
	d$first_C_year <- unimpaired[[1]]$Winter_monthly$C[[n]]$Data$Date[[1]]
	d$last_C_year <- unimpaired[[1]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$unimpaired$WMON[[n]]$C <- d
	for(i in 2:length(unimpaired)){
		d <- MannKendall(unimpaired[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
		d$first_C_year <- unimpaired[[i]]$Winter_monthly$C[[n]]$Data$Date[[1]]
		d$last_C_year <- unimpaired[[i]]$Winter_monthly$C[[n]]$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_monthly$C[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$unimpaired$WMON[[n]]$C <- rbind(MKTdaily$unimpaired$WMON[[n]]$C, d)
	}
	
	d <- MannKendall(unimpaired[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
	d$first_D_year <- unimpaired[[1]]$Winter_monthly$D[[n]]$Data$Date[[1]]
	d$last_D_year <- unimpaired[[1]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$unimpaired$WMON[[n]]$D <- d
	for(i in 2:length(unimpaired)){
		d <- MannKendall(unimpaired[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
		d$first_D_year <- unimpaired[[i]]$Winter_monthly$D[[n]]$Data$Date[[1]]
		d$last_D_year <- unimpaired[[i]]$Winter_monthly$D[[n]]$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_monthly$D[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$unimpaired$WMON[[n]]$D <- rbind(MKTdaily$unimpaired$WMON[[n]]$D, d)
	}
	
	d <- MannKendall(unimpaired[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
	d$first_BN_year <- unimpaired[[1]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
	d$last_BN_year <- unimpaired[[1]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$unimpaired$WMON[[n]]$BN <- d
	for(i in 2:length(unimpaired)){
		d <- MannKendall(unimpaired[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
		d$first_BN_year <- unimpaired[[i]]$Winter_monthly$BN[[n]]$Data$Date[[1]]
		d$last_BN_year <- unimpaired[[i]]$Winter_monthly$BN[[n]]$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_monthly$BN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$unimpaired$WMON[[n]]$BN <- rbind(MKTdaily$unimpaired$WMON[[n]]$BN, d)
	}
	
	d <- MannKendall(unimpaired[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
	d$first_AN_year <- unimpaired[[1]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
	d$last_AN_year <- unimpaired[[1]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$unimpaired$WMON[[n]]$AN <- d
	for(i in 2:length(unimpaired)){
		d <- MannKendall(unimpaired[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
		d$first_AN_year <- unimpaired[[i]]$Winter_monthly$AN[[n]]$Data$Date[[1]]
		d$last_AN_year <- unimpaired[[i]]$Winter_monthly$AN[[n]]$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_monthly$AN[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$unimpaired$WMON[[n]]$AN <- rbind(MKTdaily$unimpaired$WMON[[n]]$AN, d)
	}
	
	d <- MannKendall(unimpaired[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- unimpaired[[1]]$raw$site_no[[1]]
	d$first_W_year <- unimpaired[[1]]$Winter_monthly$W[[n]]$Data$Date[[1]]
	d$last_W_year <- unimpaired[[1]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(unimpaired[[1]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
	d$missing_years_possible <- if(any(unimpaired[[1]]$Availability$yearly$fraction_available[head(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"
	
	MKTdaily$unimpaired$WMON[[n]]$W <- d
	for(i in 2:length(unimpaired)){
		d <- MannKendall(unimpaired[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day)
		class(d) <- "list"
		d <- as.data.frame(d)
		d$gauge <- unimpaired[[i]]$raw$site_no[[1]]
		d$first_W_year <- unimpaired[[i]]$Winter_monthly$W[[n]]$Data$Date[[1]]
		d$last_W_year <- unimpaired[[i]]$Winter_monthly$W[[n]]$Data$Date[[tail(which(is.na(unimpaired[[i]]$Winter_monthly$W[[n]]$Data$Discharge_acfte6_day) == FALSE),1)]]
		d$missing_years_possible <- if(any(unimpaired[[i]]$Availability$yearly$fraction_available[head(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1):tail(which(unimpaired[[i]]$Availability$yearly$fraction_available == 1),1)<1])) "Y" else "N"
		
		MKTdaily$unimpaired$WMON[[n]]$W <- rbind(MKTdaily$unimpaired$WMON[[n]]$W, d)
	}
}