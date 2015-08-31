# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


MKTdaily <- vector("list",6)
names(MKTdaily) <- c("active","saclower","sacupper","SJ", "tulare60", "tulare80")
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

d <- MannKendall(active[[1]]$HydroYear$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_D_year <- active[[1]]$HydroYear$BN$Data$Date[[1]]
d$last_D_year <- active[[1]]$HydroYear$BN$Data$Date[[tail(which(is.na(active[[1]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$HY$D <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$HydroYear$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_D_year <- active[[i]]$HydroYear$BN$Data$Date[[1]]
	d$last_D_year <- active[[i]]$HydroYear$BN$Data$Date[[tail(which(is.na(active[[i]]$HydroYear$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
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

d <- MannKendall(active[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
class(d) <- "list"
d <- as.data.frame(d)
d$gauge <- active[[1]]$raw$site_no[[1]]
d$first_D_year <- active[[1]]$Winter_3mon$BN$Data$Date[[1]]
d$last_D_year <- active[[1]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(active[[1]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
d$missing_years_possible <- if(any(active[[1]]$Availability$yearly$fraction_available[head(which(active[[1]]$Availability$yearly$fraction_available == 1),1):tail(which(active[[1]]$Availability$yearly$fraction_available == 1),1)]<1)) "Y" else "N"

MKTdaily$active$W3MON$D <- d
for(i in 2:length(active)){
	d <- MannKendall(active[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day)
	class(d) <- "list"
	d <- as.data.frame(d)
	d$gauge <- active[[i]]$raw$site_no[[1]]
	d$first_D_year <- active[[i]]$Winter_3mon$BN$Data$Date[[1]]
	d$last_D_year <- active[[i]]$Winter_3mon$BN$Data$Date[[tail(which(is.na(active[[i]]$Winter_3mon$BN$Data$Discharge_acfte6_day) == FALSE),1)]]
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

