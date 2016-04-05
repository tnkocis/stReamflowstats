# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################
unimp_13 <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\umimpaired_13.txt",header=TRUE)
unimp_13 <- unimp_13$Value
unimpaired_13 <- vector("list",length(unimp_13))
names(unimpaired_13) <- unimp_13
unimpaired_13_split <- vector("list",length(unimp_13))
names(unimpaired_13_split) <- unimp_13

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
	if(any(names(spbatch)%in%unimp_13)){
		un <- which(names(spbatch)%in%unimp_13)
		for(i in 1:length(un)){
			position <- which(names(unimpaired_13)==names(spbatch)[[un[[i]]]])
			unimpaired_13[[position]] <- spbatch[[un[[i]]]]
			unimpaired_13_split[[position]] <-test_split[[un[[i]]]]
		}
	}
}

unimpaired_13_data <- unimpaired_13
unimpaired_13_split_data <- unimpaired_13_split


for(i in 1:length(unimpaired_13_split_data)){ #gauge
	for(j in 1:1){ #yeartype  length(unimpaired_13_split_data[[i]])
		for(k in 1:length(unimpaired_13_split_data[[i]][[j]])){ #plotperiod
			if(length(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft[which(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft!=0)])==0) next
			
			test_n0volabv <-unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft[which(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft!=0)]
			test_n0daysabv <- unimpaired_13_split_data[[i]][[j]][[k]]$TotDaysAbv[which(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft!=0)]
			test_n0meanpeakflow <- unimpaired_13_split_data[[i]][[j]][[k]]$mean_peakflow[which(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft!=0)]
			test_n0numpeaks <- unimpaired_13_split_data[[i]][[j]][[k]]$numpeaks[which(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft!=0)]
			
			plotperiod <- names(unimpaired_13_split_data[[i]][[j]])[[k]]
			gauge <- names(unimpaired_13_split_data)[[i]]
			yeartype <- names(unimpaired_13_split_data[[i]])[[j]]
			
			n0abv <- data.frame(volAF=test_n0volabv,daysabv=test_n0daysabv,
					meanpkflw=test_n0meanpeakflow, numpeaks=test_n0numpeaks,
					index=seq(1,length(test_n0volabv),1))
			if(length(n0abv$volAF)<3)next
			MKTvol <- MannKendall(n0abv$volAF)
			MKTtauvol <-round(MKTvol[[1]][[1]],4)
			MKTpvol <- round(MKTvol[[2]][[1]],4)
			MKTdays <- MannKendall(n0abv$daysabv)
			MKTtaudays <-round(MKTdays[[1]][[1]],4)
			MKTpdays <- round(MKTdays[[2]][[1]],4)
			MKTmnpks <- MannKendall(n0abv$meanpkflw)
			MKTtaumnpks <-round(MKTmnpks[[1]][[1]],4)
			MKTpmnpks <- round(MKTmnpks[[2]][[1]],4)
			MKTnumpks <- MannKendall(n0abv$numpeaks)
			MKTtaunumpks <-round(MKTnumpks[[1]][[1]],4)
			MKTpnumpks <- round(MKTnumpks[[2]][[1]],4)
			lmvol <- lm(volAF ~ index, data=n0abv,na.action=na.exclude)
			n0abv$lmvol <- lmvol$fitted
			lmdays <- lm(daysabv ~ index, data=n0abv,na.action=na.exclude)
			n0abv$lmdays <- lmdays$fitted
			lmmnpks <- lm(meanpkflw ~ index, data=n0abv,na.action=na.exclude)
			if(length(lmmnpks$fitted)!=length(n0abv$meanpkflw)){
				n <- length(n0abv$meanpkflw)-length(lmmnpks$fitted)
				n0abv$lmmnpks <- c(lmmnpks$fitted, rep(NA,n))
			} else {
				n0abv$lmmnpks <- lmmnpks$fitted
			}
			lmnumpks <- lm(numpeaks ~ index, data=n0abv,na.action=na.exclude)
			n0abv$lmnumpks <- lmnumpks$fitted
			plotvol <- ggplot(n0abv,aes(x=index,y=volAF))+geom_line(color="deepskyblue4")+ geom_line(aes(index,lmvol))+
					labs(title=paste(
									"Volume Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nMKT tau=",
									MKTtauvol," MKT p =",MKTpvol, sep=""),y="Volume Above 90% (ACFT)",
							x="time proxy")
			plotdays <- ggplot(n0abv,aes(x=index,y=daysabv))+geom_line(color="deepskyblue4")+geom_line(aes(index,lmdays))+
					labs(title=paste(
									"Number of Days Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nMKT tau=",
									MKTtaudays," MKT p =",MKTpdays, sep=""),y="Number of Days Above 90%",
							x="time proxy")
			plotmnpks <- ggplot(n0abv,aes(x=index,y=meanpkflw))+geom_line(color="deepskyblue4")+geom_line(aes(index,lmmnpks))+
					labs(title=paste(
									"Mean Peakflow Event Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nMKT tau=",
									MKTtaumnpks," MKT p =",MKTpmnpks, sep=""),y="Mean Peakflow Event Above 90%",
							x="time proxy")
			plotnumpks <- ggplot(n0abv,aes(x=index,y=numpeaks))+geom_line(color="deepskyblue4")+geom_line(aes(index,lmnumpks))+
					labs(title=paste(
									"Number of Peak Events Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nMKT tau=",
									MKTtaunumpks," MKT p =",MKTpnumpks, sep=""),y="Number of Peak Events  Above 90%",
							x="time proxy")
			ggsave(paste("C:\\Users\\tiffn_000\\Documents\\unimpaired_13\\all_yeartype_plots\\","vol_", plotperiod,"_",gauge,"_",yeartype,".png",sep=""),
					plotvol,
					width=11,height=8.5,units="in")
			ggsave(paste("C:\\Users\\tiffn_000\\Documents\\unimpaired_13\\all_yeartype_plots\\","days_", plotperiod,"_",gauge,"_",yeartype,".png",sep=""),
					plotdays,
					width=11,height=8.5,units="in")
			ggsave(paste("C:\\Users\\tiffn_000\\Documents\\unimpaired_13\\all_yeartype_plots\\","mnpks_", plotperiod,"_",gauge,"_",yeartype,".png",sep=""),
					plotmnpks,
					width=11,height=8.5,units="in")
			ggsave(paste("C:\\Users\\tiffn_000\\Documents\\unimpaired_13\\all_yeartype_plots\\","numpks_", plotperiod,"_",gauge,"_",yeartype,".png",sep=""),
					plotnumpks,
					width=11,height=8.5,units="in")
#			if(all(cs0vol)==0){
				cs0vol <- cumsum(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft==0)
				loessfit <- loess(cs0vol~unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear,na.action=na.exclude)
				loesspred <- predict(loessfit,unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear)
				loesspred_diff <- diff(loesspred)
				loessfitted <- data.frame(sthyyear=unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear,
						loesspred=loesspred, loess_diff=c(loesspred_diff,NA))
				loess_pred_lmplot <- lm(loesspred ~ sthyyear, data=loessfitted,na.action=na.exclude)
#				loess_pred_lm <- as.data.frame(summary(loess_pred_lmplot)$coeff)
				MKTpred <- MannKendall(loessfitted$loesspred)
				MKTpredtau <- round(MKTpred[[1]][[1]],4)
				MKTpredp <- round(MKTpred[[2]][[1]],4)
#				loess_pred_slope <-round(loess_pred_lm$Estimate[[2]],4)
#				loess_pred_p <-round(loess_pred_lm$`Pr(>|t|)`[[2]],4)
				loess_diff_lmplot <- lm(loess_diff ~ sthyyear,data=loessfitted,na.action=na.exclude)
				MKTdiff <- MannKendall(loessfitted$loess_diff)
				MKTdifftau <- round(MKTdiff[[1]][[1]],4)
				MKTdiffp <- round(MKTdiff[[2]][[1]],4)
#				loess_diff_lm <- as.data.frame(summary(loess_diff_lmplot)$coeff)
#				loess_diff_slope <-round(loess_diff_lm$Estimate[[2]],4)
#				loess_diff_p <-round(loess_diff_lm$`Pr(>|t|)`[[2]],4)
				loessfitted$lmdifffitted <- c(loess_diff_lmplot$fitted,NA)
				loessfitted$lmpred <- loess_pred_lmplot$fitted
				loess_pred_plot <- ggplot(loessfitted,aes(sthyyear,loesspred)) +geom_line(color="darkviolet")+ 
						geom_line(aes(sthyyear,lmpred))+
						labs(title=paste(
										"Accumulation of Zero Flow Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nMKT slope=",
										MKTpredtau," MKT p =",MKTpredp, sep=""),y="Number of Zero Flow Above 90% Years to Date",
								x="Time") 
				loess_diff_plot <- ggplot(loessfitted,aes(sthyyear,loess_diff)) +geom_line(color="magenta4") +
						geom_line(aes(sthyyear,lmdifffitted))+
						labs(title=paste(
										"Derivative of Accumulation of Zero Flow Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nMKT slope=",
										MKTdifftau," MKT p =",MKTdiffp, sep=""),y="Slope Between Accumulatio of Zero Flow Above 90% Years to Date",
								x="Time") 
				ggsave(paste("C:\\Users\\tiffn_000\\Documents\\unimpaired_13\\all_yeartype_plots\\","accum_zero_", plotperiod,"_",gauge,"_",yeartype,".png",sep=""),
						loess_pred_plot,
						width=11,height=8.5,units="in")
				ggsave(paste("C:\\Users\\tiffn_000\\Documents\\unimpaired_13\\all_yeartype_plots\\","accum_zero_slope_", plotperiod,"_",gauge,"_",yeartype,".png",sep=""),
						loess_diff_plot,
						width=11,height=8.5,units="in")
#			} else {
#				cs0vol <- cumsum(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft==0)
#				loessfit <- loess(cs0vol~unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear,na.action=na.exclude)
#				loesspred <- predict(loessfit,unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear)
#				loesspred_diff <- diff(loesspred)
#				loessfitted <- data.frame(sthyyear=unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear,
#						loesspred=loesspred, loess_diff=c(loesspred_diff,NA))
#				loess_pred_glsplot <- gls(loesspred ~ sthyyear, data=loessfitted,method="ML",na.action=na.exclude)
#				loess_pred_gls <- as.data.frame(summary(loess_pred_glsplot)$tTable)
#				loess_pred_slope <-round(loess_pred_gls$Value[[2]],4)
#				loess_pred_p <-round(loess_pred_gls$`p-value`[[2]],4)
#				loess_diff_glsplot <- gls(loess_diff ~ sthyyear,data=loessfitted, method="ML",na.action=na.exclude)
#				loess_diff_gls <- as.data.frame(summary(loess_diff_glsplot)$tTable)
#				loess_diff_slope <-round(loess_diff_gls$Value[[2]],4)
#				loess_diff_p <-round(loess_diff_gls$`p-value`[[2]],4)
#				loessfitted$glsdifffitted <- c(loess_diff_glsplot$fitted,NA)
#				loessfitted$glspred <- loess_pred_glsplot$fitted
#				loess_pred_plot <- ggplot(loessfitted,aes(x,loesspred)) +geom_line(color="darkviolet")+ geom_line(aes(x,glspred))+
#						labs(title=paste(
#										"Accumulation of Zero Flow Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nGLS slope=",
#										loess_pred_slope," GLS p =",loess_pred_p, sep=""),y="Number of Zero Flow Above 90% Years to Date",
#								x="Time") 
#				loess_diff_plot <- ggplot(loessfitted,aes(x,loess_diff)) +geom_line(color="magenta4") + geom_line(aes(x,glsdifffitted))+
#						labs(title=paste(
#										"Derivative of Accumulation of Zero Flow Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nGLS slope=",
#										loess_diff_slope," GLS p =",loess_diff_p, sep=""),y="Slope Between Accumulatio of Zero Flow Above 90% Years to Date",
#								x="Time") 
#				ggsave(paste("C:\\Users\\tiffn_000\\Google Drive\\unimpaired_13\\n0plots\\","accum_zero_", plotperiod,"_",gauge,"_",yeartype,".png",sep=""),
#						loess_pred_plot,
#						width=11,height=8.5,units="in")
#				ggsave(paste("C:\\Users\\tiffn_000\\Google Drive\\unimpaired_13\\n0plots\\","accum_zero_slope_", plotperiod,"_",gauge,"_",yeartype,".png",sep=""),
#						loess_diff_plot,
#						width=11,height=8.5,units="in")
#			}
		}
	}
}




two_gauges_zero <- c(11447650,11303500)
two_gauges_list_zero  <- vector("list",length(two_gauges_zero))
names(two_gauges_list_zero) <- two_gauges_zero
two_gauges_split_zero  <- vector("list",length(two_gauges_zero))
names(two_gauges_split_zero) <- two_gauges_zero

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\zero_threshold_spbatch_",batchnum,".RData", sep=""))
	if(any(names(spbatch)%in%two_gauges_zero)){
		un <- which(names(spbatch)%in%two_gauges_zero)
		for(i in 1:length(un)){
			position <- which(names(two_gauges_list_zero)==names(spbatch)[[un[[i]]]])
			two_gauges_list_zero[[position]] <- spbatch[[un[[i]]]]
			two_gauges_split_zero[[position]] <-test_split[[un[[i]]]]
		}
	}
}

load("C:\\Users\\tiffn_000\\Documents\\workspaces\\mar_16_activesites.RData")

two_gauges_full90 <- c(11447650,11303500)
two_gauges_list_full90  <- vector("list",length(two_gauges_full90))
names(two_gauges_list_full90) <- two_gauges_full90
two_gauges_split_full90  <- vector("list",length(two_gauges_full90))
names(two_gauges_split_full90) <- two_gauges_full90

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
	if(any(names(spbatch)%in%two_gauges_full90)){
		un <- which(names(spbatch)%in%two_gauges_full90)
		for(i in 1:length(un)){
			position <- which(names(two_gauges_list_full90)==names(spbatch)[[un[[i]]]])
			two_gauges_list_full90[[position]] <- spbatch[[un[[i]]]]
			two_gauges_split_full90[[position]] <-test_split[[un[[i]]]]
		}
	}
}

load("C:\\Users\\tiffn_000\\Documents\\workspaces\\mar_16_activesites.RData")


for(i in 1:length(unimpaired_13_split_data)){ #gauge
	for(j in 1:1){ #yeartype  length(unimpaired_13_split_data[[i]])
		for(k in 1:length(unimpaired_13_split_data[[i]][[j]])){ #plotperiod
cs0vol <- cumsum(unimpaired_13_split_data[[i]][[j]][[k]]$TotVolAbv_acft==0)
loessfit <- loess(cs0vol~unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear,na.action=na.exclude)
loesspred <- predict(loessfit,unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear)
loesspred_diff <- diff(loesspred)
loessfitted <- data.frame(sthyyear=unimpaired_13_split_data[[i]][[j]][[k]]$sthyyear,
		loesspred=loesspred, loess_diff=c(loesspred_diff,NA))

plotvol <- ggplot(n0abv,aes(x=index,y=volAF))+geom_line(color="deepskyblue4")+ geom_line(aes(index,lmvol))+
		labs(title=paste(
						"Volume Above 90% for ",plotperiod," at USGS ",gauge,"\n For ",yeartype," Year Types","\nMKT tau=",
						MKTtauvol," MKT p =",MKTpvol, sep=""),y="Volume Above 90% (ACFT)",
				x="time proxy")

COM_90 <- vector("list",length(unimpaired_13_data))
for(k in 1:length(unimpaired_13_data)){
	blahday <- rep(NA,length(unimpaired_13_data[[k]]$HydroYear$Data))
	blahyear <- rep(NA,length(unimpaired_13_data[[k]]$HydroYear$Data))
	for(i in 1:length(unimpaired_13_data[[k]]$HydroYear$Data)){
		blah <- unimpaired_13_data[[k]]$HydroYear$Data[[i]]$Discharge_acft_day
		blah[blah<(unimpaired_13_data[[k]]$thresholds_maf$P90maf*1e6)] <- 0
		blahday[[i]] <- which(cumsum(blah)>=(sum(blah,na.rm=TRUE)/2))[[1]]
		blahyear[[i]] <- as.numeric(format(unimpaired_13_data[[k]]$HydroYear$Data[[i]]$Date[[1]],"%Y"))
	}
	COM_90[[k]] <- data.frame(COMday=blahday,year=blahyear)
}

COM_90_df <- data.frame(year=seq(1900,2015,1))
for(i in 1:length(COM_90)){
	COM_90_df <- merge(COM_90_df,COM_90[[i]], by.x="year",by.y="year", all.x=TRUE)
}
names(COM_90_df) <- c("year",paste("COM",as.numeric(names(unimpaired_13_data)),sep=""))
COM_90_df[COM_90_df==1] <- NA
library(reshape2)
melt_COM_90 <- melt(COM_90_df, id.vars="year")
library(ggplot2)
ggplot(melt_COM_90,aes(year,value,color=variable))+geom_point() +scale_color_brewer(palette="Spectral")


COM_0 <- vector("list",length(unimpaired_13_data))
for(k in 1:length(unimpaired_13_data)){
	blahday <- rep(NA,length(unimpaired_13_data[[k]]$HydroYear$Data))
	blahyear <- rep(NA,length(unimpaired_13_data[[k]]$HydroYear$Data))
	for(i in 1:length(unimpaired_13_data[[k]]$HydroYear$Data)){
		blah <- unimpaired_13_data[[k]]$HydroYear$Data[[i]]$Discharge_acft_day
#		blah[blah<(unimpaired_13_data[[k]]$thresholds_maf$P0maf*1e6)] <- 0
		blahday[[i]] <- which(cumsum(blah)>=(sum(blah,na.rm=TRUE)/2))[[1]]
		blahyear[[i]] <- as.numeric(format(unimpaired_13_data[[k]]$HydroYear$Data[[i]]$Date[[1]],"%Y"))
	}
	COM_0[[k]] <- data.frame(COMday=blahday,year=blahyear)
}

COM_0_df <- data.frame(year=seq(1900,2015,1))
for(i in 1:length(COM_0)){
	COM_0_df <- merge(COM_0_df,COM_0[[i]], by.x="year",by.y="year", all.x=TRUE)
}
names(COM_0_df) <- c("year",paste("COM",as.numeric(names(unimpaired_13_data)),sep=""))
COM_0_df[COM_0_df==1|COM_0_df==365] <- NA
library(reshape2)
melt_COM_0 <- melt(COM_0_df, id.vars="year")
library(ggplot2)
ggplot(melt_COM_0,aes(year,value,color=variable))+geom_point()  
+geom_line(data=COM_0_df,aes(x=year,y=meanrw, color="blue"))

ggplot(COM_0_df, aes(year,COM11230500)) + geom_line()

COM_0_df$meanrw<- rowMeans(COM_0_df[2:14],na.rm=TRUE)
COM_0_df$meanrw[is.na(COM_0_df$meanrw)] <- NA

ats <- cpt.var(COM_0_df$COM11230500[!is.na(COM_0_df$COM11230500)],penalty="Asymptotic",pen.value=0.05,method="AMOC")
plot(COM_0_df$year[!is.na(COM_0_df$COM11237500)],rollmean(COM_0_df$COM11237500[!is.na(COM_0_df$COM11237500)],10,fill=NA),type="l")


#### threshold shift ###
quantroll <- function(x,threshold){
	num <- quantile(x,threshold,na.rm=TRUE)[[1]]
	return(num)
}

test_thresh_shift_25 <- rollapply(unimpaired_13_data$`11202001`$HydroYear$All$Data$Discharge_acfte6_day,
		width=365*20,FUN=quantroll,threshold=0.25,fill=NA)
test_thresh_mean <- rollapply(unimpaired_13_data$`11202001`$HydroYear$All$Data$Discharge_acfte6_day,
		width=365*20, function(x) mean(x, na.rm=TRUE), fill=NA)
test_thresh_var <- rollapply(unimpaired_13_data$`11202001`$HydroYear$All$Data$Discharge_acfte6_day,
		width=365*20, function(x) var(x, na.rm=TRUE), fill=NA)



###extract total volume time sereis###
all_totalvol_jan <- vector("list",93)
all_totalvol_feb <- vector("list",93)
all_totalvol_mar <- vector("list",93)
all_totalvol_apr <- vector("list",93)
all_totalvol_may <- vector("list",93)
all_totalvol_jun <- vector("list",93)
all_totalvol_jul <- vector("list",93)
all_totalvol_aug <- vector("list",93)
all_totalvol_sep <- vector("list",93)
all_totalvol_oct <- vector("list",93)
all_totalvol_nov <- vector("list",93)
all_totalvol_dec <- vector("list",93)

all_names <- rep(NA,93)
for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
		for(i in 1:length(test_split)){
			testnull <- which(sapply(all_totalvol_jan,is.null)==TRUE)[[1]]
			all_totalvol_jan[[testnull]] <- data.frame(stjanyear=test_split[[i]]$all$jan$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$jan$TotVolAbv_acft)
			all_totalvol_feb[[testnull]] <- data.frame(stfebyear=test_split[[i]]$all$feb$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$feb$TotVolAbv_acft)
			all_totalvol_mar[[testnull]] <- data.frame(stmaryear=test_split[[i]]$all$mar$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$mar$TotVolAbv_acft)
			all_totalvol_apr[[testnull]] <- data.frame(stapryear=test_split[[i]]$all$apr$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$apr$TotVolAbv_acft)
			all_totalvol_may[[testnull]] <- data.frame(stmayyear=test_split[[i]]$all$may$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$may$TotVolAbv_acft)
			all_totalvol_jun[[testnull]] <- data.frame(stjunyear=test_split[[i]]$all$jun$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$jun$TotVolAbv_acft)
			all_totalvol_jul[[testnull]] <- data.frame(stjulyear=test_split[[i]]$all$jul$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$jul$TotVolAbv_acft)
			all_totalvol_aug[[testnull]] <- data.frame(staugyear=test_split[[i]]$all$aug$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$aug$TotVolAbv_acft)
			all_totalvol_sep[[testnull]] <- data.frame(stsepyear=test_split[[i]]$all$sep$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$sep$TotVolAbv_acft)
			all_totalvol_oct[[testnull]] <- data.frame(stoctyear=test_split[[i]]$all$oct$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$oct$TotVolAbv_acft)
			all_totalvol_nov[[testnull]] <- data.frame(stnovyear=test_split[[i]]$all$nov$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$nov$TotVolAbv_acft)
			all_totalvol_dec[[testnull]] <- data.frame(stdecyear=test_split[[i]]$all$dec$sthyyear,
					TotVolAbv_acft=test_split[[i]]$all$dec$TotVolAbv_acft)
			all_names[[testnull]] <- names(test_split)[[i]]
		}
	}

totvol_90_hy <- data.frame(year=seq(1900,2015,1))
totvol_90_mon6 <- data.frame(year=seq(1900,2015,1))
totvol_90_mon3 <- data.frame(year=seq(1900,2015,1))

for(i in 1:length(all_totalvol_hy)){
	totvol_90_hy <- merge(totvol_90_hy,all_totalvol_hy[[i]],by.x="year")
}

unimpaired_13_data <- unimpaired_13
unimpaired_13_split_data <- unimpaired_13_split

library(nlme)
library(ggplot2)
library(Kendall)
statistic_tests <- names(unimpaired_13_split_data$`11202001`$all$hy)[1:5]
for(i in 1:length(unimpaired_13_split_data)){
	for(k in 1:3){
		gauge <- names(unimpaired_13_split_data)[[i]]
		period <- names(unimpaired_13_split_data[[i]]$all)[[k]]
		testplotdf <- unimpaired_13_split_data[[i]]$all[[k]]
		testplotdf[which(testplotdf$TotVolAbv_acft==0),!names(testplotdf)%in%c("sthyyear")] <- NA
		for(j in 1:length(statistic_tests)){
			if(statistic_tests[[j]]=="TotVolAbv_acft"){
				statlabel <- c("Total Volume Above 90% (acft)")
				title <- c("Zero-Deflated Volume Above")
			}else if(statistic_tests[[j]]=="TotDaysAbv"){
				statlabel <- c("Total Days Above 90%")
				title <- c("Zero-Deflated Days Above")
			}else if(statistic_tests[[j]]=="numpeaks"){
				statlabel <- c("Number of Peaks Above 90%")
				title <- c("Zero-Deflated Number of Peaks Above")
			}else if(statistic_tests[[j]]=="mean_peakflow"){
				statlabel <- c("Mean Magnitude of Peaks Above 90%")
				title <- c("Zero-Deflated Mean Magnitude of Peaks Above")
			}else if(statistic_tests[[j]]=="total_peakflow"){
				statlabel <- c("Total Magnitude of Peaks Above 90%")
				title <- c("Zero-Deflated Total Magnitude of Peaks Above")
			}
				vol_gls <- gls(as.formula(paste(statistic_tests[[j]]," ~ sthyyear",sep="")),testplotdf, method="ML", na.action=na.exclude)
				testplotdf$gls_fitted <- predict(vol_gls, data.frame(sthyyear=testplotdf$sthyyear))
				vol_loess <- loess(as.formula(paste(statistic_tests[[j]]," ~ sthyyear",sep="")),testplotdf, na.action=na.exclude)
				testplotdf$loess_fitted <- predict(vol_loess, data.frame(sthyyear=testplotdf$sthyyear))
				vol_3poly <- lm(as.formula(paste(statistic_tests[[j]]," ~ poly(sthyyear,3)",sep="")),testplotdf, na.action=na.exclude)
				testplotdf$poly3_fitted <- predict(vol_3poly, data.frame(sthyyear=testplotdf$sthyyear))
				vol_4poly <- lm(as.formula(paste(statistic_tests[[j]]," ~ poly(sthyyear,4)",sep="")),testplotdf, na.action=na.exclude)
				testplotdf$poly4_fitted <- predict(vol_4poly, data.frame(sthyyear=testplotdf$sthyyear))
				vol_5poly <- lm(as.formula(paste(statistic_tests[[j]]," ~ poly(sthyyear,5)",sep="")),testplotdf, na.action=na.exclude)
				testplotdf$poly5_fitted <- predict(vol_5poly, data.frame(sthyyear=testplotdf$sthyyear))
				vol_MKTtau <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$tau[[1]],3)
				vol_MKTp <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$sl[[1]],3)
				fitted_vol_plots <- ggplot(testplotdf,aes_string("sthyyear",statistic_tests[[j]]))+geom_point()+ geom_line(aes(sthyyear,gls_fitted,color="gls"))+
						geom_line(aes(sthyyear,loess_fitted,color="loess")) +geom_line(aes(sthyyear,poly3_fitted, color="poly3"))+
						geom_line(aes(sthyyear,poly4_fitted, color="poly4"))+
						geom_line(aes(sthyyear,poly5_fitted, color="poly5"))+
						scale_color_manual("Legend", labels=c("GLS","Loess","3rd Order Polynomial", "4th Order Polynomial", "5th Order Polynomial"),
								values=c(gls="blue", loess="red", poly3="green", poly4="purple", poly5="orange")) + xlab("Hydrologic Year (start year)")+
						ylab(paste(statlabel,sep="")) + ggtitle(paste(title," v Year (USGS",gauge,", ",period,")\nMKT tau= ",vol_MKTtau," p= ",vol_MKTp,sep=""))+
						theme(axis.text=element_text(size=12, face="bold"),
								axis.title=element_text(size=14,face="bold"),
								plot.title=element_text(size=16))
				ggsave(fitted_vol_plots, file=paste("C:\\Users\\tiffn_000\\Google Drive\\unimp_plots_3_28\\test_4_5\\",statistic_tests[[j]],"\\USGS",gauge,"_",period,".png",sep=""), height=8.5, width=11, units="in")
			
		}	
	}
}

unimpaired_13_zthr_data <- vector("list",length(unimp_13))
names(unimpaired_13_zthr_data) <- unimp_13
unimpaired_13_zthr_split_data <- vector("list",length(unimp_13))
names(unimpaired_13_zthr_split_data) <- unimp_13

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\zero_threshold_spbatch_",batchnum,".RData", sep=""))
	if(any(names(spbatch)%in%unimp_13)){
		un <- which(names(spbatch)%in%unimp_13)
		for(i in 1:length(un)){
			position <- which(names(unimpaired_13)==names(spbatch)[[un[[i]]]])
			unimpaired_13_zthr_data[[position]] <- spbatch[[un[[i]]]]
			unimpaired_13_zthr_split_data[[position]] <-test_split[[un[[i]]]]
		}
	}
}

load("C:\\Users\\tiffn_000\\Documents\\workspaces\\mar_28_activesites.RData")

for(i in 1:length(unimpaired_13_zthr_split_data)){
	for(k in 1:3){
		gauge <- names(unimpaired_13_zthr_split_data)[[i]]
		period <- names(unimpaired_13_zthr_split_data[[i]]$all)[[k]]
		testplotdf <- unimpaired_13_zthr_split_data[[i]]$all[[k]]
#		testplotdf[which(testplotdf$TotVolAbv_acft==0),!names(testplotdf)%in%c("sthyyear")] <- NA
		for(j in 1:1){
			if(statistic_tests[[j]]=="TotVolAbv_acft"){
				statlabel <- c("Total Volume (acft)")
				title <- c("Total Volume")
			}else if(statistic_tests[[j]]=="TotDaysAbv"){
				statlabel <- c("Total Days Above 90%")
				title <- c("Zero-Deflated Days Above")
			}else if(statistic_tests[[j]]=="numpeaks"){
				statlabel <- c("Number of Peaks Above 90%")
				title <- c("Zero-Deflated Number of Peaks Above")
			}else if(statistic_tests[[j]]=="mean_peakflow"){
				statlabel <- c("Mean Magnitude of Peaks Above 90%")
				title <- c("Zero-Deflated Mean Magnitude of Peaks Above")
			}else if(statistic_tests[[j]]=="total_peakflow"){
				statlabel <- c("Total Magnitude of Peaks Above 90%")
				title <- c("Zero-Deflated Total Magnitude of Peaks Above")
			}
			vol_gls <- gls(as.formula(paste(statistic_tests[[j]]," ~ sthyyear",sep="")),testplotdf, method="ML", na.action=na.exclude)
			testplotdf$gls_fitted <- predict(vol_gls, data.frame(sthyyear=testplotdf$sthyyear))
			vol_loess <- loess(as.formula(paste(statistic_tests[[j]]," ~ sthyyear",sep="")),testplotdf, na.action=na.exclude)
			testplotdf$loess_fitted <- predict(vol_loess, data.frame(sthyyear=testplotdf$sthyyear))
			vol_3poly <- lm(as.formula(paste(statistic_tests[[j]]," ~ poly(sthyyear,3)",sep="")),testplotdf, na.action=na.exclude)
			testplotdf$poly3_fitted <- predict(vol_3poly, data.frame(sthyyear=testplotdf$sthyyear))
			vol_4poly <- lm(as.formula(paste(statistic_tests[[j]]," ~ poly(sthyyear,4)",sep="")),testplotdf, na.action=na.exclude)
			testplotdf$poly4_fitted <- predict(vol_4poly, data.frame(sthyyear=testplotdf$sthyyear))
			vol_5poly <- lm(as.formula(paste(statistic_tests[[j]]," ~ poly(sthyyear,5)",sep="")),testplotdf, na.action=na.exclude)
			testplotdf$poly5_fitted <- predict(vol_5poly, data.frame(sthyyear=testplotdf$sthyyear))
			vol_MKTtau <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$tau[[1]],3)
			vol_MKTp <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$sl[[1]],3)
			fitted_vol_plots <- ggplot(testplotdf,aes_string("sthyyear",statistic_tests[[j]]))+geom_point()+ geom_line(aes(sthyyear,gls_fitted,color="gls"))+
					geom_line(aes(sthyyear,loess_fitted,color="loess")) +geom_line(aes(sthyyear,poly3_fitted, color="poly3"))+
					geom_line(aes(sthyyear,poly4_fitted, color="poly4"))+
					geom_line(aes(sthyyear,poly5_fitted, color="poly5"))+
					scale_color_manual("Legend", labels=c("GLS","Loess","3rd Order Polynomial", "4th Order Polynomial", "5th Order Polynomial"),
							values=c(gls="blue", loess="red", poly3="green", poly4="purple", poly5="orange")) + xlab("Hydrologic Year (start year)")+
					ylab(paste(statlabel,sep="")) + ggtitle(paste(title," v Year (USGS",gauge,", ",period,")\nMKT tau= ",vol_MKTtau," p= ",vol_MKTp,sep=""))+
					theme(axis.text=element_text(size=12, face="bold"),
							axis.title=element_text(size=14,face="bold"),
							plot.title=element_text(size=16))
			ggsave(fitted_vol_plots, file=paste("C:\\Users\\tiffn_000\\Google Drive\\unimp_plots_3_28\\test_zero_threshold\\",statistic_tests[[j]],"\\USGS",gauge,"_",period,".png",sep=""), height=8.5, width=11, units="in")
			
		}	
	}
}

trend_zr_hy_df <- data.frame(gauge=names(unimpaired_13),gls_slope= rep(NA,13), gls_p = rep(NA,13),
				MKT_tau=rep(NA,13), MKT_p=rep(NA,13), SN_lm_slope=rep(NA,13), SN_ratio = rep(NA,13))
trend_zr_mon3_df <- data.frame(gauge=names(unimpaired_13),gls_slope= rep(NA,13), gls_p = rep(NA,13),
				MKT_tau=rep(NA,13), MKT_p=rep(NA,13), SN_lm_slope=rep(NA,13), SN_ratio = rep(NA,13))
trend_zr_mon6_df <- data.frame(gauge=names(unimpaired_13),gls_slope= rep(NA,13), gls_p = rep(NA,13),
				MKT_tau=rep(NA,13), MKT_p=rep(NA,13), SN_lm_slope=rep(NA,13), SN_ratio = rep(NA,13))
trend_90_hy_df <- data.frame(gauge=names(unimpaired_13),gls_slope= rep(NA,13), gls_p = rep(NA,13),
				MKT_tau=rep(NA,13), MKT_p=rep(NA,13), SN_lm_slope=rep(NA,13), SN_ratio = rep(NA,13))
trend_90_mon3_df <- data.frame(gauge=names(unimpaired_13),gls_slope= rep(NA,13), gls_p = rep(NA,13),
				MKT_tau=rep(NA,13), MKT_p=rep(NA,13), SN_lm_slope=rep(NA,13), SN_ratio = rep(NA,13))
trend_90_mon6_df <- data.frame(gauge=names(unimpaired_13),gls_slope= rep(NA,13), gls_p = rep(NA,13),
				MKT_tau=rep(NA,13), MKT_p=rep(NA,13), SN_lm_slope=rep(NA,13), SN_ratio = rep(NA,13))
		
for(i in 1:length(unimpaired_13)){
	testplotdf <- unimpaired_13_zthr_split_data[[i]]$all$hy
	vol_gls <- gls(TotVolAbv_acft ~ sthyyear,testplotdf,correlation = corAR1(form=~1), method="ML", na.action=na.exclude)
	vol_gls <- as.data.frame(summary(vol_gls)$tTable)
	vol_gls_p <- vol_gls$p[[2]]
	vol_gls_slope <- vol_gls$Value[[2]]
	vol_MKTtau <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$tau[[1]],3)
	vol_MKTp <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$sl[[1]],3)
	SNmodel <- lm(TotVolAbv_acft ~ sthyyear,testplotdf)
	SN_slope <- SNmodel$coefficients[[2]]
	SN_ratio <- var(fitted.values(SNmodel))/var(residuals(SNmodel))
	trend_zr_hy_df$gls_slope[[i]] <- vol_gls_slope
	trend_zr_hy_df$gls_p[[i]] <- vol_gls_p
	trend_zr_hy_df$MKT_tau[[i]] <- vol_MKTtau
	trend_zr_hy_df$MKT_p[[i]] <- vol_MKTp
	trend_zr_hy_df$SN_lm_slope[[i]] <- SN_slope
	trend_zr_hy_df$SN_ratio[[i]] <- SN_ratio
	
	testplotdf <- unimpaired_13_zthr_split_data[[i]]$all$mon3
	vol_gls <- gls(TotVolAbv_acft ~ sthyyear,testplotdf,correlation = corAR1(form=~1), method="ML", na.action=na.exclude)
	vol_gls <- as.data.frame(summary(vol_gls)$tTable)
	vol_gls_p <- vol_gls$p[[2]]
	vol_gls_slope <- vol_gls$Value[[2]]
	vol_MKTtau <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$tau[[1]],3)
	vol_MKTp <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$sl[[1]],3)
	SNmodel <- lm(TotVolAbv_acft ~ sthyyear,testplotdf)
	SN_slope <- SNmodel$coefficients[[2]]
	SN_ratio <- var(fitted.values(SNmodel))/var(residuals(SNmodel))
	trend_zr_mon3_df$gls_slope[[i]] <- vol_gls_slope
	trend_zr_mon3_df$gls_p[[i]] <- vol_gls_p
	trend_zr_mon3_df$MKT_tau[[i]] <- vol_MKTtau
	trend_zr_mon3_df$MKT_p[[i]] <- vol_MKTp
	trend_zr_mon3_df$SN_lm_slope[[i]] <- SN_slope
	trend_zr_mon3_df$SN_ratio[[i]] <- SN_ratio
	
	testplotdf <- unimpaired_13_zthr_split_data[[i]]$all$mon6
	vol_gls <- gls(TotVolAbv_acft ~ sthyyear,testplotdf,correlation = corAR1(form=~1), method="ML", na.action=na.exclude)
	vol_gls <- as.data.frame(summary(vol_gls)$tTable)
	vol_gls_p <- vol_gls$p[[2]]
	vol_gls_slope <- vol_gls$Value[[2]]
	vol_MKTtau <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$tau[[1]],3)
	vol_MKTp <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$sl[[1]],3)
	SNmodel <- lm(TotVolAbv_acft ~ sthyyear,testplotdf)
	SN_slope <- SNmodel$coefficients[[2]]
	SN_ratio <- var(fitted.values(SNmodel))/var(residuals(SNmodel))
	trend_zr_mon6_df$gls_slope[[i]] <- vol_gls_slope
	trend_zr_mon6_df$gls_p[[i]] <- vol_gls_p
	trend_zr_mon6_df$MKT_tau[[i]] <- vol_MKTtau
	trend_zr_mon6_df$MKT_p[[i]] <- vol_MKTp
	trend_zr_mon6_df$SN_lm_slope[[i]] <- SN_slope
	trend_zr_mon6_df$SN_ratio[[i]] <- SN_ratio
	
	
	testplotdf <- unimpaired_13_split_data[[i]]$all$mon6
	vol_gls <- gls(TotVolAbv_acft ~ sthyyear,testplotdf,correlation = corAR1(form=~1), method="ML", na.action=na.exclude)
	vol_gls <- as.data.frame(summary(vol_gls)$tTable)
	vol_gls_p <- vol_gls$p[[2]]
	vol_gls_slope <- vol_gls$Value[[2]]
	vol_MKTtau <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$tau[[1]],3)
	vol_MKTp <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$sl[[1]],3)
	SNmodel <- lm(TotVolAbv_acft ~ sthyyear,testplotdf)
	SN_slope <- SNmodel$coefficients[[2]]
	SN_ratio <- var(fitted.values(SNmodel))/var(residuals(SNmodel))
	trend_90_mon6_df$gls_slope[[i]] <- vol_gls_slope
	trend_90_mon6_df$gls_p[[i]] <- vol_gls_p
	trend_90_mon6_df$MKT_tau[[i]] <- vol_MKTtau
	trend_90_mon6_df$MKT_p[[i]] <- vol_MKTp
	trend_90_mon6_df$SN_lm_slope[[i]] <- SN_slope
	trend_90_mon6_df$SN_ratio[[i]] <- SN_ratio
	
	testplotdf <- unimpaired_13_split_data[[i]]$all$mon3
	vol_gls <- gls(TotVolAbv_acft ~ sthyyear,testplotdf,correlation = corAR1(form=~1), method="ML", na.action=na.exclude)
	vol_gls <- as.data.frame(summary(vol_gls)$tTable)
	vol_gls_p <- vol_gls$p[[2]]
	vol_gls_slope <- vol_gls$Value[[2]]
	vol_MKTtau <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$tau[[1]],3)
	vol_MKTp <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$sl[[1]],3)
	SNmodel <- lm(TotVolAbv_acft ~ sthyyear,testplotdf)
	SN_slope <- SNmodel$coefficients[[2]]
	SN_ratio <- var(fitted.values(SNmodel))/var(residuals(SNmodel))
	trend_90_mon3_df$gls_slope[[i]] <- vol_gls_slope
	trend_90_mon3_df$gls_p[[i]] <- vol_gls_p
	trend_90_mon3_df$MKT_tau[[i]] <- vol_MKTtau
	trend_90_mon3_df$MKT_p[[i]] <- vol_MKTp
	trend_90_mon3_df$SN_lm_slope[[i]] <- SN_slope
	trend_90_mon3_df$SN_ratio[[i]] <- SN_ratio
	
	testplotdf <- unimpaired_13_split_data[[i]]$all$hy
	vol_gls <- gls(TotVolAbv_acft ~ sthyyear,testplotdf,correlation = corAR1(form=~1), method="ML", na.action=na.exclude)
	vol_gls <- as.data.frame(summary(vol_gls)$tTable)
	vol_gls_p <- vol_gls$p[[2]]
	vol_gls_slope <- vol_gls$Value[[2]]
	vol_MKTtau <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$tau[[1]],3)
	vol_MKTp <- round(MannKendall(testplotdf[[statistic_tests[[j]]]])$sl[[1]],3)
	SNmodel <- lm(TotVolAbv_acft ~ sthyyear,testplotdf)
	SN_slope <- SNmodel$coefficients[[2]]
	SN_ratio <- var(fitted.values(SNmodel))/var(residuals(SNmodel))
	trend_90_hy_df$gls_slope[[i]] <- vol_gls_slope
	trend_90_hy_df$gls_p[[i]] <- vol_gls_p
	trend_90_hy_df$MKT_tau[[i]] <- vol_MKTtau
	trend_90_hy_df$MKT_p[[i]] <- vol_MKTp
	trend_90_hy_df$SN_lm_slope[[i]] <- SN_slope
	trend_90_hy_df$SN_ratio[[i]] <- SN_ratio
}
		






testplotdf <- unimpaired_13_split_data$`11202001`$all$hy
testplotdf[which(testplotdf$TotVolAbv_acft==0),!names(testplotdf)%in%c("sthyyear")] <- NA
testplotdf$rlmean <- rollapply(testplotdf$TotVolAbv_acft, 5, (mean), na.rm=TRUE, fill=NA)
vol_gls <- gls(TotVolAbv_acft ~ sthyyear,testplotdf, method="ML", na.action=na.exclude)
testplotdf$gls_fitted <- predict(vol_gls, data.frame(sthyyear=testplotdf$sthyyear))
vol_loess <- loess(TotVolAbv_acft ~ sthyyear,testplotdf, na.action=na.exclude)
testplotdf$loess_fitted <- predict(vol_loess, data.frame(sthyyear=testplotdf$sthyyear))
vol_3poly <- lm(TotVolAbv_acft ~ poly(sthyyear,3),testplotdf, na.action=na.exclude)
testplotdf$poly3_fitted <- predict(vol_3poly, data.frame(sthyyear=testplotdf$sthyyear))
vol_MKTtau <- round(MannKendall(testplotdf$TotVolAbv_acft)$tau[[1]],3)
vol_MKTp <- round(MannKendall(testplotdf$TotVolAbv_acft)$sl[[1]],3)
fitted_vol_plots <- ggplot(testplotdf,aes(sthyyear,TotVolAbv_acft))+geom_point()+ geom_line(aes(sthyyear,gls_fitted,color="gls"))+
		geom_line(aes(sthyyear,loess_fitted,color="loess")) +geom_line(aes(sthyyear,poly3_fitted, color="poly3"))+
		scale_color_manual("Legend", labels=c("GLS","Loess","3rd Order Polynomial"),
				values=c(gls="blue", loess="red", poly3="green")) + xlab("Hydrologic Year (start year)")+
		ylab("Total Volume Above 90% (acft)") + ggtitle(paste("Zero-Deflated Volume Above v Year (USGS",gauge,", ",period,")\nMKT tau= ",vol_MKTtau," p= ",vol_MKTp,sep=""))+
		theme(axis.text=element_text(size=12, face="bold"),
				axis.title=element_text(size=14,face="bold"),
				plot.title=element_text(size=16))+
		geom_line(aes(sthyyear,rlmean)) +geom_smooth(method="lm", fill="lightblue", color="blue", alpha=0.6) +
		geom_smooth(method="loess", color="red", fill="pink", alpha=0.4)+
		geom_smooth(method="lm", formula=y ~ poly(x,3), color="green", fill="lightgreen", alpha=0.2)

ggplot(testplotdf,aes(sthyyear,TotVolAbv_acft))+geom_point()+ 
		scale_color_manual("Legend", labels=c("GLS","Loess","3rd Order Polynomial"),
				values=c("blue","red", "green")) + xlab("Hydrologic Year (start year)")+
		ylab("Total Volume Above 90% (acft)") + ggtitle(paste("Zero-Deflated Volume Above v Year (USGS",gauge,", ",period,")\nMKT tau= ",vol_MKTtau," p= ",vol_MKTp,sep=""))+
		theme(axis.text=element_text(size=12, face="bold"),
				axis.title=element_text(size=14,face="bold"),
				plot.title=element_text(size=16))+
		geom_line(aes(sthyyear,rlmean)) +
		geom_smooth(method="lm", formula=y ~ poly(x,3), color="green", fill="lightgreen", alpha=0.2)


testplotdf$rlmean <- rollapply(testplotdf$TotVolAbv_acft, 5, (mean), na.rm=TRUE, fill=NA)



simp_pkflows_mag_dams <- vector("list",7)
simp_pkflows_mag_1980 <- vector("list",7)
simp_pkflows_mag_full <- vector("list",7)
for(q in 1:7){
	batchnum <- q
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
	simp_pkflows_mag_dams[[q]] <- test_peakflowmags_dams_bind
	simp_pkflows_mag_full[[q]] <- test_peakflowmags_full_bind
	simp_pkflows_mag_1980[[q]] <- test_peakflowmags_1980_bind
}

for(i in 1:15){
	for(j in 1:6){
		for(k in 2:7){
			simp_pkflows_mag_dams[[1]][[j]][[i]] <- rbind.data.frame(simp_pkflows_mag_dams[[1]][[j]][[i]],simp_pkflows_mag_dams[[k]][[j]][[i]])
		}
	}
}
areas <- read.csv("C:\\Users\\tiffn_000\\Documents\\gauges_93\\usgs_area.txt", header=TRUE)
areas_join <- merge(simp_pkflows_mag_dams[[1]]$all$mon3,areas,by="gauge",all=TRUE)
areas_join$VA <- areas_join$mean_totvol_TAF/areas_join$area
areas_join$dayfrac <- areas_join$mean_totdays/90
areas_join$nonzero_scale <- NA
areas_join$nonzero_scale[(areas_join$frac_nonzero<=0.2)] <- 1
areas_join$nonzero_scale[(areas_join$frac_nonzero>0.2&areas_join$frac_nonzero<=0.4)] <- 2
areas_join$nonzero_scale[(areas_join$frac_nonzero>0.4&areas_join$frac_nonzero<=0.6)] <-3
areas_join$nonzero_scale[(areas_join$frac_nonzero>0.6&areas_join$frac_nonzero<=0.8)] <- 4
areas_join$nonzero_scale[(areas_join$frac_nonzero>0.8)] <- 5

areas_join$dayfrac_scale <- NA
areas_join$dayfrac_scale[(areas_join$dayfrac<=0.06)] <- 1
areas_join$dayfrac_scale[(areas_join$dayfrac>0.06&areas_join$dayfrac<=0.14)] <- 2
areas_join$dayfrac_scale[(areas_join$dayfrac>0.14&areas_join$dayfrac<=0.22)] <-3
areas_join$dayfrac_scale[(areas_join$dayfrac>0.22&areas_join$dayfrac<=0.3)] <- 4
areas_join$dayfrac_scale[(areas_join$dayfrac>0.3)] <- 5

areas_join$VA_scale <- NA
areas_join$VA_scale[(areas_join$VA<=0.08)] <- 1
areas_join$VA_scale[(areas_join$VA>0.08&areas_join$VA<=0.18)] <- 2
areas_join$VA_scale[(areas_join$VA>0.18&areas_join$VA<=0.28)] <-3
areas_join$VA_scale[(areas_join$VA>0.28&areas_join$VA<=0.38)] <- 4
areas_join$VA_scale[(areas_join$VA>0.38)] <- 5

areas_join$sum <- areas_join$VA_scale +areas_join$dayfrac_scale +3*areas_join$nonzero_scale
areas_join$sum_scale <- NA
areas_join$sum_scale[(areas_join$sum<=8)] <- 1
areas_join$sum_scale[(areas_join$sum>8&areas_join$sum<=12)] <- 2
areas_join$sum_scale[(areas_join$sum>12&areas_join$sum<=16)] <-3
areas_join$sum_scale[(areas_join$sum>16&areas_join$sum<=20)] <- 4
areas_join$sum_scale[(areas_join$sum>20)] <- 5