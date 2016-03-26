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