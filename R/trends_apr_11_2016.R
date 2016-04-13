# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


trendsfinal <- function(input, damyear, gauge, period, yearly_availability){
	library(zoo)
	library(nlme)
	library(Kendall)
	if(is.null(input$month)==FALSE){
		input$month <- NULL
	}
	input <- input[which(input$sthyyear>=damyear),]
	inputstartyear <- input$sthyyear[[1]]
	inputnonzero <- input[which(input$TotVolAbv_acft>0),]
	results_1 <- data.frame(gauge=rep(gauge,5), period=rep(period,5),start_sthyyear=rep(inputstartyear,5), 
			total_count=rep(length(input$TotVolAbv_acft),5),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),5),mean=(rep(NA,5)),count_trend=rep(NA,5),
			sd=(rep(NA,5)),
			median=(rep(NA,5)),metric=(rep(NA,5)),window=rep(1,5),
			lm_slope=(rep(NA,5)), lm_p=(rep(NA,5)), lm_multi_r2=(rep(NA,5)),
			lm_adj_r2=(rep(NA,5)),MKT_tau=(rep(NA,5)), MKT_p=(rep(NA,5)),
			SN_ratio=(rep(NA,5)))
	results_5 <- data.frame(gauge=rep(gauge,5), period=rep(period,5),start_sthyyear=rep(inputstartyear,5),
			total_count=rep(length(input$TotVolAbv_acft),5),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),5),mean=(rep(NA,5)),count_trend=rep(NA,5),
			sd=(rep(NA,5)),
			median=(rep(NA,5)),metric=(rep(NA,5)),window=rep(5,5),
			lm_slope=(rep(NA,5)), lm_p=(rep(NA,5)), lm_multi_r2=(rep(NA,5)),
			lm_adj_r2=(rep(NA,5)),MKT_tau=(rep(NA,5)), MKT_p=(rep(NA,5)),
			SN_ratio=(rep(NA,5)))
	results_10 <- data.frame(gauge=rep(gauge,5), period=rep(period,5),start_sthyyear=rep(inputstartyear,5),
			total_count=rep(length(input$TotVolAbv_acft),5),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),5),mean=(rep(NA,5)), count_trend=rep(NA,5),
			sd=(rep(NA,5)),
			median=(rep(NA,5)),metric=(rep(NA,5)),window=rep(10,5),
			lm_slope=(rep(NA,5)), lm_p=(rep(NA,5)), lm_multi_r2=(rep(NA,5)),
			lm_adj_r2=(rep(NA,5)),MKT_tau=(rep(NA,5)), MKT_p=(rep(NA,5)),
			SN_ratio=(rep(NA,5)))
	results_10_frac <- data.frame(gauge=rep(gauge,1), period=rep(period,1),start_sthyyear=rep(inputstartyear,1),
			total_count=rep(length(input$TotVolAbv_acft),1),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),1),mean=(rep(NA,1)), count_trend=rep(NA,1),
			sd=(rep(NA,1)),
			median=(rep(NA,1)),metric=(rep(NA,1)),window=rep(10,1),
			lm_slope=(rep(NA,1)), lm_p=(rep(NA,1)), lm_multi_r2=(rep(NA,1)),
			lm_adj_r2=(rep(NA,1)),MKT_tau=(rep(NA,1)), MKT_p=(rep(NA,1)),
			SN_ratio=(rep(NA,1)))
	results_5_frac <- data.frame(gauge=rep(gauge,1), period=rep(period,1),start_sthyyear=rep(inputstartyear,1),
			total_count=rep(length(input$TotVolAbv_acft),1),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),1),mean=(rep(NA,1)), count_trend=rep(NA,1),
			sd=(rep(NA,1)),
			median=(rep(NA,1)),metric=(rep(NA,1)),window=rep(5,1),
			lm_slope=(rep(NA,1)), lm_p=(rep(NA,1)), lm_multi_r2=(rep(NA,1)),
			lm_adj_r2=(rep(NA,1)),MKT_tau=(rep(NA,1)), MKT_p=(rep(NA,1)),
			SN_ratio=(rep(NA,1)))
	
	
	for(i in 1:5){
		results_1$metricname[[i]] <- names(inputnonzero)[[i]]
		metricraw <- inputnonzero[[i]]
		metricraw <- metricraw[!is.na(metricraw)]
		results_1$count_trend <- length(metricraw)
		if(length(metricraw)<=2){
			results_1$lm_slope[[i]] <- "insufficient data"
			results_1$lm_p[[i]] <- "insufficient data"
			results_1$lm_multi_r2[[i]] <- "insufficient data"
			results_1$lm_adj_r2[[i]] <- "insufficient data"
			results_1$MKT_tau[[i]] <- "insufficient data"
			results_1$MKT_p[[i]] <- "insufficient data"
			results_1$SN_ratio[[i]]<- "insufficient data"
			results_1$mean[[i]]<- "insufficient data"
			results_1$sd[[i]]<- "insufficient data"
			results_1$median[[i]]<- "insufficient data"
		} else {
			lm_1 <- lm(metricraw ~ index(metricraw))
			MKT_1 <- MannKendall(metricraw)
			results_1$lm_slope[[i]] <- summary(lm_1)$coefficients[[2]]
			results_1$lm_p[[i]] <- summary(lm_1)$coefficients[[8]]
			results_1$lm_multi_r2[[i]] <- summary(lm_1)$r.squared
			results_1$lm_adj_r2[[i]] <- summary(lm_1)$adj.r.squared
			results_1$MKT_tau[[i]] <- MKT_1$tau[[1]]
			results_1$MKT_p[[i]] <- MKT_1$sl[[1]]
			results_1$SN_ratio[[i]]<- var(fitted.values(lm_1))/var(residuals(lm_1))
			results_1$mean[[i]]<- mean(metricraw,na.rm=TRUE)
			results_1$sd[[i]]<- sd(metricraw,na.rm=TRUE)
			results_1$median[[i]]<- median(metricraw, na.rm=TRUE)
		}
	}
	for(i in 1:5){
		results_5$metricname[[i]] <- names(inputnonzero)[[i]]
		metricraw <- inputnonzero[[i]]
		metricraw <- metricraw[!is.na(metricraw)]
		if(length(metricraw)<=7){
			results_5$count_trend <- "insufficient data"
			results_5$lm_slope[[i]] <- "insufficient data"
			results_5$lm_p[[i]] <- "insufficient data"
			results_5$lm_multi_r2[[i]] <- "insufficient data"
			results_5$lm_adj_r2[[i]] <- "insufficient data"
			results_5$MKT_tau[[i]] <- "insufficient data"
			results_5$MKT_p[[i]] <- "insufficient data"
			results_5$SN_ratio[[i]]<- "insufficient data"
			results_5$mean[[i]]<- "insufficient data"
			results_5$sd[[i]]<- "insufficient data"
			results_5$median[[i]]<- "insufficient data"
		} else {
			metricraw_5 <- rollmean(metricraw,5)
			results_5$count_trend <- length(metricraw_5)
			lm_5 <- lm(metricraw_5 ~ index(metricraw_5))
			MKT_5 <- MannKendall(metricraw_5)
			results_5$lm_slope[[i]] <- summary(lm_5)$coefficients[[2]]
			results_5$lm_p[[i]] <- summary(lm_5)$coefficients[[8]]
			results_5$lm_multi_r2[[i]] <- summary(lm_5)$r.squared
			results_5$lm_adj_r2[[i]] <- summary(lm_5)$adj.r.squared
			results_5$MKT_tau[[i]] <- MKT_5$tau[[1]]
			results_5$MKT_p[[i]] <- MKT_5$sl[[1]]
			results_5$SN_ratio[[i]]<- var(fitted.values(lm_5))/var(residuals(lm_5))
			results_5$mean[[i]]<- mean(metricraw_5,na.rm=TRUE)
			results_5$sd[[i]]<- sd(metricraw_5,na.rm=TRUE)
			results_5$median[[i]]<- median(metricraw_5, na.rm=TRUE)
		}
	}	
	
	for(i in 1:5){
		results_10$metricname[[i]] <- names(inputnonzero)[[i]]
		metricraw <- inputnonzero[[i]]
		metricraw <- metricraw[!is.na(metricraw)]
		if(length(metricraw)<=11){
			results_10$count_trend <- "insufficient data"
			results_10$lm_slope[[i]] <- "insufficient data"
			results_10$lm_p[[i]] <- "insufficient data"
			results_10$lm_multi_r2[[i]] <- "insufficient data"
			results_10$lm_adj_r2[[i]] <- "insufficient data"
			results_10$MKT_tau[[i]] <- "insufficient data"
			results_10$MKT_p[[i]] <- "insufficient data"
			results_10$SN_ratio[[i]]<- "insufficient data"
			results_10$mean[[i]]<- "insufficient data"
			results_10$sd[[i]]<- "insufficient data"
			results_10$median[[i]]<- "insufficient data"
		} else {
			metricraw_10 <- rollmean(metricraw,10)
			results_10$count_trend <- length(metricraw_10)
			lm_10 <- lm(metricraw_10 ~ index(metricraw_10))
			MKT_10 <- MannKendall(metricraw_10)
			results_10$lm_slope[[i]] <- summary(lm_10)$coefficients[[2]]
			results_10$lm_p[[i]] <- summary(lm_10)$coefficients[[8]]
			results_10$lm_multi_r2[[i]] <- summary(lm_10)$r.squared
			results_10$lm_adj_r2[[i]] <- summary(lm_10)$adj.r.squared
			results_10$MKT_tau[[i]] <- MKT_10$tau[[1]]
			results_10$MKT_p[[i]] <- MKT_10$sl[[1]]
			results_10$SN_ratio[[i]]<- var(fitted.values(lm_10))/var(residuals(lm_10))
			results_10$mean[[i]]<- mean(metricraw_10,na.rm=TRUE)
			results_10$sd[[i]]<- sd(metricraw_10,na.rm=TRUE)
			results_10$median[[i]]<- median(metricraw_10, na.rm=TRUE)
		}
	}	
	
	yrseq <- seq(inputstartyear,2013,1)
	results_5_frac$metricname <- "frac_nonzero"
	results_10_frac$metricname <- "frac_nonzero"
	testyr2 <- which(rev(yrseq%in%input$sthyyear)==FALSE)
	if(length(testyr2)==0){
		testyr2 <- length(yrseq)
	} 
	if((sum(!yrseq%in%input$sthyyear, na.rm=TRUE)>5)|(testyr2[[1]]<20)){

		results_5_frac$count_trend <- "insufficient data" 
		results_5_frac$lm_slope[[1]] <- "insufficient data" 
		results_5_frac$lm_p[[1]] <- "insufficient data" 
		results_5_frac$lm_multi_r2[[1]] <- "insufficient data" 
		results_5_frac$lm_adj_r2[[1]] <- "insufficient data" 
		results_5_frac$MKT_tau[[1]] <- "insufficient data" 
		results_5_frac$MKT_p[[1]] <- "insufficient data" 
		results_5_frac$SN_ratio[[1]]<- "insufficient data" 
		results_5_frac$mean[[1]]<- "insufficient data" 
		results_5_frac$sd[[1]]<- "insufficient data"
		results_5_frac$median[[1]]<- "insufficient data" 
		
		results_10_frac$count_trend <- "insufficient data" 
		results_10_frac$lm_slope[[1]] <- "insufficient data" 
		results_10_frac$lm_p[[1]] <- "insufficient data" 
		results_10_frac$lm_multi_r2[[1]] <- "insufficient data" 
		results_10_frac$lm_adj_r2[[1]] <- "insufficient data" 
		results_10_frac$MKT_tau[[1]] <- "insufficient data"
		results_10_frac$MKT_p[[1]] <- "insufficient data" 
		results_10_frac$SN_ratio[[1]]<- "insufficient data" 
		results_10_frac$mean[[1]]<- "insufficient data" 
		results_10_frac$sd[[1]]<- "insufficient data" 
		results_10_frac$median[[1]]<- "insufficient data"
	}else{
		yrst <- rev(yrseq)[testyr2[[1]]]+1
		inputnonzero <- input[which(input$sthyyear>=yrst),]
		inputnonzero$TotVolAbv_acft[inputnonzero$TotVolAbv_acft>0] <- 1
		yearfrac_5 <- rollapply(inputnonzero$TotVolAbv_acft,5, function(x) sum(x, na.rm=TRUE)/5)
		yearfrac_10 <- rollapply(inputnonzero$TotVolAbv_acft,10, function(x) sum(x, na.rm=TRUE)/10)
		
		lm_5 <- lm(yearfrac_5 ~ index(yearfrac_5))
		MKT_5 <- MannKendall(yearfrac_5)
		results_5_frac$count_trend <- length(yearfrac_5)
		results_5_frac$lm_slope[[1]] <- summary(lm_5)$coefficients[[2]]
		results_5_frac$lm_p[[1]] <- summary(lm_5)$coefficients[[8]]
		results_5_frac$lm_multi_r2[[1]] <- summary(lm_5)$r.squared
		results_5_frac$lm_adj_r2[[1]] <- summary(lm_5)$adj.r.squared
		results_5_frac$MKT_tau[[1]] <- MKT_5$tau[[1]]
		results_5_frac$MKT_p[[1]] <- MKT_5$sl[[1]]
		results_5_frac$SN_ratio[[1]]<- var(fitted.values(lm_5))/var(residuals(lm_5))
		results_5_frac$mean[[1]]<- mean(yearfrac_5,na.rm=TRUE)
		results_5_frac$sd[[1]]<- sd(yearfrac_5,na.rm=TRUE)
		results_5_frac$median[[1]]<- median(yearfrac_5, na.rm=TRUE)
		
		lm_10 <- lm(yearfrac_10 ~ index(yearfrac_10))
		MKT_10 <- MannKendall(yearfrac_10)
		results_10_frac$count_trend <- length(yearfrac_10)
		results_10_frac$lm_slope[[1]] <- summary(lm_10)$coefficients[[2]]
		results_10_frac$lm_p[[1]] <- summary(lm_10)$coefficients[[8]]
		results_10_frac$lm_multi_r2[[1]] <- summary(lm_10)$r.squared
		results_10_frac$lm_adj_r2[[1]] <- summary(lm_10)$adj.r.squared
		results_10_frac$MKT_tau[[1]] <- MKT_10$tau[[1]]
		results_10_frac$MKT_p[[1]] <- MKT_10$sl[[1]]
		results_10_frac$SN_ratio[[1]]<- var(fitted.values(lm_10))/var(residuals(lm_10))
		results_10_frac$mean[[1]]<- mean(yearfrac_10,na.rm=TRUE)
		results_10_frac$sd[[1]]<- sd(yearfrac_10,na.rm=TRUE)
		results_10_frac$median[[1]]<- median(yearfrac_10, na.rm=TRUE)
	}
	
	output <- rbind.data.frame(results_1,results_5,results_10,results_5_frac,results_10_frac)
	if(inputstartyear>=damyear){
		testyear <- inputstartyear
	}else{
		testyear <- damyear
	}
	availableyrs <- sum(yearly_availability$fraction_available[which(as.character(yearly_availability$year)>testyear)])
	numyears <- 2013-testyear
	availfrac <- availableyrs/numyears
	if(availfrac<.2){
		output$missig_data_flag <- FALSE
	}else{
		output$missig_data_flag <- TRUE
	}
	output$MKT_sign <- "insufficient data"
	output$lm_sign<- "insufficient data"
	output$lm_sig <- "insufficient data"
	output$MKT_sig <- "insufficient data"
	output$SN_sig <- "insufficient data"

	output$MKT_sign[which(output$MKT_tau<=0)] <- "negative"
	output$MKT_sign[which(output$MKT_tau>0)] <- "positive"
	output$lm_sign[which(output$lm_slope<=0)] <- "negative"
	output$lm_sign[which(output$lm_slope>0)] <- "positive"
	output$MKT_sig[which(output$MKT_p>0.05)] <- "insignificant"
	output$MKT_sig[which(output$MKT_p<=0.05)] <- "significant"
	output$lm_sig[which(output$lm_p>0.05)] <- "insignificant"
	output$lm_sig[which(output$lm_p<=0.05)] <- "significant"
	output$SN_sig[which(output$SN_ratio<0.2)] <- "insignificant"
	output$SN_sig[which(output$SN_ratio>=0.2)] <- "significant"
	output$SN_sig[which(output$SN_ratio=="insufficient data")] <- "insufficient data"
	output$lm_sig[which(output$lm_p=="insufficient data")] <- "insufficient data"
	output$MKT_sig[which(output$MKT_p=="insufficient data")] <- "insufficient data"
	output$MKT_sign[which(output$MKT_tau=="insufficient data")] <- "insufficient data"
	output$lm_sign[which(output$lm_slope=="insufficient data")] <- "insufficient data"
	
	return(output)
}
