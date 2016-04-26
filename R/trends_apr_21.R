# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


trendsfinal2 <- function(input, damyear, gauge, period){
	library(zoo)
	library(nlme)
	library(Kendall)
	if(is.null(input$month)==FALSE){
		input$month <- NULL
	}
	input <- input[which(input$sthyyear>=damyear),]
	inputstartyear <- input$sthyyear[[1]]
	inputnonzero <- input[which(input$TotVolAbv_acft>0),]
	results_5 <- data.frame(gauge=rep(gauge,3), period=rep(period,3),start_sthyyear=rep(inputstartyear,3),
			total_count=rep(length(input$TotVolAbv_acft),3),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),3),mean=(rep(NA,3)),count_trend=rep(NA,3),
			sd=(rep(NA,3)),
			median=(rep(NA,3)),window=rep(5,3),
			lm_slope=(rep(NA,3)), lm_p=(rep(NA,3)), lm_multi_r2=(rep(NA,3)),
			lm_adj_r2=(rep(NA,3)),MKT_tau=(rep(NA,3)), MKT_p=(rep(NA,3)),
			SN_ratio=(rep(NA,3)))
	results_5_frac <- data.frame(gauge=rep(gauge,1), period=rep(period,1),start_sthyyear=rep(inputstartyear,1),
			total_count=rep(length(input$TotVolAbv_acft),1),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),1),mean=(rep(NA,1)), count_trend=rep(NA,1),
			sd=(rep(NA,1)),
			median=(rep(NA,1)),window=rep(5,1),
			lm_slope=(rep(NA,1)), lm_p=(rep(NA,1)), lm_multi_r2=(rep(NA,1)),
			lm_adj_r2=(rep(NA,1)),MKT_tau=(rep(NA,1)), MKT_p=(rep(NA,1)),
			SN_ratio=(rep(NA,1)))
	

	for(i in 1:3){
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
	
	
	yrseq <- seq(inputstartyear,2013,1)
	results_5_frac$metricname <- "frac_nonzero"
	testyr2 <- cumsum(rev(yrseq%in%input$sthyyear)==FALSE)
	if(length(which((cumsum(yrseq%in%input$sthyyear)==c(cumsum(yrseq%in%input$sthyyear)[2:length(cumsum(yrseq%in%input$sthyyear))],NA))==FALSE))<3){
		yrst <- yrseq[[which((cumsum(yrseq%in%input$sthyyear)==c(cumsum(yrseq%in%input$sthyyear)[2:length(cumsum(yrseq%in%input$sthyyear))],NA))==FALSE)[[2]]]]
	}else{
		yrst <- yrseq[[which((cumsum(yrseq%in%input$sthyyear)==c(cumsum(yrseq%in%input$sthyyear)[2:length(cumsum(yrseq%in%input$sthyyear))],NA))==FALSE)[[3]]]]
	}
	yrstseq <- seq(yrst,2013,1)
#	if(length(testyr2)==0){
#		testyr2 <- length(yrseq)
#	} 
	if(length(testyr2)<20){
		testyr220 <- testyr2[[length(testyr2)]]
	}else{
		testyr220 <- testyr2[[20]]
	}
	if(yrst==1991){
		yrst<-1989
	}else if(yrst==1972){
		yrst <- 1970
	}
	if((sum(!yrstseq%in%input$sthyyear, na.rm=TRUE)>7)|(testyr220>10)){
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

	}else{
#		yrst <- rev(yrseq)[testyr2[[1]]]+1
		inputnonzero <- input[which(input$sthyyear>=yrst),]
		inputnonzero$TotVolAbv_acft[inputnonzero$TotVolAbv_acft>0] <- 1
		yearfrac_5 <- rollapply(inputnonzero$TotVolAbv_acft,5, function(x) sum(x, na.rm=TRUE)/5)		
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
		results_5_frac$start_sthyyear <- yrst
		

	}
	output <- rbind.data.frame(results_5,results_5_frac)
	
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
	output$lm_sig[is.na(output$lm_p)|output$lm_p=="NaN"] <- "insignificant"
	output$MKT_sig[is.na(output$MKT_p)|output$MKT_p=="NaN"] <- "insignificant"
	output$SN_sig[is.na(output$SN_ratio)|output$SN_ratio=="NaN"] <- "insignificant"
	output$SN_sig[which(output$SN_ratio=="insufficient data")] <- "insufficient data"
	output$lm_sig[which(output$lm_p=="insufficient data")] <- "insufficient data"
	output$MKT_sig[which(output$MKT_p=="insufficient data")] <- "insufficient data"
	output$MKT_sign[which(output$MKT_tau=="insufficient data")] <- "insufficient data"
	output$lm_sign[which(output$lm_slope=="insufficient data")] <- "insufficient data"
	
	return(output)
}



trendsfinal2_1 <- function(input, damyear, gauge, period){
	library(zoo)
	library(nlme)
	library(Kendall)
	if(is.null(input$month)==FALSE){
		input$month <- NULL
	}
	input <- input[which(input$sthyyear>=damyear),]
	inputstartyear <- input$sthyyear[[1]]
	inputnonzero <- input[which(input$TotVolAbv_acft>0),]
	results_5 <- data.frame(gauge=rep(gauge,3), period=rep(period,3),start_sthyyear=rep(inputstartyear,3),
			total_count=rep(length(input$TotVolAbv_acft),3),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),3),mean=(rep(NA,3)),count_trend=rep(NA,3),
			sd=(rep(NA,3)),
			median=(rep(NA,3)),window=rep(1,3),
			lm_slope=(rep(NA,3)), lm_p=(rep(NA,3)), lm_multi_r2=(rep(NA,3)),
			lm_adj_r2=(rep(NA,3)),MKT_tau=(rep(NA,3)), MKT_p=(rep(NA,3)),
			SN_ratio=(rep(NA,3)))
	results_5_frac <- data.frame(gauge=rep(gauge,1), period=rep(period,1),start_sthyyear=rep(inputstartyear,1),
			total_count=rep(length(input$TotVolAbv_acft),1),
			count_nonzero=rep(length(inputnonzero$TotVolAbv_acft),1),mean=(rep(NA,1)), count_trend=rep(NA,1),
			sd=(rep(NA,1)),
			median=(rep(NA,1)),window=rep(5,1),
			lm_slope=(rep(NA,1)), lm_p=(rep(NA,1)), lm_multi_r2=(rep(NA,1)),
			lm_adj_r2=(rep(NA,1)),MKT_tau=(rep(NA,1)), MKT_p=(rep(NA,1)),
			SN_ratio=(rep(NA,1)))
	
	
	for(i in 1:3){
		results_5$metricname[[i]] <- names(inputnonzero)[[i]]
		metricraw <- inputnonzero[[i]]
		metricraw <- metricraw[!is.na(metricraw)]
		if(length(metricraw)<=3){
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
			metricraw_5 <- rollmean(metricraw,1)
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
	
	
	yrseq <- seq(inputstartyear,2013,1)
	results_5_frac$metricname <- "frac_nonzero"
	testyr2 <- cumsum(rev(yrseq%in%input$sthyyear)==FALSE)
	if(length(which((cumsum(yrseq%in%input$sthyyear)==c(cumsum(yrseq%in%input$sthyyear)[2:length(cumsum(yrseq%in%input$sthyyear))],NA))==FALSE))<3){
		yrst <- yrseq[[which((cumsum(yrseq%in%input$sthyyear)==c(cumsum(yrseq%in%input$sthyyear)[2:length(cumsum(yrseq%in%input$sthyyear))],NA))==FALSE)[[2]]]]
	}else{
	yrst <- yrseq[[which((cumsum(yrseq%in%input$sthyyear)==c(cumsum(yrseq%in%input$sthyyear)[2:length(cumsum(yrseq%in%input$sthyyear))],NA))==FALSE)[[3]]]]
	}
	if(yrst==1991){
		yrst <- 1989
	}else if(yrst==1972){
		yrst <- 1970
	}
	yrstseq <- seq(yrst,2013,1)
#	if(length(testyr2)==0){
#		testyr2 <- length(yrseq)
#	} 
	if(length(testyr2)<20){
		testyr220 <- testyr2[[length(testyr2)]]
	}else{
 		testyr220 <- testyr2[[20]]
	}
	if((sum(!yrstseq%in%input$sthyyear, na.rm=TRUE)>7)|(testyr220>10)){
		
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
		
	}else{
#		yrst <- rev(yrseq)[testyr2[[1]]]+1
		inputnonzero <- input[which(input$sthyyear>=yrst),]
		inputnonzero$TotVolAbv_acft[inputnonzero$TotVolAbv_acft>0] <- 1
		yearfrac_5 <- rollapply(inputnonzero$TotVolAbv_acft,1, function(x) sum(x, na.rm=TRUE)/5)		
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
		results_5_frac$start_sthyyear <- yrst
		
		
	}
	output <- rbind.data.frame(results_5,results_5_frac)
	
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
	output$lm_sig[is.na(output$lm_p)|output$lm_p=="NaN"] <- "insignificant"
	output$MKT_sig[is.na(output$MKT_p)|output$MKT_p=="NaN"] <- "insignificant"
	output$SN_sig[is.na(output$SN_ratio)|output$SN_ratio=="NaN"] <- "insignificant"
	output$SN_sig[which(output$SN_ratio=="insufficient data")] <- "insufficient data"
	output$lm_sig[which(output$lm_p=="insufficient data")] <- "insufficient data"
	output$MKT_sig[which(output$MKT_p=="insufficient data")] <- "insufficient data"
	output$MKT_sign[which(output$MKT_tau=="insufficient data")] <- "insufficient data"
	output$lm_sign[which(output$lm_slope=="insufficient data")] <- "insufficient data"
	
	return(output)
}

trendsfinalCOM <- function(input, sthyyear, actual_start, damyear, gauge, period){
	library(zoo)
	library(nlme)
	library(Kendall)
	inyear <- sthyyear[which(sthyyear>=actual_start)]
	input <- input[which(sthyyear>=actual_start)]
	input <- input[which(inyear>=damyear)]
	if(actual_start>=damyear){
	inputstartyear <- actual_start}else{
	inputstartyear <- damyear}
	inputnonzero <- input[!is.na(input)]
	results_5 <- data.frame(gauge=rep(gauge,1), period=rep(period,1),start_sthyyear=rep(inputstartyear,1),
			total_count=rep(length(input),1),
			count_nonzero=rep(length(inputnonzero),1),mean=(rep(NA,1)),count_trend=rep(NA,1),
			sd=(rep(NA,1)),
			median=(rep(NA,1)),window=rep(5,1),
			lm_slope=(rep(NA,1)), lm_p=(rep(NA,1)), lm_multi_r2=(rep(NA,1)),
			lm_adj_r2=(rep(NA,1)),MKT_tau=(rep(NA,1)), MKT_p=(rep(NA,1)),
			SN_ratio=(rep(NA,1)))
	
	for(i in 1:1){
		results_5$metricname[[i]] <- "COM_90_DOHY"
		metricraw <- inputnonzero
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
	

	output <- results_5
	
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
	output$lm_sig[is.na(output$lm_p)|output$lm_p=="NaN"] <- "insignificant"
	output$MKT_sig[is.na(output$MKT_p)|output$MKT_p=="NaN"] <- "insignificant"
	output$SN_sig[is.na(output$SN_ratio)|output$SN_ratio=="NaN"] <- "insignificant"
	output$SN_sig[which(output$SN_ratio=="insufficient data")] <- "insufficient data"
	output$lm_sig[which(output$lm_p=="insufficient data")] <- "insufficient data"
	output$MKT_sig[which(output$MKT_p=="insufficient data")] <- "insufficient data"
	output$MKT_sign[which(output$MKT_tau=="insufficient data")] <- "insufficient data"
	output$lm_sign[which(output$lm_slope=="insufficient data")] <- "insufficient data"
	
	return(output)
}

trendsfinalCOM_1 <- function(input, sthyyear, actual_start, damyear, gauge, period){
	library(zoo)
	library(nlme)
	library(Kendall)
	inyear <- sthyyear[which(sthyyear>=actual_start)]
	input <- input[which(sthyyear>=actual_start)]
	input <- input[which(inyear>=damyear)]
	if(actual_start>=damyear){
		inputstartyear <- actual_start}else{
		inputstartyear <- damyear}
	inputnonzero <- input[!is.na(input)]
	results_5 <- data.frame(gauge=rep(gauge,1), period=rep(period,1),start_sthyyear=rep(inputstartyear,1),
			total_count=rep(length(input),1),
			count_nonzero=rep(length(inputnonzero),1),mean=(rep(NA,1)),count_trend=rep(NA,1),
			sd=(rep(NA,1)),
			median=(rep(NA,1)),window=rep(1,1),
			lm_slope=(rep(NA,1)), lm_p=(rep(NA,1)), lm_multi_r2=(rep(NA,1)),
			lm_adj_r2=(rep(NA,1)),MKT_tau=(rep(NA,1)), MKT_p=(rep(NA,1)),
			SN_ratio=(rep(NA,1)))
	
	for(i in 1:1){
		results_5$metricname[[i]] <- "COM_90_DOHY"
		metricraw <- inputnonzero
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
			metricraw_5 <- rollmean(metricraw,1)
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
	
	
	output <- results_5
	
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
	output$lm_sig[is.na(output$lm_p)|output$lm_p=="NaN"] <- "insignificant"
	output$MKT_sig[is.na(output$MKT_p)|output$MKT_p=="NaN"] <- "insignificant"
	output$SN_sig[is.na(output$SN_ratio)|output$SN_ratio=="NaN"] <- "insignificant"
	output$SN_sig[which(output$SN_ratio=="insufficient data")] <- "insufficient data"
	output$lm_sig[which(output$lm_p=="insufficient data")] <- "insufficient data"
	output$MKT_sig[which(output$MKT_p=="insufficient data")] <- "insufficient data"
	output$MKT_sign[which(output$MKT_tau=="insufficient data")] <- "insufficient data"
	output$lm_sign[which(output$lm_slope=="insufficient data")] <- "insufficient data"
	
	return(output)
}
