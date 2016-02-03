# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


thresholds <- function(input){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	
	discharge_maf <- input$Discharge_cfs*86400*2.29568411e-5*1e-6
	percentile_maf <- quantile(discharge_maf, probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	names(percentile_maf) <- paste("P",c(5,10,20,25,50,75,90,95),"maf",sep="")
	percentile_maf <- as.list(percentile_maf)
	
	return(Percentile_maf=as.data.frame(percentile_maf))

}

thresholdsdams <- function(input, dams){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	maxdamyear <- max(dams$YEAR_BUILT)
	discharge_maf <- input$Discharge_cfs[which(as.numeric(format(input$Date,"%Y"))>maxdamyear)]*86400*2.29568411e-5*1e-6
	percentile_maf <- quantile(discharge_maf, probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	names(percentile_maf) <- paste("P",c(5,10,20,25,50,75,90,95),"maf",sep="")
	percentile_maf <- as.list(percentile_maf)
	
	return(Percentile_maf=as.data.frame(percentile_maf))
	
}

thresholds_pre_imp <- function(input, dams){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	mindamyear <- min(dams$YEAR_BUILT)
	if(mindamyear==1800){
		discharge_maf <- input$Discharge_cfs*86400*2.29568411e-5*1e-6
		percentile_maf <- quantile(discharge_maf, probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		names(percentile_maf) <- paste("P",c(5,10,20,25,50,75,90,95),"maf",sep="")
		percentile_maf <- as.list(percentile_maf)
	}else if(mindamyear<= as.numeric(format(input$Date[[1]],"%Y"))){
		discharge_maf <- Inf
		percentile_maf <- rep(Inf,8)
		names(percentile_maf) <- paste("P",c(5,10,20,25,50,75,90,95),"maf",sep="")
		percentile_maf <- as.list(percentile_maf)
	}else{
		discharge_maf <- input$Discharge_cfs[which(as.numeric(format(input$Date,"%Y"))<mindamyear)]*86400*2.29568411e-5*1e-6
		percentile_maf <- quantile(discharge_maf, probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
		names(percentile_maf) <- paste("P",c(5,10,20,25,50,75,90,95),"maf",sep="")
		percentile_maf <- as.list(percentile_maf)
	}
	return(Percentile_maf=as.data.frame(percentile_maf))
}

thresholdsdams3mon <- function(input, dams){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	maxdamyear <- max(dams$YEAR_BUILT)
	discharge_maf <- input$All$Data$Discharge_acfte6_day[which(as.numeric(format(input$All$Data$Date,"%Y"))>maxdamyear)]
	percentile_maf <- quantile(discharge_maf, probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	names(percentile_maf) <- paste("P",c(5,10,20,25,50,75,90,95),"maf",sep="")
	percentile_maf <- as.list(percentile_maf)
	
	return(Percentile_maf=as.data.frame(percentile_maf))
	
}