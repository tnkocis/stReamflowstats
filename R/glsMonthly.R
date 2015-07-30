# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################




glsMonthlyThres <- function(data){
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
	if(!require(nlme)){
		install.packages("nlme")
		library(fitdistrplus)
	}
	
	if (missing(data))
		stop("Input data is required.")
	p <- list()
	for(i in 1:length(data)){
		nums <- data[[i]][[2]]*1.23348184e9
		Date <- as.Date(as.character(data[[i]][[1]]), format="%Y")
		g <- gls(nums ~ Date,correlation=corARMA(p=1), method='ML' )
		p[[i]] <- as.data.frame(summary(g)$tTable)
		p[[i]]$coeff_names <-  dimnames(summary(g)$tTable)[[1]]
		
	}
	names(p) <- paste(names(data),"_Q_m3", sep="")
	return(p)
}

glsMonthlyRelQ <- function(data){
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
	if(!require(nlme)){
		install.packages("nlme")
		library(fitdistrplus)
	}
	
	if (missing(data))
		stop("Input data is required.")
	p <- list()
	for(i in 1:length(data)){
		nums <- data[[i]][[2]]
		Date <- as.Date(as.character(data[[i]][[1]]), format="%Y")
		g <- gls(nums ~ Date,correlation=corARMA(p=1), method='ML' )
		p[[i]] <- as.data.frame(summary(g)$tTable)
		p[[i]]$coeff_names <-  dimnames(summary(g)$tTable)[[1]]
		
	}
	names(p) <- paste(names(data),"_relQ", sep="")
	return(p)
}



