# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


glsPIII <- function(data){
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
		library(nlme)
	}

	if (missing(data))
		stop("Input data is required.")
	p <- list()
for(i in 1:length(data)){
	nums <- data[[i]][[1]]
	Date <- data[[i]][[2]]
	g <- gls(nums ~ Date,correlation=corARMA(p=1), method='ML' )
	p[[i]] <- as.data.frame(summary(g)$tTable)
	p[[i]]$coeff_names <-  dimnames(summary(g)$tTable)[[1]]
}
names(p) <- names(data)
return(p)
}

glsPIIIMonthly <- function(data){
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
	p <- vector("list",6)
	for(k in 1:6){
		for(i in 1:length(data[[k]])){
			nums <- data[[k]][[i]][[1]]*1.23348184e9
			Date <- as.Date(as.character(data[[k]][[i]][[2]]), format="%Y")
			g <- gls(nums ~ Date,correlation=corARMA(p=1), method='ML' )
			p[[k]][[i]] <- as.data.frame(summary(g)$tTable)
			p[[k]][[i]]$coeff_names <-  dimnames(summary(g)$tTable)[[1]]
		
		}
		names(p[[k]]) <- names(data[[k]])
	}
	names(p) <- names(data)
	return(p)
}