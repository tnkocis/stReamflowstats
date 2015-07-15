# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


FitPearsonIIIroll <- function(zooinput,  movewidth, startparams, probs){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
	if(!require(fitdistrplus)){
		install.packages("fitdistrplus")
		library(fitdistrplus)
	}
	if(!require(PearsonDS)){
		install.packages("PearsonDS")
		library(PearsonDS)
	}
	if (missing(zooinput))
		stop("Zoo class input data is required.")
	if (missing(movewidth))
		stop("numeric width of moving window required")
	if (missing(probs))
		stop("Non Exceedence Probability vector required")
	
	 if(missing(startparams)){
		L <- length(coredata(zooinput))
		output <- list()
		output$shape <- rep(NA,length.out=L)
		output$scale <- rep(NA, length.out=L)
		output$location <- rep(NA, length.out=L)
		output$location <- as.Date(rep(NA, length.out=L))
		
		lm <- L-(movewidth-1)
		p3 <- rep(NA, lm)
		p4 <- as.Date(rep(NA,lm))
		for (i in 1:lm){
###!!!!!!!!!! addedprobs			p3[[i]] <- FitPearsonIII(zooinput[i:(i+(movewidth-1))], probs=probs)
			p4[[i]] <- index(zooinput)[[i]]
		}
		
		out <- data.frame(prob0.1Q_maf=exp(p3), Date=p4)
	} else {}
return(out)	
}
