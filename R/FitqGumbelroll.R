# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


FitqGumbelroll <- function(zooinput,  movewidth, startparams, probs){
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
		p3 <- vector("list", length(probs))
		p5 <- vector("list", length(probs))
		p4 <- as.Date(rep(NA,lm))
		for (i in 1:lm){
			p3[[i]] <- FitqGumbel(zooinput=zooinput[i:(i+(movewidth-1))], prob=probs)
		}
		for (n in 1:length(probs)){
			for(i in 1:lm){
				p5[[n]][[i]] <- p3[[i]]$Qmaf[[n]]
				p4[[i]] <- index(zooinput)[[i]]
			}
		}
		
		
		out <- vector("list", length(probs))
		for (n in 1:length(probs)){
			out[[n]] <- data.frame((exp(p5[[n]])), p4)
			names(out[[n]]) <- c(paste("prob_",probs[[n]],"_Q_maf", sep=""), "Date")
			names(out)[[n]] <- paste("prob_",probs[[n]],"_Q_maf", sep="")
		}
	} else {}
	return(out)	
}

