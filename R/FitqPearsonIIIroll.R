# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


FitqPearsonIIIroll <- function(zooinput,  movewidth, startparams, probs, npoints){
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
	if (missing(npoints))
		stop("Minimum number of final points required.")
	 if(missing(startparams)){
		L <- length(coredata(zooinput))
#		output <- list()
#		output$shape <- rep(NA,length.out=L)
#		output$scale <- rep(NA, length.out=L)
#		output$location <- rep(NA, length.out=L)
#		output$location <- as.Date(rep(NA, length.out=L))
#		
		if(L == 0){
			out <- c("Length of data is 0")
		} else if(L < ((npoints-1)+movewidth)){
		 	out <- c("Not enough data points to satistfy minimum points required.")
		} else if(L >= ((npoints-1)+movewidth)){
			lm <- L-(movewidth-1)
			p3 <- vector("list", length(probs))
			p5 <- vector("list", length(probs))
			p4 <- as.Date(rep(NA,lm))
			for (i in 1:lm){
				p3[[i]] <- FitqPearsonIII(zooinput=zooinput[i:(i+(movewidth-1))], prob=probs)
			}
			for (n in 1:length(probs)){
				for(i in 1:lm){
					p5[[n]][[i]] <- p3[[i]]$Qm3[[n]]
					p4[[i]] <- index(zooinput)[[i]]
					}
				}
			
			
			out <- vector("list", length(probs))
			for (n in 1:length(probs)){
				out[[n]] <- data.frame((exp(p5[[n]])), p4)
				names(out[[n]]) <- c(paste("prob_",probs[[n]],"_Q_m3", sep=""), "Date")
				names(out)[[n]] <- paste("prob_",probs[[n]],"_Q_m3", sep="")
			}
		} else {out <- NA}
	} else {}
return(out)	
}

FitqPearsonIIIrollMonthly <- function(inputzoolist,  movewidth, startparams, probs, npoints){
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
	if (missing(inputzoolist))
		stop("Input Zoo list is required.")
	if (missing(movewidth))
		stop("numeric width of moving window required")
	if (missing(probs))
		stop("Non Exceedence Probability vector required")
	if (missing(npoints))
		stop("Minimum number of final points required.")
	if(missing(startparams)){
		out <- vector("list",6)
		p3 <- vector("list", 6)
		p5 <- vector("list", 6)
		p4 <- vector("list", 6)
		for(k in 1:6){
			input <- inputzoolist[[k]]
			L <- length(coredata(input))
	#		output <- list()
	#		output$shape <- rep(NA,length.out=L)
	#		output$scale <- rep(NA, length.out=L)
	#		output$location <- rep(NA, length.out=L)
	#		output$location <- as.Date(rep(NA, length.out=L))
	#		
			if(L == 0){
				out <- c("Length of data is 0")
			} else if(L < ((npoints-1)+movewidth)){
				out <- c("Not enough data points to satistfy minimum points required.")
			} else if(L >= ((npoints-1)+movewidth)){
				lm <- L-(movewidth-1)
				p3[[k]] <- vector("list", lm)
				p5[[k]] <- vector("list", length(probs))
				p4[[k]] <- as.Date(rep(NA,lm))
				for (o in 1:lm){
					p3[[k]][[o]]<- FitqPearsonIII(zooinput=input[o:(o+(movewidth-1))], prob=probs)
				}
				for (n in 1:length(probs)){
					for(i in 1:lm){
						p5[[k]][[n]][[i]] <- p3[[k]][[i]]$Qm3[[n]]
						p4[[k]][[i]] <- index(input)[[i]]
					}
				}
				
				
				out[[k]] <- vector("list", length(probs))
				for (n in 1:length(probs)){
					out[[k]][[n]] <- data.frame((exp(p5[[k]][[n]])), p4[[k]])
					names(out[[k]][[n]]) <- c(paste("prob_",probs[[n]],"_Q_m3", sep=""), "Date")
					names(out[[k]])[[n]] <- paste("prob_",probs[[n]],"_Q_m3", sep="")
				}
				names(out) <- c("NOV","DEC","JAN","FEB","MAR","APR")
			} else {out <- NA}
		}
	} else {}
	return(out)	
}
