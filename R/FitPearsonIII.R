# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

FitqPearsonIII <- function(zooinput, startparams, probs){
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
		stop("Input data is required, by hydrologic year.")
	if (missing(probs))
		stop("Nonexceedence Probability Vector is required, by hydrologic year.")
	if(anyNA(coredata(zooinput))){
		p3 <- NA
	}else{
	if (missing(startparams)) {

	
		# Annual maximum discharge data for the St Mary River at Stillwater Nova Scotia (Kite, 2004)
		# PIII fit to the logs of the discharges
		
		
		#for(i in 1:length(vday)){
#		lzooinput <- log(coredata(zooinput))
		#}
#		m <- mean(lzooinput)
#		v <- var(lzooinput)
#		s <- sd(lzooinput)
#		g <- e1071::skewness(lzooinput, type=1)
#		
#		# Correct the sample skew for bias using the recommendation of 
#		# Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and Log Pearson Type 3 distributions revisited." 
#		# Water Resources Reseach 13(2): 427-443, as used by Kite
#		
#		n <- length(coredata(zooinput))
#		g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)
#		
#		# We will use method of moment estimates as starting values for the MLE search
#		
#		my.shape <- (2/g)^2
#		my.scale <- sqrt(v)/sqrt(my.shape)
#		my.location <- m-sqrt(v * my.shape)
		
		lmoments <- lmom.ub(log(coredata(zooinput)))
		params <- parpe3(lmoments, checklmom=TRUE)
#		my.param <- list(shape=params$para[[1]], scale=params$para[[2]], location=params$para[[3]])
		
#		my.param <- list(shape=my.shape, scale=my.scale, location=my.location)
	
	} else {
	
#		my.param <- list(shape=startparams[[1]], scale=startparams[[2]], location=startparams[[3]])
	}

	
#	dPIII<<-function(x, shape, location, scale){ PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)}
#	pPIII<<-function(q, shape, location, scale){ PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)}
#	qPIII<<-function(p, shape, location, scale){ PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)}
#	
#	
#	
#	p3 <- fitdist(log(coredata(zooinput)), distr="PIII", method="mle", start=my.param)
	for (i in 1:length(probs)){
		p3[[i]] <- quape3(probs[[i]], para=params, paracheck=TRUE)
	}
}
	p5 <- data.frame(nonexcprobs=probs, Qmaf=p3)
return(p5)
}


