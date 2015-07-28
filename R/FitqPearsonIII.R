# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

FitqPearsonIII <- function(zooinput, startparams, prob){ #, prob){
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
#	if(!require(PearsonDS)){
#		install.packages("PearsonDS")
#		library(PearsonDS)
#	}
#	
	if(!require(lmomco)){
		install.packages("lmomco")
		library(lmomco)
	}
	
	if (missing(zooinput))
		stop("Input data is required, by hydrologic year.")
	if (missing(prob))
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
		lmoments <- lmom.ub(log(coredata(zooinput)*1.23348184e9))
		params <- parpe3(lmoments, checklmom=TRUE)
########## Lmomco consistent with Handbook of Hydrology by Maidment
#		standard <- list(mu=params$para[[1]], sigma=params$para[[2]], gamma=params$para[[3]])
#		tau <- standard$mu - (2*(standard$sigma/standard$gamma))
#		sigma <- 4/(standard$gamma^2)
#		beta <- 2/(standard$sigma*standard$gamma)
#		my.param <- list(shape=tau, scale=sigma, location=beta)
	
	} else {
	
#		my.param <- list(shape=startparams[[1]], scale=startparams[[2]], location=startparams[[3]])
	}

	
#	dPIII<<-function(x, shape, location, scale){ PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)}
#	pPIII<<-function(q, shape, location, scale){ PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)}
#	qPIII<<-function(p, shape, location, scale){ PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)}
####using Lmomco equations	
	dPIII<<-function(x, mu, sigma, gamma){ lmomco::pdfpe3(x, para=vec2par(c(mu, sigma, gamma), type="pe3"))}
	pPIII<<-function(q, mu, sigma, gamma){ lmomco::cdfpe3(x=q, para=vec2par(c(mu, sigma, gamma), type="pe3"))}
	qPIII<<-function(p, mu, sigma, gamma){ lmomco::quape3(f=p, para=vec2par(c(mu, sigma, gamma), type="pe3"), paracheck=TRUE)}
	
	
#	p3 <- fitdist(log(coredata(zooinput)*1e6), distr="PIII", method="mle",start=list(mu=params$para[[1]], sigma=params$para[[2]], gamma=params$para[[3]]))
###	p3 <- plotdist(log(coredata(zooinput)), distr="PIII", para=list(mu=params$para[[1]], sigma=params$para[[2]], gamma=params$para[[3]]))
	p4 <- rep(NA, length(prob))
	for (i in 1:length(prob)){
		p4[[i]] <- qPIII(prob[[i]], mu=params$para[[1]], sigma=params$para[[2]], gamma=params$para[[3]])
	}

	p5 <- data.frame(nonexcprob=prob, Qm3=p4)
#return(p5)
	#p6 <- list(plots=p3, quantiles=p4)
return(p5)
}
}

