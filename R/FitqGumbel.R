# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

FitqGumbel <- function(zooinput, startparams){ #, prob){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
#	if(!require(fitdistrplus)){
#		install.packages("fitdistrplus")
#		library(fitdistrplus)
#	}
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
#	if (missing(prob))
#		stop("Nonexceedence Probability Vector is required, by hydrologic year.")
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
			params <- pargum(lmoments, checklmom=TRUE)
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
		dgum<<-function(x, xi, alpha){ lmomco::pdfgum(x=x, para=vec2par(c( xi, alpha), type="gum"))}
		pgum<<-function(q, xi, alpha){ lmomco::cdfgum(x=q, para=vec2par(c( xi, alpha), type="gum"))}
		qgum<<-function(p, xi, alpha){ lmomco::quagum(f=p, para=vec2par(c( xi, alpha), type="gum"), paracheck=TRUE)}
		
		
#		p3 <- fitdist(log(coredata(zooinput)), distr="gum", method="mle",start=list(xi=params$para[[1]], alpha=params$para[[2]]))
	p3 <- plotdist(log(coredata(zooinput)), distr="gum", para=list(xi=params$para[[1]], alpha=params$para[[2]]))
#	p3 <- rep(NA, length(prob))
#	for (i in 1:length(prob)){
#		p3[[i]] <- quape3(prob[[i]], para=params, paracheck=TRUE)
#	}
#}
#	p5 <- data.frame(nonexcprob=prob, Qmaf=p3)
#return(p5)
		return(p3)
	}
}

