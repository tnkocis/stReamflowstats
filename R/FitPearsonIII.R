# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

FitPearsonIII <- function(input, vday, vmovewidth, startparams){
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
	if (missing(input))
		stop("Input data is required, by hydrologic year.")
	if (missing(vday))
		stop("Vector containing days to max required")
	if (missing(vmovewidth))
		stop("Vector containing width of moving window required")
	
# Annual maximum discharge data for the St Mary River at Stillwater Nova Scotia (Kite, 2004)
# PIII fit to the logs of the discharges


#for(i in 1:length(vday)){
	linput <- log(input$Discharge_maf)
#}
m <- mean(linput)
v <- var(linput)
s <- sd(linput)
g <- e1071::skewness(linput, type=1)

# Correct the sample skew for bias using the recommendation of 
# Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and Log Pearson Type 3 distributions revisited." 
# Water Resources Reseach 13(2): 427-443, as used by Kite

n <- length(input$Discharge_maf)
g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)

# We will use method of moment estimates as starting values for the MLE search

my.shape <- (2/g)^2
my.scale <- sqrt(v)/sqrt(my.shape)
my.location <- m-sqrt(v * my.shape)

my.param <- list(shape=my.shape, scale=my.scale, location=my.location)


dPIII<<-function(x, shape, location, scale){ PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)}
pPIII<<-function(q, shape, location, scale){ PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)}
qPIII<<-function(p, shape, location, scale){ PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)}



  p3 <- fitdist(linput, distr="PIII", method="mle", start=my.param)

return(p3)
}


