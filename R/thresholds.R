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
	percentile_maf <- quantile(discharge_maf, probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95),na.rm=TRUE)
	
	return(Percentile_maf=percentile_maf)

}