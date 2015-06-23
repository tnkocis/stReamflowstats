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
	percentile_maf <- quantile(discharge_maf, probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE)
	names(percentile_maf) <- paste("P",c(5,10,20,25,50,75,90,95),sep="")
	percentile_maf <- as.list(percentile_maf)
	
	return(Percentile_maf=as.data.frame(percentile_maf))

}