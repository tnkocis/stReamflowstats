# TODO: Add comment
# 
# Author: tiffnk
###############################################################################


#finding percentile interval of SVI
# g1 USGS 11377100
# g2 USGS 11407000
# g3 USGS 11418000
# g4 USGS 11446220

SVI_gauges <- list()
SVI_gauges$g1  <- readNWISdv(11377100,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
SVI_gauges$g2  <- readNWISdv(11407000,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
SVI_gauges$g3  <- readNWISdv(11418000,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")
SVI_gauges$g4  <- readNWISdv(11446220,"00060", startDate="1900-01-01",
		endDate=Sys.Date(), statCd="00003")

SVI <- read.csv("/Users/tiffnk/Documents/UCDAVIS/Lab/Streamflow_Analysis/svi.csv", header=TRUE)

SVI_quant <- list()
for(i in 1:9){
	SVI_quant[[i]] <- quantile(SVI$Index, probs=seq(0,1,0.005), type=i)
}

SVI_quant <- quantile(SVI$Index, probs=seq(0,1,0.005), type=)
SVI_quant_oct <- quantile(SVI$oct_mar, probs=seq(0,1,0.005))
SVI_quant_apr <- quantile(SVI$apr_jul, probs=seq(0,1,0.005))
SVI_quant_wy <- quantile(SVI$Wysum, probs=seq(0,1,0.005))

hist(SVI$Index, breaks=40, xlim=c(0,20))