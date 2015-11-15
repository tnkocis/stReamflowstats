# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


impfiles <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\scatter_sites\\all_damsCSV")

impairments <- vector("list", length(impfiles))

for(i in 1:length(impfiles)){
	impairments[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\scatter_sites\\all_damsCSV\\",impfiles[[i]],sep=""), header=TRUE, sep=";")
	if(length(impairments[[i]][[1]])==0){
		naob <- rep(NA, length(impairments[[i]]))
		names(naob) <- names(impairments[[i]])
		naob <- as.data.frame(t(naob))
		impairments[[i]] <- rbind.data.frame(impairments[[i]], naob)
	}
	namest <- basename(path=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\scatter_sites\\all_damsCSV\\",impfiles[[i]],sep=""))
	impairments[[i]]$downstreamgauge <- strsplit(strsplit(namest,"_")[[1]][2],".", fixed=TRUE)[[1]][1]
}

impairmentsdf <- do.call(rbind.data.frame,impairments)

damsearch <- function(input, damdf){
	gauge <- input[[1]]$site_no[[1]]
	damsgauge <- damdf[which(damdf$downstreamgauge==gauge),]
	return(damsgauge)
}

noimp <- impairmentsdf$downstreamgauge[is.na(impairmentsdf$DSTR_GAUGE)]