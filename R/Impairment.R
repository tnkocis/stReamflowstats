# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


impfiles <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\dams")

impairments <- vector("list", length(impfiles))

for(i in 1:length(impfiles)){
	impairments[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\dams\\",impfiles[[i]],sep=""), header=TRUE, sep=";")
	if(length(impairments[[i]][[1]])==0){
		naob <- rep(NA, length(impairments[[i]]))
		names(naob) <- names(impairments[[i]])
		naob <- as.data.frame(t(naob))
		impairments[[i]] <- rbind.data.frame(impairments[[i]], naob)
	}
	namest <- basename(path=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\dams\\",impfiles[[i]],sep=""))
	impairments[[i]]$downstreamgauge <- strsplit(strsplit(namest,"_")[[1]][2],".", fixed=TRUE)[[1]][1]
}
for(i in 1:12){
	if(is.na(impairments[[i]]$XCoord)){
		impairments[[i]]$DSTR_GAUGE <- NA
	}else{
		impairments[[i]]$DSTR_GAUGE <- as.numeric(impairments[[i]]$downstreamgauge)
	}
}
impairmentsdf <- do.call(rbind.data.frame,impairments)

impairmentsdf <- impairmentsdf[-which(impairmentsdf$YEAR_BUILT ==0),]
rmimp <- c("11202000","11206500","11208000","11231500","11237000","11244000","11244000","11289000","11427750")
impairmentsdf <- impairmentsdf[-which(impairmentsdf$downstreamgauge%in%rmimp),]
noimp <- impairmentsdf$downstreamgauge[is.na(impairmentsdf$DSTR_GAUGE)]
impairmentsdf$YEAR_BUILT[which(is.na(impairmentsdf$DSTR_GAUGE))] <- 1800

damsearch <- function(input, damdf){
	gauge <- input[[1]]$site_no[[1]]
	damsgauge <- damdf[which(damdf$downstreamgauge==gauge),]
	return(damsgauge)
}

noimp <- impairmentsdf$downstreamgauge[is.na(impairmentsdf$DSTR_GAUGE)]

uniquegauges <- unique(impairmentsdf$downstreamgauge)
uniquemaxyear <- rep(NA, length(uniquegauges))
for(i in 1:length(uniquegauges)){
	uniquemaxyear[[i]] <- max(impairmentsdf$YEAR_BUILT[which(impairmentsdf$downstreamgauge==uniquegauges[[i]])])
}

maximpdf <- data.frame(gauge=uniquegauges, year=uniquemaxyear)