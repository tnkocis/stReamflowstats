# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


MonthlyThresholdQ <- function(input, prob){
	
	if (missing(input))
		stop("Input monthly data output list is required.")
	p <- vector("list",6)
	for(k in 1:6){
		p[[k]] <- vector("list",6)
		names(p[[k]]) <- c("NOV", "DEC","JAN","FEB", "MAR","APR")
		
	}
	names(p) <- names(input)[3:8]
	for(n in 1:6){
		if(length(input[[n+2]])==1){
			p[[n]] <- NA
		} else{
		for(i in 1:6){
			yearvec <- unique(format(input[[n+2]][[i]]$Data$Date, "%Y"))
			p[[n]][[i]] <- data.frame(year=as.numeric(yearvec), Qmaf=rep(NA,length(yearvec)))
				for(j in 1:length(yearvec)){
					yrloc <- which(format(input[[n+2]][[i]]$Data$Date, "%Y")== yearvec[[j]])
					p[[n]][[i]]$Qmaf[[j]] <- quantile(input[[n+2]][[i]]$Data[[1]][yrloc], probs=prob, na.rm=TRUE)[[1]]
				}
				names(p[[n]][[i]])[[2]] <- paste("thresh",prob,"Qmaf",sep="_")
			}
		}
	}
return(p)	

	
}
