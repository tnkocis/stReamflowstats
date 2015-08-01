# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


MKT <- function(input){
	if(!require(Kendall)){
		install.packages("Kendall")
		library(Kendall)
	}
#
	if (missing(input))
		stop("Input data is required.")
	
	if(length(input[[1]])==6){
		
		MKL <- vector("list",6)
		names(MKL) <- names(input$daydf)
		for(i in 1:6){
			MKL[[i]] <- vector("list",2)
			names(MKL[[i]]) <- c("MKTday", "MKTvol")
			MKL[[i]][[1]] <- MannKendall(input$daydf[[i]]$dayfracabove)
			MKL[[i]][[2]] <- MannKendall(input$voldf[[i]]$volfracabove)
		}
		return(MKL)
	} else {
		MKday <- MannKendall(input$daydf$dayfracabove)
		MKvol <- MannKendall(input$voldf$volfracabove)
		out <- list(MKTday=MKday, MKTvol=MKvol)
		return(out)
	}
}
