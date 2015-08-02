# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


glsFracAbove <- function(input){
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
	if(!require(nlme)){
		install.packages("nlme")
		library(nlme)
	}
	
	if(length(input$daydf)==6){
		
		glsmon <- vector("list",6)
		names(glsmon) <- names(input$daydf)
		for(i in 1:6){
			glsmon[[i]] <- vector("list",2)
			names(glsmon[[i]]) <- c("glsday", "glsvol")
			numsday <- input$daydf[[i]]$dayfracabove
			numsvol <- input$voldf[[i]]$volfracabove
			if(i == 1){
				styear  <- as.Date(paste(input$daydf[[i]]$startyear,"-11-1"), format="%Y-%m-%d")
			} else if(i == 2){
				styear  <- as.Date(paste(input$daydf[[i]]$startyear,"-12-1"), format="%Y-%m-%d")
			}else if(i == 3){
				styear  <- as.Date(paste(input$daydf[[i]]$startyear,"-1-1"), format="%Y-%m-%d")
			}else if(i == 4){
				styear  <- as.Date(paste(input$daydf[[i]]$startyear,"-2-1"), format="%Y-%m-%d")
			}else if(i == 5){
				styear  <- as.Date(paste(input$daydf[[i]]$startyear,"-3-1"), format="%Y-%m-%d")
			}else if(i == 6){
				styear  <- as.Date(paste(input$daydf[[i]]$startyear,"-4-1"), format="%Y-%m-%d")
			}
			glsmon[[i]][[1]] <- gls(numsday ~ styear, correlation=corARMA(p=1), method="ML")
			glsmon[[i]][[2]] <- gls(numsvol ~ styear, correlation=corARMA(p=1), method="ML")
		}
		return(glsmon)
		
	} else {
		numsday <- input$daydf$dayfracabove
		styear <- as.Date(paste(input$daydf$startyear,"-12-31"), format="%Y-%m-%d")
		numsvol <- input$voldf$volfracabove
		glsday <- gls(numsday ~styear, correlation=corARMA(p=1), method="ML")
		glsvol <- gls(numsvol ~styear, correlation=corARMA(p=1), method="ML")
		out <- list(glsday=glsday, glsvol=glsvol)
		return(out)
	}
	
	
}


