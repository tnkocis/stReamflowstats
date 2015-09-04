# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


FreqAnalysis <- function(input, vday, index){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
#
	if (missing(input))
		stop("Input data is required, by hydrologic year.")
	if (missing(vday))
		stop("Vector containing days to max required")
	if (missing(index))
		stop("Index data location is required")
	
	z <- length(vday)
	daysmax <- vector("list", z)
	for(n in 1:z){
		daysmax[[n]] <- list()
		daysmax[[n]]$Date <- as.Date(rep(NA, length.out=length(input$Data)))
		daysmax[[n]]$Discharge_maf <- rep(NA, length.out=length(input$Data))
	}
	
	for(i in 1:length(input$Data)){
		ts.zoo <- zoo(input$Data[[i]]$Discharge_maf, input$Data[[i]]$Date)
		for(n in 1:z){
			ts.zoo.roll <- rollapply(ts.zoo, vday[[n]], mean, fill=NA, align=c("center"))
			daysmax[[n]]$Discharge_maf[[i]]<- coredata(ts.zoo.roll[which.max(ts.zoo.roll)])
			daysmax[[n]]$Date[[i]]<- index(ts.zoo.roll[which.max(ts.zoo.roll)])
		}
	}
	
	
	for(n in 1:z){
		daysmax[[n]] <- as.data.frame(daysmax[[n]])
		names(daysmax)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
	}
	
	daysmax2 <- list()
	daysmax2$df <- daysmax
	
	for(n in 1:z){
		daysmax[[n]] <- zoo(as.numeric(daysmax[[n]]$Discharge_maf), as.Date(daysmax[[n]]$Date))
		names(daysmax)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
	}
	
	daysmax2$zoo <- daysmax
	daysmax3 <- list()
	daysmax3$All <- daysmax2

	
	daysmaxXC <- vector("list", z)
	XCyears <- which(names(input$Data) %in% index$Year[which(index$Index==1)])
	for(n in 1:z){
		daysmaxXC[[n]] <- list()
		daysmaxXC[[n]]$Date <- as.Date(rep(NA, length.out=length(XCyears)))
		daysmaxXC[[n]]$Discharge_maf <- rep(NA, length.out=length(XCyears))
	}
	if(length(XCyears) == 0){
		daysmax3$C <- "No C Years in Input Dataset"
	} else {
		for(i in 1:length(XCyears)){
			ts.zoo <- zoo(input$Data[[XCyears[[i]]]]$Discharge_maf, input$Data[[XCyears[[i]]]]$Date)
			for(n in 1:z){
				ts.zoo.roll <- rollapply(ts.zoo, vday[[n]], mean, fill=NA, align=c("center"))
				daysmaxXC[[n]]$Discharge_maf[[i]]<- coredata(ts.zoo.roll[which.max(ts.zoo.roll)])
				daysmaxXC[[n]]$Date[[i]]<- index(ts.zoo.roll[which.max(ts.zoo.roll)])
			}
		}
		for(n in 1:z){
			daysmaxXC[[n]] <- as.data.frame(daysmaxXC[[n]])
			names(daysmaxXC)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXC2 <- list()
		daysmaxXC2$df <- daysmaxXC
		
		for(n in 1:z){
			daysmaxXC[[n]] <- zoo(as.numeric(daysmaxXC[[n]]$Discharge_maf), as.Date(daysmaxXC[[n]]$Date))
			names(daysmaxXC)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXC2$zoo <- daysmaxXC
		daysmax3$C <- daysmaxXC2
	}
############

	daysmaxXD <- vector("list", z)
	XDyears <- which(names(input$Data) %in% index$Year[which(index$Index==2)])
	for(n in 1:z){
		daysmaxXD[[n]] <- list()
		daysmaxXD[[n]]$Date <- as.Date(rep(NA, length.out=length(XDyears)))
		daysmaxXD[[n]]$Discharge_maf <- rep(NA, length.out=length(XDyears))
	}
	if(length(XDyears) == 0){
		daysmax3$D <- "No D Years in Input Dataset"
	} else {
		for(i in 1:length(XDyears)){
			ts.zoo <- zoo(input$Data[[XDyears[[i]]]]$Discharge_maf, input$Data[[XDyears[[i]]]]$Date)
			for(n in 1:z){
				ts.zoo.roll <- rollapply(ts.zoo, vday[[n]], mean, fill=NA, align=c("center"))
				daysmaxXD[[n]]$Discharge_maf[[i]]<- coredata(ts.zoo.roll[which.max(ts.zoo.roll)])
				daysmaxXD[[n]]$Date[[i]]<- index(ts.zoo.roll[which.max(ts.zoo.roll)])
			}
		}
		for(n in 1:z){
			daysmaxXD[[n]] <- as.data.frame(daysmaxXD[[n]])
			names(daysmaxXD)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXD2 <- list()
		daysmaxXD2$df <- daysmaxXD
		
		for(n in 1:z){
			daysmaxXD[[n]] <- zoo(as.numeric(daysmaxXD[[n]]$Discharge_maf), as.Date(daysmaxXD[[n]]$Date))
			names(daysmaxXD)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXD2$zoo <- daysmaxXD
		daysmax3$D <- daysmaxXD2
	}

	###############
	
	daysmaxXBN <- vector("list", z)
	XBNyears <- which(names(input$Data) %in% index$Year[which(index$Index==3)])
	for(n in 1:z){
		daysmaxXBN[[n]] <- list()
		daysmaxXBN[[n]]$Date <- as.Date(rep(NA, length.out=length(XBNyears)))
		daysmaxXBN[[n]]$Discharge_maf <- rep(NA, length.out=length(XBNyears))
	}
	if(length(XBNyears) == 0){
		daysmax3$BN <- "No BN Years in Input Dataset"
	} else {
		for(i in 1:length(XBNyears)){
			ts.zoo <- zoo(input$Data[[XBNyears[[i]]]]$Discharge_maf, input$Data[[XBNyears[[i]]]]$Date)
			for(n in 1:z){
				ts.zoo.roll <- rollapply(ts.zoo, vday[[n]], mean, fill=NA, align=c("center"))
				daysmaxXBN[[n]]$Discharge_maf[[i]]<- coredata(ts.zoo.roll[which.max(ts.zoo.roll)])
				daysmaxXBN[[n]]$Date[[i]]<- index(ts.zoo.roll[which.max(ts.zoo.roll)])
			}
		}
		for(n in 1:z){
			daysmaxXBN[[n]] <- as.data.frame(daysmaxXBN[[n]])
			names(daysmaxXBN)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXBN2 <- list()
		daysmaxXBN2$df <- daysmaxXBN
		
		for(n in 1:z){
			daysmaxXBN[[n]] <- zoo(as.numeric(daysmaxXBN[[n]]$Discharge_maf), as.Date(daysmaxXBN[[n]]$Date))
			names(daysmaxXBN)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXBN2$zoo <- daysmaxXBN
		daysmax3$BN <- daysmaxXBN2
	}
	
	##############
	
	daysmaxXAN <- vector("list", z)
	XANyears <- which(names(input$Data) %in% index$Year[which(index$Index==4)])
	for(n in 1:z){
		daysmaxXAN[[n]] <- list()
		daysmaxXAN[[n]]$Date <- as.Date(rep(NA, length.out=length(XANyears)))
		daysmaxXAN[[n]]$Discharge_maf <- rep(NA, length.out=length(XANyears))
	}
	if(length(XANyears) == 0){
		daysmax3$AN <- "No AN Years in Input Dataset"
	} else {
		for(i in 1:length(XANyears)){
			ts.zoo <- zoo(input$Data[[XANyears[[i]]]]$Discharge_maf, input$Data[[XANyears[[i]]]]$Date)
			for(n in 1:z){
				ts.zoo.roll <- rollapply(ts.zoo, vday[[n]], mean, fill=NA, align=c("center"))
				daysmaxXAN[[n]]$Discharge_maf[[i]]<- coredata(ts.zoo.roll[which.max(ts.zoo.roll)])
				daysmaxXAN[[n]]$Date[[i]]<- index(ts.zoo.roll[which.max(ts.zoo.roll)])
			}
		}
		for(n in 1:z){
			daysmaxXAN[[n]] <- as.data.frame(daysmaxXAN[[n]])
			names(daysmaxXAN)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXAN2 <- list()
		daysmaxXAN2$df <- daysmaxXAN
		
		for(n in 1:z){
			daysmaxXAN[[n]] <- zoo(as.numeric(daysmaxXAN[[n]]$Discharge_maf), as.Date(daysmaxXAN[[n]]$Date))
			names(daysmaxXAN)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXAN2$zoo <- daysmaxXAN
		daysmax3$AN <- daysmaxXAN2
	}
	############
	
	daysmaxXW <- vector("list", z)
	XWyears <- which(names(input$Data) %in% index$Year[which(index$Index==5)])
	for(n in 1:z){
		daysmaxXW[[n]] <- list()
		daysmaxXW[[n]]$Date <- as.Date(rep(NA, length.out=length(XWyears)))
		daysmaxXW[[n]]$Discharge_maf <- rep(NA, length.out=length(XWyears))
	}
	if(length(XWyears) == 0){
		daysmax3$W <- "No W Years in Input Dataset"
	} else {
		for(i in 1:length(XWyears)){
			ts.zoo <- zoo(input$Data[[XWyears[[i]]]]$Discharge_maf, input$Data[[XWyears[[i]]]]$Date)
			for(n in 1:z){
				ts.zoo.roll <- rollapply(ts.zoo, vday[[n]], mean, fill=NA, align=c("center"))
				daysmaxXW[[n]]$Discharge_maf[[i]]<- coredata(ts.zoo.roll[which.max(ts.zoo.roll)])
				daysmaxXW[[n]]$Date[[i]]<- index(ts.zoo.roll[which.max(ts.zoo.roll)])
			}
		}
		for(n in 1:z){
			daysmaxXW[[n]] <- as.data.frame(daysmaxXW[[n]])
			names(daysmaxXW)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXW2 <- list()
		daysmaxXW2$df <- daysmaxXW
		
		for(n in 1:z){
			daysmaxXW[[n]] <- zoo(as.numeric(daysmaxXW[[n]]$Discharge_maf), as.Date(daysmaxXW[[n]]$Date))
			names(daysmaxXW)[[n]] <- paste("X",vday[[n]],"DayMaxQ_maf", sep="")
		}
		
		daysmaxXW2$zoo <- daysmaxXW
		daysmax3$W <- daysmaxXW2
	}
				
	return(daysmax3)
}


FreqAnalysisMonthly <- function(input, vday, index){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
	if(!require(zoo)){
		install.packages("zoo")
		library(zoo)
	}
#
	if (missing(input))
		stop("Input data is required, by hydrologic year.")
	if (missing(vday))
		stop("Vector containing days to max required")
	if (missing(index))
		stop("Index data location is required")
	
	z <- length(vday)
	daysmax <- vector("list", z)
	for(n in 1:z){
		daysmax[[n]] <- list()
		for(k in 1:6){
			daysmax[[n]][[k]] <- list()
			daysmax[[n]][[k]]$Date <- as.Date(rep(NA, length.out=length(input$Data)))
			daysmax[[n]][[k]]$Discharge_maf <- rep(NA, length.out=length(input$Data))
		}
	}
	
	if(length(input$Data[[i]])==6){
		
		for(i in 1:length(input$Data)){
			for(k in 1:6){
				ts.zoo <- zoo(input$Data[[i]][[k]]$Discharge_maf, input$Data[[i]][[k]]$Date)
				for(n in 1:z){
					ts.zoo.roll <- rollapply(ts.zoo, vday[[n]], mean, fill=NA, align=c("center"))
						daysmax[[n]][[k]]$Discharge_maf[[i]]<- coredata(ts.zoo.roll[which.max(ts.zoo.roll)])
						daysmax[[n]][[k]]$Date[[i]]<- index(ts.zoo.roll[which.max(ts.zoo.roll)])
					
				}
			}
		}
		
		
		for(n in 1:z){
			for(k in 1:6){
				daysmax[[n]][[k]] <- as.data.frame(daysmax[[n]][[k]])
				name <- names(input$Data[[1]])[[k]]
				names(daysmax[[n]])[[k]] <- paste(name,"X",vday[[n]],"DayMaxQ_maf", sep="")
			}
		}
		
	
		df <- daysmax
		
		for(n in 1:z){
			for(k in 1:6){
				daysmax[[n]][[k]] <- zoo(as.numeric(daysmax[[n]][[k]]$Discharge_maf), as.Date(daysmax[[n]][[k]]$Date))
				name <- names(input$Data[[1]])[[k]]
				names(daysmax[[n]])[[k]] <- paste(name,"X",vday[[n]],"DayMaxQ_maf", sep="")
			}
		}
	} else {
		p <- length()
	}
	
	zoo <- daysmax
	
	daysmax3 <- list()
	daysmax3$All[[1]] <- df
	daysmax3$All[[2]] <- zoo
	daysmax3$All[[1]] <- daysmax3$All[[1]][[1]]
	daysmax3$All[[2]] <- daysmax3$All[[2]][[1]]
	names(daysmax3$All) <- c("df","zoo")
 return(daysmax3)
 
  }