# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

library(dplyr)
library(hydroTSM)
library(dataRetrieval)

load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\functions_5_13_16",".RData", sep=""))

sites <- list.files("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\Data_HCDN_gaguesii_US_active_60yrs\\Data\\", pattern=".txt")
sites <- unlist(strsplit(unlist(strsplit(sites,".csv")),"g"))
######################
spall <- sites[sites != ""]

cleanupHYfrom6MON <- function(inputhy, input6mon){
	if (missing(inputhy))
		stop("Input data to clean is required.")
	if (missing(input6mon))
		stop("Input data to clean is required.")
	for(i in length(inputhy$Data):1){
		if(sum(input6mon$Data[[i]]$Available, na.rm=TRUE)<165){
			inputhy$Data[[i]] <- NULL
		}
	}	
	return(inputhy)
}

for(i in 1:1){
	batchnum <- i
	spbatch <- spall
	spbatch <- spbatch[!is.na(spbatch)]
	
	spbatch_g <- spbatch
	
	
	txtgauges <- list.files("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\Data_HCDN_gaguesii_US_active_60yrs\\Data\\")
	txtgauges <- unlist(strsplit(txtgauges,".txt"))
	txtgauges <- txtgauges[txtgauges != ""]
	spbatch_g <- txtgauges
	
	
	spbatch <- vector("list", length(spbatch_g))
	for(z in 1:length(spbatch_g)){
		spbatch[[z]]$raw <- read.table(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\Data_HCDN_gaguesii_US_active_60yrs\\Data\\",spbatch_g[[z]],".txt",sep=""),sep="\t", header=TRUE,stringsAsFactors=FALSE)
		spbatch[[z]]$raw$Date <- as.Date(spbatch[[z]]$raw$datetime, "%Y-%m-%d")
		spbatch[[z]]$raw$X_00060_0003 <- spbatch[[z]]$raw[,min(which(grepl("00060_00003",names(spbatch[[z]]$raw))&!grepl("cd",names(spbatch[[z]]$raw))))]
		spbatch[[z]]$raw <- spbatch[[z]]$raw[which(spbatch[[z]]$raw$X_00060_0003!=""),]
		spbatch[[z]]$raw <- RemoveLeapDays(spbatch[[z]]$raw)

		spbatch[[z]]$Availability <- DataAvailability(spbatch[[z]]$raw)

	}
	names(spbatch) <- spbatch_g
	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\sn_data",batchnum,".RData",sep=""))
}


save.image(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\sn_data",".RData",sep=""))
save.image(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\SN_MKT_analysis\\sn_data",".RData",sep=""))

load(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\SN_MKT_analysis\\sn_data",".RData",sep=""))

library(ggplot2)
availability_plots <- vector("list",length(spbatch))
for(i in 1:length(spbatch)){
	spbatch[[i]]$Availability$yearly$year <- as.numeric(as.character(spbatch[[i]]$Availability$yearly$year))
	availability_plots[[i]] <- ggplot(spbatch[[i]]$Availability$yearly, aes(x=year,y=fraction_available)) +
			geom_area() + coord_flip() +scale_x_reverse(breaks=seq(1900,2016,10))+
			geom_line() + ggtitle(names(spbatch)[[i]])
	ggsave(availability_plots[[i]], file=paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\SN_MKT_analysis\\US_60yrs_availability_plots\\",names(spbatch)[[i]],".png", sep=""))
}
names(availability_plots) <- names(spbatch)

save.image(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\sn_data",".RData",sep=""))
save.image(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\SN_MKT_analysis\\sn_data",".RData",sep=""))

test <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\tesxt.csv",stringsAsFactors=FALSE)
test$Date <- as.Date(test$Date, "%m/%d/%Y")
test$X_00060_0003 <-test$value
test <- test[which(test$value!="Saknas"),]
test <- RemoveLeapDays(test)

testAvailability <- DataAvailability(test)

testAvailability$yearly$year <- as.numeric(as.character(testAvailability$yearly$year))
availability_plotstest <- ggplot(testAvailability$yearly, aes(x=year,y=fraction_available)) +
		geom_area() + coord_flip() +scale_x_reverse(breaks=seq(1900,2016,10))+
		geom_line() + ggtitle(names(spbatch)[[i]])
ggsave(availability_plotstest, file="C:\\Users\\tiffn_000\\Google Drive\\Bettina\\availability_plotstest.png")



####Canada Data#####
library(dplyr)
library(hydroTSM)
library(dataRetrieval)

load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\functions_5_13_16",".RData", sep=""))

#sites <- list.files("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\Canada 40 years\\Data\\", pattern=".csv")
#sites <- unlist(strsplit(unlist(strsplit(sites,".csv")),"g"))
#######################
#spall <- sites[sites != ""]

cleanupHYfrom6MON <- function(inputhy, input6mon){
	if (missing(inputhy))
		stop("Input data to clean is required.")
	if (missing(input6mon))
		stop("Input data to clean is required.")
	for(i in length(inputhy$Data):1){
		if(sum(input6mon$Data[[i]]$Available, na.rm=TRUE)<165){
			inputhy$Data[[i]] <- NULL
		}
	}	
	return(inputhy)
}

#for(i in 1:1){
#	batchnum <- i
#	spbatch <- spall
#	spbatch <- spbatch[!is.na(spbatch)]
#	
#	spbatch_g <- spbatch
	
	
	txtgauges <- list.files("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\SN_MKT_analysis\\Canada 40 years\\Data\\", pattern=".csv")
	txtgauges <- unlist(strsplit(txtgauges,".csv"))
	txtgauges <- txtgauges[txtgauges != ""]
	spbatch_g <- txtgauges
	
	
	spbatch <- vector("list", length(spbatch_g))
	for(z in 1:length(spbatch_g)){
		spbatch[[z]]$raw <- read.csv(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\SN_MKT_analysis\\Canada 40 years\\Data\\",spbatch_g[[z]],".csv",sep=""),stringsAsFactors=FALSE, skip=1)
		spbatch[[z]]$raw$Date <- as.Date(spbatch[[z]]$raw$Date, "%d/%m/%Y")
		spbatch[[z]]$raw$X_00060_0003 <- spbatch[[z]]$raw$Value
		spbatch[[z]]$raw <- spbatch[[z]]$raw[which(spbatch[[z]]$raw$X_00060_0003!=""),]
		spbatch[[z]]$raw <- RemoveLeapDays(spbatch[[z]]$raw)
		
		spbatch[[z]]$Availability <- DataAvailability(spbatch[[z]]$raw)
		
	}
	names(spbatch) <- spbatch_g
#	save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\sn_data_CANADA",batchnum,".RData",sep=""))
#}
#
record_length <- data.frame(station = names(spbatch),total_num_years_available=NA)
for(z in 1:length(spbatch)){
	record_length$total_num_years_available[[z]] <- sum(spbatch[[z]]$Availability$yearly$fraction_available)
}

library(ggplot2)
availability_plots <- vector("list",length(spbatch))
for(i in 1:length(spbatch)){
	spbatch[[i]]$Availability$yearly$year <- as.numeric(as.character(spbatch[[i]]$Availability$yearly$year))
	availability_plots[[i]] <- ggplot(spbatch[[i]]$Availability$yearly, aes(x=year,y=fraction_available)) +
									geom_area() + coord_flip() +scale_x_reverse(breaks=seq(1900,2016,10))+
									geom_line() + ggtitle(spbatch[[i]]$raw$ID[[1]])
	ggsave(availability_plots[[i]], file=paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\SN_MKT_analysis\\Canada 40 years\\availability plots\\",spbatch[[i]]$raw$ID[[1]],".png", sep=""))
}
names(availability_plots) <- names(spbatch)
		
save.image(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\sn_data_CANADA",".RData",sep=""))
save.image(paste("C:\\Users\\tiffn_000\\Google Drive\\Bettina\\SN_MKT_analysis\\sn_data_CANADA",".RData",sep=""))



