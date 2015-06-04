# TODO: Add comment
# 
# Author: tnkocis
###############################################################################


#finding percentile interval of SVI
# g1 USGS 11377100
# g2 USGS 11407000
# g3 USGS 11418000
# g4 USGS 11446500

gauges_yeartype <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\yeartype_gauges_pass3.csv")
yeartype <- list()
for (i in 1:length(gauges_yeartype$SiteNumber)){
	yeartype[[i]] <- list()
}
names(yeartype) <- gauges_yeartype$SiteNumber

for (i in 1:length(yeartype)){
	tryCatch({
				file_name <- paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\streamflow_yeartype_txt\\",gauges_yeartype$SiteNumber[[i]],".csv", sep="")
				write.csv(readNWISdv(paste(gauges_yeartype$SiteNumber[[i]]),"00060", startDate="1900-01-01",
								endDate=Sys.Date(), statCd="00003"), file=file_name, row.names=FALSE)}, 
			error=function(e){cat("ERROR :", conditionMessage(e),"\n")})
}


for (i in 1:length(yeartype)){
		yeartype[[i]]$raw <- readNWISdv(paste(gauges_yeartype$SiteNumber[[i]]),"00060", startDate="1900-01-01",
				endDate=Sys.Date(), statCd="00003")
	}
for (i in 1:length(yeartype)){	
		yeartype[[i]]$raw <- RemoveLeapDays(yeartype[[i]]$raw)
	}
for (i in 1:length(yeartype)){	
		yeartype[[i]]$prep <- prepdata(yeartype[[i]]$raw)
	}
for (i in 1:length(yeartype)){	
		yeartype[[i]]$Yearly <- SplitHydroYear(yeartype[[i]]$prep)
}
for (i in 1:length(yeartype)){	
	yeartype[[i]]$Availability <- DataAvailability(yeartype[[i]]$prep)
}

#for (i in 1:length(yeartype)){	
#pdf(file= paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\streamflow_yeartype_figs\\std\\",
#				names(yeartype[i]),".pdf", sep=""))
#plot(density(scale(yeartype[[i]]$Yearly$Summary$total_Q_yearly, center=TRUE, scale=TRUE), na.rm=TRUE),col=heat.colors(60),
#		xlab="Total Q HydroYearly (maf)",ylab="Count",main=names(yeartype[i]))
#dev.off()
#}
#
#dev.new()
#pdf(file=paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\streamflow_yeartype_figs\\",
#				"all_density_std",".pdf", sep=""))
#plot(density(scale(yeartype[[1]]$Yearly$Summary$total_Q_yearly, center=TRUE, scale=TRUE), na.rm=TRUE),
#		xlab="Total Q HydroYearly (maf)",ylab="Density",main="All Gauges >100 Years Data (standardized)",xlim=c(-6,6),ylim=c(0,1))
#for(i in 2:length(yeartype)){
#lines(density(scale(yeartype[[i]]$Yearly$Summary$total_Q_yearly, center=TRUE, scale=TRUE),na.rm=TRUE))
#}
#dev.off()
#
#
#plot(density(yeartype[[1]]$Yearly$Summary$total_Q_yearly, na.rm=TRUE),
#		xlab="Total Q HydroYearly (maf)",ylab="Density",main="All Gauges >100 Years Data (standardized)")

##make summary table and write to csv
yeartype_summary <- vector("list",length=(length(yeartype)+1))
names(yeartype_summary)[1] <- "all"
yeartype_summary$all <- data.frame(gauge=rep(NA,18),mean_Q=rep(NA,18), median_Q=rep(NA,18), std_dev_Q=rep(NA,18),
		max=rep(NA,18), min=rep(NA,18))
for(i in 1:length(yeartype)){
	yeartype_summary$all$gauge[[i]] <- names(yeartype[i])
	yeartype_summary$all$mean_Q[[i]] <- yeartype[[i]]$Yearly$Summary$mean_Q[[1]]
	yeartype_summary$all$median_Q[[i]] <- yeartype[[i]]$Yearly$Summary$median_Q[[1]]
	yeartype_summary$all$std_dev_Q[[i]] <- yeartype[[i]]$Yearly$Summary$std_dev_mean_Q[[1]]
	yeartype_summary$all$max[[i]] <- yeartype[[i]]$Yearly$Summary$max_Q[[1]]
	yeartype_summary$all$min[[i]] <- yeartype[[i]]$Yearly$Summary$min_Q[[1]]
}


for(i in 2:length(yeartype_summary)){
	names(yeartype_summary)[i] <- names(yeartype)[i-1]
	yeartype_summary[[i]]$Year <- names(yeartype[[i-1]]$Yearly$Data)
	yeartype_summary[[i]]$Q_maf_yearly <- yeartype[[i-1]]$Yearly$Summary$total_Q_yearly
	yeartype_summary[[i]]$Q_maf_yearly_std <- scale(yeartype_summary[[i]]$Q_maf_yearly, center=TRUE, scale=TRUE)
	yeartype_summary[[i]]$Year_Type <- rep(NA,length(yeartype_summary[[i]]$Year))
	yeartype_summary[[i]]$Stats <- yeartype[[i-1]]$Yearly$Summary[2:6]
	yeartype_summary[[i]]$Stats$Quantiles <- quantile(yeartype_summary[[i]]$Q_maf_yearly, probs=c(0.05,0.1,0.15,0.2,0.25,0.3,
					0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95), na.rm=TRUE )
	yeartype_summary[[i]]$Split_Mean <- list()
	yeartype_summary[[i]]$Split_Mean$total_yrs <- length(yeartype_summary[[i]]$Q_maf_yearly) - length(which(is.na(yeartype_summary[[i]]$Q_maf_yearly)))
	yeartype_summary[[i]]$Split_Mean$count_20per <- round(yeartype_summary[[i]]$Split_Mean$total_yrs*.2)
	yeartype_summary[[i]]$Split_Mean$order <- sort.list(yeartype_summary[[i]]$Q_maf_yearly)
#	yeartype_summary[[i]]$Split_Mean$above <- list()
#	yeartype_summary[[i]]$Split_Mean$below <- list()
#		for (n in 1:length(yeartype_summary[[i]]$Year)){
#			if(is.na(yeartype_summary[[i]]$Q_maf_yearly[[n]])){
#				yeartype_summary[[i]]$Split_Mean$above[[]] <- NA
#				
#			}
#		}
	yeartype_summary[[i]]$Stats_std <- list()
	yeartype_summary[[i]]$Stats_std$mean_Q <- mean(yeartype_summary[[i]]$Q_maf_yearly_std, na.rm=TRUE)
	yeartype_summary[[i]]$Stats_std$std_dev_mean_Q <- sd(yeartype_summary[[i]]$Q_maf_yearly_std, na.rm=TRUE)
	yeartype_summary[[i]]$Stats_std$median_Q <- median(yeartype_summary[[i]]$Q_maf_yearly_std, na.rm=TRUE)
	yeartype_summary[[i]]$Stats_std$min_Q <- min(yeartype_summary[[i]]$Q_maf_yearly_std, na.rm=TRUE)
	yeartype_summary[[i]]$Stats_std$max_Q <- max(yeartype_summary[[i]]$Q_maf_yearly_std, na.rm=TRUE)
	yeartype_summary[[i]]$Stats_std$Quantiles <- quantile(yeartype_summary[[i]]$Q_maf_yearly_std,probs=c(0.05,0.1,0.15,0.2,0.25,0.3,
					0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95), na.rm=TRUE)
	yeartype_summary[[i]]$Year_Type_std <- rep(NA,length(yeartype_summary[[i]]$Year))
	
	
	for(n in 1:length(yeartype_summary[[i]]$Year_Type)){
		if(is.na(yeartype_summary[[i]]$Q_maf_yearly[[n]])){
			yeartype_summary[[i]]$Year_Type[[n]] <- NA
			yeartype_summary[[i]]$Year_Type_num[[n]] <- NA
		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]>=yeartype_summary[[i]]$Stats$Quantiles[[16]]){
			yeartype_summary[[i]]$Year_Type[[n]] <- "W"	
			yeartype_summary[[i]]$Year_Type_num[[n]] <- 5
		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]>=yeartype_summary[[i]]$Stats$Quantiles[[12]] & yeartype_summary[[i]]$Q_maf_yearly[[n]]<yeartype_summary[[i]]$Stats$Quantiles[[16]]){
			yeartype_summary[[i]]$Year_Type[[n]] <- "AN"
			yeartype_summary[[i]]$Year_Type_num[[n]] <- 4
		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]>=yeartype_summary[[i]]$Stats$Quantiles[[8]] & yeartype_summary[[i]]$Q_maf_yearly[[n]]<yeartype_summary[[i]]$Stats$Quantiles[[12]]){
			yeartype_summary[[i]]$Year_Type[[n]] <- "BN"
			yeartype_summary[[i]]$Year_Type_num[[n]] <- 3
		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]>=yeartype_summary[[i]]$Stats$Quantiles[[4]] & yeartype_summary[[i]]$Q_maf_yearly[[n]]<yeartype_summary[[i]]$Stats$Quantiles[[8]]){
			yeartype_summary[[i]]$Year_Type[[n]] <- "D"
			yeartype_summary[[i]]$Year_Type_num[[n]] <- 2
		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]<yeartype_summary[[i]]$Stats$Quantiles[[4]]){
			yeartype_summary[[i]]$Year_Type[[n]] <- "CD"
			yeartype_summary[[i]]$Year_Type_num[[n]] <- 1
		} else {
			yeartype_summary[[i]]$Year_Type[[n]] <- "ERROR"
		}
	}
	
#	for(n in 1:length(yeartype_summary[[i]]$Year_Type)){
#		if(is.na(yeartype_summary[[i]]$Q_maf_yearly[[n]])){
#			yeartype_summary[[i]]$Year_Type[[n]] <- NA
#			yeartype_summary[[i]]$Year_Type_num[[n]] <- NA
#		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]>=yeartype_summary[[i]]$Stats$Quantiles[[1]] & yeartype_summary[[i]]$Q_maf_yearly[[n]]<(yeartype_summary[[i]]$Stats[[1]]+yeartype_summary[[i]]$Stats[[2]])){
#			yeartype_summary[[i]]$Year_Type[[n]] <- "W"	
#			yeartype_summary[[i]]$Year_Type_num[[n]] <- 4
#		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]>=(yeartype_summary[[i]]$Stats[[1]]+yeartype_summary[[i]]$Stats[[2]]) & yeartype_summary[[i]]$Q_maf_yearly[[n]]<(yeartype_summary[[i]]$Stats[[1]]+(2*yeartype_summary[[i]]$Stats[[2]]))){
#			yeartype_summary[[i]]$Year_Type[[n]] <- "VW"
#			yeartype_summary[[i]]$Year_Type_num[[n]] <- 5
#		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]>=(yeartype_summary[[i]]$Stats[[1]]+(2*yeartype_summary[[i]]$Stats[[2]]))){
#			yeartype_summary[[i]]$Year_Type[[n]] <- "EW"
#			yeartype_summary[[i]]$Year_Type_num[[n]] <- 6
#		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]<yeartype_summary[[i]]$Stats[[1]] & yeartype_summary[[i]]$Q_maf_yearly[[n]]>=(yeartype_summary[[i]]$Stats[[1]]-yeartype_summary[[i]]$Stats[[2]])){
#			yeartype_summary[[i]]$Year_Type[[n]] <- "D"
#			yeartype_summary[[i]]$Year_Type_num[[n]] <- 3
#		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]<(yeartype_summary[[i]]$Stats[[1]]-yeartype_summary[[i]]$Stats[[2]]) & yeartype_summary[[i]]$Q_maf_yearly[[n]]>=(yeartype_summary[[i]]$Stats[[1]]-(2*yeartype_summary[[i]]$Stats[[2]]))){
#			yeartype_summary[[i]]$Year_Type[[n]] <- "VD"
#			yeartype_summary[[i]]$Year_Type_num[[n]] <- 2
#		} else if(yeartype_summary[[i]]$Q_maf_yearly[[n]]<(yeartype_summary[[i]]$Stats[[1]]-(2*yeartype_summary[[i]]$Stats[[2]]))){
#			yeartype_summary[[i]]$Year_Type[[n]] <- "CD"
#			yeartype_summary[[i]]$Year_Type_num[[n]] <- 1
#		} else {
#			yeartype_summary[[i]]$Year_Type[[n]] <- "ERROR"
#		}
#	}
	
	for(n in 1:length(yeartype_summary[[i]]$Year_Type_std)){
		if(is.na(yeartype_summary[[i]]$Q_maf_yearly_std[[n]])){
			yeartype_summary[[i]]$Year_Type_std[[n]] <- NA
			yeartype_summary[[i]]$Year_Type_std_num[[n]] <- NA
		} else if(yeartype_summary[[i]]$Q_maf_yearly_std[[n]]>=yeartype_summary[[i]]$Stats_std[[1]] & yeartype_summary[[i]]$Q_maf_yearly_std[[n]]<(yeartype_summary[[i]]$Stats_std[[1]]+yeartype_summary[[i]]$Stats_std[[2]])){
			yeartype_summary[[i]]$Year_Type_std[[n]] <- "W"	
			yeartype_summary[[i]]$Year_Type_std_num[[n]] <- 4
		} else if(yeartype_summary[[i]]$Q_maf_yearly_std[[n]]>=(yeartype_summary[[i]]$Stats_std[[1]]+yeartype_summary[[i]]$Stats_std[[2]]) & yeartype_summary[[i]]$Q_maf_yearly_std[[n]]<(yeartype_summary[[i]]$Stats_std[[1]]+(2*yeartype_summary[[i]]$Stats_std[[2]]))){
			yeartype_summary[[i]]$Year_Type_std[[n]] <- "VW"
			yeartype_summary[[i]]$Year_Type_std_num[[n]] <- 5
		} else if(yeartype_summary[[i]]$Q_maf_yearly_std[[n]]>=(yeartype_summary[[i]]$Stats_std[[1]]+(2*yeartype_summary[[i]]$Stats_std[[2]]))){
			yeartype_summary[[i]]$Year_Type_std[[n]] <- "EW"
			yeartype_summary[[i]]$Year_Type_std_num[[n]] <- 6
		} else if(yeartype_summary[[i]]$Q_maf_yearly_std[[n]]<yeartype_summary[[i]]$Stats_std[[1]] & yeartype_summary[[i]]$Q_maf_yearly_std[[n]]>=(yeartype_summary[[i]]$Stats_std[[1]]-yeartype_summary[[i]]$Stats_std[[2]])){
			yeartype_summary[[i]]$Year_Type_std[[n]] <- "D"
			yeartype_summary[[i]]$Year_Type_std_num[[n]] <- 3
		} else if(yeartype_summary[[i]]$Q_maf_yearly_std[[n]]<(yeartype_summary[[i]]$Stats_std[[1]]-yeartype_summary[[i]]$Stats_std[[2]]) & yeartype_summary[[i]]$Q_maf_yearly_std[[n]]>=(yeartype_summary[[i]]$Stats_std[[1]]-(2*yeartype_summary[[i]]$Stats_std[[2]]))){
			yeartype_summary[[i]]$Year_Type_std[[n]] <- "VD"
			yeartype_summary[[i]]$Year_Type_std_num[[n]] <- 2
		} else if(yeartype_summary[[i]]$Q_maf_yearly_std[[n]]<(yeartype_summary[[i]]$Stats_std[[1]]-(2*yeartype_summary[[i]]$Stats_std[[2]]))){
			yeartype_summary[[i]]$Year_Type_std[[n]] <- "CD"
			yeartype_summary[[i]]$Year_Type_std_num[[n]] <- 1
		} else {
			yeartype_summary[[i]]$Year_Type_std[[n]] <- "ERROR"
		}
	}
}

YEARTYPE <- vector("list",length=length(yeartype_summary))
length_yeartype <- rep(NA,length(yeartype_summary))
for(i in 1:length(yeartype_summary)){
	length_yeartype[[i]] <- length(yeartype_summary[[i]][[1]])
}
names(YEARTYPE)[1] <- "Year"
names(YEARTYPE)[2:length(YEARTYPE)] <- names(yeartype_summary)[2:length(yeartype_summary)]
YEARTYPE$Year <- yeartype_summary[[which(length_yeartype==max(length_yeartype))[[1]]]]$Year

pos_year <- vector("list", length=(length(YEARTYPE)))
for(i in 2:length(YEARTYPE)){
	for(n in 1:length(YEARTYPE$Year)){
		if(length(which(yeartype_summary[[i]]$Year==YEARTYPE$Year[[n]]))<1){
			pos_year[[i]][[n]] <- NA
		} else {
			pos_year[[i]][[n]] <- which(yeartype_summary[[i]]$Year==YEARTYPE$Year[[n]])
		}
	}
}
for(i in 2:length(YEARTYPE)){
	for(n in 1: length(YEARTYPE$Year)){
		if(is.na(pos_year[[i]][[n]])){
			YEARTYPE[[i]][[n]] <- NA
		} else {
			YEARTYPE[[i]][[n]] <- yeartype_summary[[i]]$Year_Type_num[[pos_year[[i]][[n]]]]
		}
	}
}

YEARTYPEdf <- as.data.frame(YEARTYPE)
YEARTYPEdf$SJV_Averages <- rowMeans(YEARTYPEdf[,2:9], na.rm=TRUE)
YEARTYPEdf$SacV_Averages <- rowMeans(YEARTYPEdf[,10:length(YEARTYPE)], na.rm=TRUE)
YEARTYPEdf$SJV_Round <- rep(NA, length(YEARTYPEdf$SJV_Averages))
YEARTYPEdf$SacV_Round <- rep(NA, length(YEARTYPEdf$SacV_Averages))
for(i in 1:length(YEARTYPEdf$SJV_Averages)){
	if(YEARTYPEdf$SJV_Averages[[i]]>=1 & YEARTYPEdf$SJV_Averages[[i]]<1.5){
		YEARTYPEdf$SJV_Round[[i]] <- 1
	} else if(YEARTYPEdf$SJV_Averages[[i]]>1.5 & YEARTYPEdf$SJV_Averages[[i]]<2.5){
		YEARTYPEdf$SJV_Round[[i]] <- 2
	} else if(YEARTYPEdf$SJV_Averages[[i]]>2.5 & YEARTYPEdf$SJV_Averages[[i]]<3.5){
		YEARTYPEdf$SJV_Round[[i]] <- 3
	} else if(YEARTYPEdf$SJV_Averages[[i]]>3.5 & YEARTYPEdf$SJV_Averages[[i]]<4.5){
		YEARTYPEdf$SJV_Round[[i]] <- 4
	} else if(YEARTYPEdf$SJV_Averages[[i]]>4.5){
		YEARTYPEdf$SJV_Round[[i]] <- 5
	} else if(YEARTYPEdf$SJV_Averages[[i]]==1.5 & YEARTYPEdf$SJV_Round[[i-1]]!=2){
		YEARTYPEdf$SJV_Round[[i]] <- 2
	} else if(YEARTYPEdf$SJV_Averages[[i]]==1.5 & YEARTYPEdf$SJV_Round[[i-1]]==2){
		YEARTYPEdf$SJV_Round[[i]] <- 1
	} else if(YEARTYPEdf$SJV_Averages[[i]]==2.5 & YEARTYPEdf$SJV_Round[[i-1]]!=3){
		YEARTYPEdf$SJV_Round[[i]] <- 3
	} else if(YEARTYPEdf$SJV_Averages[[i]]==2.5 & YEARTYPEdf$SJV_Round[[i-1]]==3){
		YEARTYPEdf$SJV_Round[[i]] <- 2
	} else if(YEARTYPEdf$SJV_Averages[[i]]==3.5 & YEARTYPEdf$SJV_Round[[i-1]]!=4){
		YEARTYPEdf$SJV_Round[[i]] <- 4
	} else if(YEARTYPEdf$SJV_Averages[[i]]==3.5 & YEARTYPEdf$SJV_Round[[i-1]]==4){
		YEARTYPEdf$SJV_Round[[i]] <- 3
	} else if(YEARTYPEdf$SJV_Averages[[i]]==4.5 & YEARTYPEdf$SJV_Round[[i-1]]!=5){
		YEARTYPEdf$SJV_Round[[i]] <- 5
	} else if(YEARTYPEdf$SJV_Averages[[i]]==4.5 & YEARTYPEdf$SJV_Round[[i-1]]==5){
		YEARTYPEdf$SJV_Round[[i]] <- 4
	} else { 
		YEARTYPEdf$SJV_Round[[i]] <- NA 
	}
}
for(i in 1:length(YEARTYPEdf$SacV_Averages)){
	if(YEARTYPEdf$SacV_Averages[[i]]>=1 & YEARTYPEdf$SacV_Averages[[i]]<1.5){
		YEARTYPEdf$SacV_Round[[i]] <- 1
	} else if(YEARTYPEdf$SacV_Averages[[i]]>1.5 & YEARTYPEdf$SacV_Averages[[i]]<2.5){
		YEARTYPEdf$SacV_Round[[i]] <- 2
	} else if(YEARTYPEdf$SacV_Averages[[i]]>2.5 & YEARTYPEdf$SacV_Averages[[i]]<3.5){
		YEARTYPEdf$SacV_Round[[i]] <- 3
	} else if(YEARTYPEdf$SacV_Averages[[i]]>3.5 & YEARTYPEdf$SacV_Averages[[i]]<4.5){
		YEARTYPEdf$SacV_Round[[i]] <- 4
	} else if(YEARTYPEdf$SacV_Averages[[i]]>4.5){
		YEARTYPEdf$SacV_Round[[i]] <- 5
	} else if(YEARTYPEdf$SacV_Averages[[i]]==1.5 & YEARTYPEdf$SacV_Round[[i-1]]!=2){
		YEARTYPEdf$SacV_Round[[i]] <- 2
	} else if(YEARTYPEdf$SacV_Averages[[i]]==1.5 & YEARTYPEdf$SacV_Round[[i-1]]==2){
		YEARTYPEdf$SacV_Round[[i]] <- 1
	} else if(YEARTYPEdf$SacV_Averages[[i]]==2.5 & YEARTYPEdf$SacV_Round[[i-1]]!=3){
		YEARTYPEdf$SacV_Round[[i]] <- 3
	} else if(YEARTYPEdf$SacV_Averages[[i]]==2.5 & YEARTYPEdf$SacV_Round[[i-1]]==3){
		YEARTYPEdf$SacV_Round[[i]] <- 2
	} else if(YEARTYPEdf$SacV_Averages[[i]]==3.5 & YEARTYPEdf$SacV_Round[[i-1]]!=4){
		YEARTYPEdf$SacV_Round[[i]] <- 4
	} else if(YEARTYPEdf$SacV_Averages[[i]]==3.5 & YEARTYPEdf$SacV_Round[[i-1]]==4){
		YEARTYPEdf$SacV_Round[[i]] <- 3
	} else if(YEARTYPEdf$SacV_Averages[[i]]==4.5 & YEARTYPEdf$SacV_Round[[i-1]]!=5){
		YEARTYPEdf$SacV_Round[[i]] <- 5
	} else if(YEARTYPEdf$SacV_Averages[[i]]==4.5 & YEARTYPEdf$SacV_Round[[i-1]]==5){
		YEARTYPEdf$SacV_Round[[i]] <- 4
	} else { 
		YEARTYPEdf$SacV_Round[[i]] <- NA 
	}
}


#total_Q <-vector("list", length=(length(yeartype_summary)))
#total_Q[[1]] <- yeartype_summary[[which(length_yeartype==max(length_yeartype))]]$Year
#names(total_Q)[[1]] <- "Year"
#for (i in 2:length(yeartype_summary)){
#	for(n in 1: length(total_Q$Year)){
#		if(is.na(pos_year[[i]][[n]])){
#			total_Q[[i]][[n]] <- NA
#		} else {
#			total_Q[[i]][[n]] <- yeartype_summary[[i]]$Q_maf_yearly[[pos_year[[i]][[n]]]]
#		}
#	}
#	names(total_Q)[[i]] <- names(yeartype_summary)[[i]]
#}
#total_Q_df <- as.data.frame(total_Q)
#
#write.csv(total_Q_df,
#		file="C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\streamflow_yeartype_tables\\total_Q_edited.csv")
write.csv(YEARTYPEdf,
		file="C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\streamflow_yeartype_tables\\yeartype_edited_classified_20per_finaltest.csv")