# TODO: Add comment
# 
# Author: tnkocis
###############################################################################


#finding percentile interval of SVI
# g1 USGS 11377100
# g2 USGS 11407000
# g3 USGS 11418000
# g4 USGS 11446500

gauges_yeartype <- read.csv("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\yeartype_gauges.csv")
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

for (i in 1:length(yeartype)){	
pdf(file= paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\streamflow_yeartype_figs\\std\\",
				names(yeartype[i]),".pdf", sep=""))
plot(density(scale(yeartype[[i]]$Yearly$Summary$total_Q_yearly, center=TRUE, scale=TRUE), na.rm=TRUE),col=heat.colors(60),
		xlab="Total Q HydroYearly (maf)",ylab="Count",main=names(yeartype[i]))
dev.off()
}

dev.new()
pdf(file=paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\streamflow_yeartype_figs\\",
				"all_density_std",".pdf", sep=""))
plot(density(scale(yeartype[[1]]$Yearly$Summary$total_Q_yearly, center=TRUE, scale=TRUE), na.rm=TRUE),
		xlab="Total Q HydroYearly (maf)",ylab="Density",main="All Gauges >100 Years Data (standardized)",xlim=c(-6,6),ylim=c(0,2))
for(i in 2:length(yeartype)){
lines(density(scale(yeartype[[i]]$Yearly$Summary$total_Q_yearly, center=TRUE, scale=TRUE),na.rm=TRUE))
}
dev.off()


plot(density(yeartype[[1]]$Yearly$Summary$total_Q_yearly, na.rm=TRUE),
		xlab="Total Q HydroYearly (maf)",ylab="Density",main="All Gauges >100 Years Data (standardized)")

##make summary table and write to csv
yeartype_summary <- data.frame(gauge=rep(NA,32),mean_Q=rep(NA,32), median_Q=rep(NA,32), std_dev_Q=rep(NA,32),
		max=rep(NA,32), min=rep(NA,32))
for(i in 1:length(yeartype)){
	yeartype_summary$gauge[[i]] <- names(yeartype[i])
	yeartype_summary$mean_Q[[i]] <- yeartype[[i]]$Yearly$Summary$mean_Q[[1]]
	yeartype_summary$median_Q[[i]] <- yeartype[[i]]$Yearly$Summary$median_Q[[1]]
	yeartype_summary$std_dev_Q[[i]] <- yeartype[[i]]$Yearly$Summary$std_dev_mean_Q[[1]]
	yeartype_summary$max[[i]] <- yeartype[[i]]$Yearly$Summary$max_Q[[1]]
	yeartype_summary$min[[i]] <- yeartype[[i]]$Yearly$Summary$min_Q[[1]]
}

write.csv(yeartype_summary,
		file="C:\\Users\\tiffn_000\\Documents\\workspaces\\eclipse_workspace\\streamflow_yeartype_tables\\yeartype.csv")