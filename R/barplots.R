# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

library(ggplot2)
library(reshape2)
library(scales)


for(i in 1:length(active)){
	vol <- rep(NA, 5)
	avgvol <- rep(NA, 5)
	frac <- rep(NA, 5)
	for(n in 1:5){
		vol[[n]] <- active[[i]]$Winter_3mon[[n+3]]$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
		blah <- data.frame(year=as.numeric(format(active[[i]]$Winter_3mon[[n+3]]$Data$Date, "%Y")),
				month=as.numeric(format(active[[i]]$Winter_3mon[[n+3]]$Data$Date, "%m")))
		blah[-which(blah$month!=1),]
		avgvol[[n]] <-  vol[[n]]/(length(unique(blah$year)))
		frac[[n]] <- vol[[n]]/active[[i]]$Winter_3mon$All$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
	}

	d <- data.frame(yeartype=c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
			Volume_TAF=vol*1000,
			Average_Volume_TAF=avgvol*1000,
			Frac=frac,
			gauge=rep(names(active)[[i]]))
	d["yeartype"] <- factor(d$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
	
	active[[i]]$Plots$stacked_90 <- ggplot(d, aes(x = gauge, y = Frac, fill = yeartype, width=0.25)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
							November to January", 
					x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
			guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
#			scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
			theme(axis.text.x=element_text(size=8))
	
	active[[i]]$Plots$avgvol_90 <- ggplot(d, aes(x = yeartype, y = Average_Volume_TAF, width=0.5, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
			theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
			labs(title="Total Flows Above 90th Percentile For Average Year Type
							November to January", 
					x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
			scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 
	ggsave(plot=active[[i]]$Plots$stacked_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Active_gauges\\stacked_bar\\",d$gauge[[1]],"_stacked_90.pdf"),
			width=8.5, height=11)
	ggsave(plot=active[[i]]$Plots$avgvol_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Active_gauges\\bar\\",d$gauge[[1]],"_avgvol_90.pdf"),
			width=8.5, height=11)
	
}

for(i in 1:length(unimpaired)){
	vol <- rep(NA, 5)
	avgvol <- rep(NA, 5)
	frac <- rep(NA, 5)
	for(n in 1:5){
		vol[[n]] <- unimpaired[[i]]$Winter_6mon[[n+3]]$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
		blah <- data.frame(year=as.numeric(format(unimpaired[[i]]$Winter_6mon[[n+3]]$Data$Date, "%Y")),
				month=as.numeric(format(unimpaired[[i]]$Winter_6mon[[n+3]]$Data$Date, "%m")))
		blah[-which(blah$month!=1),]
		avgvol[[n]] <-  vol[[n]]/(length(unique(blah$year)))
		frac[[n]] <- vol[[n]]/unimpaired[[i]]$Winter_6mon$All$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
	}
	
	d <- data.frame(yeartype=c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
			Volume_TAF=vol*1000,
			Average_Volume_TAF=avgvol*1000,
			Frac=frac,
			gauge=rep(names(unimpaired)[[i]]))
	d["yeartype"] <- factor(d$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
	
	unimpaired[[i]]$Plots$stacked_90 <- ggplot(d, aes(x = gauge, y = Frac, fill = yeartype, width=0.25)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
							November to April", 
					x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
			guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
#			scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
			theme(axis.text.x=element_text(size=8))
	
	unimpaired[[i]]$Plots$avgvol_90 <- ggplot(d, aes(x = yeartype, y = Average_Volume_TAF, width=0.5, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
			theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
			labs(title="Total Flows Above 90th Percentile For Average Year Type
							November to April", 
					x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
			scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 
	ggsave(plot=unimpaired[[i]]$Plots$stacked_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Unimpaired_70\\stacked_bar\\",d$gauge[[1]],"_stacked_90.pdf"),
			width=8.5, height=11)
	ggsave(plot=unimpaired[[i]]$Plots$avgvol_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Unimpaired_70\\bar\\",d$gauge[[1]],"_avgvol_90.pdf"),
			width=8.5, height=11)
	
}

for(i in 1:length(saclower)){
	vol <- rep(NA, 5)
	avgvol <- rep(NA, 5)
	frac <- rep(NA, 5)
	for(n in 1:5){
		vol[[n]] <- saclower[[i]]$Winter_3mon[[n+3]]$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
		blah <- data.frame(year=as.numeric(format(saclower[[i]]$Winter_3mon[[n+3]]$Data$Date, "%Y")),
				month=as.numeric(format(saclower[[i]]$Winter_3mon[[n+3]]$Data$Date, "%m")))
		blah[-which(blah$month!=1),]
		avgvol[[n]] <-  vol[[n]]/(length(unique(blah$year)))
		frac[[n]] <- vol[[n]]/saclower[[i]]$Winter_3mon$All$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
	}
	
	d <- data.frame(yeartype=c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
			Volume_TAF=vol*1000,
			Average_Volume_TAF=avgvol*1000,
			Frac=frac,
			gauge=rep(names(saclower)[[i]]))
	d["yeartype"] <- factor(d$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
	
	saclower[[i]]$Plots$stacked_90 <- ggplot(d, aes(x = gauge, y = Frac, fill = yeartype, width=0.25)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
							November to January", 
					x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
			guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
#			scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
			theme(axis.text.x=element_text(size=8))
	
	saclower[[i]]$Plots$avgvol_90 <- ggplot(d, aes(x = yeartype, y = Average_Volume_TAF, width=0.5, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
			theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
			labs(title="Total Flows Above 90th Percentile For Average Year Type
							November to January", 
					x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
			scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 
	ggsave(plot=saclower[[i]]$Plots$stacked_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Sacramento\\Lower\\stacked_bar\\",d$gauge[[1]],"_stacked_90.pdf"),
			width=8.5, height=11)
	ggsave(plot=saclower[[i]]$Plots$avgvol_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Sacramento\\Lower\\bar\\",d$gauge[[1]],"_avgvol_90.pdf"),
			width=8.5, height=11)
	
}

for(i in 1:length(sacupper)){
	vol <- rep(NA, 5)
	avgvol <- rep(NA, 5)
	frac <- rep(NA, 5)
	for(n in 1:5){
		vol[[n]] <- sacupper[[i]]$Winter_3mon[[n+3]]$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
		blah <- data.frame(year=as.numeric(format(sacupper[[i]]$Winter_3mon[[n+3]]$Data$Date, "%Y")),
				month=as.numeric(format(sacupper[[i]]$Winter_3mon[[n+3]]$Data$Date, "%m")))
		blah[-which(blah$month!=1),]
		avgvol[[n]] <-  vol[[n]]/(length(unique(blah$year)))
		frac[[n]] <- vol[[n]]/sacupper[[i]]$Winter_3mon$All$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
	}
	
	d <- data.frame(yeartype=c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
			Volume_TAF=vol*1000,
			Average_Volume_TAF=avgvol*1000,
			Frac=frac,
			gauge=rep(names(sacupper)[[i]]))
	d["yeartype"] <- factor(d$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
	
	sacupper[[i]]$Plots$stacked_90 <- ggplot(d, aes(x = gauge, y = Frac, fill = yeartype, width=0.25)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
							November to January", 
					x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
			guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
#			scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
			theme(axis.text.x=element_text(size=8))
	
	sacupper[[i]]$Plots$avgvol_90 <- ggplot(d, aes(x = yeartype, y = Average_Volume_TAF, width=0.5, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
			theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
			labs(title="Total Flows Above 90th Percentile For Average Year Type
							November to January", 
					x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
			scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 
	ggsave(plot=sacupper[[i]]$Plots$stacked_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Sacramento\\Upper\\stacked_bar\\",d$gauge[[1]],"_stacked_90.pdf"),
			width=8.5, height=11)
	ggsave(plot=sacupper[[i]]$Plots$avgvol_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Sacramento\\Upper\\bar\\",d$gauge[[1]],"_avgvol_90.pdf"),
			width=8.5, height=11)
	
}

for(i in 1:length(SJ)){
	vol <- rep(NA, 5)
	avgvol <- rep(NA, 5)
	frac <- rep(NA, 5)
	for(n in 1:5){
		vol[[n]] <- SJ[[i]]$Winter_3mon[[n+3]]$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
		blah <- data.frame(year=as.numeric(format(SJ[[i]]$Winter_3mon[[n+3]]$Data$Date, "%Y")),
				month=as.numeric(format(SJ[[i]]$Winter_3mon[[n+3]]$Data$Date, "%m")))
		blah[-which(blah$month!=1),]
		avgvol[[n]] <-  vol[[n]]/(length(unique(blah$year)))
		frac[[n]] <- vol[[n]]/SJ[[i]]$Winter_3mon$All$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
	}
	
	d <- data.frame(yeartype=c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
			Volume_TAF=vol*1000,
			Average_Volume_TAF=avgvol*1000,
			Frac=frac,
			gauge=rep(names(SJ)[[i]]))
	d["yeartype"] <- factor(d$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
	
	SJ[[i]]$Plots$stacked_90 <- ggplot(d, aes(x = gauge, y = Frac, fill = yeartype, width=0.25)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
							November to January", 
					x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
			guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
#			scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
			theme(axis.text.x=element_text(size=8))
	
	SJ[[i]]$Plots$avgvol_90 <- ggplot(d, aes(x = yeartype, y = Average_Volume_TAF, width=0.5, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
			theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
			labs(title="Total Flows Above 90th Percentile For Average Year Type
							November to January", 
					x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
			scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 
	ggsave(plot=SJ[[i]]$Plots$stacked_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\San_Joaquin\\stacked_bar\\",d$gauge[[1]],"_stacked_90.pdf"),
			width=8.5, height=11)
	ggsave(plot=SJ[[i]]$Plots$avgvol_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\San_Joaquin\\bar\\",d$gauge[[1]],"_avgvol_90.pdf"),
			width=8.5, height=11)
	
}

for(i in 1:length(SJ)){
	vol <- rep(NA, 5)
	avgvol <- rep(NA, 5)
	frac <- rep(NA, 5)
	for(n in 1:5){
		vol[[n]] <- SJ[[i]]$Winter_3mon[[n+3]]$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
		blah <- data.frame(year=as.numeric(format(SJ[[i]]$Winter_3mon[[n+3]]$Data$Date, "%Y")),
				month=as.numeric(format(SJ[[i]]$Winter_3mon[[n+3]]$Data$Date, "%m")))
		blah[-which(blah$month!=1),]
		avgvol[[n]] <-  vol[[n]]/(length(unique(blah$year)))
		frac[[n]] <- vol[[n]]/SJ[[i]]$Winter_3mon$All$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
	}
	
	d <- data.frame(yeartype=c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
			Volume_TAF=vol*1000,
			Average_Volume_TAF=avgvol*1000,
			Frac=frac,
			gauge=rep(names(SJ)[[i]]))
	d["yeartype"] <- factor(d$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
	
	SJ[[i]]$Plots$stacked_90 <- ggplot(d, aes(x = gauge, y = Frac, fill = yeartype, width=0.25)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
							November to January", 
					x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
			guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
#			scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
			theme(axis.text.x=element_text(size=8))
	
	SJ[[i]]$Plots$avgvol_90 <- ggplot(d, aes(x = yeartype, y = Average_Volume_TAF, width=0.5, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
			theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
			labs(title="Total Flows Above 90th Percentile For Average Year Type
							November to January", 
					x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
			scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 
	ggsave(plot=SJ[[i]]$Plots$stacked_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\San_Joaquin\\stacked_bar\\",d$gauge[[1]],"_stacked_90.pdf"),
			width=8.5, height=11)
	ggsave(plot=SJ[[i]]$Plots$avgvol_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\San_Joaquin\\bar\\",d$gauge[[1]],"_avgvol_90.pdf"),
			width=8.5, height=11)
	
}

for(i in 1:length(tulare60)){
	vol <- rep(NA, 5)
	avgvol <- rep(NA, 5)
	frac <- rep(NA, 5)
	for(n in 1:5){
		vol[[n]] <- tulare60[[i]]$Winter_3mon[[n+3]]$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
		blah <- data.frame(year=as.numeric(format(tulare60[[i]]$Winter_3mon[[n+3]]$Data$Date, "%Y")),
				month=as.numeric(format(tulare60[[i]]$Winter_3mon[[n+3]]$Data$Date, "%m")))
		blah[-which(blah$month!=1),]
		avgvol[[n]] <-  vol[[n]]/(length(unique(blah$year)))
		frac[[n]] <- vol[[n]]/tulare60[[i]]$Winter_3mon$All$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
	}
	
	d <- data.frame(yeartype=c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
			Volume_TAF=vol*1000,
			Average_Volume_TAF=avgvol*1000,
			Frac=frac,
			gauge=rep(names(tulare60)[[i]]))
	d["yeartype"] <- factor(d$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
	
	tulare60[[i]]$Plots$stacked_90 <- ggplot(d, aes(x = gauge, y = Frac, fill = yeartype, width=0.25)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
							November to January", 
					x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
			guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
#			scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
			theme(axis.text.x=element_text(size=8))
	
	tulare60[[i]]$Plots$avgvol_90 <- ggplot(d, aes(x = yeartype, y = Average_Volume_TAF, width=0.5, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
			theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
			labs(title="Total Flows Above 90th Percentile For Average Year Type
							November to January", 
					x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
			scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 
	ggsave(plot=tulare60[[i]]$Plots$stacked_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Tulare\\t60\\stacked_bar\\",d$gauge[[1]],"_stacked_90.pdf"),
			width=8.5, height=11)
	ggsave(plot=tulare60[[i]]$Plots$avgvol_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Tulare\\t60\\bar\\",d$gauge[[1]],"_avgvol_90.pdf"),
			width=8.5, height=11)
	
}

for(i in 1:length(tulare80)){
	vol <- rep(NA, 5)
	avgvol <- rep(NA, 5)
	frac <- rep(NA, 5)
	for(n in 1:5){
		vol[[n]] <- tulare80[[i]]$Winter_3mon[[n+3]]$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
		blah <- data.frame(year=as.numeric(format(tulare80[[i]]$Winter_3mon[[n+3]]$Data$Date, "%Y")),
				month=as.numeric(format(tulare80[[i]]$Winter_3mon[[n+3]]$Data$Date, "%m")))
		blah[-which(blah$month!=1),]
		avgvol[[n]] <-  vol[[n]]/(length(unique(blah$year)))
		frac[[n]] <- vol[[n]]/tulare80[[i]]$Winter_3mon$All$Stats$Thresholds$Totals$Volume_Abv_acfte6[[7]]
	}
	
	d <- data.frame(yeartype=c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
			Volume_TAF=vol*1000,
			Average_Volume_TAF=avgvol*1000,
			Frac=frac,
			gauge=rep(names(tulare80)[[i]]))
	d["yeartype"] <- factor(d$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))
	
	tulare80[[i]]$Plots$stacked_90 <- ggplot(d, aes(x = gauge, y = Frac, fill = yeartype, width=0.25)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
							November to January", 
					x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
			guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
#			scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
			theme(axis.text.x=element_text(size=8))
	
	tulare80[[i]]$Plots$avgvol_90 <- ggplot(d, aes(x = yeartype, y = Average_Volume_TAF, width=0.5, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
			theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
			scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
			labs(title="Total Flows Above 90th Percentile For Average Year Type
							November to January", 
					x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
			scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 
	ggsave(plot=tulare80[[i]]$Plots$stacked_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Tulare\\t80\\stacked_bar\\",d$gauge[[1]],"_stacked_90.pdf"),
			width=8.5, height=11)
	ggsave(plot=tulare80[[i]]$Plots$avgvol_90, 
			filename=paste("C:\\Users\\tiffn_000\\Documents\\Figures\\Plots\\Tulare\\t80\\bar\\",d$gauge[[1]],"_avgvol_90.pdf"),
			width=8.5, height=11)
	
}

#dev.new <- function(width = 7, height = 7) 
#{ platform <- sessionInfo()$platform if (grepl("linux",platform)) 
#			{ x11(width=width, height=height) } 
#			else if (grepl("pc",platform)) 
#			{ windows(width=width, height=height) } 
#			else if (grepl("apple", platform)) 
#			{ quartz(width=width, height=height) } }

d <- data.frame(yeartype = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
		"11389500" = c(0.020150689,0.047697314,0.085786837,0.174020099,0.672345084),
		"11274000" = c(0,0.000669948,0.00088804,0.149870466,0.840618653),
		"11186001" = c(0,0,0,0.110539691,0.889420053)
)
names(d) <- c("yeartype", "Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)")


m <- melt(d, id.vars = "yeartype", variable.name = "gauge", value.name = "frac")

m$Volume <- c(169.2562,286.1676667,635.7958235,1686.560769,3025.381429,0,0.862,1.008186471,160.6944444,523.3529032,0,0,0,4.881555556,22.80645161)

m["yeartype"] <- factor(m$yeartype, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))


ggplot(m, aes(x = gauge, y = frac, fill = yeartype, width=0.5)) + geom_bar(type = "stacked", stat = "identity") + labs(title="Flows Above 90th Percentile Over 80 Years
November to January", 
		x="Gauge", y="Fraction of Total Flows Above 90th Percentile")+ 
guides(fill=guide_legend(title="Year Type", reverse=TRUE)) + scale_fill_brewer(palette = "YlGnBu")+
scale_x_discrete(labels=c("Sacramento (11389500)", "San Joaquin (11274000)", "Tulare (11186001)"))+
theme(axis.text.x=element_text(size=8))


ggplot(m, aes(x = yeartype, y = Volume, width=0.9, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
		theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		labs(title="Total Flows Above 90th Percentile For Average Year Type (100 Years of Data)
November to January", 
				x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
		scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 


