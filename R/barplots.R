# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

library(ggplot2)
library(reshape2)
library(scales)

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


