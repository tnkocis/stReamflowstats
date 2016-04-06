# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

library(ggplot2)
library(scales)
plot1_thresh <- unimpaired_13_zthr_data$`11202001`$thresholds_maf$P90maf*1e6*43559.9/86400
plot1_data <- unimpaired_13_zthr_data$`11202001`$prep
plot1_datasub <- plot1_data
plot1_datasub$Discharge_cfs[which(plot1_datasub$Discharge_cfs<plot1_thresh)] <- NA
plot1 <- ggplot(plot1_data, aes(Date,Discharge_cfs))+geom_line()+
		scale_x_date(limits=c(as.Date("10-1-2010","%m-%d-%Y"),as.Date("9-30-2011","%m-%d-%Y")),
				labels=date_format("%b"), breaks=date_breaks("months"), name="Date")+
		scale_y_continuous(limits=c(0,1500), name="Discharge (cfs)")+ 
		geom_hline(aes(yintercept=(plot1_thresh),color="90th Percentile"))+
		geom_ribbon(data=plot1_datasub,
				aes(ymin=(plot1_thresh),ymax=Discharge_cfs))+
		ggtitle("Available Threshold (90th Percentile)")+
		scale_color_manual("", values=c("90th Percentile"="red"))+
		theme(axis.text.x=element_text(color="black", size=12),
			  axis.text.y=element_text(color="black", size=14),
			  axis.title.x = element_text(color="black", size=16),
			  axis.title.y = element_text(color="black", size=16),
			  title = element_text(color="black", size=16),
			  legend.position = "bottom",
			  legend.text= element_text(color="black", size=14))
ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\threshold_example.png", plot1, width=6,height=10, units="in")

index_data <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\data_index_plot.csv", header=TRUE)
index_data_sv <- index_data[,c(1:6)]
names(index_data_sv) <- c("WY","OctMar","AprJul","WYsum","Index","Yrtype")
index_data_sv$location <- "Sacramento Valley Index"
index_data_sj <- index_data[,c(1,7:11)]
names(index_data_sj) <- c("WY","OctMar","AprJul","WYsum","Index","Yrtype")
index_data_sj$location <- "San Joaquin Valley Index"
index_data_sv$Yrtype <- factor(index_data$SVYrtype, levels=c("W","AN","BN","D","C"))
index_data_sj$Yrtype <- factor(index_data$SJYrtype, levels=c("W","AN","BN","D","C"))
index_data <- rbind.data.frame(index_data_sj,index_data_sv)
index_plot <- ggplot(index_data, aes(WY,WYsum)) +geom_bar(stat="identity",aes(fill=Yrtype))+
					scale_fill_manual("Year Type", values=c("W"="dodgerblue4", "AN"="dodgerblue",
							"BN"="yellow","D"="orange","C"="firebrick")) +
					facet_wrap(~location, ncol=1, scales="free_y")+
					ylab("Unimpaired Runoff (maf)") + xlab("Water Year")+
					ggtitle("Water Year Type Indicies")+
					theme(legend.position="top",
						  axis.text.x=element_text(color="black", size=12),
						  axis.text.y=element_text(color="black", size=12),
						  axis.title.x = element_text(color="black", size=16),
						  axis.title.y = element_text(color="black", size=16),
						  title=element_text(color="black", size=16),
						  strip.text = element_text(color="black", size=14))
ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\indicies.png", index_plot, width=10,height=7, units="in")
		  

plot2_thresh <- spbatch[[3]]$thresholds_maf$P90maf*1e6*43559.9/86400
plot2_data <- spbatch[[3]]$prep
plot2_datasub <- plot2_data
plot2_datasub$Discharge_cfs[which(plot2_datasub$Discharge_cfs<plot2_thresh)] <- NA
plot2 <- ggplot(plot2_data, aes(Date,Discharge_cfs))+geom_line()+
		scale_x_date(limits=c(as.Date("12-1-1996","%m-%d-%Y"),as.Date("2-28-1997","%m-%d-%Y")),
				labels=date_format("%b"), breaks=date_breaks("months"), name="Date")+
		scale_y_continuous(limits=c(0,15000), name="Discharge (cfs)")+ 
		geom_hline(aes(yintercept=(plot2_thresh),color="90th Percentile"))+
		geom_ribbon(data=plot2_datasub,
			aes(ymin=(plot2_thresh),ymax=Discharge_cfs), fill="lightgreen",alpha=0.5)+
		ggtitle("Sample Metric Analysis")+
		scale_color_manual("", values=c("90th Percentile"="red"))+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=16),
				legend.position = "bottom",
				legend.text= element_text(color="black", size=14))
ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\sample_metric_analysis.png", plot2, width=10,height=8, units="in")

plot3_thresh <- spbatch[[1]]$thresholds_maf$P90maf*1e6*43559.9/86400
plot3_data <- spbatch[[1]]$prep
plot3_data96 <- plot3_data[plot3_data$Date>=as.Date("1996-10-01","%Y-%m-%d")&plot3_data$Date<=as.Date("1997-9-30","%Y-%m-%d"),]
plot3_data96$yeartype <- "Wet"
plot3_data03 <- plot3_data[plot3_data$Date>=as.Date("2003-10-01","%Y-%m-%d")&plot3_data$Date<=as.Date("2004-9-30","%Y-%m-%d"),]
plot3_data03$yeartype <- "Below Normal"
plot3_data07 <- plot3_data[plot3_data$Date>=as.Date("2007-10-01","%Y-%m-%d")&plot3_data$Date<=as.Date("2008-9-30","%Y-%m-%d"),]
plot3_data07$yeartype <- "Critical"
plot3_data <- rbind.data.frame(plot3_data96,plot3_data03,plot3_data07)
plot3_data$yeartype <- factor(plot3_data$yeartype, levels=c("Wet","Below Normal","Critical"))
plot3_datasub <- plot3_data
plot3_datasub$Discharge_cfs[which(plot3_datasub$Discharge_cfs<plot3_thresh)] <- NA
plot3 <- ggplot(plot3_data, aes(Date,Discharge_cfs))+geom_line()+
		scale_x_date(labels=date_format("%b"), name="Date")+
		scale_y_continuous(name="Discharge (cfs)")+ 
		geom_hline(aes(yintercept=(plot3_thresh),color="90th Percentile"))+
		facet_wrap(~yeartype, scales="free_x")+
		geom_ribbon(data=plot3_datasub,
				aes(ymin=(plot3_thresh),ymax=Discharge_cfs), fill="lightgreen",alpha=0.5)+
		ggtitle("Sample Hydrograph and Available Threshold Across 3 Year Types")+
		scale_color_manual("", values=c("90th Percentile"="red"))+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=16),
				legend.position = "bottom",
				legend.text= element_text(color="black", size=14),
				strip.text = element_text(color="black", size=14))
ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\sample_hydrograph_yeartypes.png", plot3, width=10,height=8, units="in")


monthlyfullrecord <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\figures\\SGMA_meeting\\monthly_fullrecord_numbers.csv")
monthlyfullrecord$month <- factor(monthlyfullrecord$month, levels=c("nov","dec","jan","feb","mar","apr"))
monthlyfullrecord$yeartype <- factor(monthlyfullrecord$yeartype, levels=c("C","D","BN","AN","W"))
monthlyfullrecord$volume_maf <- monthlyfullrecord$volume_taf/1000
monthlyfullrecord_sac <- monthlyfullrecord[which(monthlyfullrecord$gauge==11447650),]
monthlyfullrecord_sj <- monthlyfullrecord[which(monthlyfullrecord$gauge==11303500),]


monthlybarplot_sac <- ggplot(monthlyfullrecord_sac, aes(x = yeartype, y = volume_maf, width=0.9, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~month, ncol=6)+ 
		theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		labs(title="Total Flows Above 90th Percentile For Average Year Type With Non-Zero Flow
						Sacramento USGS 11447650 (45 years of Data, 1970-2014)
						", 
				x="\nYear Type", y="Magnitude of Average Year Type Total Flow (MAF)\n")+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=16),
				legend.position = "bottom",
				legend.text= element_text(color="black", size=14),
				strip.text = element_text(color="black", size=14))

ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\sac_monthlybarplot_fullrecordthresold.png", monthlybarplot_sac, width=11, height=8.5, units="in")

monthlybarplot_sj <- ggplot(monthlyfullrecord_sj, aes(x = yeartype, y = volume_maf, width=0.9, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~month, ncol=6)+ 
		theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		labs(title="Total Flows Above 90th Percentile For Average Year Type With Non-Zero Flow
						San Joaquin USGS 11303500 (25 years of Data, 1989-2014)
						", 
				x="\nYear Type", y="Magnitude of Average Year Type Total Flow (MAF)\n")+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=16),
				legend.position = "bottom",
				legend.text= element_text(color="black", size=14),
				strip.text = element_text(color="black", size=14))

ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\sj_monthlybarplot_fullrecordthresold.png", monthlybarplot_sj, width=11, height=8.5, units="in")

