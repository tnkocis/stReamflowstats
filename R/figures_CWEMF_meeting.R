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


delta_merge$test <- c(NA,delta_merge$code_description[1:(length(delta_merge$code_description)-1)])
delta_merge$change <- delta_merge$test!=delta_merge$code_description
delta_merge$change[[1]] <- TRUE

delta_change <- data.frame(date=delta_merge$date[delta_merge$change],code_description=delta_merge$code_description[delta_merge$change])
delta_change$datemax <- c(delta_change$date[2:length(delta_change$date)],delta_merge$date[[length(delta_merge$date)]])
delta_change$code_description[which(delta_change$code_description=="excess with Fish and Export/Inflow concerns")] <- "excess with Export to Inflow ratio concerns"
delta_change$code_description <- factor(delta_change$code_description)
delta_change$description <- as.character(delta_change$code_description)
delta_change$description[which(delta_change$code_description=="excess")] <- "true excess"
delta_change$description[which(delta_change$code_description=="excess with fish concerns"|delta_change$code_description=="excess with Export to Inflow ratio concerns")] <- "excess with restrictions"
delta_change$description[which(delta_change$code_description=="balance with no storage withdrawals"|delta_change$code_description=="balanced with storage withdrawals")] <- "balanced"
delta_change$description <- factor(delta_change$description)

delta_ribbon <- data.frame(date=delta_merge$date,sac_dis_TAF=delta_merge$sac_discharge_TAF,sj_dis_TAF=delta_merge$sj_discharge_TAF)
delta_ribbon$sac_thresh <- delta_ribbon$sac_dis_TAF
delta_ribbon$sac_thresh[which(delta_ribbon$sac_dis_TAF<delta_merge$sac_90_full)] <- NA
delta_ribbon$sac_90_full <- delta_merge$sac_90_full

delta_ribbon$sj_thresh <- delta_ribbon$sj_dis_TAF
delta_ribbon$sj_thresh[which(delta_ribbon$sj_dis_TAF<delta_merge$sj_90_full)] <- NA
delta_ribbon$sj_90_full <- delta_merge$sj_90_full

write.csv(delta_merge, file="C:\\Users\\tnkocis\\Google Drive\\figures\\updated_figures\\delta_merge.csv")
write.csv(delta_change, file="C:\\Users\\tnkocis\\Google Drive\\figures\\updated_figures\\delta_change.csv")
write.csv(delta_ribbon, file="C:\\Users\\tnkocis\\Google Drive\\figures\\updated_figures\\delta_ribbon.csv")
delta_merge <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\delta_merge.csv", header=TRUE)
delta_change<- read.csv("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\delta_change.csv", header=TRUE)
delta_ribbon <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\figures\\updated_figures\\delta_ribbon.csv", header=TRUE)
delta_change$date <- as.Date(delta_change$date)
delta_merge$date <- as.Date(delta_merge$date)
delta_ribbon$date <- as.Date(delta_ribbon$date)
delta_change$datemax <- as.Date(delta_change$datemax)


delta_plot_sac <-ggplot() +
		geom_rect(data=delta_change, aes(xmin=as.Date(date), xmax=as.Date(datemax), ymin=0, ymax=Inf, fill=description), alpha=0.4)+
		scale_fill_manual("Delta Status", values=c("true excess"="gray46","excess with restrictions"="gray46","balanced"="gray88"))+
		scale_color_manual("",values=c("90th Percentile"="black"))+
		geom_line(data=delta_merge,aes(x=date,y=sac_discharge_TAF),color="blue")+
		geom_ribbon(data=delta_merge, aes(x=date,ymin=0,ymax=sac_discharge_TAF),fill="gray26",color="gray26")+
		geom_ribbon(data=delta_ribbon, aes(x=date,ymin=sac_90_full,ymax=sac_thresh),fill="blue",color="blue")+
		scale_y_continuous(limits=c(0,240),breaks=seq(0,240,20))+
		labs(title="Delta Conditions Compared to Discharge at Sacramento USGS 11447650 ", 
				x="Date", y="Discharge (TAF)\n")+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=16),
				legend.position = "top",
				legend.text= element_text(color="black", size=14))+
		geom_hline(aes(yintercept=0))+
		geom_segment(aes(x=as.Date(delta_merge$date[[1]]),y=delta_merge$sac_90_full,xend=as.Date(delta_merge$date[[length(delta_merge$date)]]),
						yend=delta_merge$sac_90_full,color="90th Percentile"), size=0.7)+
		scale_x_date(date_breaks="3 years", date_labels="%Y")
		

ggsave("C:\\Users\\tnkocis\\Google Drive\\figures\\updated_figures\\deltaplot_sac.png",delta_plot_sac, width=11,height=8)

delta_plot_sj <- ggplot() +
		geom_rect(data=delta_change, aes(xmin=as.Date(date), xmax=as.Date(datemax), ymin=0, ymax=Inf, fill=description), alpha=0.4)+
		scale_fill_manual("Delta Status", values=c("true excess"="gray46","excess with restrictions"="gray46","balanced"="gray88"))+
		scale_color_manual("",values=c("90th Percentile"="black"))+
		geom_line(data=delta_merge,aes(x=date,y=sj_discharge_TAF),color="blue")+
		geom_ribbon(data=delta_merge, aes(x=date,ymin=0,ymax=sj_discharge_TAF),fill="gray26",color="gray26")+
		geom_ribbon(data=delta_ribbon, aes(x=date,ymin=sj_90_full,ymax=sj_thresh),fill="blue",color="blue")+
		scale_y_continuous(limits=c(0,110),breaks=seq(0,110,10))+
		labs(title="Delta Conditions Compared to Discharge at San Joaquin USGS 11303500", 
				x="Date", y="Discharge (TAF)\n")+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=16),
				legend.position = "top",
				legend.text= element_text(color="black", size=14))+
		geom_hline(aes(yintercept=0))+
		geom_segment(aes(x=as.Date(delta_merge$date[[1]]),y=delta_merge$sj_90_full,xend=as.Date(delta_merge$date[[length(delta_merge$date)]]),
						yend=delta_merge$sj_90_full,color="90th Percentile"), size=0.7)+
		scale_x_date(date_breaks="3 years", date_labels="%Y")


ggsave("C:\\Users\\tnkocis\\Google Drive\\figures\\updated_figures\\deltaplot_sj.png",delta_plot_sj, width=11,height=8)


barplotdf <- data.frame(vol=c(4159.270177,0,6144,3125,1178.181816,951.3388414,0,7027.173542,0,11179,5002,1772.495865,1018.561982,0,
				1048.462808,0,1267,393,0,0,0,2172.826443,0,2601,674,0,0,0), yeartype=c(rep(c("All"," ","Wet","Above Normal","Below Normal","Dry","Critical"),4)), 
		gauge_period=c(rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB",7),
				rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR",7),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB",7),
				rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR",7)))
barplotdf$vol_MAF <- barplotdf$vol/1000
barplotdf$yeartype <- factor(barplotdf$yeartype, levels=c("Critical","Dry","Below Normal","Above Normal","Wet"," ","All"))
ggplot() + geom_bar(data=barplotdf, aes(x=yeartype,y=vol_MAF,fill=yeartype), stat="identity") + 
		facet_wrap(~gauge_period, nrow=1)+scale_fill_manual(values=c("lightcoral","lemonchiffon","mediumaquamarine",
						"dodgerblue3","darkblue","white","chartreuse4"))+
		scale_y_continuous(limits=c(0,12),breaks=seq(0,12,1))+
		labs(title="Total Flow Above 90th Percentile For Average Year Type With Non-Zero Flows", 
				x="\n Year Type", y="Discharge (MAF)\n")+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=18),
				legend.position = "right",
				legend.text= element_text(color="black", size=14),
				strip.text = element_text(color="black", size=14))+
		guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W"," ","All"))

barplotdf <- data.frame(vol=c(4159.270177,0,6144,3125,1178.181816,951.3388414,0,7027.173542,0,11179,5002,1772.495865,1018.561982,0,
				1048.462808,0,1267,393,0,0,0,2172.826443,0,2601,674,0,0,0), yeartype=c(rep(c("All"," ","Wet","Above Normal","Below Normal","Dry","Critical"),4)), 
		gauge_period=c(rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",7),
				rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",7),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",7),
				rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",7)))
barplotdf$vol_MAF <- barplotdf$vol/1000
barplotdf$yeartype <- factor(barplotdf$yeartype, levels=c("Critical","Dry","Below Normal","Above Normal","Wet"," ","All"))
volplot <- ggplot() + geom_bar(data=barplotdf, aes(x=yeartype,y=vol_MAF,fill=yeartype), stat="identity") + 
		facet_wrap(~gauge_period, nrow=1)+scale_fill_manual(values=c("lightcoral","lemonchiffon","mediumaquamarine",
						"dodgerblue3","darkblue","white","chartreuse4"))+
		scale_y_continuous(limits=c(0,12),breaks=seq(0,12,1))+
		labs(title="Total Flow Above 90th Percentile For Average Year Type With Non-Zero Flows", 
				x="\n Year Type", y="Discharge (MAF)\n")+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=18),
				legend.position = "right",
				legend.text= element_text(color="black", size=14),
				strip.text = element_text(color="black", size=14))+
		guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W"," ","All"))

barplotdf_yrs <- data.frame(yeartype=c(rep("All",17),rep("All",26),rep("Wet",0),rep("Wet",13),
				rep("Above Normal",0),rep("Above Normal",7), rep("Below Normal",2),rep("Below Normal",3),
				rep("Dry",6),rep("Dry",3),rep("Critical",9),rep("Critical",0),
				rep("All",16),rep("All",27),rep("Wet",0),rep("Wet",13),
				rep("Above Normal",0),rep("Above Normal",7), rep("Below Normal",2),rep("Below Normal",3),
				rep("Dry",5),rep("Dry",4),rep("Critical",9),rep("Critical",0),
				rep("All",16),rep("All",8),rep("Wet",2),rep("Wet",6),
				rep("Above Normal",1),rep("Above Normal",2), rep("Below Normal",2),rep("Below Normal",0),
				rep("Dry",4),rep("Dry",0),rep("Critical",7),rep("Critical",0),
				rep("All",9),rep("All",15),rep("Wet",1),rep("Wet",7),
				rep("Above Normal",1),rep("Above Normal",2), rep("Below Normal",2),rep("Below Normal",0),
				rep("Dry",4),rep("Dry",0),rep("Critical",7),rep("Critical",0)),
		wwo=c(rep("Years Without Flow",17),rep("Years With Flow",26),rep("Years Without Flow",0),rep("Years With Flow",13), #sac3
				rep("Years Without Flow",0),rep("Years With Flow",7), rep("Years Without Flow",2),rep("Years With Flow",3), #sac3
				rep("Years Without Flow",6),rep("Years With Flow",3),rep("Years Without Flow",9),rep("Years With Flow",0), #sac3
				rep("Years Without Flow",16),rep("Years With Flow",27),rep("Years Without Flow",0),rep("Years With Flow",13), #sac6
				rep("Years Without Flow",0),rep("Years With Flow",7), rep("Years Without Flow",2),rep("Years With Flow",3), #sac6
				rep("Years Without Flow",5),rep("Years With Flow",4),rep("Years Without Flow",9),rep("Years With Flow",0), #sac6
				rep("Years Without Flow",16),rep("Years With Flow",8),rep("Years Without Flow",2),rep("Years With Flow",6), #sj3
				rep("Years Without Flow",1),rep("Years With Flow",2), rep("Years Without Flow",2),rep("Years With Flow",0), #sj3
				rep("Years Without Flow",4),rep("Years With Flow",0),rep("Years Without Flow",7),rep("Years With Flow",0),  #sj3
				rep("Years Without Flow",9),rep("Years With Flow",15),rep("Years Without Flow",1),rep("Years With Flow",7),  #sj6
				rep("Years Without Flow",1),rep("Years With Flow",2), rep("Years Without Flow",2),rep("Years With Flow",0), #sj6
				rep("Years Without Flow",4),rep("Years With Flow",0),rep("Years Without Flow",7),rep("Years With Flow",0)),#sj6
		gauge_period=c(rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",17),rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",26),rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",0),rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",13), #sac3
				rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",0),rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",7), rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",2),rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",3), #sac3
				rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",6),rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",3),rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",9),rep("SACRAMENTO RIVER \n USGS 11447650\n DEC - FEB\n 1970-2014",0), #sac3
				rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",16),rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",27),rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",0),rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",13), #sac6
				rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",0),rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",7), rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",2),rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",3), #sac6
				rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",5),rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",4),rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",9),rep("SACRAMENTO RIVER \n USGS 11447650\n NOV - APR\n 1970-2014",0), #sac6
				rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",16),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",8),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",2),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",6), #sj3
				rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",1),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",2), rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",2),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",0), #sj3
				rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",4),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",0),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",7),rep("SAN JOAQUIN RIVER \n USGS 11303500\n DEC - FEB\n 1989-2014",0),  #sj3
				rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",9),rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",15),rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",1),rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",7),  #sj6
				rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",1),rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",2), rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",2),rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",0), #sj6
				rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",4),rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",0),rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",7),rep("SAN JOAQUIN RIVER \n USGS 11303500\n NOV - APR\n 1989-2014",0))#sj6
		) 
barplotdf_yrs$yeartype <- factor(barplotdf_yrs$yeartype, levels=c("Critical","Dry","Below Normal","Above Normal","Wet"," ","All"))
yrsplot <- ggplot(barplotdf_yrs,aes(yeartype,y=(..count..),fill=wwo)) +geom_bar(color="black") + facet_wrap(~gauge_period, nrow=1)+
		scale_fill_manual(values=c("darkblue","gray22","purple"))+
		scale_y_continuous(limits=c(0,43),breaks=seq(0,44,2))+
		labs(title="Number of Years With and Without Flow Above the 90th Percentile", 
				x="\n Year Type", y="Number of Years \n")+
		theme(axis.text.x=element_text(color="black", size=14),
				axis.text.y=element_text(color="black", size=14),
				axis.title.x = element_text(color="black", size=16),
				axis.title.y = element_text(color="black", size=16),
				title = element_text(color="black", size=18),
				legend.position = "top",
				legend.text= element_text(color="black", size=16),
				strip.text = element_text(color="black", size=14))+
		guides(fill=guide_legend(title=" ", reverse=TRUE))+
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W"," ","All"), drop=FALSE)+
		stat_count(geom = "text",
				aes(label = (..count..)),
				vjust = 1.2, color="white") 

ggsave(yrsplot, file="C:\\users\\tiffn_000\\Google Drive\\figures\\updated_figures\\years_plot.png", width=11, height=8, units="in")
ggsave(volplot, file="C:\\users\\tiffn_000\\Google Drive\\figures\\updated_figures\\volume_plot.png", width=11, height=8, units="in")

library(dplyr)
barplotdf_yrs_test <- ddply(barplotdf_yrs, .(yeartype), 
		transform, pos = cumsum(Frequency) - (0.5 * Frequency)
)
#delta_change$datemax <- delta_change$datemax-1