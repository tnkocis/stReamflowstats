# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

max <- data.frame(maxvalue=rep(NA,length(spbatch$`11452500`$Winter_6mon$Data)),
		maxdate=as.Date(rep(NA,length(spbatch$`11452500`$Winter_6mon$Data))),
		sthyyear=rep(NA,length(spbatch$`11452500`$Winter_6mon$Data)))

for(i in 1:length(spbatch$`11452500`$Winter_6mon$Data)){
	max$maxvalue[[i]] <- spbatch$`11452500`$Winter_6mon$Data[[i]]$Discharge_cfs[which.max(spbatch$`11452500`$Winter_6mon$Data[[i]]$Discharge_cfs)]
	max$maxdate[[i]] <- spbatch$`11452500`$Winter_6mon$Data[[i]]$Date[which.max(spbatch$`11452500`$Winter_6mon$Data[[i]]$Discharge_cfs)]
	max$sthyyear[[i]] <- as.numeric(strsplit(names(spbatch$`11452500`$Winter_6mon$Data)[[i]]," ")[[1]][[1]])
}

max$maxdate <- as.Date(max$maxdate)

impairmentsplot <- ggplot(spbatch$`11447650`$Winter_6mon$All$Data,aes(x=Date,y=Discharge_acfte6_day*1000)) +
		geom_line() +
		geom_vline(xintercept=c(as.numeric(as.Date("01-01-1970", format="%m-%d-%Y"))), color="red")+
		ggtitle("Sacramento, USGS 11447650")+
		theme(plot.title=element_text(size=20))+
		labs(x="Year", y="November to April Discharge (TAF)")+
		geom_hline(yintercept=c(spbatch$`11447650`$thresholdsdams_maf$P90maf*1000,spbatch$`11447650`$thresholds_maf$P90maf*1000))

ggsave("C:\\Users\\tiffn_000\\Google Drive\\figures\\impairments.jpg", plot=impairmentsplot, width=10, height=8, units="in", dpi=300)