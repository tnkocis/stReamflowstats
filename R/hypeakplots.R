# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

#input i speakflowsummary df#

hypeakplots <- function(input,gauge){
	library(ggplot2)
	startyear <- input$year[[1]]
	startmon <- format(input$peak_date[[1]],"%m")
	endyear <- format(tail(input$peak_date,1),"%Y")
	endmon <- format(tail(input$peak_date,1),"%m")
	
	yrseq <- unique(input$year)
	
	pkdatem <- format(input$peak_date,"%m")
	pkdatey <- format(input$peak_date,"%Y")
	octlog <- pkdatem=="10"
	novlog <- pkdatem=="11" 
	declog <- pkdatem=="12" 
	janlog <- pkdatem=="01" 
	feblog <- pkdatem=="02" 
	marlog <- pkdatem=="03" 
	aprlog <- pkdatem=="04" 
	maylog <- pkdatem=="05" 
	junlog <- pkdatem=="06" 
	jullog <- pkdatem=="07" 
	auglog <- pkdatem=="08" 
	seplog <- pkdatem=="09" 
	
	octcount <- sum( octlog , na.rm=TRUE)
	novcount <- sum( novlog , na.rm=TRUE)
	deccount <- sum( declog , na.rm=TRUE)
	jancount <- sum( janlog , na.rm=TRUE)
	febcount <- sum( feblog , na.rm=TRUE)
	marcount <- sum( marlog , na.rm=TRUE)
	aprcount <- sum( aprlog , na.rm=TRUE)
	maycount <- sum( maylog , na.rm=TRUE)
	juncount <- sum( junlog , na.rm=TRUE)
	julcount <- sum( jullog , na.rm=TRUE)
	augcount <- sum( auglog , na.rm=TRUE)
	sepcount <- sum( seplog , na.rm=TRUE)
	
	octdf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		octdf$numpeaks[[i]] <- sum(pkdatem=="10"&pkdatey==yrseq[[i]], na.rm=TRUE)
		octdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		octdf$mon[[i]] <- 10
		octdf$plotorder[[i]] <- 1
		if(octdf$numpeaks[[i]]==0&octdf$totpeaks_yr[[i]]==0){
			octdf$prop_peaks[[i]] <- 0
		}else {
			octdf$prop_peaks[[i]] <- octdf$numpeaks[[i]]/octdf$totpeaks_yr[[i]]
		}
	}
	
	novdf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		novdf$numpeaks[[i]] <- sum(pkdatem=="11"&pkdatey==yrseq[[i]], na.rm=TRUE)
		novdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		novdf$mon[[i]] <- 11
		novdf$plotorder[[i]] <- 2
		if(novdf$numpeaks[[i]]==0&novdf$totpeaks_yr[[i]]==0){
			novdf$prop_peaks[[i]] <- 0
		}else {
			novdf$prop_peaks[[i]] <- novdf$numpeaks[[i]]/novdf$totpeaks_yr[[i]]
		}
	}
	
	decdf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		decdf$numpeaks[[i]] <- sum(pkdatem=="12"&pkdatey==yrseq[[i]], na.rm=TRUE)
		decdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		decdf$mon[[i]] <- 12
		decdf$plotorder[[i]] <- 3
		if(decdf$numpeaks[[i]]==0&decdf$totpeaks_yr[[i]]==0){
			decdf$prop_peaks[[i]] <- 0
		}else {
			decdf$prop_peaks[[i]] <- decdf$numpeaks[[i]]/decdf$totpeaks_yr[[i]]
		}
	}
	
	jandf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		jandf$numpeaks[[i]] <- sum(pkdatem=="01"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		jandf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		jandf$mon[[i]] <- 1
		jandf$plotorder[[i]] <- 4
		if(jandf$numpeaks[[i]]==0&jandf$totpeaks_yr[[i]]==0){
			jandf$prop_peaks[[i]] <- 0
		}else {
			jandf$prop_peaks[[i]] <- jandf$numpeaks[[i]]/jandf$totpeaks_yr[[i]]
		}
	}
	
	febdf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		febdf$numpeaks[[i]] <- sum(pkdatem=="02"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		febdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		febdf$mon[[i]] <- 2
		febdf$plotorder[[i]] <- 5
		if(febdf$numpeaks[[i]]==0&febdf$totpeaks_yr[[i]]==0){
			febdf$prop_peaks[[i]] <- 0
		}else {
			febdf$prop_peaks[[i]] <- febdf$numpeaks[[i]]/febdf$totpeaks_yr[[i]]
		}
	}
	
	mardf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		mardf$numpeaks[[i]] <- sum(pkdatem=="03"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		mardf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		mardf$mon[[i]] <- 3
		mardf$plotorder[[i]] <- 6
		if(mardf$numpeaks[[i]]==0&mardf$totpeaks_yr[[i]]==0){
			mardf$prop_peaks[[i]] <- 0
		}else {
			mardf$prop_peaks[[i]] <- mardf$numpeaks[[i]]/mardf$totpeaks_yr[[i]]
		}
	}
	
	aprdf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		aprdf$numpeaks[[i]] <- sum(pkdatem=="04"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		aprdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		aprdf$mon[[i]] <- 4
		aprdf$plotorder[[i]] <- 7
		if(aprdf$numpeaks[[i]]==0&aprdf$totpeaks_yr[[i]]==0){
			aprdf$prop_peaks[[i]] <- 0
		}else {
			aprdf$prop_peaks[[i]] <- aprdf$numpeaks[[i]]/aprdf$totpeaks_yr[[i]]
		}
	}
	
	maydf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		maydf$numpeaks[[i]] <- sum(pkdatem=="05"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		maydf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		maydf$mon[[i]] <- 5
		maydf$plotorder[[i]] <- 8
		if(maydf$numpeaks[[i]]==0&maydf$totpeaks_yr[[i]]==0){
			maydf$prop_peaks[[i]] <- 0
		}else {
			maydf$prop_peaks[[i]] <- maydf$numpeaks[[i]]/maydf$totpeaks_yr[[i]]
		}
	}
	
	jundf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		jundf$numpeaks[[i]] <- sum(pkdatem=="06"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		jundf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		jundf$mon[[i]] <- 6
		jundf$plotorder[[i]] <- 9
		if(jundf$numpeaks[[i]]==0&jundf$totpeaks_yr[[i]]==0){
			jundf$prop_peaks[[i]] <- 0
		}else {
			jundf$prop_peaks[[i]] <- jundf$numpeaks[[i]]/jundf$totpeaks_yr[[i]]
		}
	}
	
	
	juldf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		juldf$numpeaks[[i]] <- sum(pkdatem=="07"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		juldf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		juldf$mon[[i]] <- 7
		juldf$plotorder[[i]] <- 10
		if(juldf$numpeaks[[i]]==0&juldf$totpeaks_yr[[i]]==0){
			juldf$prop_peaks[[i]] <- 0
		}else {
			juldf$prop_peaks[[i]] <- juldf$numpeaks[[i]]/juldf$totpeaks_yr[[i]]
		}
	}
	
	augdf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		augdf$numpeaks[[i]] <- sum(pkdatem=="08"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		augdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		augdf$mon[[i]] <- 8
		augdf$plotorder[[i]] <- 11
		if(augdf$numpeaks[[i]]==0&augdf$totpeaks_yr[[i]]==0){
			augdf$prop_peaks[[i]] <- 0
		}else {
			augdf$prop_peaks[[i]] <- augdf$numpeaks[[i]]/augdf$totpeaks_yr[[i]]
		}
	}
	
	sepdf <- data.frame(st_hy_year=yrseq, numpeaks=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		sepdf$numpeaks[[i]] <- sum(pkdatem=="09"&pkdatey==yrseq[[i+1]], na.rm=TRUE)
		sepdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		sepdf$mon[[i]] <- 9
		sepdf$plotorder[[i]] <- 12
		if(sepdf$numpeaks[[i]]==0&sepdf$totpeaks_yr[[i]]==0){
			sepdf$prop_peaks[[i]] <- 0
		}else {
			sepdf$prop_peaks[[i]] <- sepdf$numpeaks[[i]]/sepdf$totpeaks_yr[[i]]
		}
	}
	
	monlist <- list(octdf,novdf,decdf,jandf,febdf,mardf,aprdf,maydf,jundf,juldf,augdf,sepdf)
	colgrad <- c(	"#CAC393","#CAC393","#0067B2",
			"#0067B2","#0067B2","#25CB7B","#25CB7B",
			"#25CB7B","#F4F478","#F4F478","#F4F478","#F4F478")
	leglabels <- c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP")
	mon_merge <- do.call(rbind.data.frame,monlist)
	mon_plot <- ggplot(data=mon_merge,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_area(aes(fill=factor(plotorder)),color="black",position="fill") + scale_x_reverse(name="Year")+ coord_flip() +
			scale_fill_manual(values=colgrad,guide = "legend", labels=leglabels) + ylab("Number of Peaks as a Fraction of Total Peaks Per Year")+
			ggtitle(paste("USGS",gauge,sep=""))
	
	
	
	plotoctnum <- ggplot(data=octdf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("oct") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotnovnum <- ggplot(data=novdf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("nov") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotdecnum <- ggplot(data=decdf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("dec") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotjannum <- ggplot(data=jandf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jan") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotfebnum <- ggplot(data=febdf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("feb") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotmarnum <- ggplot(data=mardf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("mar") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotaprnum <- ggplot(data=aprdf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("apr") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotmaynum <- ggplot(data=maydf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("may") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotjunnum <- ggplot(data=jundf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jun") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotjulnum <- ggplot(data=juldf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jul") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotaugnum <- ggplot(data=augdf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("aug") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	plotsepnum <- ggplot(data=sepdf,mapping=aes(x=st_hy_year,y=numpeaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("sep") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,50))
	numplots <- list(plotoctnum,plotnovnum,plotdecnum,plotjannum,plotfebnum,plotmarnum,plotaprnum,plotmaynum,
			plotjunnum,plotjulnum,plotaugnum,plotsepnum)
	names(numplots)<-c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP")
	
	plotoctprop <- ggplot(data=octdf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("oct") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotnovprop <- ggplot(data=novdf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("nov") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotdecprop <- ggplot(data=decdf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("dec") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotjanprop <- ggplot(data=jandf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jan") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotfebprop <- ggplot(data=febdf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("feb") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotmarprop <- ggplot(data=mardf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("mar") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotaprprop <- ggplot(data=aprdf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("apr") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotmayprop <- ggplot(data=maydf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("may") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotjunprop <- ggplot(data=jundf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jun") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotjulprop <- ggplot(data=juldf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jul") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotaugprop <- ggplot(data=augdf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("aug") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotsepprop <- ggplot(data=sepdf,mapping=aes(x=st_hy_year,y=prop_peaks))+
			geom_bar(stat="identity", color="blue")+ ggtitle("sep") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	propplots <- list(plotoctprop,plotnovprop,plotdecprop,plotjanprop,plotfebprop,plotmarprop,plotaprprop,plotmayprop,
			plotjunprop,plotjulprop,plotaugprop,plotsepprop)
	names(propplots)<-c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP")
	
	djflog <- pkdatem=="12" |pkdatem=="01"|pkdatem=="02"
	mamlog <- pkdatem=="03" |pkdatem=="04"|pkdatem=="05"
	jjalog <- pkdatem=="06" |pkdatem=="07"|pkdatem=="08"
	sonlog <- pkdatem=="09" |pkdatem=="10"|pkdatem=="11"
	
	
	djfcount <- sum( djflog , na.rm=TRUE)
	mamcount <- sum( mamlog , na.rm=TRUE)
	jjacount <- sum( jjalog , na.rm=TRUE)
	soncount <- sum( sonlog , na.rm=TRUE)
	
	djfdf <- data.frame(st_hy_year=yrseq, numpeaks_djf=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_djf=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		djfdf$numpeaks_djf[[i]] <- sum((pkdatem=="12"&pkdatey==yrseq[[i]])|(pkdatem=="01"&pkdatey==yrseq[[i+1]])|(pkdatem=="02"&pkdatey==yrseq[[i+1]]), na.rm=TRUE)
		djfdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(djfdf$numpeaks_djf[[i]]==0&djfdf$totpeaks_yr[[i]]==0){
			djfdf$prop_peaks_djf[[i]] <- 0
		}else {
			djfdf$prop_peaks_djf[[i]] <- djfdf$numpeaks_djf[[i]]/djfdf$totpeaks_yr[[i]]
		}
	}
	
	mamdf <- data.frame(st_hy_year=yrseq, numpeaks_mam=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_mam=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		mamdf$numpeaks_mam[[i]] <- sum((pkdatem=="03"&pkdatey==yrseq[[i+1]])|(pkdatem=="04"&pkdatey==yrseq[[i+1]])|(pkdatem=="05"&pkdatey==yrseq[[i+1]]), na.rm=TRUE)
		mamdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(mamdf$numpeaks_mam[[i]]==0&mamdf$totpeaks_yr[[i]]==0){
			mamdf$prop_peaks_mam[[i]] <- 0
		}else {
			mamdf$prop_peaks_mam[[i]] <- mamdf$numpeaks_mam[[i]]/mamdf$totpeaks_yr[[i]]
		}
	}
	
	jjadf <- data.frame(st_hy_year=yrseq, numpeaks_jja=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_jja=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		jjadf$numpeaks_jja[[i]] <- sum((pkdatem=="06"&pkdatey==yrseq[[i+1]])|(pkdatem=="07"&pkdatey==yrseq[[i+1]])|(pkdatem=="08"&pkdatey==yrseq[[i+1]]), na.rm=TRUE)
		jjadf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(jjadf$numpeaks_jja[[i]]==0&jjadf$totpeaks_yr[[i]]==0){
			jjadf$prop_peaks_jja[[i]] <- 0
		}else {
			jjadf$prop_peaks_jja[[i]] <- jjadf$numpeaks_jja[[i]]/jjadf$totpeaks_yr[[i]]
		}
	}
	
	ndjdf <- data.frame(st_hy_year=yrseq, numpeaks_ndj=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_ndj=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		ndjdf$numpeaks_ndj[[i]] <- sum((pkdatem=="11"&pkdatey==yrseq[[i]])|(pkdatem=="12"&pkdatey==yrseq[[i]])|(pkdatem=="01"&pkdatey==yrseq[[i+1]]), na.rm=TRUE)
		ndjdf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(ndjdf$numpeaks_ndj[[i]]==0&ndjdf$totpeaks_yr[[i]]==0){
			ndjdf$prop_peaks_ndj[[i]] <- 0
		}else {
			ndjdf$prop_peaks_ndj[[i]] <- ndjdf$numpeaks_ndj[[i]]/ndjdf$totpeaks_yr[[i]]
		}
	}
	fmadf <- data.frame(st_hy_year=yrseq, numpeaks_fma=rep(NA,length(yrseq)),totpeaks_yr=rep(NA,length(yrseq)),
			prop_peaks_fma=rep(NA,length(yrseq)))
	for(i in 1:(length(yrseq)-1)){
		fmadf$numpeaks_fma[[i]] <- sum((pkdatem=="02"&pkdatey==yrseq[[i+1]])|(pkdatem=="03"&pkdatey==yrseq[[i+1]])|(pkdatem=="04"&pkdatey==yrseq[[i+1]]), na.rm=TRUE)
		fmadf$totpeaks_yr[[i]] <- sum((pkdatem>="10"&pkdatey==yrseq[[i]])|(pkdatem<"10"&pkdatey==yrseq[[i+1]]),na.rm=TRUE)
		if(fmadf$numpeaks_fma[[i]]==0&fmadf$totpeaks_yr[[i]]==0){
			fmadf$prop_peaks_fma[[i]] <- 0
		}else {
			fmadf$prop_peaks_fma[[i]] <- fmadf$numpeaks_fma[[i]]/fmadf$totpeaks_yr[[i]]
		}
	}
	
	plotdjfnum <- ggplot(data=djfdf,mapping=aes(x=st_hy_year,y=numpeaks_djf))+
			geom_bar(stat="identity", color="blue")+ ggtitle("djf") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,100))
	plotmamnum <- ggplot(data=mamdf,mapping=aes(x=st_hy_year,y=numpeaks_mam))+
			geom_bar(stat="identity", color="blue")+ ggtitle("mam") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,100))
	plotjjanum <- ggplot(data=jjadf,mapping=aes(x=st_hy_year,y=numpeaks_jja))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jja") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,100))
	plotndjnum <- ggplot(data=ndjdf,mapping=aes(x=st_hy_year,y=numpeaks_ndj))+
			geom_bar(stat="identity", color="blue")+ ggtitle("ndj") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,100))
	plotfmanum <- ggplot(data=fmadf,mapping=aes(x=st_hy_year,y=numpeaks_fma))+
			geom_bar(stat="identity", color="blue")+ ggtitle("fma") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,100))
	
	plotdjfprop <- ggplot(data=djfdf,mapping=aes(x=st_hy_year,y=prop_peaks_djf))+
			geom_bar(stat="identity", color="blue")+ ggtitle("djf") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotmamprop <- ggplot(data=mamdf,mapping=aes(x=st_hy_year,y=prop_peaks_mam))+
			geom_bar(stat="identity", color="blue")+ ggtitle("mam") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotjjaprop <- ggplot(data=jjadf,mapping=aes(x=st_hy_year,y=prop_peaks_jja))+
			geom_bar(stat="identity", color="blue")+ ggtitle("jja") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotndjprop <- ggplot(data=ndjdf,mapping=aes(x=st_hy_year,y=prop_peaks_ndj))+
			geom_bar(stat="identity", color="blue")+ ggtitle("ndj") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	plotfmaprop <- ggplot(data=fmadf,mapping=aes(x=st_hy_year,y=prop_peaks_fma))+
			geom_bar(stat="identity", color="blue")+ ggtitle("fma") + xlab("Year [Hydrologic (Y-Y+1)])") + ylab("Number of Peaks Above 90%") +
			ylim(c(0,1))
	
	trinumplots <- list(plotdjfnum,plotmamnum,plotjjanum,plotndjnum,plotfmanum)
	names(trinumplots) <- c("djf","mam","jja","ndj","fma")
	tripropplots <- list(plotdjfprop,plotmamprop,plotjjaprop,plotndjprop,plotfmaprop)
	names(tripropplots) <- c("djf","mam","jja","ndj","fma")	
	
	
	#####decades#######
	totalnumyrs <- 2015-as.numeric(startyear)
	numdecades <- floor(totalnumyrs/10)
	decadesyrseq <- seq(as.numeric(startyear),length.out=(numdecades+1), by=10)
	decades <- vector("list", length(numdecades))
	monlist <- c("10","11","12","01","02","03","04","05","06","07","08","09")
	
	for(i in 1:numdecades){
		decades[[i]] <- data.frame(stdecade=decadesyrseq[[i]], mon=rep(NA,12), numpeaks=rep(NA,12),totpeaks=rep(NA,12),
				prop_peaks=rep(NA,12), plotorder=seq(1,12,1))
		for(n in 1:length(monlist)){
			decades[[i]]$mon[[n]] <- monlist[[n]]
			decades[[i]]$numpeaks[[n]] <- sum(pkdatem==monlist[[n]]&(pkdatey>=decadesyrseq[[i]]&pkdatey<decadesyrseq[[i+1]]), na.rm=TRUE)
			decades[[i]]$totpeaks[[n]] <- sum((pkdatem>="10"&pkdatey>=decadesyrseq[[i]]&pkdatey<(decadesyrseq[[i+1]]))|(pkdatem<"10"&pkdatey>decadesyrseq[[i]]&pkdatey<=decadesyrseq[[i+1]]), na.rm=TRUE)
			if(decades[[i]]$numpeaks[[n]]==0&decades[[i]]$totpeaks[[n]]==0){
				decades[[i]]$prop_peaks[[n]] <- 0
			} else {
				decades[[i]]$prop_peaks[[n]] <- decades[[i]]$numpeaks[[n]]/decades[[i]]$totpeaks[[n]]
			}
			
		}
		names(decades)[[i]] <- paste(decadesyrseq[[i]],"-",decadesyrseq[[i+1]], sep="")
	}
	
	colgrad <- c(	"#CAC393","#548DA6","#0067B2",
			"#0BA5BB","#18C3A4","#25CB7B","#34D356",
			"#51DC43","#8CE454","#C3EC65","#F4F478","#FDDA8C")
	leglabels <- c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP")
	decades_merge <- do.call(rbind.data.frame,decades)
	decades_merge$plotyear <- decades_merge$stdecade+5
	
	decades_plot <- ggplot(data=decades_merge,mapping=aes(x=plotyear,y=prop_peaks))+
			geom_area(aes(fill=factor(plotorder)),color="black",position="fill") + scale_x_reverse(name="Decade", breaks=unique(decades_merge$stdecade))+ coord_flip() +
			scale_fill_manual(name="Month",values=colgrad, labels=leglabels) + ylab("Number of Peaks as a Fraction of Total Peaks Per Decade")+
			ggtitle(paste("USGS",gauge,sep=""))
	
	
	return(list(numplotsmon=numplots, propplotsmon=propplots, trinumplots=trinumplots, tripropplots=tripropplots, decadesplot = decades_plot, monplot=mon_plot, decades_merge=decades_merge))
}


#inputstatsdf
hypeakplotsstats <- function(inputstats, gauge){
	library(ggplot2)
	startyear <- inputstats$sthyyear[[1]]
	startmon <- inputstats$month[[1]]
	endyear <- tail(inputstats$sthyyear,1)
	endmon <- tail(inputstats$month,1)
	
	yrseq <- unique(inputstats$sthyyear)
	
	pkdatem <- inputstats$month
	pkdatey <- inputstats$sthyyear
	totalnumyrs <- 2015-as.numeric(startyear)
	numdecades <- floor(totalnumyrs/10)
	decadesyrseq <- seq(as.numeric(startyear),length.out=(numdecades+1), by=10)
	decades <- vector("list", length(numdecades))
	monlist <- c("10","11","12","01","02","03","04","05","06","07","08","09")
	
	for(i in 1:numdecades){
		decades[[i]] <- data.frame(stdecade=decadesyrseq[[i]], mon=rep(NA,12), daysabv=rep(NA,12),totdaysabv=rep(NA,12),
				volabv_acft=rep(NA,12), totvolabv_acft=rep(NA,12),
				prop_days=rep(NA,12),prop_vol=rep(NA,12), plotorder=seq(1,12,1))
		for(n in 1:length(monlist)){
			decades[[i]]$mon[[n]] <- monlist[[n]]
			decades[[i]]$daysabv[[n]] <- sum(inputstats$TotDaysAbv[(pkdatem==monlist[[n]]&(pkdatey>=decadesyrseq[[i]]&pkdatey<decadesyrseq[[i+1]]))], na.rm=TRUE)
			decades[[i]]$totdaysabv[[n]] <- sum(inputstats$TotDaysAbv[(pkdatey>=decadesyrseq[[i]]&pkdatey<(decadesyrseq[[i+1]]))], na.rm=TRUE)
			decades[[i]]$volabv_acft[[n]] <- sum(inputstats$TotVolAbv_acft[(pkdatem==monlist[[n]]&(pkdatey>=decadesyrseq[[i]]&pkdatey<decadesyrseq[[i+1]]))], na.rm=TRUE)
			decades[[i]]$totvolabv_acft[[n]] <- sum(inputstats$TotVolAbv_acft[(pkdatey>=decadesyrseq[[i]]&pkdatey<(decadesyrseq[[i+1]]))], na.rm=TRUE)
			if(decades[[i]]$daysabv[[n]]==0&decades[[i]]$totdaysabv[[n]]==0){
				decades[[i]]$prop_days[[n]] <- 0
			} else {
				decades[[i]]$prop_days[[n]] <- decades[[i]]$daysabv[[n]]/decades[[i]]$totdaysabv[[n]]
			}
			if(decades[[i]]$volabv_acft[[n]]==0&decades[[i]]$totvolabv_acft[[n]]==0){
				decades[[i]]$prop_vol[[n]] <- 0
			} else {
				decades[[i]]$prop_vol[[n]] <- decades[[i]]$volabv_acft[[n]]/decades[[i]]$totvolabv_acft[[n]]
			}
			
		}
		names(decades)[[i]] <- paste(decadesyrseq[[i]],"-",decadesyrseq[[i+1]], sep="")
	}
	
	colgrad <- c(	"#CAC393","#548DA6","#0067B2",
			"#0BA5BB","#18C3A4","#25CB7B","#34D356",
			"#51DC43","#8CE454","#C3EC65","#F4F478","#FDDA8C")
	leglabels <- c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP")
	decades_merge <- do.call(rbind.data.frame,decades)
	decades_merge$plotyear <- decades_merge$stdecade+5
	
	decades_plotdays <- ggplot(data=decades_merge,mapping=aes(x=plotyear,y=prop_days))+
			geom_area(aes(fill=factor(plotorder)),color="black",position="fill") + scale_x_reverse(name="Decade", breaks=unique(decades_merge$stdecade))+ coord_flip() +
			scale_fill_manual(name="Month",values=colgrad, labels=leglabels) + ylab("Number of Days Above 90% as a Fraction of Total Days Above 90% Per Decade")+
			ggtitle(paste("USGS",gauge,sep=""))
	decades_plotvol <- ggplot(data=decades_merge,mapping=aes(x=plotyear,y=prop_vol))+
			geom_area(aes(fill=factor(plotorder)),color="black",position="fill") + scale_x_reverse(name="Decade", breaks=unique(decades_merge$stdecade))+ coord_flip() +
			scale_fill_manual(name="Month",values=colgrad, labels=leglabels) + ylab("Volume Above 90% as a Fraction of Total Volume Above 90% Per Decade")+
			ggtitle(paste("USGS",gauge,sep=""))
	return(list(decades_plotdays=decades_plotdays, decades_plotvol=decades_plotvol))
}

