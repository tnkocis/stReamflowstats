# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################
library(EcoHydRology)

streamflow <- american$`11446500`$Winter_3mon$Data[[1]]$Discharge_cfs
streamflowdate <- american$`11446500`$Winter_3mon$Data[[1]]$Date
baseflowtest <- BaseflowSeparation(streamflow, filter_parameter = 0.9, passes = 1)
floodflow <- streamflow - baseflowtest$bt 
zeroline <- rep(0,length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x90line <- rep(x90_3mon[["90%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x95line <- rep(x90_3mon[["95%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x80line <- rep(x90_3mon[["80%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))

plot(streamflowdate, streamflow, type="l",ylim=c(0,20000))
lines(streamflowdate, baseflowtest$bt)
lines(streamflowdate, floodflow)
lines(streamflowdate, zeroline, col="red")
lines(streamflowdate, x90line, col="red")
lines(streamflowdate, x95line, col="red")
lines(streamflowdate, x80line, col="red")

	strlist <- vector("list",length(american$`11446500`$Winter_3mon$Data)-57)
for (i in 57:length(american$`11446500`$Winter_3mon$Data)){
	streamflow <- american$`11446500`$Winter_3mon$Data[[i]]$Discharge_cfs
	streamflowdate <- american$`11446500`$Winter_3mon$Data[[i]]$Date
	baseflowtest <- BaseflowSeparation(streamflow, filter_parameter = 0.9, passes = 3)
	floodflow <- streamflow - baseflowtest$bt 
	num <- seq(1,length(american$`11446500`$Winter_3mon$Data[[i]]$Discharge_cfs),1)
	strlist[[i-56]]<- data.frame(streamflow=streamflow, date=streamflowdate, baseflow= baseflowtest, floodflows=floodflow, num=num)	
	names(strlist)[[i-56]] <- names(american$`11446500`$Winter_3mon$Data)[[i]]
}
plot(strlist$`1960 - 1961`$num, strlist$`1960 - 1961`$streamflow, type="n",ylim=c(0,60000))
for(i in 1:15){
	lines(strlist[[i]]$num,strlist[[i]]$floodflows)
}
lines(strlist[[1]]$num, x90line, col="red")
lines(strlist[[1]]$num, x95line, col="red")
lines(strlist[[1]]$num, x80line, col="red")


x90_3mon <- quantile(american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day/(86400*2.29568411e-5*1e-6),probs=c(0.5,0.6,0.7,0.8,0.9,0.95), na.rm=TRUE)

x3monall <- american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day/(86400*2.29568411e-5*1e-6)

line <- rep(x90_3mon[["90%"]],length(american$`11446500`$Winter_3mon$All$Data$Date))
line2 <- rep(x90_3mon[["95%"]],length(american$`11446500`$Winter_3mon$All$Data$Date))

plot(american$`11446500`$Winter_3mon$All$Data$Date,american$`11446500`$Winter_3mon$All$Data$Discharge_acfte6_day/(86400*2.29568411e-5*1e-6),
		type="l", xlim=c(as.Date("1900-12-01", format="%Y-%m-%d"),as.Date("2015-12-01", format="%Y-%m-%d")))
lines(american$`11446500`$Winter_3mon$All$Data$Date, line)
lines(american$`11446500`$Winter_3mon$All$Data$Date, line2)




peakflows <- vector("list", length=length(american$`11446500`$Winter_6mon$Data))
names(peakflows) <- names(american$`11446500`$Winter_6mon$Data)
for(i in 1:length(american$`11446500`$Winter_6mon$Data)){
	peakflows[[i]] <-  peaks(input=american$`11446500`$Winter_6mon$Data[[i]],width=3, threshold=x90_3mon[["90%"]], thresholdname="95%", mastertime="3mon", Index=american$`11446500`$Index)
}
peakflowsstatslist <- vector("list", length(peakflows))
for(i in 1:length(peakflows)){
	peakflowsstatslist[[i]] <- peakflows[[i]][[2]]
}
peakflowstats <- do.call(rbind.data.frame,peakflowsstatslist)
peakflowstats$color <- rep(NA, length(peakflowstats$year))
peakflowstats$color[which(peakflowstats$yeartype_index==1)] <- "red"
peakflowstats$color[which(peakflowstats$yeartype_index==2)] <- "orange"
peakflowstats$color[which(peakflowstats$yeartype_index==3)] <- "yellow"
peakflowstats$color[which(peakflowstats$yeartype_index==4)] <- "green"
peakflowstats$color[which(peakflowstats$yeartype_index==5)] <- "blue"

peakflowstats1970 <- peakflowstats[peakflowstats$year >=1970,]
peakflowstatspreimp <-  peakflowstats[peakflowstats$year <1970,]
plot(peakflowstats$TotDaysAbv,peakflowstats$TotVolAbv_acft, col=peakflowstats$color)
plot(peakflowstats$TotDaysAbv,peakflowstats$numpeaks, col=peakflowstats$color)
plot(peakflowstats$TotVolAbv_acft,peakflowstats$numpeaks, col=peakflowstats$color)
plot(peakflowstats$year,peakflowstats$TotVolAbv_acft, col=peakflowstats$color)
pdf(file="C:\\Users\\tiffn_000\\Desktop\\Figures\\stats.pdf", width=11, height=8.5)
par(mfrow=c(2,3))
plot(peakflowstats1970$TotDaysAbv,peakflowstats1970$TotVolAbv_acft, bg=peakflowstats1970$color, pch=24, xlim=c(0,60), ylim=c(0,3000000),
		xlab="Total Days Above", ylab="Total Volume Above (acft)")
points(peakflowstatspreimp$TotDaysAbv,peakflowstatspreimp$TotVolAbv_acft, col=peakflowstatspreimp$color)

plot(peakflowstats1970$TotDaysAbv,peakflowstats1970$numpeaks, bg=peakflowstats1970$color, pch=24, xlim=c(0,60), ylim=c(0,10),
		xlab="Total Days Above", ylab="Number of Peaks")
points(peakflowstatspreimp$TotDaysAbv,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color)

plot(peakflowstats1970$TotVolAbv_acft,peakflowstats1970$numpeaks, bg=peakflowstats1970$color,pch=24, xlim=c(0,3000000), ylim=c(0,10),
		xlab="Total Volume Above (acft)", ylab="Number of Peaks")
points(peakflowstatspreimp$TotVolAbv_acft,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color)

plot(peakflowstats1970$year,peakflowstats1970$TotVolAbv_acft, bg=peakflowstats1970$color,pch=24, xlim=c(1900,2015), ylim=c(0,3000000),
		xlab="Year", ylab="Total Volume Above (acft)")
points(peakflowstatspreimp$year,peakflowstatspreimp$TotVolAbv_acft, col=peakflowstatspreimp$color)

plot(peakflowstats1970$year,peakflowstats1970$numpeaks, bg=peakflowstats1970$color,pch=24, xlim=c(1900,2015), ylim=c(0,10),
		xlab="Year", ylab="Number of Peaks")
points(peakflowstatspreimp$year,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color)

plot(peakflowstats1970$year,peakflowstats1970$TotDaysAbv, bg=peakflowstats1970$color,pch=24, xlim=c(1900,2015), ylim=c(0,60),
		xlab="Year", ylab="Total Days Above")
points(peakflowstatspreimp$year,peakflowstatspreimp$TotDaysAbv, col=peakflowstatspreimp$color)
dev.off()

points(peakflowstatspreimp$TotDaysAbv,peakflowstatspreimp$TotVolAbv_acft, col=peakflowstatspreimp$color, xlim=c(0,60), ylim=c(0,2500000))
points(peakflowstatspreimp$TotDaysAbv,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color,xlim=c(0,60), ylim=c(0,10))
plot(peakflowstatspreimp$TotVolAbv_acft,peakflowstatspreimp$numpeaks, col=peakflowstatspreimp$color)
plot(peakflowstatspreimp$year,peakflowstatspreimp$TotVolAbv_acft, col=peakflowstatspreimp$color)

#baseflow flood separation##
library(hydrostats)
library(zoo)

mov_discharge <- rollapply(american$`11446500`$Winter_3mon$Data[[91]]$Discharge_cfs, 3, function(x) mean(x, na.rm=TRUE),
		align = "center", fill=NA)
mov_disch <- data.frame(Q=mov_discharge, Date=american$`11446500`$Winter_3mon$Data[[1]]$Date)
streamflow <- american$`11446500`$Winter_3mon$Data[[91]]$Discharge_cfs
streamflowdate <- american$`11446500`$Winter_3mon$Data[[91]]$Date
baseflowtest <- baseflows(mov_disch, a = 0.82, n.reflected = 30, ts="daily")

floodflow <- american$`11446500`$Winter_3mon$Data[[91]]$Discharge_cfs - baseflowtest$bf
zeroline <- rep(0,length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x90line <- rep(x90_3mon[["90%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x95line <- rep(x90_3mon[["95%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))
x80line <- rep(x90_3mon[["80%"]],length(american$`11446500`$Winter_3mon$Data[[4]]$Date))

par(mfrow=c(1,1))
plot(baseflowtest$Date, streamflow, type="l",ylim=c(0,5000))
lines(baseflowtest$Date, baseflowtest$bf)
lines(baseflowtest$Date, floodflow)
lines(baseflowtest$Date, zeroline, col="red")
lines(baseflowtest$Date, x90line, col="red")
lines(baseflowtest$Date, x95line, col="red")
lines(streamflowdate, x80line, col="red")
#############################

peakflowssummarylist <- vector("list", length(peakflows))
for(i in 1:length(peakflows)){
	peakflowssummarylist[[i]] <- peakflows[[i]][[1]]
}
peakflowsummary <- do.call(rbind.data.frame,peakflowssummarylist)

peakflowstats$volday_is_zero <- rep(NA, length(peakflowstats$TotDaysAbv))
for(i in 1:length(peakflowstats$TotDaysAbv)){
	if(peakflowstats$TotDaysAbv[[i]]==0){
	peakflowstats$volday_is_zero[[i]] <- 1
}else {peakflowstats$volday_is_zero[[i]] <- 0}
}
peakflowstats$volday_is_zero_cumsum <- cumsum(peakflowstats$volday_is_zero)

gmtest <- glm(peakflowstats$TotVolAbv_acft[peakflowstats$TotVolAbv_acft != 0]~peakflowstats$year[peakflowstats$TotVolAbv_acft != 0], family=gaussian)
volpredict <- predict(gmtest)
glmzeropre <- glm(peakflowstats$volday_is_zero_cumsum[peakflowstats$year<1957]~peakflowstats$year[peakflowstats$year<1957], family=gaussian)
prezeropredict <- predict(glmzeropre)
glmzeropost <- glm(peakflowstats$volday_is_zero_cumsum[peakflowstats$year>1956]~peakflowstats$year[peakflowstats$year>1956], family=gaussian)
postzeropredict <- predict(glmzeropost)

pdf(file="C:\\Users\\tiffn_000\\Desktop\\Figures\\zerosep.pdf", width=8.5, height=11)
par(mfrow=c(2,1))
plot(peakflowstats$year[peakflowstats$TotVolAbv_acft != 0],peakflowstats$TotVolAbv_acft[peakflowstats$TotVolAbv_acft != 0], xlab="Year",ylab="Total Volume Above 95% (acft)")
lines(peakflowstats$year[peakflowstats$TotVolAbv_acft != 0],volpredict)

plot(peakflowstats$year,peakflowstats$volday_is_zero_cumsum, xlab="Year", ylab="Cumulative Number of Years With Zero Flow Above 95%")
lines(peakflowstats$year[peakflowstats$year<1957],prezeropredict)
lines(peakflowstats$year[peakflowstats$year>1956],postzeropredict)
dev.off()


######hyroyear#########
peakflowshy90 <- vector("list", length=length(american$`11446500`$HydroYear$Data))
names(peakflowshy90) <- names(american$`11446500`$HydroYear$Data)
for(i in 1:length(american$`11446500`$HydroYear$Data)){
	peakflowshy90[[i]] <-  peaks(input=american$`11446500`$HydroYear$Data[[i]],width=3, threshold=x90_3mon[["90%"]], thresholdname="90%", mastertime="hy", Index=american$`11446500`$Index)
}
peakflowsstatslisthy90 <- vector("list", length(peakflowshy90))
for(i in 1:length(peakflowshy90)){
	peakflowsstatslisthy90[[i]] <- peakflowshy90[[i]][[2]]
}
peakflowstatshy90 <- do.call(rbind.data.frame,peakflowsstatslisthy90)

peakflowssummarylisthy90 <- vector("list", length(peakflowshy90))
for(i in 1:length(peakflowshy90)){
	peakflowssummarylisthy90[[i]] <- peakflowshy90[[i]][[1]]
}
peakflowsummaryhy90 <- do.call(rbind.data.frame,peakflowssummarylisthy90)

peakflowsmonthly_statslisthy90 <- vector("list", length(peakflowshy90))
for(i in 1:length(peakflowshy90)){
	peakflowsmonthly_statslisthy90[[i]] <- peakflowshy90[[i]][[3]]
}
peakflowsmonthly_statshy90 <- do.call(rbind.data.frame,peakflowsmonthly_statslisthy90)

peakflowstatshy90$volday_is_zero <- rep(NA, length(peakflowstatshy90$TotDaysAbv))
for(i in 1:length(peakflowstatshy90$TotDaysAbv)){
	if(peakflowstatshy90$TotDaysAbv[[i]]==0){
		peakflowstatshy90$volday_is_zero[[i]] <- 1
	}else {peakflowstatshy90$volday_is_zero[[i]] <- 0}
}
peakflowstatshy90$volday_is_zero_cumsum <- cumsum(peakflowstatshy90$volday_is_zero)

gmtesthy90 <- glm(peakflowstatshy90$TotVolAbv_acft[peakflowstatshy90$TotVolAbv_acft != 0]~peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0], family=gaussian)
volpredicthy90 <- predict(gmtesthy90)
glmzeroprehy90 <- glm(peakflowstatshy90$volday_is_zero_cumsum[peakflowstatshy90$year<1957]~peakflowstatshy90$year[peakflowstatshy90$year<1957], family=gaussian)
prezeropredicthy90 <- predict(glmzeroprehy90)
glmzeroposthy90 <- glm(peakflowstatshy90$volday_is_zero_cumsum[peakflowstatshy90$year>1956]~peakflowstatshy90$year[peakflowstatshy90$year>1956], family=gaussian)
postzeropredicthy90 <- predict(glmzeroposthy90)

pdf(file="C:\\Users\\tiffn_000\\Desktop\\Figures\\zerosephy90.pdf", width=8.5, height=11)
par(mfrow=c(2,1))
plot(peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0],peakflowstatshy90$TotVolAbv_acft[peakflowstatshy90$TotVolAbv_acft != 0], xlab="Year",ylab="Total Volume Above 90% (acft)")
lines(peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0],volpredicthy90)

plot(peakflowstatshy90$year,peakflowstatshy90$volday_is_zero_cumsum, xlab="Year", ylab="Cumulative Number of Years With Zero Flow Above 90%")
lines(peakflowstatshy90$year[peakflowstatshy90$year<1957],prezeropredicthy90)
lines(peakflowstatshy90$year[peakflowstatshy90$year>1956],postzeropredicthy90)
dev.off()

plotstest <- hypeakplots(peakflowsummaryhy90, names(american)[[11]])
pdf(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[11]],"_areaplot.pdf",sep=""), width=8.5, height=11)
plotstest$decadesplot
dev.off()
plotstestmon <- hypeakplotsstats(peakflowsmonthly_statshy90, names(american)[[11]])
pdf(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[11]],"_areaplot_Daysabove.pdf",sep=""), width=8.5, height=11)
plotstestmon$decades_plotdays
dev.off()
pdf(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[11]],"_areaplot_volabove.pdf",sep=""), width=8.5, height=11)
plotstestmon$decades_plotvol
dev.off()


png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[11]],"_areaplot.png",sep=""), width=8.5, height=11, units="in", res=600)
plotstest$decadesplot
dev.off()
png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[11]],"_areaplot_Daysabove.png",sep=""), width=8.5, height=11, units="in", res=600)
plotstestmon$decades_plotdays
dev.off()
png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[11]],"_areaplot_volabove.png",sep=""), width=8.5, height=11, units="in", res=600)
plotstestmon$decades_plotvol
dev.off()


plotstest <- hypeakplots(peakflowsummaryhy90, names(american)[[11]])
pdf(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[11]],"_areaplot.pdf",sep=""), width=8.5, height=11)
plotstest$decadesplot
dev.off()


######hyroyear#########
peakflowshy90 <- vector("list", length=length(american$`11441500`$HydroYear$Data))
names(peakflowshy90) <- names(american$`11441500`$HydroYear$Data)
for(i in 1:length(american$`11441500`$HydroYear$Data)){
	peakflowshy90[[i]] <-  peaks(input=american$`11441500`$HydroYear$Data[[i]],width=3, threshold=(american$`11441500`$thresholds_maf$P90maf/(86400*2.29568411e-5*1e-6)), thresholdname="90%", mastertime="hy", Index=american$`11441500`$Index)
}
peakflowsstatslisthy90 <- vector("list", length(peakflowshy90))
for(i in 1:length(peakflowshy90)){
	peakflowsstatslisthy90[[i]] <- peakflowshy90[[i]][[2]]
}
peakflowstatshy90 <- do.call(rbind.data.frame,peakflowsstatslisthy90)

peakflowssummarylisthy90 <- vector("list", length(peakflowshy90))
for(i in 1:length(peakflowshy90)){
	peakflowssummarylisthy90[[i]] <- peakflowshy90[[i]][[1]]
}
peakflowsummaryhy90 <- do.call(rbind.data.frame,peakflowssummarylisthy90)

peakflowstatshy90$volday_is_zero <- rep(NA, length(peakflowstatshy90$TotDaysAbv))
for(i in 1:length(peakflowstatshy90$TotDaysAbv)){
	if(peakflowstatshy90$TotDaysAbv[[i]]==0){
		peakflowstatshy90$volday_is_zero[[i]] <- 1
	}else {peakflowstatshy90$volday_is_zero[[i]] <- 0}
}
peakflowstatshy90$volday_is_zero_cumsum <- cumsum(peakflowstatshy90$volday_is_zero)

peakflowsmonthly_statslisthy90 <- vector("list", length(peakflowshy90))
for(i in 1:length(peakflowshy90)){
	peakflowsmonthly_statslisthy90[[i]] <- peakflowshy90[[i]][[3]]
}
peakflowsmonthly_statshy90 <- do.call(rbind.data.frame,peakflowsmonthly_statslisthy90)

gmtesthy90 <- glm(peakflowstatshy90$TotVolAbv_acft[peakflowstatshy90$TotVolAbv_acft != 0]~peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0], family=gaussian)
volpredicthy90 <- predict(gmtesthy90)
glmzeroprehy90 <- glm(peakflowstatshy90$volday_is_zero_cumsum[peakflowstatshy90$year<1974]~peakflowstatshy90$year[peakflowstatshy90$year<1974], family=gaussian)
prezeropredicthy90 <- predict(glmzeroprehy90)
glmzeroposthy90 <- glm(peakflowstatshy90$volday_is_zero_cumsum[peakflowstatshy90$year>1973]~peakflowstatshy90$year[peakflowstatshy90$year>1973], family=gaussian)
postzeropredicthy90 <- predict(glmzeroposthy90)

pdf(file="C:\\Users\\tiffn_000\\Desktop\\Figures\\zerosephy90_11441500.pdf", width=8.5, height=11)
par(mfrow=c(2,1))
plot(peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0],peakflowstatshy90$TotVolAbv_acft[peakflowstatshy90$TotVolAbv_acft != 0], xlab="Year",ylab="Total Volume Above 90% (acft)")
lines(peakflowstatshy90$year[peakflowstatshy90$TotVolAbv_acft != 0],volpredicthy90)

plot(peakflowstatshy90$year,peakflowstatshy90$volday_is_zero_cumsum, xlab="Year", ylab="Cumulative Number of Years With Zero Flow Above 90%")
lines(peakflowstatshy90$year[peakflowstatshy90$year<1974],prezeropredicthy90)
lines(peakflowstatshy90$year[peakflowstatshy90$year>1973],postzeropredicthy90)
dev.off()


plotstest <- hypeakplots(peakflowsummaryhy90, names(american)[[9]])
pdf(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[9]],"_areaMONplot.pdf",sep=""), width=8.5, height=11)
plotstest$monplot
dev.off()

pdf(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[9]],"_areaplot.pdf",sep=""), width=8.5, height=11)
plotstest$decadesplot
dev.off()


plotstestmon <- hypeakplotsstats(peakflowsmonthly_statshy90, names(american)[[9]])
pdf(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[9]],"_areaplot_Daysabove.pdf",sep=""), width=8.5, height=11)
plotstestmon$decades_plotdays
dev.off()
pdf(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[9]],"_areaplot_volabove.pdf",sep=""), width=8.5, height=11)
plotstestmon$decades_plotvol
dev.off()
png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[9]],"_areaplot.png",sep=""), width=8.5, height=11, units="in", res=600)
plotstest$decadesplot
dev.off()
png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[9]],"_areaplot_Daysabove.png",sep=""), width=8.5, height=11, units="in", res=600)
plotstestmon$decades_plotdays
dev.off()
png(file=paste("C:\\Users\\tiffn_000\\Desktop\\Figures\\",names(american)[[9]],"_areaplot_volabove.png",sep=""), width=8.5, height=11, units="in", res=600)
plotstestmon$decades_plotvol
dev.off()


############
colors <- c("black","red","orange","darkgoldenrod1","yellow","yellowgreen","green","cyan4","blue","purple","purple4","hotpink")
#colors <- c(rep("black",5),"green",rep("red",5))
#colors <- c(rep(153,6),rep(34,5))
monlist <- c("10","11","12","01","02","03","04","05","06","07","08","09")

plot(decades$`1904-1914`$plotorder,decades$`1904-1914`$prop_peaks,ylim=c(0,0.6),
		xaxt="n", xlab="Month", ylab=" number of peaks as a fraction of total peaks per decade",type="l")
axis(1,at=decades$`1904-1914`$plotorder, labels=monlist)
legend("right",legend=names(decades),lty=rep(1,length(colors)),col=colors, inset=0.05 )
for(i in 1:11){
	lines(decades[[i]]$plotorder,decades[[i]]$prop_peaks, col=colors[[i]])
}

pdf(file="C:\\Users\\tiffn_000\\Desktop\\Figures\\peaksmultiplot4.pdf", width=6, height = 44)
par(mfrow=c(11,1))
for(i in 1:11){
	plot(decades[[i]]$plotorder,decades[[i]]$prop_peaks,ylim=c(0,0.6),
			xaxt="n", xlab="Month", ylab=" number of peaks as a fraction of total peaks per decade",
			type="l", main=names(decades)[[i]], col=colors[[i]])
	axis(1,at=decades[[i]]$plotorder, labels=monlist)
	abline(v=c(2,7))
}
dev.off()