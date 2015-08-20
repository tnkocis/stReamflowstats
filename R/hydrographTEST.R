# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


plot(blah$All$Continuous, type="n")
lines(blah$All$`1960s`, col="red")
lines(blah$All$`1970s`, col="orange")
lines(blah$All$`1980s`, col="yellow")
lines(blah$All$`1990s`, col="green")
lines(blah$All$`2000s`, col="blue")
lines(blah$All$`2010s`, col="purple")

plot(blah$W$Continuous, type="n")
lines(blah$C$Continuous, col="red")
lines(blah$D$Continuous, col="orange")
lines(blah$BN$Continuous, col="yellow")
lines(blah$AN$Continuous, col="green")
lines(blah$W$Continuous, col="blue")
legend(x=as.Date("0001-10-01"), y=.0020, legend=c("C", "D", "BN", "AN"),
		fill=c("red","orange","yellow","green"))

dev.new()
plot(blah2$W$Continuous, type="n")
lines(blah2$C$Continuous, col="red")
lines(blah2$D$Continuous, col="orange")
lines(blah2$BN$Continuous, col="yellow")
lines(blah2$AN$Continuous, col="green")
lines(blah2$W$Continuous, col="blue")
legend(x=as.Date("0001-10-01"), y=.015, legend=c("C", "D", "BN", "AN","W"), 
		fill=c("red","orange","yellow","green","blue"))

dev.new()
par( mfrow=c(7,1))
date.seq <- seq(from=as.Date("0001-12-01"), to= as.Date("0002-02-28"), by="day")
val.seq <- rep(NA, length(date.seq))
plot(date.seq,val.seq,
		#xlim=c(as.Date("0001-12-01"),as.Date(("0002-02-28"))),
		ylim=c(0.01,0.08),
		ylab="Discharge (maf)",
		xlab="Date")
lines(blah2$All$`1960s`, col="red")
lines(blah2$All$`1970s`, col="orange")
lines(blah2$All$`1980s`, col="yellow")
lines(blah2$All$`1990s`, col="green")
lines(blah2$All$`2000s`, col="blue")
lines(blah2$All$`2010s`, col="purple")
legend(x=as.Date("0001-11-01"), y=.00010, 
		legend=c("1960s", "1970s", "1980s", "1990s","2000s", "2010s"), 
		fill=c("red","orange","yellow","green","blue","purple"))



ggplot(blah2$W$Continuous, aes(x = Date, y = Discharge_maf_avg)) +
		geom_line(data = blah2$C$Continuous, aes(color = "Critical")) +
		geom_line(data = blah2$D$Continuous, aes(color = "Dry")) +
		geom_line(data = blah2$BN$Continuous, aes(color = "Below Normal")) +
		geom_line(data = blah2$AN$Continuous, aes(color = "Above Normal")) +
		geom_line(data = blah2$W$Continuous, aes(color = "Wet")) +
		scale_color_manual(values = c( "Critical" = "red", "Dry" = "orange", 
						"Below Normal" = "yellow", "Above Normal" = "green", "Wet" = "blue"), name="Year Type")+
		labs(title = "Mean Year Type Hydrograph for San Joaquin River Near Newman, CA (11274000)",
				x = "Month", y="Discharge (MAF)")+
		scale_x_date(labels = date_format("%b"), breaks=date_breaks("months"))



ggplot(m, aes(x = yeartype, y = Volume, width=0.9, fill=yeartype)) + geom_bar(stat="identity") + facet_wrap(~gauge)+ 
		theme(axis.text.x=element_text(size=10)) + scale_fill_brewer(palette = "YlGnBu") +
		scale_x_discrete(labels=c("C", "D", "BN", "AN","W")) + guides(fill=guide_legend(title="Year Type", reverse=TRUE))+
		labs(title="Total Flows Above 90th Percentile For Average Year Type (100 Years of Data)
						November to January", 
				x="Year Type", y="Magnitude of Average Year Type Total Flow (TAF)")+
		scale_y_continuous(labels = comma, breaks=pretty_breaks(n=10)) 