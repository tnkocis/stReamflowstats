# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


plot(blah$All$Continuous, type="n", ylim=c(0,2e-4))
lines(blah$All$`1960s`, col="red")
lines(blah$All$`1970s`, col="orange")
lines(blah$All$`1980s`, col="yellow")
lines(blah$All$`1990s`, col="green")
lines(blah$All$`2000s`, col="blue")
lines(blah$All$`2010s`, col="purple")

plot(blah$AN$Continuous, type="n", ylim=c(0,3.75e-4))
lines(blah$C$Continuous, col="red")
lines(blah$D$Continuous, col="orange")
lines(blah$BN$Continuous, col="yellow")
lines(blah$AN$Continuous, col="green")
lines(blah$W$Continuous, col="blue")
legend(x=as.Date("0001-10-01"), y=.00015, legend=c("C", "D", "BN", "AN"),
		fill=c("red","orange","yellow","green"))

dev.new()
plot(blah2$W$Continuous, type="n")
lines(blah2$C$Continuous, col="red")
lines(blah2$D$Continuous, col="orange")
lines(blah2$BN$Continuous, col="yellow")
lines(blah2$AN$Continuous, col="green")
lines(blah2$W$Continuous, col="blue")
legend(x=as.Date("0001-10-01"), y=.00015, legend=c("C", "D", "BN", "AN"), 
		fill=c("red","orange","yellow","green"))

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
