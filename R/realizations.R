# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################
mux <- mean(test_split$`11452500`$all$hy$TotVolAbv_acft, na.rm=TRUE)
varx <- var(test_split$`11452500`$all$hy$TotVolAbv_acft, na.rm=TRUE)
p1 <- acf(test_split$`11452500`$all$hy$TotVolAbv_acft, na.action=na.pass, plot=FALSE)$acf[2]

to <- 0
xto <- 0
mu <- mux 
sigma <- sqrt(varx) 
slope <- 2e6/100

#number of realizations
n <-1000

#assign delta t
deltat <- 1 

#generate a sequence of times to calculate X, from to to t=200, by 0.5 hour increments
tseq <- seq(to,100,deltat)

#initalize matrix to populate with values for X
realizations2 <-as.data.frame(matrix(0, ncol=n, nrow=length(tseq), dimnames=list(NULL, 
						paste0('x',1:n))))
realizations2$t <- tseq
realizations2[1,1:n] <- xto

#populate matrix using x(t+deltat)= mu*deltat + sigma*dWt + x(t)
#dWt is calculated using rnorm(1,0,0.5), where rnorm generates 1 random number generated from a normal distribution with
# mean = 0 and variance=deltat = 0.5
# rnorm uses a C translation of the code developed in
#Wichura, M. J. (1988) Algorithm AS 241: The percentage points of the normal distribution. Applied Statistics, 37, 477–484.
for(i in 1:n){
	for(k in 2:length(realizations2$t)){
		realizations2[k,i] <- mu + p1*(realizations2[(k-1),i]-mux)+sigma*sqrt(1-(p1^2))*rnorm(1,0,1) #+slope*t[[i]]
	}
}

#load plotting libraries
library("reshape2")
library("ggplot2")
library("plyr")

#coerce data to necessary plottable format
melted2 <- melt(realizations2,id.vars="t")

#create plot of all realizations
plotdata <- ggplot(data=melted2, aes(x=t, y=value, group=variable)) + geom_line(alpha=0.01) +
		labs(title=paste("value v t
								
								delta-t = 1 year, ",n," Realizations",sep=""), x = "t (years)", y= "value")
snrealizationtest <- SNtest(realizations2[,1], realizations2$t, dwt=FALSE,gauge="test")
for(i in 1:n){
	snrealizationtest2 <- SNtest(realizations2[,i], realizations2$t, dwt=FALSE,gauge="test")
	snrealizationtest <- rbind.data.frame(snrealizationtest,snrealizationtest2)
}

meandist <- rep(NA,101)
for(i in 1:length(meandist)){
		meandist[[i]] <- mean(as.numeric(realizations2[i,1:100]))
}

snrealizationtestroll <- SNtest(rollmean(realizations2[,1],30),index(rollmean(realizations2[,1],30)), dwt=FALSE,gauge="test")
for(i in 1:n){
	snrealizationtest2 <- SNtest(rollmean(realizations2[,i],30),index(rollmean(realizations2[,i],30)), dwt=FALSE,gauge="test")
	snrealizationtestroll <- rbind.data.frame(snrealizationtestroll,snrealizationtest2)
}
