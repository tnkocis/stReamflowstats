# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


#OLS estimates

return.data <- read.csv("C:\\Users\\tiffn_000\\Desktop\\return_period_t85.csv",header=T)
#return.data <- read.csv("return_period_ab2.csv",header=T)
head(return.data)    

mod.ols <- lm(T_100 ~ time, data=return.data)
summary(mod.ols)

plot(return.data$time, residuals(mod.ols), type='o')
abline(h=0, lty=2)

##library(ts) - this is now part of the base installation
par(mfrow=c(2,1))
acf(residuals(mod.ols))
acf(residuals(mod.ols), type='partial')

#durbin.watson(mod.ols, max.lag=10)
durbinWatsonTest(mod.ols, max.lag=10)

## GLS estimates

#library(nlme) #- only load ones at the beginning
mod.gls <- gls(T_100 ~ time, correlation=corARMA(p=1), method='ML',data=return.data)
summary(mod.gls)
summary(mod.gls)$tTable #gives full p-value




mod.ols <- lm(test.zoo.lp3.sac$prob0.1Q_maf ~ test.zoo.lp3.sac$Date)
summary(mod.ols)

plot(test.zoo.lp3.sac$Date, residuals(mod.ols), type='o')
abline(h=0, lty=2)

##library(ts) - this is now part of the base installation
par(mfrow=c(2,1))
acf(residuals(mod.ols))
acf(residuals(mod.ols), type='partial')

#durbin.watson(mod.ols, max.lag=10)
durbinWatsonTest(mod.ols, max.lag=10)

## GLS estimates

#library(nlme) #- only load ones at the beginning
mod.gls <- gls(prob0.1Q_maf ~ Date, data=test.zoo.lp3.sac, correlation=corARMA(p=1), method='ML')
summary(mod.gls)
summary(mod.gls)$tTable #gives full p-value





mod.ols <- lm(test.zoo.lp3$prob0.1Q_maf ~ test.zoo.lp3$Date)
summary(mod.ols)

plot(test.zoo.lp3$Date, residuals(mod.ols), type='o')
abline(h=0, lty=2)

##library(ts) - this is now part of the base installation
par(mfrow=c(2,1))
acf(residuals(mod.ols))
acf(residuals(mod.ols), type='partial')

#durbin.watson(mod.ols, max.lag=10)
durbinWatsonTest(mod.ols, max.lag=10)

## GLS estimates

#library(nlme) #- only load ones at the beginning
mod.gls <- gls(prob0.1Q_maf ~ Date, data=test.zoo.lp3, correlation=corARMA(p=1), method='ML')
summary(mod.gls)
summary(mod.gls)$tTable #gives full p-value