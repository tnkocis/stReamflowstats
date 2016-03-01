# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


MKTFPW <- function (y, x = 1:length(y), conf.intervals = TRUE, preserve.range.for.sig.test = TRUE) 
{
	library(Kendall)
	library(zoo)
	library(zyp)
	dat <- as.numeric(as.vector(y))
	if (is.logical(x)) 
		stop("x cannot be of type 'logical' (perhaps you meant to specify conf.intervals?)")
	n <- length(dat)
	t <- x
	t.prime <- t[1:(n - 1)]
	y <- dat
	ret <- c(lbound = NA, trend = NA, trendp = NA, ubound = NA, 
			tau = NA, sig = NA, nruns = NA, autocor = NA, valid_frac = NA, 
			linear = NA, intercept = NA)
	dmap <- which(!is.na(y))
	ynm <- as.numeric(y[dmap])
	tnm <- as.numeric(t[dmap])
	if (length(dmap) <= 3 | length(which(ynm != 0)) < 3 | length(dmap)/n < 
			0.1) {
		return(ret)
	}
	sen <- zyp.sen(ynm ~ tnm)
	trend <- sen$coefficients[2]
	xt.prime <- dat[1:n] - trend * t
	ac <- acf(xt.prime, lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2]
	if (is.na(ac)) {
		return(ret)
	}
	yt.prime <- ifelse(rep(preserve.range.for.sig.test, n - 1), 
			(xt.prime[2:n] - ac * xt.prime[1:(n - 1)])/(1 - ac), 
			xt.prime[2:n] - ac * xt.prime[1:(n - 1)])
	yt <- yt.prime[1:(n - 1)] + trend * t.prime
#	dmap.prime <- which(!is.na(yt))
	ytnm <- as.numeric(yt)
	MK <- MannKendall(ytnm)
	tau <- MK$tau[[1]]
	Bsig <- MK$sl[[1]]
	if (conf.intervals) {
		ci <- confint(sen)
	}
	else {
		ci <- matrix(rep(NA, 4), nrow = 2, ncol = 2)
	}
	ret <- c(lbound = as.numeric(ci[2, 1]), trend = as.numeric(trend), 
			trendp = as.numeric(trend) * n, ubound = as.numeric(ci[2, 
							2]), tau = as.numeric(tau), sig = as.numeric(Bsig), 
			nruns = 1, autocor = as.numeric(ac), valid_frac = as.numeric(length(dmap)/length(y)), 
			linear = as.numeric(lm(dat ~ t)$coefficients[2]), intercept = as.numeric(sen$coefficients[1]))
	return(ret)
}
