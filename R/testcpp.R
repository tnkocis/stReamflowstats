# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


dataset1<-structure(list(y=c(-1.49719416897806,  -1.04139301128124,   0.0945236304367192, 2.34517600028403,  -2.01910934921224, 
						0.738818483718415, -1.29894622500239,  -2.28749064172037,  -1.83460914213215,   1.7264401742697, 
						-0.282651383157718, -0.86805194949488,  -0.864427449770363,  1.3890252146112,   -1.06755713953516, 
						-2.32897163610783,   1.66038960720445,   1.61487788113575,   1.0899088587434,    1.59281468275878, 
						-3.92950882381207,  -2.86897048491236,   0.756543092818702,  0.75605628995123,  -1.94551222893336, 
						-0.521647504402827,  0.750976521143092,  2.05661942206825,  -0.0553654356461042, 1.83487282390805, 
						-0.681331876121451, -1.09742432884331,   0.809885352209668, -1.75444757180733,  -1.32646138930983, 
						0.93932182852857,   2.69573032525144,   1.19980666244453,   2.66656782068436,  -2.04469844361269, 
						2.25443895842938,  -0.287436324399811, -0.646793604946014, -1.12134602060874,  -0.123057506010716, 
						3.40147762961413,  -0.105013757259212,  2.57888932230486,  -0.248784082384957, -0.80495910311552, 
						-1.62736886754023,  -1.80623698171649,   0.0826863988021116,-1.02443966620053,  -0.624640883344864), 
				x = c(-0.414242088066754,  0.0475817644136152,-0.815134097927552,  0.578444566236884, -0.459034615425856, 
						0.809378901441766, -0.977367705972685, -1.67738162375379,  -0.430291674794372,  1.42019731595199, 
						-0.749035091787927, -1.35225060418292,  -0.492196888552811,  0.539778435067201,  0.0459914831995694, 
						-1.2521152391345,    1.00360926349214,   1.09644063218488,   0.508112310623724,  1.32898360956295, 
						-1.5116989382724,   -1.80654236075974,   1.06240878829949,  -0.091275817909321, -0.570211504099203, 
						-0.268748606177556,  0.764654371144826,  1.9589087924292,   -0.906694839411102,  0.450015221346758, 
						-0.254560403793998, -1.30082788198172,   1.15863187888594,  -1.61536768356543,  -0.858653684897159, 
						1.13914138747698,   0.775779787679006,  0.267099868753253,  1.32191674807275,  -0.23978424525859,
						1.93490877730752,   1.08101027023282,  -1.51349689876255,  -0.758066482747475, -0.592268993080326, 
						0.676081370171539, -0.342776942447623,  1.0517207133769,   -0.39294982859476,   0.622178484006425, 
						6.22656137599703,   6.45244734765708,   8.50469010153436,   6.24821960890362,   7.56808156590792
				)), .Names = c("y", "x"), row.names = c(NA, -55L), class = "data.frame")


tsreg_C_edit <- function (x, y, xout = FALSE, outfun = out, iter = 10, varfun = pbvar, 
		corfun = pbcor, plotit = FALSE, WARN = TRUE, HD = FALSE, 
		...) 
{
	x <- as.matrix(x)
	xx <- cbind(x, y)
	xx <- elimna(xx)
	x <- xx[, 1:ncol(x)]
	x <- as.matrix(x)
	y <- xx[, ncol(x) + 1]
	temp <- NA
	x <- as.matrix(x)
	if (xout) {
		x <- as.matrix(x)
		flag <- outfun(x, plotit = plotit, ...)$keep
		x <- x[flag, ]
		y <- y[flag]
		x <- as.matrix(x)
	}
	if (ncol(x) == 1) {
		temp1 <- .Call("tsp1reg_C", X = x, Y = y, HD = as.integer(HD))
		coef <- temp1[1]
		res <- temp1[2]
	}
	if (ncol(x) > 1) {
		temp1 <- .Call("tsreg_C", X = x, Y = y, IT = as.integer(iter), 
				HD = as.integer(HD))
		coef <- c(temp1$alpha, temp1$beta)
		res <- temp1$res
	}
	yhat <- y - res
	stre = NULL
	temp = varfun(y)
	if (temp == 0) {
		if (WARN) 
			print("Warning: When computing strength of association, measure of variation=0")
	}
	e.pow = NULL
	if (temp > 0) {
		e.pow <- varfun(yhat)/varfun(y)
		if (!is.na(e.pow)) {
			if (e.pow >= 1) 
				e.pow <- corfun(yhat, y)$cor^2
			e.pow = as.numeric(e.pow)
			stre = sqrt(e.pow)
		}
	}
	list(coef = coef, residuals = res, Strength.Assoc = stre, 
			Explanatory.Power = e.pow)
}

Atest.tsregC <- list(coef = coef, residuals = res, Strength.Assoc = stre, 
		Explanatory.Power = e.pow)


library(Rcpp)
sourceCpp("C:/Users/tiffn_000/Documents/theilsen_v2.cpp")

xs <- c(seq(100), NA, NA)
ys <- sort(rnorm(102))

xs1 <- seq(102)
ys1 <- sort(rnorm(102))

slopemat <- theilsenslopes(xs, ys)
medianslope <- theilsenmedian(slopemat)
interceptvec <- theilsenintercepts(xs, ys, medianslope)

theilsen.test <- list()
theilsen.test$slopes <- theilsenslopes(x,y)
theilsen.test$medianslope <- theilsenmedian(theilsen.test$slopes)
theilsen.test$slopesomit <- naomit(theilsen.test$slopesprep)
#theilsen.test$medianslope <- median(theilsen.test$slopes, na.rm=TRUE)
theilsen.test$intercepts <- theilsenintercepts(x, y, theilsen.test$medianslope)
theilsen.test$medianintercept <- median(theilsen.test$intercepts,na.rm=TRUE)

