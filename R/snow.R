# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


library(sharpshootR)

SHASTA <- c(1,	2,	3)
SCOTT <- c(5,	417,	311,	4,	298,	285)	
TRINITY <- c(9,	10,	11,	12,	13,	16,	15,	14,	17)
EEL <- c(63)	
SACRAMENTO <- c(18,	20,	19,	21,	22)
MCCLOUD <- c(23,	25,	26,	27)	
PIT <- c(30,	28,	31,	32,	33,	35,	34,	343,	37,	41)	
STONYCR <- c(62)	
FEATHER <- c(47,	361,	48,	359,	279,	388,	360,	280,	75,	53,	51,	52,	290,	54,	387,	353,	355,	354,	49,	55,	56,	58,	59,	434,	61)
YUBA <- c(64,	65,	67,	66,	68,	69,	74,	76,	77,	78,	389,	390,	80,	372,	83,	391,	277,	85,	429)	
AMERICAN <- c(106,	331,	107,	365,	338,	108,	110,	109,	316,	113,	320,	115,	289,	114,	369,	120,	371,	122,	322,	127)	
MOKELUMNE <- c(323,	364,	129,	131,	132,	133,	363,	134,	435,	136,	137)	
STANISLAUS <- c(345,	344,	427,	138,	139,	384,	140,	142,	143,	416,	432,	430,	373,	145,	386)
TUOLUMNE <- c(157,	158,	159,	368,	161,	162,	163,	165,	166,	164,	167,	348,	173,	168,	169,	172,	171)	
MERCED <- c(176,	177,	178,	179,	180)
SANJOAQUIN <- c(182,	183,	184,	276,	185,	186,	187,	188,	189,	191,	190,	192,	193,	346,	194,	324,	440,	196,	347,	198,	197,	199,	200,	201,	202,	204)	
KINGS <- c(222,	299,	307,	398,	223,	396,	225,	224,	397,	229,	226,	227,	232,	426,	233,	230,	234,	308,	236,	237,	438,	239)	
KAWEAH <- c(292,	243,	244,	245,	246)	
TULE <- c(247,	248)	
KERN <- c(250,	251,	252,	253,	254,	255,	275,	257,	256,	258,	259,	260,	264,	261,	262,	265,	249)	
SUSAN <- c(45,	46)	
TRUCKEE <- c(334,	318,	89)
LAKETAHOE <- c(96,	97,	99,	100,	101,	333,	103,	104)
WALKER <- c(377,	152,	422,	154)	
MONOLAKE <- c(281,	181,	287,	286,	282)
OWENS <- c(284,	220,	212,	213,	221,	217,	209,	218,	219,	214,	205,	423,	210,	206,	211,	208)


LSHASTA <- vector("list", length=length(SHASTA))
names(LSHASTA) <- SHASTA
for(i in 1:length(SHASTA)){
	LSHASTA[[i]] <- CDECsnowQuery(SHASTA[[i]],1900,2015)
}

LSCOTT <- vector("list", length=length(SCOTT))
names(LSCOTT) <- SCOTT
for(i in 1:length(SCOTT)){
	LSCOTT[[i]] <- CDECsnowQuery(SCOTT[[i]],1900,2015)
}

### continue working on this section
for(n in 1:3){
assign(paste("L",basins[[n]],sep=""), vector("list", length=length(get(basins[[n]]))))
names(get(paste("L",basins[[n]],sep=""))) <- get(basins[[n]])
	for(i in 1:length(get(basins[[n]]))){
		a CDECsnowQuery(get(basins[[n]])[[i]],1900,2015)
	}
	paste("L",basins[[n]],sep="") <- a
}
basins <- c("SHASTA","SCOTT","TRINITY","EEL","SACRAMENTO","MCCLOUD",  "PIT",   "STONYCR",   "FEATHER",   "YUBA",   "AMERICAN",   "MOKELUMNE",   "STANISLAUS",   "TUOLUMNE",   "MERCED", "SANJOAQUIN",   "KINGS",   "KAWEAH",   	"TULE", "KERN", "SUSAN",  "TRUCKEE",   "LAKETAHOE",   "WALKER",   "MONOLAKE",   "OWENS")

LAMERICAN <- vector("list", length=length(AMERICAN))
names(LAMERICAN) <- AMERICAN
for(i in 1:length(AMERICAN)){
	LAMERICAN[[i]] <- CDECsnowQuery(AMERICAN[[i]],1900,2015)
}

LAMERICAN55 <- vector("list", length=length(AMERICAN))
names(LAMERICAN55) <- AMERICAN
for(i in 1:length(AMERICAN)){
	LAMERICAN55[[i]] <- CDECsnowQuery(AMERICAN[[i]],1955,2015)
}

MKTSnowAmerican_DEC55 <- data.frame(tau=rep(NA, length(LAMERICAN55)), pvalue=rep(NA, length(LAMERICAN55)), gauge=rep(NA, length(LAMERICAN55)), n=rep(NA, length(LAMERICAN55)), st_yr=rep(NA, length(LAMERICAN55)), end_yr=rep(NA, length(LAMERICAN55)))
for (i in 1:length(LAMERICAN55)){
	n <- length(which(LAMERICAN55[[i]]$month == "December"))
	if( n >5){
		a <- MannKendall(LAMERICAN55[[i]]$SWE[which(LAMERICAN55[[i]]$month == "December")])
		MKTSnowAmerican_DEC55$tau[[i]] <- a$tau
		MKTSnowAmerican_DEC55$pvalue[[i]] <- a$sl
		MKTSnowAmerican_DEC55$gauge[[i]] <- names(LAMERICAN55)[[i]]
		MKTSnowAmerican_DEC55$n[[i]] <- n
		MKTSnowAmerican_DEC55$st_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "December")[[1]]]]
		MKTSnowAmerican_DEC55$end_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "December")[[length(which(LAMERICAN55[[i]]$month == "December"))]]]]
	} else {
		MKTSnowAmerican_DEC55$n[[i]] <- n
	}
}


MKTSnowAmerican_JAN55 <- data.frame(tau=rep(NA, length(LAMERICAN55)), pvalue=rep(NA, length(LAMERICAN55)), gauge=rep(NA, length(LAMERICAN55)), n=rep(NA, length(LAMERICAN55)), st_yr=rep(NA, length(LAMERICAN55)), end_yr=rep(NA, length(LAMERICAN55)))
for (i in 1:length(LAMERICAN55)){
	n <- length(which(LAMERICAN55[[i]]$month == "January"))
	if( n >5){
		a <- MannKendall(LAMERICAN55[[i]]$SWE[which(LAMERICAN55[[i]]$month == "January")])
		MKTSnowAmerican_JAN55$tau[[i]] <- a$tau
		MKTSnowAmerican_JAN55$pvalue[[i]] <- a$sl
		MKTSnowAmerican_JAN55$gauge[[i]] <- names(LAMERICAN55)[[i]]
		MKTSnowAmerican_JAN55$n[[i]] <- n
		MKTSnowAmerican_JAN55$st_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "January")[[1]]]]
		MKTSnowAmerican_JAN55$end_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "January")[[length(which(LAMERICAN55[[i]]$month == "January"))]]]]
	} else {
		MKTSnowAmerican_JAN55$n[[i]] <- n
	}
}

MKTSnowAmerican_FEB55 <- data.frame(tau=rep(NA, length(LAMERICAN55)), pvalue=rep(NA, length(LAMERICAN55)), gauge=rep(NA, length(LAMERICAN55)), n=rep(NA, length(LAMERICAN55)), st_yr=rep(NA, length(LAMERICAN55)), end_yr=rep(NA, length(LAMERICAN55)))
for (i in 1:length(LAMERICAN55)){
	n <- length(which(LAMERICAN55[[i]]$month == "February"))
	if( n >5){
		a <- MannKendall(LAMERICAN55[[i]]$SWE[which(LAMERICAN55[[i]]$month == "February")])
		MKTSnowAmerican_FEB55$tau[[i]] <- a$tau
		MKTSnowAmerican_FEB55$pvalue[[i]] <- a$sl
		MKTSnowAmerican_FEB55$gauge[[i]] <- names(LAMERICAN55)[[i]]
		MKTSnowAmerican_FEB55$n[[i]] <- n
		MKTSnowAmerican_FEB55$st_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "February")[[1]]]]
		MKTSnowAmerican_FEB55$end_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "February")[[length(which(LAMERICAN55[[i]]$month == "February"))]]]]
	} else {
		MKTSnowAmerican_FEB55$n[[i]] <- n
	}
}

MKTSnowAmerican_MAR55 <- data.frame(tau=rep(NA, length(LAMERICAN55)), pvalue=rep(NA, length(LAMERICAN55)), gauge=rep(NA, length(LAMERICAN55)), n=rep(NA, length(LAMERICAN55)), st_yr=rep(NA, length(LAMERICAN55)), end_yr=rep(NA, length(LAMERICAN55)))
for (i in 1:length(LAMERICAN55)){
	n <- length(which(LAMERICAN55[[i]]$month == "March"))
	if( n >5){
		a <- MannKendall(LAMERICAN55[[i]]$SWE[which(LAMERICAN55[[i]]$month == "March")])
		MKTSnowAmerican_MAR55$tau[[i]] <- a$tau
		MKTSnowAmerican_MAR55$pvalue[[i]] <- a$sl
		MKTSnowAmerican_MAR55$gauge[[i]] <- names(LAMERICAN55)[[i]]
		MKTSnowAmerican_MAR55$n[[i]] <- n
		MKTSnowAmerican_MAR55$st_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "March")[[1]]]]
		MKTSnowAmerican_MAR55$end_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "March")[[length(which(LAMERICAN55[[i]]$month == "March"))]]]]
	} else {
		MKTSnowAmerican_MAR55$n[[i]] <- n
	}
}


MKTSnowAmerican_APR55 <- data.frame(tau=rep(NA, length(LAMERICAN55)), pvalue=rep(NA, length(LAMERICAN55)), gauge=rep(NA, length(LAMERICAN55)), n=rep(NA, length(LAMERICAN55)), st_yr=rep(NA, length(LAMERICAN55)), end_yr=rep(NA, length(LAMERICAN55)))
for (i in 1:length(LAMERICAN55)){
	n <- length(which(LAMERICAN55[[i]]$month == "April"))
	if( n >5){
		a <- MannKendall(LAMERICAN55[[i]]$SWE[which(LAMERICAN55[[i]]$month == "April")])
		MKTSnowAmerican_APR55$tau[[i]] <- a$tau
		MKTSnowAmerican_APR55$pvalue[[i]] <- a$sl
		MKTSnowAmerican_APR55$gauge[[i]] <- names(LAMERICAN55)[[i]]
		MKTSnowAmerican_APR55$n[[i]] <- n
		MKTSnowAmerican_APR55$st_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "April")[[1]]]]
		MKTSnowAmerican_APR55$end_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "April")[[length(which(LAMERICAN55[[i]]$month == "April"))]]]]
	} else {
		MKTSnowAmerican_APR55$n[[i]] <- n
	}
}


MKTSnowAmerican_MAY55 <- data.frame(tau=rep(NA, length(LAMERICAN55)), pvalue=rep(NA, length(LAMERICAN55)), gauge=rep(NA, length(LAMERICAN55)), n=rep(NA, length(LAMERICAN55)), st_yr=rep(NA, length(LAMERICAN55)), end_yr=rep(NA, length(LAMERICAN55)))
for (i in 1:length(LAMERICAN55)){
	n <- length(which(LAMERICAN55[[i]]$month == "May"))
	if( n >5){
		a <- MannKendall(LAMERICAN55[[i]]$SWE[which(LAMERICAN55[[i]]$month == "May")])
		MKTSnowAmerican_MAY55$tau[[i]] <- a$tau
		MKTSnowAmerican_MAY55$pvalue[[i]] <- a$sl
		MKTSnowAmerican_MAY55$gauge[[i]] <- names(LAMERICAN55)[[i]]
		MKTSnowAmerican_MAY55$n[[i]] <- n
		MKTSnowAmerican_MAY55$st_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "May")[[1]]]]
		MKTSnowAmerican_MAY55$end_yr[[i]] <- LAMERICAN55[[i]]$year[[which(LAMERICAN55[[i]]$month == "May")[[length(which(LAMERICAN55[[i]]$month == "May"))]]]]
	} else {
		MKTSnowAmerican_MAY55$n[[i]] <- n
	}
}


MKTSnowAmerican_DEC <- data.frame(tau=rep(NA, length(LAMERICAN)), pvalue=rep(NA, length(LAMERICAN)), gauge=rep(NA, length(LAMERICAN)), n=rep(NA, length(LAMERICAN)), st_yr=rep(NA, length(LAMERICAN)), end_yr=rep(NA, length(LAMERICAN)))
for (i in 1:length(LAMERICAN)){
	n <- length(which(LAMERICAN[[i]]$month == "December"))
	if( n >5){
		a <- MannKendall(LAMERICAN[[i]]$SWE[which(LAMERICAN[[i]]$month == "December")])
		MKTSnowAmerican_DEC$tau[[i]] <- a$tau
		MKTSnowAmerican_DEC$pvalue[[i]] <- a$sl
		MKTSnowAmerican_DEC$gauge[[i]] <- names(LAMERICAN)[[i]]
		MKTSnowAmerican_DEC$n[[i]] <- n
		MKTSnowAmerican_DEC$st_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "December")[[1]]]]
		MKTSnowAmerican_DEC$end_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "December")[[length(which(LAMERICAN[[i]]$month == "December"))]]]]
	} else {
		MKTSnowAmerican_DEC$n[[i]] <- n
	}
}


MKTSnowAmerican_JAN <- data.frame(tau=rep(NA, length(LAMERICAN)), pvalue=rep(NA, length(LAMERICAN)), gauge=rep(NA, length(LAMERICAN)), n=rep(NA, length(LAMERICAN)), st_yr=rep(NA, length(LAMERICAN)), end_yr=rep(NA, length(LAMERICAN)))
for (i in 1:length(LAMERICAN)){
	n <- length(which(LAMERICAN[[i]]$month == "January"))
	if( n >5){
		a <- MannKendall(LAMERICAN[[i]]$SWE[which(LAMERICAN[[i]]$month == "January")])
		MKTSnowAmerican_JAN$tau[[i]] <- a$tau
		MKTSnowAmerican_JAN$pvalue[[i]] <- a$sl
		MKTSnowAmerican_JAN$gauge[[i]] <- names(LAMERICAN)[[i]]
		MKTSnowAmerican_JAN$n[[i]] <- n
		MKTSnowAmerican_JAN$st_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "January")[[1]]]]
		MKTSnowAmerican_JAN$end_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "January")[[length(which(LAMERICAN[[i]]$month == "January"))]]]]
	} else {
		MKTSnowAmerican_JAN$n[[i]] <- n
	}
}

MKTSnowAmerican_FEB <- data.frame(tau=rep(NA, length(LAMERICAN)), pvalue=rep(NA, length(LAMERICAN)), gauge=rep(NA, length(LAMERICAN)), n=rep(NA, length(LAMERICAN)), st_yr=rep(NA, length(LAMERICAN)), end_yr=rep(NA, length(LAMERICAN)))
for (i in 1:length(LAMERICAN)){
	n <- length(which(LAMERICAN[[i]]$month == "February"))
	if( n >5){
		a <- MannKendall(LAMERICAN[[i]]$SWE[which(LAMERICAN[[i]]$month == "February")])
		MKTSnowAmerican_FEB$tau[[i]] <- a$tau
		MKTSnowAmerican_FEB$pvalue[[i]] <- a$sl
		MKTSnowAmerican_FEB$gauge[[i]] <- names(LAMERICAN)[[i]]
		MKTSnowAmerican_FEB$n[[i]] <- n
		MKTSnowAmerican_FEB$st_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "February")[[1]]]]
		MKTSnowAmerican_FEB$end_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "February")[[length(which(LAMERICAN[[i]]$month == "February"))]]]]
	} else {
		MKTSnowAmerican_FEB$n[[i]] <- n
	}
}

MKTSnowAmerican_MAR <- data.frame(tau=rep(NA, length(LAMERICAN)), pvalue=rep(NA, length(LAMERICAN)), gauge=rep(NA, length(LAMERICAN)), n=rep(NA, length(LAMERICAN)), st_yr=rep(NA, length(LAMERICAN)), end_yr=rep(NA, length(LAMERICAN)))
for (i in 1:length(LAMERICAN)){
	n <- length(which(LAMERICAN[[i]]$month == "March"))
	if( n >5){
		a <- MannKendall(LAMERICAN[[i]]$SWE[which(LAMERICAN[[i]]$month == "March")])
		MKTSnowAmerican_MAR$tau[[i]] <- a$tau
		MKTSnowAmerican_MAR$pvalue[[i]] <- a$sl
		MKTSnowAmerican_MAR$gauge[[i]] <- names(LAMERICAN)[[i]]
		MKTSnowAmerican_MAR$n[[i]] <- n
		MKTSnowAmerican_MAR$st_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "March")[[1]]]]
		MKTSnowAmerican_MAR$end_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "March")[[length(which(LAMERICAN[[i]]$month == "March"))]]]]
	} else {
		MKTSnowAmerican_MAR$n[[i]] <- n
	}
}


MKTSnowAmerican_APR <- data.frame(tau=rep(NA, length(LAMERICAN)), pvalue=rep(NA, length(LAMERICAN)), gauge=rep(NA, length(LAMERICAN)), n=rep(NA, length(LAMERICAN)), st_yr=rep(NA, length(LAMERICAN)), end_yr=rep(NA, length(LAMERICAN)))
for (i in 1:length(LAMERICAN)){
	n <- length(which(LAMERICAN[[i]]$month == "April"))
	if( n >5){
		a <- MannKendall(LAMERICAN[[i]]$SWE[which(LAMERICAN[[i]]$month == "April")])
		MKTSnowAmerican_APR$tau[[i]] <- a$tau
		MKTSnowAmerican_APR$pvalue[[i]] <- a$sl
		MKTSnowAmerican_APR$gauge[[i]] <- names(LAMERICAN)[[i]]
		MKTSnowAmerican_APR$n[[i]] <- n
		MKTSnowAmerican_APR$st_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "April")[[1]]]]
		MKTSnowAmerican_APR$end_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "April")[[length(which(LAMERICAN[[i]]$month == "April"))]]]]
	} else {
		MKTSnowAmerican_APR$n[[i]] <- n
	}
}


MKTSnowAmerican_MAY <- data.frame(tau=rep(NA, length(LAMERICAN)), pvalue=rep(NA, length(LAMERICAN)), gauge=rep(NA, length(LAMERICAN)), n=rep(NA, length(LAMERICAN)), st_yr=rep(NA, length(LAMERICAN)), end_yr=rep(NA, length(LAMERICAN)))
for (i in 1:length(LAMERICAN)){
	n <- length(which(LAMERICAN[[i]]$month == "May"))
	if( n >5){
		a <- MannKendall(LAMERICAN[[i]]$SWE[which(LAMERICAN[[i]]$month == "May")])
		MKTSnowAmerican_MAY$tau[[i]] <- a$tau
		MKTSnowAmerican_MAY$pvalue[[i]] <- a$sl
		MKTSnowAmerican_MAY$gauge[[i]] <- names(LAMERICAN)[[i]]
		MKTSnowAmerican_MAY$n[[i]] <- n
		MKTSnowAmerican_MAY$st_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "May")[[1]]]]
		MKTSnowAmerican_MAY$end_yr[[i]] <- LAMERICAN[[i]]$year[[which(LAMERICAN[[i]]$month == "May")[[length(which(LAMERICAN[[i]]$month == "May"))]]]]
	} else {
		MKTSnowAmerican_MAY$n[[i]] <- n
	}
}

write.csv(MKTSnowAmerican_JAN,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_JAN.csv")
write.csv(MKTSnowAmerican_JAN55,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_JAN55.csv")
write.csv(MKTSnowAmerican_DEC,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_DEC.csv")
write.csv(MKTSnowAmerican_DEC55,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_DEC55.csv")
write.csv(MKTSnowAmerican_FEB,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_FEB.csv")
write.csv(MKTSnowAmerican_FEB55,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_FEB55.csv")
write.csv(MKTSnowAmerican_MAR,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_MAR.csv")
write.csv(MKTSnowAmerican_MAR55,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_MAR55.csv")
write.csv(MKTSnowAmerican_APR,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_APR.csv")
write.csv(MKTSnowAmerican_APR55,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_APR55.csv")
write.csv(MKTSnowAmerican_MAY,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_MAY.csv")
write.csv(MKTSnowAmerican_MAY55,file="C:\\Users\\tiffn_000\\Documents\\Data\\American\\MKTSnowAmerican_MAY55.csv")
