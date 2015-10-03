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


