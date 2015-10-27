# TODO: Add comment
# 
# Author: tnkocis
###############################################################################

AMERICAN_precip_stations <- c("BLC",
		"SGP",
		'CLF',
		'GKS',
		'HLH',
		'FRH',
		'LON',
		'VVL',
		'GRG',
		'GTW',
		'RBB',
		'RBP',
		'ADR',
		'NCS',
		'FRN',
		'ALP',
		'PCF',
		'PHM',
		'PFH',
		'EDI',
		'SCN',
		'CAP',
		'CPT',
		'PCV',
		'PWS',
		'FLD',
		'FOL',
		'SIL',
		'CHG',
		'RNC',
		'ARW',
		'CSU')
AMERICAN_precip <- vector("list", length=length(AMERICAN_precip_stations))
names(AMERICAN_precip) <- AMERICAN_precip_stations
for(i in 1:length(AMERICAN_precip_stations)){
	AMERICAN_precip[[i]] <- CDECquery(AMERICAN_precip_stations[[i]] ,45, interval="D","1900-10-01","2015-10-01")
}

for(i in 1:length(AMERICAN_precip_stations)){
	write.csv(AMERICAN_precip[[i]], file=paste("C:\\Users\\tnkocis\\Google Drive\\precip\\",AMERICAN_precip_stations[[i]],".csv",sep=""))
}