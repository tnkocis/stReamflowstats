# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


peakflowanalysis_split <- function(pfmonthlystats){
	oct <- pfmonthlystats[which(pfmonthlystats$month=="10"),]
	oct_5 <- pfmonthlystats[which(pfmonthlystats$month=="10" & pfmonthlystats$yeartype_index==5),]
	oct_4 <- pfmonthlystats[which(pfmonthlystats$month=="10" & pfmonthlystats$yeartype_index==4),]
	oct_3 <- pfmonthlystats[which(pfmonthlystats$month=="10" & pfmonthlystats$yeartype_index==3),]
	oct_2 <- pfmonthlystats[which(pfmonthlystats$month=="10" & pfmonthlystats$yeartype_index==2),]
	oct_1 <- pfmonthlystats[which(pfmonthlystats$month=="10" & pfmonthlystats$yeartype_index==1),]
	
	nov <- pfmonthlystats[which(pfmonthlystats$month=="11"),]
	nov_5 <- pfmonthlystats[which(pfmonthlystats$month=="11" & pfmonthlystats$yeartype_index==5),]
	nov_4 <- pfmonthlystats[which(pfmonthlystats$month=="11" & pfmonthlystats$yeartype_index==4),]
	nov_3 <- pfmonthlystats[which(pfmonthlystats$month=="11" & pfmonthlystats$yeartype_index==3),]
	nov_2 <- pfmonthlystats[which(pfmonthlystats$month=="11" & pfmonthlystats$yeartype_index==2),]
	nov_1 <- pfmonthlystats[which(pfmonthlystats$month=="11" & pfmonthlystats$yeartype_index==1),]
	
	dec <- pfmonthlystats[which(pfmonthlystats$month=="12"),]
	dec_5 <- pfmonthlystats[which(pfmonthlystats$month=="12" & pfmonthlystats$yeartype_index==5),]
	dec_4 <- pfmonthlystats[which(pfmonthlystats$month=="12" & pfmonthlystats$yeartype_index==4),]
	dec_3 <- pfmonthlystats[which(pfmonthlystats$month=="12" & pfmonthlystats$yeartype_index==3),]
	dec_2 <- pfmonthlystats[which(pfmonthlystats$month=="12" & pfmonthlystats$yeartype_index==2),]
	dec_1 <- pfmonthlystats[which(pfmonthlystats$month=="12" & pfmonthlystats$yeartype_index==1),]
	
	jan <- pfmonthlystats[which(pfmonthlystats$month=="01"),]
	jan_5 <- pfmonthlystats[which(pfmonthlystats$month=="01" & pfmonthlystats$yeartype_index==5),]
	jan_4 <- pfmonthlystats[which(pfmonthlystats$month=="01" & pfmonthlystats$yeartype_index==4),]
	jan_3 <- pfmonthlystats[which(pfmonthlystats$month=="01" & pfmonthlystats$yeartype_index==3),]
	jan_2 <- pfmonthlystats[which(pfmonthlystats$month=="01" & pfmonthlystats$yeartype_index==2),]
	jan_1 <- pfmonthlystats[which(pfmonthlystats$month=="01" & pfmonthlystats$yeartype_index==1),]
	
	feb <- pfmonthlystats[which(pfmonthlystats$month=="02"),]
	feb_5 <- pfmonthlystats[which(pfmonthlystats$month=="02" & pfmonthlystats$yeartype_index==5),]
	feb_4 <- pfmonthlystats[which(pfmonthlystats$month=="02" & pfmonthlystats$yeartype_index==4),]
	feb_3 <- pfmonthlystats[which(pfmonthlystats$month=="02" & pfmonthlystats$yeartype_index==3),]
	feb_2 <- pfmonthlystats[which(pfmonthlystats$month=="02" & pfmonthlystats$yeartype_index==2),]
	feb_1 <- pfmonthlystats[which(pfmonthlystats$month=="02" & pfmonthlystats$yeartype_index==1),]
	
	mar <- pfmonthlystats[which(pfmonthlystats$month=="03"),]
	mar_5 <- pfmonthlystats[which(pfmonthlystats$month=="03" & pfmonthlystats$yeartype_index==5),]
	mar_4 <- pfmonthlystats[which(pfmonthlystats$month=="03" & pfmonthlystats$yeartype_index==4),]
	mar_3 <- pfmonthlystats[which(pfmonthlystats$month=="03" & pfmonthlystats$yeartype_index==3),]
	mar_2 <- pfmonthlystats[which(pfmonthlystats$month=="03" & pfmonthlystats$yeartype_index==2),]
	mar_1 <- pfmonthlystats[which(pfmonthlystats$month=="03" & pfmonthlystats$yeartype_index==1),]
	
	apr <- pfmonthlystats[which(pfmonthlystats$month=="04"),]
	apr_5 <- pfmonthlystats[which(pfmonthlystats$month=="04" & pfmonthlystats$yeartype_index==5),]
	apr_4 <- pfmonthlystats[which(pfmonthlystats$month=="04" & pfmonthlystats$yeartype_index==4),]
	apr_3 <- pfmonthlystats[which(pfmonthlystats$month=="04" & pfmonthlystats$yeartype_index==3),]
	apr_2 <- pfmonthlystats[which(pfmonthlystats$month=="04" & pfmonthlystats$yeartype_index==2),]
	apr_1 <- pfmonthlystats[which(pfmonthlystats$month=="04" & pfmonthlystats$yeartype_index==1),]
	
	may <- pfmonthlystats[which(pfmonthlystats$month=="05"),]
	may_5 <- pfmonthlystats[which(pfmonthlystats$month=="05" & pfmonthlystats$yeartype_index==5),]
	may_4 <- pfmonthlystats[which(pfmonthlystats$month=="05" & pfmonthlystats$yeartype_index==4),]
	may_3 <- pfmonthlystats[which(pfmonthlystats$month=="05" & pfmonthlystats$yeartype_index==3),]
	may_2 <- pfmonthlystats[which(pfmonthlystats$month=="05" & pfmonthlystats$yeartype_index==2),]
	may_1 <- pfmonthlystats[which(pfmonthlystats$month=="05" & pfmonthlystats$yeartype_index==1),]
	
	jun <- pfmonthlystats[which(pfmonthlystats$month=="06"),]
	jun_5 <- pfmonthlystats[which(pfmonthlystats$month=="06" & pfmonthlystats$yeartype_index==5),]
	jun_4 <- pfmonthlystats[which(pfmonthlystats$month=="06" & pfmonthlystats$yeartype_index==4),]
	jun_3 <- pfmonthlystats[which(pfmonthlystats$month=="06" & pfmonthlystats$yeartype_index==3),]
	jun_2 <- pfmonthlystats[which(pfmonthlystats$month=="06" & pfmonthlystats$yeartype_index==2),]
	jun_1 <- pfmonthlystats[which(pfmonthlystats$month=="06" & pfmonthlystats$yeartype_index==1),]
	
	jul <- pfmonthlystats[which(pfmonthlystats$month=="07"),]
	jul_5 <- pfmonthlystats[which(pfmonthlystats$month=="07" & pfmonthlystats$yeartype_index==5),]
	jul_4 <- pfmonthlystats[which(pfmonthlystats$month=="07" & pfmonthlystats$yeartype_index==4),]
	jul_3 <- pfmonthlystats[which(pfmonthlystats$month=="07" & pfmonthlystats$yeartype_index==3),]
	jul_2 <- pfmonthlystats[which(pfmonthlystats$month=="07" & pfmonthlystats$yeartype_index==2),]
	jul_1 <- pfmonthlystats[which(pfmonthlystats$month=="07" & pfmonthlystats$yeartype_index==1),]
	
	aug <- pfmonthlystats[which(pfmonthlystats$month=="08"),]
	aug_5 <- pfmonthlystats[which(pfmonthlystats$month=="08" & pfmonthlystats$yeartype_index==5),]
	aug_4 <- pfmonthlystats[which(pfmonthlystats$month=="08" & pfmonthlystats$yeartype_index==4),]
	aug_3 <- pfmonthlystats[which(pfmonthlystats$month=="08" & pfmonthlystats$yeartype_index==3),]
	aug_2 <- pfmonthlystats[which(pfmonthlystats$month=="08" & pfmonthlystats$yeartype_index==2),]
	aug_1 <- pfmonthlystats[which(pfmonthlystats$month=="08" & pfmonthlystats$yeartype_index==1),]
	
	sep <- pfmonthlystats[which(pfmonthlystats$month=="09"),]
	sep_5 <- pfmonthlystats[which(pfmonthlystats$month=="09" & pfmonthlystats$yeartype_index==5),]
	sep_4 <- pfmonthlystats[which(pfmonthlystats$month=="09" & pfmonthlystats$yeartype_index==4),]
	sep_3 <- pfmonthlystats[which(pfmonthlystats$month=="09" & pfmonthlystats$yeartype_index==3),]
	sep_2 <- pfmonthlystats[which(pfmonthlystats$month=="09" & pfmonthlystats$yeartype_index==2),]
	sep_1 <- pfmonthlystats[which(pfmonthlystats$month=="09" & pfmonthlystats$yeartype_index==1),]
	
	uniqueyear <- unique(pfmonthlystats$sthyyear)
	mon3 <- data.frame(TotVolAbv_acft=rep(NA,length(uniqueyear)),TotDaysAbv=rep(NA,length(uniqueyear)),numpeaks=rep(NA,length(uniqueyear)),mean_peakflow=rep(NA,length(uniqueyear)),
			total_peakflow=rep(NA,length(uniqueyear)),sthyyear=rep(NA,length(uniqueyear)),yeartype_index=rep(NA,length(uniqueyear)))
	for(i in 1:length(uniqueyear)){
		mon3$TotVolAbv_acft[[i]] <- sum(pfmonthlystats$TotVolAbv_acft[which((pfmonthlystats$month=="12" |pfmonthlystats$month=="01"|pfmonthlystats$month=="02")&pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		mon3$TotDaysAbv[[i]] <- sum(pfmonthlystats$TotDaysAbv[which((pfmonthlystats$month=="12" |pfmonthlystats$month=="01"|pfmonthlystats$month=="02")&pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		mon3$numpeaks[[i]] <- sum(pfmonthlystats$numpeaks[which((pfmonthlystats$month=="12" |pfmonthlystats$month=="01"|pfmonthlystats$month=="02")&pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		mon3$total_peakflow[[i]] <- sum(pfmonthlystats$total_peakflow[which((pfmonthlystats$month=="12" |pfmonthlystats$month=="01"|pfmonthlystats$month=="02")&pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		mon3$mean_peakflow[[i]] <- mon3$total_peakflow[[i]]/mon3$numpeaks[[i]] 
		mon3$sthyyear[[i]] <- uniqueyear[[i]]
		mon3$yeartype_index[[i]] <- pfmonthlystats$yeartype_index[which(pfmonthlystats$sthyyear==uniqueyear[[i]])][[1]]
	}
	mon3_5 <- mon3[which(mon3$yeartype_index==5),]
	mon3_4 <- mon3[which(mon3$yeartype_index==4),]
	mon3_3 <- mon3[which(mon3$yeartype_index==3),]
	mon3_2 <- mon3[which(mon3$yeartype_index==2),]
	mon3_1 <- mon3[which(mon3$yeartype_index==1),]
	
	
	
	
	mon6 <- data.frame(TotVolAbv_acft=rep(NA,length(uniqueyear)),TotDaysAbv=rep(NA,length(uniqueyear)),numpeaks=rep(NA,length(uniqueyear)),mean_peakflow=rep(NA,length(uniqueyear)),
			total_peakflow=rep(NA,length(uniqueyear)),sthyyear=rep(NA,length(uniqueyear)),yeartype_index=rep(NA,length(uniqueyear)))
	for(i in 1:length(uniqueyear)){
		mon6$TotVolAbv_acft[[i]] <- sum(pfmonthlystats$TotVolAbv_acft[which((pfmonthlystats$month=="11"|pfmonthlystats$month=="12" |pfmonthlystats$month=="01"|pfmonthlystats$month=="02"|pfmonthlystats$month=="03"|pfmonthlystats$month=="04")&pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		mon6$TotDaysAbv[[i]] <- sum(pfmonthlystats$TotDaysAbv[which((pfmonthlystats$month=="11"|pfmonthlystats$month=="12" |pfmonthlystats$month=="01"|pfmonthlystats$month=="02"|pfmonthlystats$month=="03"|pfmonthlystats$month=="04")&pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		mon6$numpeaks[[i]] <- sum(pfmonthlystats$numpeaks[which((pfmonthlystats$month=="11"|pfmonthlystats$month=="12" |pfmonthlystats$month=="01"|pfmonthlystats$month=="02"|pfmonthlystats$month=="03"|pfmonthlystats$month=="04")&pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		mon6$total_peakflow[[i]] <- sum(pfmonthlystats$total_peakflow[which((pfmonthlystats$month=="11"|pfmonthlystats$month=="12" |pfmonthlystats$month=="01"|pfmonthlystats$month=="02"|pfmonthlystats$month=="03"|pfmonthlystats$month=="04")&pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		mon6$mean_peakflow[[i]] <- mon6$total_peakflow[[i]]/mon6$numpeaks[[i]] 
		mon6$sthyyear[[i]] <- uniqueyear[[i]]
		mon6$yeartype_index[[i]] <- pfmonthlystats$yeartype_index[which(pfmonthlystats$sthyyear==uniqueyear[[i]])][[1]]
	}
	mon6_5 <- mon6[which(mon6$yeartype_index==5),]
	mon6_4 <- mon6[which(mon6$yeartype_index==4),]
	mon6_3 <- mon6[which(mon6$yeartype_index==3),]
	mon6_2 <- mon6[which(mon6$yeartype_index==2),]
	mon6_1 <- mon6[which(mon6$yeartype_index==1),]
	
	
	
	
	hy <- data.frame(TotVolAbv_acft=rep(NA,length(uniqueyear)),TotDaysAbv=rep(NA,length(uniqueyear)),numpeaks=rep(NA,length(uniqueyear)),mean_peakflow=rep(NA,length(uniqueyear)),
			total_peakflow=rep(NA,length(uniqueyear)),sthyyear=rep(NA,length(uniqueyear)),yeartype_index=rep(NA,length(uniqueyear)))
	for(i in 1:length(uniqueyear)){
		hy$TotVolAbv_acft[[i]] <- sum(pfmonthlystats$TotVolAbv_acft[which(pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		hy$TotDaysAbv[[i]] <- sum(pfmonthlystats$TotDaysAbv[which(pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		hy$numpeaks[[i]] <- sum(pfmonthlystats$numpeaks[which(pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		hy$total_peakflow[[i]] <- sum(pfmonthlystats$total_peakflow[which(pfmonthlystats$sthyyear==uniqueyear[[i]])], na.rm=TRUE)
		hy$mean_peakflow[[i]] <- hy$total_peakflow[[i]]/hy$numpeaks[[i]] 
		hy$sthyyear[[i]] <- uniqueyear[[i]]
		hy$yeartype_index[[i]] <- pfmonthlystats$yeartype_index[which(pfmonthlystats$sthyyear==uniqueyear[[i]])][[1]]
	}
	hy_5 <- hy[which(hy$yeartype_index==5),]
	hy_4 <- hy[which(hy$yeartype_index==4),]
	hy_3 <- hy[which(hy$yeartype_index==3),]
	hy_2 <- hy[which(hy$yeartype_index==2),]
	hy_1 <- hy[which(hy$yeartype_index==1),]
	
	all <- list(hy,mon6,mon3,oct,nov,dec,jan,feb,mar,apr,may,jun,jul,aug,sep)
	names(all) <- c("hy","mon6","mon3","oct","nov","dec","jan","feb","mar","apr","may","jun","jul","aug","sep")
	W <- list(hy_5,mon6_5,mon3_5,oct_5,nov_5,dec_5,jan_5,feb_5,mar_5,apr_5,may_5,jun_5,jul_5,aug_5,sep_5)
	names(W) <- c("hy_5","mon6_5","mon3_5","oct_5","nov_5","dec_5","jan_5","feb_5","mar_5","apr_5","may_5","jun_5","jul_5","aug_5","sep_5")
	AN <- list(hy_4,mon6_4,mon3_4,oct_4,nov_4,dec_4,jan_4,feb_4,mar_4,apr_4,may_4,jun_4,jul_4,aug_4,sep_4)
	names(AN) <- c("hy_4","mon6_4","mon3_4","oct_4","nov_4","dec_4","jan_4","feb_4","mar_4","apr_4","may_4","jun_4","jul_4","aug_4","sep_4")
	BN <- list(hy_3,mon6_3,mon3_3,oct_3,nov_3,dec_3,jan_3,feb_3,mar_3,apr_3,may_3,jun_3,jul_3,aug_3,sep_3)
	names(BN) <- c("hy_3","mon6_3","mon3_3","oct_3","nov_3","dec_3","jan_3","feb_3","mar_3","apr_3","may_3","jun_3","jul_3","aug_3","sep_3")
	D <- list(hy_2,mon6_2,mon3_2,oct_2,nov_2,dec_2,jan_2,feb_2,mar_2,apr_2,may_2,jun_2,jul_2,aug_2,sep_2)
	names(D) <- c("hy_2","mon6_2","mon3_2","oct_2","nov_2","dec_2","jan_2","feb_2","mar_2","apr_2","may_2","jun_2","jul_2","aug_2","sep_2")
	C <- list(hy_1,mon6_1,mon3_1,oct_1,nov_1,dec_1,jan_1,feb_1,mar_1,apr_1,may_1,jun_1,jul_1,aug_1,sep_1)
	names(C) <- c("hy_1","mon6_1","mon3_1","oct_1","nov_1","dec_1","jan_1","feb_1","mar_1","apr_1","may_1","jun_1","jul_1","aug_1","sep_1")
	
	finallist <- list(all=all,W=W,AN=AN,BN=BN,D=D,C=C)
	
	return(finallist)
	
}
