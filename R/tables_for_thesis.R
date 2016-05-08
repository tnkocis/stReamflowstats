# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################



all_availability <- vector("list",7)
for(q in 1:7){
	batchnum <- q
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
	availabilitytest <- vector("list",length(spbatch))
	for(i in 1:length(spbatch)){
		availabilitytest[[i]] <- spbatch[[i]]$Availability$yearly
	}
	names(availabilitytest) <- names(spbatch)
	all_availability[[q]] <- availabilitytest
	
}

allavailability <- unlist(all_availability, recursive=FALSE)

load("C:\\Users\\tiffn_000\\Documents\\workspaces\\apr_8.5_activesites.RData")

simp_mags_trends <- vector("list",7)
for(i in 1:length(simp_mags_data)){
	simp_mags_trends[[i]] <- vector("list",length(simp_mags_data[[i]]))
	names(simp_mags_trends[[i]]) <- names(simp_mags_data[[i]])
	for(k in 1:length(simp_mags_data[[i]])){
		simp_mags_trends[[i]][[k]] <- vector("list",length(c(1,2,3,5,6,7,8,9,10)))
		for(j in 1:9){
			vec<- c(1,2,3,5,6,7,8,9,10)
			simp_mags_trends[[i]][[k]][[j]] <- trendsfinal(simp_mags_data[[i]][[k]][[1]][[vec[[j]]]],1800,
					names(simp_mags_data[[i]])[[k]],names(simp_mags_data[[i]][[k]][[1]])[[vec[[j]]]],
					all_availability[[i]][[k]])
		}
	}
}
simp_mags_trends_unlist <- unlist(simp_mags_trends, recursive=FALSE)
simp_mags_trends_unlist2 <- unlist(simp_mags_trends_unlist, recursive=FALSE)
simp_mags_trends_df <- do.call("rbind.data.frame",simp_mags_trends_unlist2)

write.csv(simp_mags_trends_df, file="C:\\Users\\tiffn_000\\Google Drive\\trends_4_12_2016.csv")



simp_mags_data_vol_90 <- vector("list",7)
for(q in 1:7){
	batchnum <- q
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
	simp_mags_data_vol_90[[q]] <- test_split
	
}
load("C:\\Users\\tiffn_000\\Documents\\workspaces\\apr_21_activesites.RData")


simp_mags_data_nmpks_90 <-simp_mags_data_vol_90


simp_mags_data_vol_90_all <- data.frame(gauge=names(simp_mags_data_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="all")
simp_mags_data_vol_90_W <- data.frame(gauge=names(simp_mags_data_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="W")
simp_mags_data_vol_90_AN <- data.frame(gauge=names(simp_mags_data_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="AN")
simp_mags_data_vol_90_BN <- data.frame(gauge=names(simp_mags_data_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="BN")
simp_mags_data_vol_90_D <- data.frame(gauge=names(simp_mags_data_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="D")
simp_mags_data_vol_90_C <- data.frame(gauge=names(simp_mags_data_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_vol_90)){
	simp_mags_data_vol_90_all$vol_nov[[i]]  <- sd(simp_mags_data_vol_90[[i]]$all$nov$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_all$vol_dec[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$all$dec$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_all$vol_jan[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$all$jan$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_all$vol_feb[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$all$feb$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_all$vol_mar[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$all$mar$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_all$vol_apr[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$all$apr$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_all$vol_mon3[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$all$mon3$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_all$vol_mon6[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$all$mon6$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_all$vol_hy[[i]]  <- sd(simp_mags_data_vol_90[[i]]$all$hy$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$all$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	simp_mags_data_vol_90_W$vol_nov[[i]]  <- sd(simp_mags_data_vol_90[[i]]$W$nov$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_W$vol_dec[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$W$dec$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_W$vol_jan[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$W$jan$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_W$vol_feb[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$W$feb$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_W$vol_mar[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$W$mar$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_W$vol_apr[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$W$apr$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_W$vol_mon3[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$W$mon3$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_W$vol_mon6[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$W$mon6$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_W$vol_hy[[i]]  <- sd(simp_mags_data_vol_90[[i]]$W$hy$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$W$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	simp_mags_data_vol_90_AN$vol_nov[[i]]  <- sd(simp_mags_data_vol_90[[i]]$AN$nov$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_AN$vol_dec[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$AN$dec$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_AN$vol_jan[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$AN$jan$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_AN$vol_feb[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$AN$feb$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_AN$vol_mar[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$AN$mar$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_AN$vol_apr[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$AN$apr$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_AN$vol_mon3[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$AN$mon3$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_AN$vol_mon6[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$AN$mon6$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_AN$vol_hy[[i]]  <- sd(simp_mags_data_vol_90[[i]]$AN$hy$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$AN$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_vol_90_BN$vol_nov[[i]]  <- sd(simp_mags_data_vol_90[[i]]$BN$nov$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_BN$vol_dec[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$BN$dec$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_BN$vol_jan[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$BN$jan$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_BN$vol_feb[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$BN$feb$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_BN$vol_mar[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$BN$mar$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_BN$vol_apr[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$BN$apr$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_BN$vol_mon3[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$BN$mon3$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_BN$vol_mon6[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$BN$mon6$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_BN$vol_hy[[i]]  <- sd(simp_mags_data_vol_90[[i]]$BN$hy$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$BN$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_vol_90_D$vol_nov[[i]]  <- sd(simp_mags_data_vol_90[[i]]$D$nov$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_D$vol_dec[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$D$dec$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_D$vol_jan[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$D$jan$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_D$vol_feb[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$D$feb$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_D$vol_mar[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$D$mar$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_D$vol_apr[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$D$apr$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_D$vol_mon3[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$D$mon3$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_D$vol_mon6[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$D$mon6$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_D$vol_hy[[i]]  <- sd(simp_mags_data_vol_90[[i]]$D$hy$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$D$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	simp_mags_data_vol_90_C$vol_nov[[i]]  <- sd(simp_mags_data_vol_90[[i]]$C$nov$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_C$vol_dec[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$C$dec$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_C$vol_jan[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$C$jan$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_C$vol_feb[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$C$feb$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_C$vol_mar[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$C$mar$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_C$vol_apr[[i]]  <-   sd(simp_mags_data_vol_90[[i]]$C$apr$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_C$vol_mon3[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$C$mon3$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_C$vol_mon6[[i]]  <-  sd(simp_mags_data_vol_90[[i]]$C$mon6$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_vol_90_C$vol_hy[[i]]  <- sd(simp_mags_data_vol_90[[i]]$C$hy$TotVolAbv_acft[which( simp_mags_data_vol_90[[i]]$C$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
}

write.csv(simp_mags_data_vol_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_vol_90_all.csv")
write.csv(simp_mags_data_vol_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_vol_90_W.csv")
write.csv(simp_mags_data_vol_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_vol_90_AN.csv")
write.csv(simp_mags_data_vol_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_vol_90_BN.csv")
write.csv(simp_mags_data_vol_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_vol_90_D.csv")
write.csv(simp_mags_data_vol_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_vol_90_C.csv")



simp_mags_data_fracywf_90 <-simp_mags_data_vol_90


simp_mags_data_fracywf_90_all <- data.frame(gauge=names(simp_mags_data_fracywf_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA,  yeartype="all")
simp_mags_data_fracywf_90_W <- data.frame(gauge=names(simp_mags_data_fracywf_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="W")
simp_mags_data_fracywf_90_AN <- data.frame(gauge=names(simp_mags_data_fracywf_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="AN")
simp_mags_data_fracywf_90_BN <- data.frame(gauge=names(simp_mags_data_fracywf_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="BN")
simp_mags_data_fracywf_90_D <- data.frame(gauge=names(simp_mags_data_fracywf_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="D")
simp_mags_data_fracywf_90_C <- data.frame(gauge=names(simp_mags_data_fracywf_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_fracywf_90)){
	simp_mags_data_fracywf_90_all$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$nov$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$nov$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_nov[[i]]  <- simp_mags_data_fracywf_90_all$countwf_nov[[i]]/simp_mags_data_fracywf_90_all$count_tot_nov[[i]]
	simp_mags_data_fracywf_90_all$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$dec$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$dec$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_dec[[i]]  <- simp_mags_data_fracywf_90_all$countwf_dec[[i]]/simp_mags_data_fracywf_90_all$count_tot_dec[[i]]
	simp_mags_data_fracywf_90_all$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$jan$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$jan$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_jan[[i]]  <- simp_mags_data_fracywf_90_all$countwf_jan[[i]]/simp_mags_data_fracywf_90_all$count_tot_jan[[i]]
	simp_mags_data_fracywf_90_all$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$feb$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$feb$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_feb[[i]]  <- simp_mags_data_fracywf_90_all$countwf_feb[[i]]/simp_mags_data_fracywf_90_all$count_tot_feb[[i]]
	simp_mags_data_fracywf_90_all$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$mar$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$mar$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_mar[[i]]  <- simp_mags_data_fracywf_90_all$countwf_mar[[i]]/simp_mags_data_fracywf_90_all$count_tot_mar[[i]]
	simp_mags_data_fracywf_90_all$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$apr$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$apr$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_apr[[i]]  <- simp_mags_data_fracywf_90_all$countwf_apr[[i]]/simp_mags_data_fracywf_90_all$count_tot_apr[[i]]
	simp_mags_data_fracywf_90_all$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$mon3$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$mon3$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_90_all$countwf_mon3[[i]]/simp_mags_data_fracywf_90_all$count_tot_mon3[[i]]
	simp_mags_data_fracywf_90_all$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$mon6$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$mon6$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_90_all$countwf_mon6[[i]]/simp_mags_data_fracywf_90_all$count_tot_mon6[[i]]
	simp_mags_data_fracywf_90_all$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$all$hy$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$all$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_all$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$all$hy$TotVolAbv))
	simp_mags_data_fracywf_90_all$fracywf_hy[[i]]  <- simp_mags_data_fracywf_90_all$countwf_hy[[i]]/simp_mags_data_fracywf_90_all$count_tot_hy[[i]]
	
	simp_mags_data_fracywf_90_W$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$nov$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$nov$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_nov[[i]]  <- simp_mags_data_fracywf_90_W$countwf_nov[[i]]/simp_mags_data_fracywf_90_W$count_tot_nov[[i]]
	simp_mags_data_fracywf_90_W$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$dec$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$dec$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_dec[[i]]  <- simp_mags_data_fracywf_90_W$countwf_dec[[i]]/simp_mags_data_fracywf_90_W$count_tot_dec[[i]]
	simp_mags_data_fracywf_90_W$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$jan$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$jan$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_jan[[i]]  <- simp_mags_data_fracywf_90_W$countwf_jan[[i]]/simp_mags_data_fracywf_90_W$count_tot_jan[[i]]
	simp_mags_data_fracywf_90_W$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$feb$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$feb$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_feb[[i]]  <- simp_mags_data_fracywf_90_W$countwf_feb[[i]]/simp_mags_data_fracywf_90_W$count_tot_feb[[i]]
	simp_mags_data_fracywf_90_W$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$mar$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$mar$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_mar[[i]]  <- simp_mags_data_fracywf_90_W$countwf_mar[[i]]/simp_mags_data_fracywf_90_W$count_tot_mar[[i]]
	simp_mags_data_fracywf_90_W$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$apr$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$apr$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_apr[[i]]  <- simp_mags_data_fracywf_90_W$countwf_apr[[i]]/simp_mags_data_fracywf_90_W$count_tot_apr[[i]]
	simp_mags_data_fracywf_90_W$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$mon3$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$mon3$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_90_W$countwf_mon3[[i]]/simp_mags_data_fracywf_90_W$count_tot_mon3[[i]]
	simp_mags_data_fracywf_90_W$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$mon6$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$mon6$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_90_W$countwf_mon6[[i]]/simp_mags_data_fracywf_90_W$count_tot_mon6[[i]]
	simp_mags_data_fracywf_90_W$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$W$hy$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$W$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_W$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$W$hy$TotVolAbv))
	simp_mags_data_fracywf_90_W$fracywf_hy[[i]]  <- simp_mags_data_fracywf_90_W$countwf_hy[[i]]/simp_mags_data_fracywf_90_W$count_tot_hy[[i]]
	
	simp_mags_data_fracywf_90_AN$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$nov$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$nov$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_nov[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_nov[[i]]/simp_mags_data_fracywf_90_AN$count_tot_nov[[i]]
	simp_mags_data_fracywf_90_AN$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$dec$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$dec$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_dec[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_dec[[i]]/simp_mags_data_fracywf_90_AN$count_tot_dec[[i]]
	simp_mags_data_fracywf_90_AN$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$jan$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$jan$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_jan[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_jan[[i]]/simp_mags_data_fracywf_90_AN$count_tot_jan[[i]]
	simp_mags_data_fracywf_90_AN$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$feb$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$feb$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_feb[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_feb[[i]]/simp_mags_data_fracywf_90_AN$count_tot_feb[[i]]
	simp_mags_data_fracywf_90_AN$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$mar$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$mar$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_mar[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_mar[[i]]/simp_mags_data_fracywf_90_AN$count_tot_mar[[i]]
	simp_mags_data_fracywf_90_AN$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$apr$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$apr$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_apr[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_apr[[i]]/simp_mags_data_fracywf_90_AN$count_tot_apr[[i]]
	simp_mags_data_fracywf_90_AN$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$mon3$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$mon3$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_mon3[[i]]/simp_mags_data_fracywf_90_AN$count_tot_mon3[[i]]
	simp_mags_data_fracywf_90_AN$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$mon6$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$mon6$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_mon6[[i]]/simp_mags_data_fracywf_90_AN$count_tot_mon6[[i]]
	simp_mags_data_fracywf_90_AN$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$AN$hy$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$AN$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_AN$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$AN$hy$TotVolAbv))
	simp_mags_data_fracywf_90_AN$fracywf_hy[[i]]  <- simp_mags_data_fracywf_90_AN$countwf_hy[[i]]/simp_mags_data_fracywf_90_AN$count_tot_hy[[i]]
	
	simp_mags_data_fracywf_90_BN$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$nov$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$nov$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_nov[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_nov[[i]]/simp_mags_data_fracywf_90_BN$count_tot_nov[[i]]
	simp_mags_data_fracywf_90_BN$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$dec$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$dec$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_dec[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_dec[[i]]/simp_mags_data_fracywf_90_BN$count_tot_dec[[i]]
	simp_mags_data_fracywf_90_BN$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$jan$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$jan$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_jan[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_jan[[i]]/simp_mags_data_fracywf_90_BN$count_tot_jan[[i]]
	simp_mags_data_fracywf_90_BN$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$feb$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$feb$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_feb[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_feb[[i]]/simp_mags_data_fracywf_90_BN$count_tot_feb[[i]]
	simp_mags_data_fracywf_90_BN$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$mar$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$mar$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_mar[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_mar[[i]]/simp_mags_data_fracywf_90_BN$count_tot_mar[[i]]
	simp_mags_data_fracywf_90_BN$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$apr$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$apr$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_apr[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_apr[[i]]/simp_mags_data_fracywf_90_BN$count_tot_apr[[i]]
	simp_mags_data_fracywf_90_BN$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$mon3$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$mon3$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_mon3[[i]]/simp_mags_data_fracywf_90_BN$count_tot_mon3[[i]]
	simp_mags_data_fracywf_90_BN$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$mon6$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$mon6$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_mon6[[i]]/simp_mags_data_fracywf_90_BN$count_tot_mon6[[i]]
	simp_mags_data_fracywf_90_BN$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$BN$hy$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$BN$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_BN$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$BN$hy$TotVolAbv))
	simp_mags_data_fracywf_90_BN$fracywf_hy[[i]]  <- simp_mags_data_fracywf_90_BN$countwf_hy[[i]]/simp_mags_data_fracywf_90_BN$count_tot_hy[[i]]
	
	simp_mags_data_fracywf_90_D$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$nov$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$nov$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_nov[[i]]  <- simp_mags_data_fracywf_90_D$countwf_nov[[i]]/simp_mags_data_fracywf_90_D$count_tot_nov[[i]]
	simp_mags_data_fracywf_90_D$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$dec$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$dec$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_dec[[i]]  <- simp_mags_data_fracywf_90_D$countwf_dec[[i]]/simp_mags_data_fracywf_90_D$count_tot_dec[[i]]
	simp_mags_data_fracywf_90_D$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$jan$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$jan$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_jan[[i]]  <- simp_mags_data_fracywf_90_D$countwf_jan[[i]]/simp_mags_data_fracywf_90_D$count_tot_jan[[i]]
	simp_mags_data_fracywf_90_D$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$feb$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$feb$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_feb[[i]]  <- simp_mags_data_fracywf_90_D$countwf_feb[[i]]/simp_mags_data_fracywf_90_D$count_tot_feb[[i]]
	simp_mags_data_fracywf_90_D$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$mar$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$mar$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_mar[[i]]  <- simp_mags_data_fracywf_90_D$countwf_mar[[i]]/simp_mags_data_fracywf_90_D$count_tot_mar[[i]]
	simp_mags_data_fracywf_90_D$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$apr$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$apr$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_apr[[i]]  <- simp_mags_data_fracywf_90_D$countwf_apr[[i]]/simp_mags_data_fracywf_90_D$count_tot_apr[[i]]
	simp_mags_data_fracywf_90_D$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$mon3$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$mon3$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_90_D$countwf_mon3[[i]]/simp_mags_data_fracywf_90_D$count_tot_mon3[[i]]
	simp_mags_data_fracywf_90_D$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$mon6$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$mon6$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_90_D$countwf_mon6[[i]]/simp_mags_data_fracywf_90_D$count_tot_mon6[[i]]
	simp_mags_data_fracywf_90_D$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$D$hy$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$D$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_D$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$D$hy$TotVolAbv))
	simp_mags_data_fracywf_90_D$fracywf_hy[[i]]  <- simp_mags_data_fracywf_90_D$countwf_hy[[i]]/simp_mags_data_fracywf_90_D$count_tot_hy[[i]]
	
	
	simp_mags_data_fracywf_90_C$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$nov$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$nov$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_nov[[i]]  <- simp_mags_data_fracywf_90_C$countwf_nov[[i]]/simp_mags_data_fracywf_90_C$count_tot_nov[[i]]
	simp_mags_data_fracywf_90_C$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$dec$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$dec$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_dec[[i]]  <- simp_mags_data_fracywf_90_C$countwf_dec[[i]]/simp_mags_data_fracywf_90_C$count_tot_dec[[i]]
	simp_mags_data_fracywf_90_C$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$jan$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$jan$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_jan[[i]]  <- simp_mags_data_fracywf_90_C$countwf_jan[[i]]/simp_mags_data_fracywf_90_C$count_tot_jan[[i]]
	simp_mags_data_fracywf_90_C$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$feb$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$feb$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_feb[[i]]  <- simp_mags_data_fracywf_90_C$countwf_feb[[i]]/simp_mags_data_fracywf_90_C$count_tot_feb[[i]]
	simp_mags_data_fracywf_90_C$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$mar$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$mar$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_mar[[i]]  <- simp_mags_data_fracywf_90_C$countwf_mar[[i]]/simp_mags_data_fracywf_90_C$count_tot_mar[[i]]
	simp_mags_data_fracywf_90_C$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$apr$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$apr$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_apr[[i]]  <- simp_mags_data_fracywf_90_C$countwf_apr[[i]]/simp_mags_data_fracywf_90_C$count_tot_apr[[i]]
	simp_mags_data_fracywf_90_C$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$mon3$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$mon3$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_90_C$countwf_mon3[[i]]/simp_mags_data_fracywf_90_C$count_tot_mon3[[i]]
	simp_mags_data_fracywf_90_C$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$mon6$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$mon6$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_90_C$countwf_mon6[[i]]/simp_mags_data_fracywf_90_C$count_tot_mon6[[i]]
	simp_mags_data_fracywf_90_C$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_90[[i]]$C$hy$TotVolAbv[which( simp_mags_data_fracywf_90[[i]]$C$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_90_C$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_90[[i]]$C$hy$TotVolAbv))
	simp_mags_data_fracywf_90_C$fracywf_hy[[i]]  <- simp_mags_data_fracywf_90_C$countwf_hy[[i]]/simp_mags_data_fracywf_90_C$count_tot_hy[[i]]
	
}

write.csv(simp_mags_data_fracywf_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_90_all.csv")
write.csv(simp_mags_data_fracywf_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_90_W.csv")
write.csv(simp_mags_data_fracywf_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_90_AN.csv")
write.csv(simp_mags_data_fracywf_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_90_BN.csv")
write.csv(simp_mags_data_fracywf_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_90_D.csv")
write.csv(simp_mags_data_fracywf_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_90_C.csv")






COM90_full <- vector("list",7)
for(q in 1:7){
	batchnum <- q
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
	COM_90 <- vector("list",length(spbatch))
	for(k in 1:length(spbatch)){
		blahday <- rep(NA,length(spbatch[[k]]$HydroYear$Data))
		blahyear <- rep(NA,length(spbatch[[k]]$HydroYear$Data))
		for(i in 1:length(spbatch[[k]]$HydroYear$Data)){
			blah <- spbatch[[k]]$HydroYear$Data[[i]]$Discharge_acft_day
			blah[blah<(spbatch[[k]]$thresholds_maf$P90maf*1e6)] <- 0
			blahday[[i]] <- which(cumsum(blah)>=(sum(blah,na.rm=TRUE)/2))[[1]]
			blahyear[[i]] <- as.numeric(format(spbatch[[k]]$HydroYear$Data[[i]]$Date[[1]],"%Y"))
		}
		COM_90[[k]] <- data.frame(COMday=blahday,sthyyear=blahyear)
	}
	
	COM_90_df <- data.frame(sthyyear=seq(1900,2015,1))
	for(i in 1:length(COM_90)){
		COM_90_df <- merge(COM_90_df,COM_90[[i]], by.x="sthyyear",by.y="sthyyear", all.x=TRUE)
	}
	names(COM_90_df) <- c("sthyyear",paste(as.numeric(names(spbatch)),sep=""))
	COM_90_df[COM_90_df==1] <- NA
	COM90_full[[q]] <- COM_90_df
}
load("C:\\Users\\tiffn_000\\Documents\\workspaces\\apr_14_activesites.RData")
COM90_full_df <- COM90_full[[1]]
for(i in 2:7){
	COM90_full_df <- merge(COM90_full_df,COM90_full[[i]],by="sthyyear")
}

for(i in 2:length(COM90_full_df)){
	COM90_full_df[[i]][which(COM90_full_df[[i]]==365)] <- NA	
}
names(COM90_full_df) <- c("sthyyear", paste("COM_90_",names(COM90_full_df)[2:length(COM90_full_df)], sep=""))
com90roll <- rollapply(COM90_full_df[[10]],5,function(x) mean(x, na.rm=TRUE), fill=NA)
com90rolldf <- data.frame(sthyyear=COM90_full_df$sthyyear, roll=com90roll)
ggplot(COM90_full_df, aes(x=sthyyear, y=COM_90_11224500)) +geom_point() + geom_line(data=com90rolldf,aes(x=sthyyear, y=roll))

COM90_avg_DOHY <- rep(NA, 93)
COM90_sd_DOHY <- rep(NA, 93)
gauge_COM90 <- rep(NA,93)
W_avg <- rep(NA,93)
W_sd <- rep(NA,93)
AN_avg <- rep(NA,93)
AN_sd <- rep(NA,93)
BN_avg <- rep(NA,93)
BN_sd <- rep(NA,93)
D_avg <- rep(NA,93)
D_sd <- rep(NA,93)
C_avg <- rep(NA,93)
C_sd <- rep(NA,93)

for(i in 1:93){
	gauge_COM90[[i]] <- strsplit(names(COM90_full_df)[[i+1]],"_")[[1]][[3]]
	COM90_avg_DOHY[[i]] <- round(mean(COM90_full_df[[i+1]], na.rm=TRUE))
	COM90_sd_DOHY[[i]] <- round(sd(COM90_full_df[[i+1]], na.rm=TRUE))
	gauge_COM90[[i]] <- strsplit(names(COM90_full_df)[[i+1]],"_")[[1]][[3]]
	if(gauge_COM90[[i]]%in%SacV_gauges$site_no){
		W_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="5")]],na.rm=TRUE)     )
		W_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="5")]],na.rm=TRUE)        )
		AN_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="4")]],na.rm=TRUE)    )
		AN_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="4")]],na.rm=TRUE)       )
		BN_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="3")]],na.rm=TRUE)    )
		BN_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="3")]],na.rm=TRUE)       )
		D_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="2")]],na.rm=TRUE)     )
		D_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="2")]],na.rm=TRUE)        )
		C_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="1")]],na.rm=TRUE)     )
		C_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="1")]],na.rm=TRUE)        )
                                                                                                                                                    
	} else if(gauge_COM90[[i]]%in%SJV_gauges$site_no){                                                                                              
			W_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="5")]],na.rm=TRUE) )
			W_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="5")]],na.rm=TRUE)    )
			AN_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="4")]],na.rm=TRUE))
			AN_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="4")]],na.rm=TRUE)   )
			BN_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="3")]],na.rm=TRUE))
			BN_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="3")]],na.rm=TRUE)   )
			D_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="2")]],na.rm=TRUE) )
			D_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="2")]],na.rm=TRUE)    )
			C_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="1")]],na.rm=TRUE) )
			C_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="1")]],na.rm=TRUE)    )
			
		}
	}
COM90_mag <- data.frame(gauge =gauge_COM90, avg_DOHY=COM90_avg_DOHY, sd_DOHY= COM90_sd_DOHY,
		W_avg = W_avg,
W_sd    =  W_sd    ,
AN_avg  =  AN_avg  ,
AN_sd   =  AN_sd   ,
BN_avg  =  BN_avg  ,
BN_sd   =  BN_sd   ,
D_avg   =  D_avg   ,
D_sd    =  D_sd    ,
C_avg   =  C_avg   ,
C_sd    =  C_sd    )

write.csv(COM90_mag, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\COM90_mag_sd.csv")

for(i in 1:115){
	yeartype_old$sthyyear[[i]] <- as.numeric(strsplit(as.character(yeartype_old$Year[[i]])," - ")[[1]][1])
}


simp_mags_trends2 <- vector("list",7)
for(i in 1:length(simp_mags_data)){
	simp_mags_trends2[[i]] <- vector("list",length(simp_mags_data[[i]]))
	names(simp_mags_trends2[[i]]) <- names(simp_mags_data[[i]])
	for(k in 1:length(simp_mags_data[[i]])){
		simp_mags_trends2[[i]][[k]] <- vector("list",length(c(1,2,3,5,6,7,8,9,10)))
		for(j in 1:9){
			vec<- c(1,2,3,5,6,7,8,9,10)
			simp_mags_trends2[[i]][[k]][[j]] <- trendsfinal2_1(simp_mags_data[[i]][[k]][[1]][[vec[[j]]]],1800,
					names(simp_mags_data[[i]])[[k]],names(simp_mags_data[[i]][[k]][[1]])[[vec[[j]]]])
		}
	}
}
simp_mags_trends2_unlist <- unlist(simp_mags_trends2, recursive=FALSE)
simp_mags_trends2_unlist2 <- unlist(simp_mags_trends2_unlist, recursive=FALSE)
simp_mags_trends2_df <- do.call("rbind.data.frame",simp_mags_trends2_unlist2)

write.csv(simp_mags_trends2_df, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\2_trends_full_1.csv")


simp_mags_trendsimp <- vector("list",7)
for(i in 1:length(simp_mags_data)){
	simp_mags_trendsimp[[i]] <- vector("list",length(simp_mags_data[[i]]))
	names(simp_mags_trendsimp[[i]]) <- names(simp_mags_data[[i]])
	for(k in 1:length(simp_mags_data[[i]])){
		simp_mags_trendsimp[[i]][[k]] <- vector("list",length(c(1,2,3,5,6,7,8,9,10)))
		for(j in 1:9){
			vec<- c(1,2,3,5,6,7,8,9,10)
			if(names(simp_mags_data[[i]])[[k]]%in%SacV_gauges$site_no){
				simp_mags_trendsimp[[i]][[k]][[j]] <- trendsfinal2(simp_mags_data[[i]][[k]][[1]][[vec[[j]]]],1970,
						names(simp_mags_data[[i]])[[k]],names(simp_mags_data[[i]][[k]][[1]])[[vec[[j]]]])
			}else if(names(simp_mags_data[[i]])[[k]]%in%SJV_gauges$site_no){
				simp_mags_trendsimp[[i]][[k]][[j]] <- trendsfinal2(simp_mags_data[[i]][[k]][[1]][[vec[[j]]]],1989,
						names(simp_mags_data[[i]])[[k]],names(simp_mags_data[[i]][[k]][[1]])[[vec[[j]]]])	
			}
		}
	}
}
simp_mags_trendsimp_unlist <- unlist(simp_mags_trendsimp, recursive=FALSE)
simp_mags_trendsimp_unlist2 <- unlist(simp_mags_trendsimp_unlist, recursive=FALSE)
simp_mags_trendsimp_df <- do.call("rbind.data.frame",simp_mags_trendsimp_unlist2)

write.csv(simp_mags_trendsimp_df, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\2_trends_imp.csv")
dohy <- data.frame(DOHY= seq(1,365,1),DOY=seq.Date(as.Date("10-01-2000",format="%m-%d-%Y"),as.Date("09-30-2001",format="%m-%d-%Y"),by="day"))

simp_mags_data_unlist <- unlist(simp_mags_data, recursive=FALSE)

COM90_full_trend <- vector("list",93)
actual_start <- rep(NA,93)
for(i in 1:93){ 
	actual_start[[i]] <- simp_mags_data_unlist[[i]]$all$hy$sthyyear[[1]]
	COM90_full_trend[[i]] <- trendsfinalCOM(COM90_full_df[[i+1]], COM90_full_df[[1]],actual_start[[i]],1800, gauge_COM90[[i]], period="hy")
}
COM90_full_trenddf <- do.call(rbind.data.frame,COM90_full_trend)

write.csv(COM90_full_trenddf, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\2_trend_COM_full_5.csv")

COM90_full_trend1 <- vector("list",93)
actual_start <- rep(NA,93)
for(i in 1:93){ 
	actual_start[[i]] <- simp_mags_data_unlist[[i]]$all$hy$sthyyear[[1]]
	COM90_full_trend1[[i]] <- trendsfinalCOM_1(COM90_full_df[[i+1]], COM90_full_df[[1]],actual_start[[i]],1800, gauge_COM90[[i]], period="hy")
}
COM90_full_trend1df <- do.call(rbind.data.frame,COM90_full_trend1)
write.csv(COM90_full_trend1df, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\2_trend_COM_full_1.csv")


COM90_imp_trend <- vector("list",93)
actual_start <- rep(NA,93)
for(i in 1:93){ 
	actual_start[[i]] <- simp_mags_data_unlist[[i]]$all$hy$sthyyear[[1]]
	if(gauge_COM90[[i]]%in%SacV_gauges$site_no){
		COM90_imp_trend[[i]] <- trendsfinalCOM(COM90_full_df[[i+1]], COM90_full_df[[1]],actual_start[[i]],1970, gauge_COM90[[i]], period="hy")
	}else if(gauge_COM90[[i]]%in%SJV_gauges$site_no){
		COM90_imp_trend[[i]] <- trendsfinalCOM(COM90_full_df[[i+1]], COM90_full_df[[1]],actual_start[[i]],1989, gauge_COM90[[i]], period="hy")
	}
}
COM90_imp_trenddf <- do.call(rbind.data.frame,COM90_imp_trend)

write.csv(COM90_imp_trenddf, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\2_trend_COM_imp_5.csv")

COM90_imp_trend1 <- vector("list",93)
actual_start <- rep(NA,93)
for(i in 1:93){ 
	actual_start[[i]] <- simp_mags_data_unlist[[i]]$all$hy$sthyyear[[1]]
	if(gauge_COM90[[i]]%in%SacV_gauges$site_no){
		COM90_imp_trend1[[i]] <- trendsfinalCOM_1(COM90_full_df[[i+1]], COM90_full_df[[1]],actual_start[[i]],1970, gauge_COM90[[i]], period="hy")
	}else if(gauge_COM90[[i]]%in%SJV_gauges$site_no){
		COM90_imp_trend1[[i]] <- trendsfinalCOM_1(COM90_full_df[[i+1]], COM90_full_df[[1]],actual_start[[i]],1989, gauge_COM90[[i]], period="hy")
	}
}
COM90_imp_trend1df <- do.call(rbind.data.frame,COM90_imp_trend1)

write.csv(COM90_imp_trend1df, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\trend_COM_imp_1.csv")







simp_mags_data_90_imp <- simp_mags_data_vol_90


for(i in 1:length(simp_mags_data_90_imp)){
	if(names(simp_mags_data_90_imp)[[i]]%in%SacV_gauges$site_no){
		for(k in 1:length(simp_mags_data_90_imp[[i]])){
			for(j in 1:length(simp_mags_data_90_imp[[i]][[k]])){
				test <- simp_mags_data_90_imp[[i]][[k]][[j]][which(simp_mags_data_90_imp[[i]][[k]][[j]]$sthyyear>=1970),]
				simp_mags_data_90_imp[[i]][[k]][[j]] <- c(NA)
				simp_mags_data_90_imp[[i]][[k]][[j]] <- test
				
			}
		}
	}else if(names(simp_mags_data_90_imp)[[i]]%in%SJV_gauges$site_no){
		for(k in 1:length(simp_mags_data_90_imp[[i]])){
			for(j in 1:length(simp_mags_data_90_imp[[i]][[k]])){
				test <- simp_mags_data_90_imp[[i]][[k]][[j]][which(simp_mags_data_90_imp[[i]][[k]][[j]]$sthyyear>=1989),]
				simp_mags_data_90_imp[[i]][[k]][[j]] <- c(NA)
				simp_mags_data_90_imp[[i]][[k]][[j]] <- test
			}
		}
	}
}

simp_mags_data_fracywf_imp_90 <-simp_mags_data_90_imp
simp_mags_data_fracywf_imp_90_all <- data.frame(gauge=names(simp_mags_data_fracywf_imp_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA,  yeartype="all")
simp_mags_data_fracywf_imp_90_W <- data.frame(gauge=names(simp_mags_data_fracywf_imp_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="W")
simp_mags_data_fracywf_imp_90_AN <- data.frame(gauge=names(simp_mags_data_fracywf_imp_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="AN")
simp_mags_data_fracywf_imp_90_BN <- data.frame(gauge=names(simp_mags_data_fracywf_imp_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="BN")
simp_mags_data_fracywf_imp_90_D <- data.frame(gauge=names(simp_mags_data_fracywf_imp_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="D")
simp_mags_data_fracywf_imp_90_C <- data.frame(gauge=names(simp_mags_data_fracywf_imp_90),fracywf_nov=NA, countwf_nov=NA, count_tot_nov=NA, 
		fracywf_dec=NA, countwf_dec=NA, count_tot_dec=NA, fracywf_jan=NA, countwf_jan=NA, count_tot_jan=NA, 
		fracywf_feb=NA,countwf_feb=NA, count_tot_feb=NA,fracywf_mar=NA,countwf_mar=NA, count_tot_mar=NA, 
		fracywf_apr=NA,countwf_apr=NA, count_tot_apr=NA, 
		fracywf_mon3=NA, countwf_mon3=NA, count_tot_mon3=NA, 
		fracywf_mon6=NA, countwf_mon6=NA, count_tot_mon6=NA, 
		fracywf_hy=NA,countwf_hy=NA, count_tot_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_fracywf_imp_90)){
	simp_mags_data_fracywf_imp_90_all$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$nov$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$nov$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_nov[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_nov[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_nov[[i]]
	simp_mags_data_fracywf_imp_90_all$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$dec$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$dec$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_dec[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_dec[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_dec[[i]]
	simp_mags_data_fracywf_imp_90_all$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$jan$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$jan$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_jan[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_jan[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_jan[[i]]
	simp_mags_data_fracywf_imp_90_all$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$feb$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$feb$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_feb[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_feb[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_feb[[i]]
	simp_mags_data_fracywf_imp_90_all$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$mar$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$mar$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_mar[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_mar[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_mar[[i]]
	simp_mags_data_fracywf_imp_90_all$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$apr$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$apr$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_apr[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_apr[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_apr[[i]]
	simp_mags_data_fracywf_imp_90_all$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$mon3$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$mon3$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_mon3[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_mon3[[i]]
	simp_mags_data_fracywf_imp_90_all$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$mon6$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$mon6$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_mon6[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_mon6[[i]]
	simp_mags_data_fracywf_imp_90_all$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$all$hy$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$all$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_all$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$all$hy$TotVolAbv))
	simp_mags_data_fracywf_imp_90_all$fracywf_hy[[i]]  <- simp_mags_data_fracywf_imp_90_all$countwf_hy[[i]]/simp_mags_data_fracywf_imp_90_all$count_tot_hy[[i]]
	
	simp_mags_data_fracywf_imp_90_W$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$nov$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$nov$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_nov[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_nov[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_nov[[i]]
	simp_mags_data_fracywf_imp_90_W$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$dec$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$dec$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_dec[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_dec[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_dec[[i]]
	simp_mags_data_fracywf_imp_90_W$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$jan$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$jan$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_jan[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_jan[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_jan[[i]]
	simp_mags_data_fracywf_imp_90_W$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$feb$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$feb$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_feb[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_feb[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_feb[[i]]
	simp_mags_data_fracywf_imp_90_W$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$mar$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$mar$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_mar[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_mar[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_mar[[i]]
	simp_mags_data_fracywf_imp_90_W$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$apr$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$apr$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_apr[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_apr[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_apr[[i]]
	simp_mags_data_fracywf_imp_90_W$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$mon3$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$mon3$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_mon3[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_mon3[[i]]
	simp_mags_data_fracywf_imp_90_W$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$mon6$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$mon6$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_mon6[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_mon6[[i]]
	simp_mags_data_fracywf_imp_90_W$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$W$hy$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$W$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_W$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$W$hy$TotVolAbv))
	simp_mags_data_fracywf_imp_90_W$fracywf_hy[[i]]  <- simp_mags_data_fracywf_imp_90_W$countwf_hy[[i]]/simp_mags_data_fracywf_imp_90_W$count_tot_hy[[i]]
	
	simp_mags_data_fracywf_imp_90_AN$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$nov$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$nov$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_nov[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_nov[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_nov[[i]]
	simp_mags_data_fracywf_imp_90_AN$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$dec$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$dec$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_dec[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_dec[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_dec[[i]]
	simp_mags_data_fracywf_imp_90_AN$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$jan$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$jan$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_jan[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_jan[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_jan[[i]]
	simp_mags_data_fracywf_imp_90_AN$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$feb$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$feb$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_feb[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_feb[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_feb[[i]]
	simp_mags_data_fracywf_imp_90_AN$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$mar$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$mar$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_mar[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_mar[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_mar[[i]]
	simp_mags_data_fracywf_imp_90_AN$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$apr$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$apr$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_apr[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_apr[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_apr[[i]]
	simp_mags_data_fracywf_imp_90_AN$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$mon3$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$mon3$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_mon3[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_mon3[[i]]
	simp_mags_data_fracywf_imp_90_AN$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$mon6$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$mon6$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_mon6[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_mon6[[i]]
	simp_mags_data_fracywf_imp_90_AN$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$AN$hy$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$AN$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_AN$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$AN$hy$TotVolAbv))
	simp_mags_data_fracywf_imp_90_AN$fracywf_hy[[i]]  <- simp_mags_data_fracywf_imp_90_AN$countwf_hy[[i]]/simp_mags_data_fracywf_imp_90_AN$count_tot_hy[[i]]
	
	simp_mags_data_fracywf_imp_90_BN$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$nov$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$nov$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_nov[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_nov[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_nov[[i]]
	simp_mags_data_fracywf_imp_90_BN$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$dec$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$dec$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_dec[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_dec[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_dec[[i]]
	simp_mags_data_fracywf_imp_90_BN$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$jan$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$jan$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_jan[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_jan[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_jan[[i]]
	simp_mags_data_fracywf_imp_90_BN$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$feb$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$feb$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_feb[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_feb[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_feb[[i]]
	simp_mags_data_fracywf_imp_90_BN$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$mar$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$mar$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_mar[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_mar[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_mar[[i]]
	simp_mags_data_fracywf_imp_90_BN$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$apr$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$apr$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_apr[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_apr[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_apr[[i]]
	simp_mags_data_fracywf_imp_90_BN$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$mon3$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$mon3$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_mon3[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_mon3[[i]]
	simp_mags_data_fracywf_imp_90_BN$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$mon6$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$mon6$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_mon6[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_mon6[[i]]
	simp_mags_data_fracywf_imp_90_BN$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$BN$hy$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$BN$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_BN$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$BN$hy$TotVolAbv))
	simp_mags_data_fracywf_imp_90_BN$fracywf_hy[[i]]  <- simp_mags_data_fracywf_imp_90_BN$countwf_hy[[i]]/simp_mags_data_fracywf_imp_90_BN$count_tot_hy[[i]]
	
	simp_mags_data_fracywf_imp_90_D$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$nov$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$nov$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_nov[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_nov[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_nov[[i]]
	simp_mags_data_fracywf_imp_90_D$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$dec$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$dec$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_dec[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_dec[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_dec[[i]]
	simp_mags_data_fracywf_imp_90_D$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$jan$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$jan$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_jan[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_jan[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_jan[[i]]
	simp_mags_data_fracywf_imp_90_D$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$feb$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$feb$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_feb[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_feb[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_feb[[i]]
	simp_mags_data_fracywf_imp_90_D$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$mar$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$mar$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_mar[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_mar[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_mar[[i]]
	simp_mags_data_fracywf_imp_90_D$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$apr$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$apr$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_apr[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_apr[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_apr[[i]]
	simp_mags_data_fracywf_imp_90_D$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$mon3$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$mon3$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_mon3[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_mon3[[i]]
	simp_mags_data_fracywf_imp_90_D$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$mon6$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$mon6$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_mon6[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_mon6[[i]]
	simp_mags_data_fracywf_imp_90_D$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$D$hy$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$D$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_D$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$D$hy$TotVolAbv))
	simp_mags_data_fracywf_imp_90_D$fracywf_hy[[i]]  <- simp_mags_data_fracywf_imp_90_D$countwf_hy[[i]]/simp_mags_data_fracywf_imp_90_D$count_tot_hy[[i]]
	
	
	simp_mags_data_fracywf_imp_90_C$countwf_nov[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$nov$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$nov$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_nov[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$nov$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_nov[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_nov[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_nov[[i]]
	simp_mags_data_fracywf_imp_90_C$countwf_dec[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$dec$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$dec$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_dec[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$dec$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_dec[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_dec[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_dec[[i]]
	simp_mags_data_fracywf_imp_90_C$countwf_jan[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$jan$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$jan$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_jan[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$jan$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_jan[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_jan[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_jan[[i]]
	simp_mags_data_fracywf_imp_90_C$countwf_feb[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$feb$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$feb$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_feb[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$feb$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_feb[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_feb[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_feb[[i]]
	simp_mags_data_fracywf_imp_90_C$countwf_mar[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$mar$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$mar$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_mar[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$mar$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_mar[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_mar[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_mar[[i]]
	simp_mags_data_fracywf_imp_90_C$countwf_apr[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$apr$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$apr$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_apr[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$apr$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_apr[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_apr[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_apr[[i]]
	simp_mags_data_fracywf_imp_90_C$countwf_mon3[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$mon3$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$mon3$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_mon3[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$mon3$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_mon3[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_mon3[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_mon3[[i]]
	simp_mags_data_fracywf_imp_90_C$countwf_mon6[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$mon6$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$mon6$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_mon6[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$mon6$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_mon6[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_mon6[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_mon6[[i]]
	simp_mags_data_fracywf_imp_90_C$countwf_hy[[i]]  <- length(simp_mags_data_fracywf_imp_90[[i]]$C$hy$TotVolAbv[which( simp_mags_data_fracywf_imp_90[[i]]$C$hy$TotVolAbv!=0)])
	simp_mags_data_fracywf_imp_90_C$count_tot_hy[[i]] <- length(!is.na(simp_mags_data_fracywf_imp_90[[i]]$C$hy$TotVolAbv))
	simp_mags_data_fracywf_imp_90_C$fracywf_hy[[i]]  <- simp_mags_data_fracywf_imp_90_C$countwf_hy[[i]]/simp_mags_data_fracywf_imp_90_C$count_tot_hy[[i]]
	
}

write.csv(simp_mags_data_fracywf_imp_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_imp_90_all.csv")
write.csv(simp_mags_data_fracywf_imp_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_imp_90_W.csv")
write.csv(simp_mags_data_fracywf_imp_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_imp_90_AN.csv")
write.csv(simp_mags_data_fracywf_imp_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_imp_90_BN.csv")
write.csv(simp_mags_data_fracywf_imp_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_imp_90_D.csv")
write.csv(simp_mags_data_fracywf_imp_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mags_data_fracywf_imp_90_C.csv")



simp_mags_data_imp_vol_90 <- simp_mags_data_90_imp
simp_mags_data_imp_vol_90_all <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="all")
simp_mags_data_imp_vol_90_W <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="W")
simp_mags_data_imp_vol_90_AN <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="AN")
simp_mags_data_imp_vol_90_BN <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="BN")
simp_mags_data_imp_vol_90_D <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="D")
simp_mags_data_imp_vol_90_C <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_imp_vol_90)){
	simp_mags_data_imp_vol_90_all$vol_nov[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$all$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_dec[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$all$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_jan[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$all$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_feb[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$all$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_mar[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$all$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_apr[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$all$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_mon3[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$all$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_mon6[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$all$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_hy[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$all$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_vol_90_W$vol_nov[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$W$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_dec[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$W$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_jan[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$W$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_feb[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$W$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_mar[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$W$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_apr[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$W$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_mon3[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$W$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_mon6[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$W$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_hy[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$W$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_vol_90_AN$vol_nov[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$AN$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_dec[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$AN$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_jan[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$AN$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_feb[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$AN$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_mar[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$AN$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_apr[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$AN$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_mon3[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$AN$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_mon6[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$AN$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_hy[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$AN$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_vol_90_BN$vol_nov[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$BN$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_dec[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$BN$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_jan[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$BN$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_feb[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$BN$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_mar[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$BN$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_apr[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$BN$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_mon3[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$BN$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_mon6[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$BN$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_hy[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$BN$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_vol_90_D$vol_nov[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$D$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_dec[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$D$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_jan[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$D$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_feb[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$D$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_mar[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$D$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_apr[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$D$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_mon3[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$D$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_mon6[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$D$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_hy[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$D$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	simp_mags_data_imp_vol_90_C$vol_nov[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$C$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_dec[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$C$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_jan[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$C$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_feb[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$C$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_mar[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$C$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_apr[[i]]  <-   sd(simp_mags_data_imp_vol_90[[i]]$C$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_mon3[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$C$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_mon6[[i]]  <-  sd(simp_mags_data_imp_vol_90[[i]]$C$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_hy[[i]]  <- sd(simp_mags_data_imp_vol_90[[i]]$C$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
}

write.csv(simp_mags_data_imp_vol_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_vol_90_all.csv")
write.csv(simp_mags_data_imp_vol_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_vol_90_W.csv")
write.csv(simp_mags_data_imp_vol_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_vol_90_AN.csv")
write.csv(simp_mags_data_imp_vol_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_vol_90_BN.csv")
write.csv(simp_mags_data_imp_vol_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_vol_90_D.csv")
write.csv(simp_mags_data_imp_vol_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_vol_90_C.csv")

simp_mags_data_imp_vol_90_all <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="all")
simp_mags_data_imp_vol_90_W <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="W")
simp_mags_data_imp_vol_90_AN <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="AN")
simp_mags_data_imp_vol_90_BN <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="BN")
simp_mags_data_imp_vol_90_D <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="D")
simp_mags_data_imp_vol_90_C <- data.frame(gauge=names(simp_mags_data_imp_vol_90),vol_nov=NA, vol_dec=NA, vol_feb=NA,vol_mar=NA, vol_apr=NA,
		vol_mon3=NA, vol_mon6=NA, vol_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_imp_vol_90)){
	simp_mags_data_imp_vol_90_all$vol_nov[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$all$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_dec[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$all$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_jan[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$all$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_feb[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$all$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_mar[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$all$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_apr[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$all$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_mon3[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$all$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_mon6[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$all$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_all$vol_hy[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$all$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$all$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_vol_90_W$vol_nov[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$W$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_dec[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$W$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_jan[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$W$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_feb[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$W$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_mar[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$W$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_apr[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$W$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_mon3[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$W$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_mon6[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$W$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_W$vol_hy[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$W$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$W$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_vol_90_AN$vol_nov[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$AN$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_dec[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$AN$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_jan[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$AN$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_feb[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$AN$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_mar[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$AN$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_apr[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$AN$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_mon3[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$AN$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_mon6[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$AN$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_AN$vol_hy[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$AN$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$AN$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_vol_90_BN$vol_nov[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$BN$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_dec[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$BN$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_jan[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$BN$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_feb[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$BN$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_mar[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$BN$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_apr[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$BN$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_mon3[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$BN$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_mon6[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$BN$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_BN$vol_hy[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$BN$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$BN$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_vol_90_D$vol_nov[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$D$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_dec[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$D$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_jan[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$D$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_feb[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$D$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_mar[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$D$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_apr[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$D$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_mon3[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$D$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_mon6[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$D$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_D$vol_hy[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$D$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$D$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
	
	simp_mags_data_imp_vol_90_C$vol_nov[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$C$nov$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$nov$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_dec[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$C$dec$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$dec$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_jan[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$C$jan$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$jan$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_feb[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$C$feb$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$feb$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_mar[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$C$mar$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$mar$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_apr[[i]]  <-   mean(simp_mags_data_imp_vol_90[[i]]$C$apr$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$apr$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_mon3[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$C$mon3$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$mon3$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_mon6[[i]]  <-  mean(simp_mags_data_imp_vol_90[[i]]$C$mon6$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$mon6$TotVolAbv_acft!=0)],na.rm=TRUE)
	simp_mags_data_imp_vol_90_C$vol_hy[[i]]  <- mean(simp_mags_data_imp_vol_90[[i]]$C$hy$TotVolAbv_acft[which( simp_mags_data_imp_vol_90[[i]]$C$hy$TotVolAbv_acft!=0)],na.rm=TRUE)
	
}

write.csv(simp_mags_data_imp_vol_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_vol_90_all.csv")
write.csv(simp_mags_data_imp_vol_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_vol_90_W.csv")
write.csv(simp_mags_data_imp_vol_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_vol_90_AN.csv")
write.csv(simp_mags_data_imp_vol_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_vol_90_BN.csv")
write.csv(simp_mags_data_imp_vol_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_vol_90_D.csv")
write.csv(simp_mags_data_imp_vol_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_vol_90_C.csv")


simp_mags_data_imp_dur_90 <- simp_mags_data_90_imp
simp_mags_data_imp_dur_90_all <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="all")
simp_mags_data_imp_dur_90_W <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="W")
simp_mags_data_imp_dur_90_AN <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="AN")
simp_mags_data_imp_dur_90_BN <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="BN")
simp_mags_data_imp_dur_90_D <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="D")
simp_mags_data_imp_dur_90_C <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_imp_dur_90)){
	simp_mags_data_imp_dur_90_all$dur_nov[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$all$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_dec[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$all$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_jan[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$all$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_feb[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$all$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_mar[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$all$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_apr[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$all$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_mon3[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$all$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_mon6[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$all$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_hy[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$all$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_dur_90_W$dur_nov[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$W$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_dec[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$W$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_jan[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$W$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_feb[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$W$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_mar[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$W$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_apr[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$W$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_mon3[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$W$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_mon6[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$W$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_hy[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$W$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_dur_90_AN$dur_nov[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$AN$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_dec[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$AN$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_jan[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$AN$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_feb[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$AN$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_mar[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$AN$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_apr[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$AN$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_mon3[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$AN$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_mon6[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$AN$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_hy[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$AN$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_dur_90_BN$dur_nov[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$BN$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_dec[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$BN$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_jan[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$BN$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_feb[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$BN$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_mar[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$BN$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_apr[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$BN$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_mon3[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$BN$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_mon6[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$BN$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_hy[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$BN$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_dur_90_D$dur_nov[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$D$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_dec[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$D$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_jan[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$D$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_feb[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$D$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_mar[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$D$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_apr[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$D$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_mon3[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$D$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_mon6[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$D$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_hy[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$D$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	
	simp_mags_data_imp_dur_90_C$dur_nov[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$C$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_dec[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$C$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_jan[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$C$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_feb[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$C$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_mar[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$C$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_apr[[i]]  <-   sd(simp_mags_data_imp_dur_90[[i]]$C$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_mon3[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$C$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_mon6[[i]]  <-  sd(simp_mags_data_imp_dur_90[[i]]$C$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_hy[[i]]  <- sd(simp_mags_data_imp_dur_90[[i]]$C$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
}

write.csv(simp_mags_data_imp_dur_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_dur_90_all.csv")
write.csv(simp_mags_data_imp_dur_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_dur_90_W.csv")
write.csv(simp_mags_data_imp_dur_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_dur_90_AN.csv")
write.csv(simp_mags_data_imp_dur_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_dur_90_BN.csv")
write.csv(simp_mags_data_imp_dur_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_dur_90_D.csv")
write.csv(simp_mags_data_imp_dur_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_dur_90_C.csv")

simp_mags_data_imp_dur_90_all <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="all")
simp_mags_data_imp_dur_90_W <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="W")
simp_mags_data_imp_dur_90_AN <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="AN")
simp_mags_data_imp_dur_90_BN <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="BN")
simp_mags_data_imp_dur_90_D <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="D")
simp_mags_data_imp_dur_90_C <- data.frame(gauge=names(simp_mags_data_imp_dur_90),dur_nov=NA, dur_dec=NA, dur_feb=NA,dur_mar=NA, dur_apr=NA,
		dur_mon3=NA, dur_mon6=NA, dur_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_imp_dur_90)){
	simp_mags_data_imp_dur_90_all$dur_nov[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$all$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_dec[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$all$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_jan[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$all$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_feb[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$all$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_mar[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$all$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_apr[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$all$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_mon3[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$all$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_mon6[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$all$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_all$dur_hy[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$all$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$all$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_dur_90_W$dur_nov[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$W$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_dec[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$W$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_jan[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$W$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_feb[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$W$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_mar[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$W$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_apr[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$W$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_mon3[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$W$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_mon6[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$W$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_W$dur_hy[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$W$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$W$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_dur_90_AN$dur_nov[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$AN$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_dec[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$AN$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_jan[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$AN$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_feb[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$AN$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_mar[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$AN$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_apr[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$AN$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_mon3[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$AN$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_mon6[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$AN$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_AN$dur_hy[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$AN$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$AN$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_dur_90_BN$dur_nov[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$BN$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_dec[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$BN$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_jan[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$BN$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_feb[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$BN$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_mar[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$BN$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_apr[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$BN$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_mon3[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$BN$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_mon6[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$BN$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_BN$dur_hy[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$BN$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$BN$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_dur_90_D$dur_nov[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$D$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_dec[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$D$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_jan[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$D$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_feb[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$D$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_mar[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$D$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_apr[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$D$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_mon3[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$D$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_mon6[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$D$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_D$dur_hy[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$D$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$D$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
	
	simp_mags_data_imp_dur_90_C$dur_nov[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$C$nov$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$nov$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_dec[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$C$dec$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$dec$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_jan[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$C$jan$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$jan$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_feb[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$C$feb$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$feb$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_mar[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$C$mar$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$mar$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_apr[[i]]  <-   mean(simp_mags_data_imp_dur_90[[i]]$C$apr$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$apr$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_mon3[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$C$mon3$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$mon3$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_mon6[[i]]  <-  mean(simp_mags_data_imp_dur_90[[i]]$C$mon6$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$mon6$TotDaysAbv!=0)],na.rm=TRUE)
	simp_mags_data_imp_dur_90_C$dur_hy[[i]]  <- mean(simp_mags_data_imp_dur_90[[i]]$C$hy$TotDaysAbv[which( simp_mags_data_imp_dur_90[[i]]$C$hy$TotDaysAbv!=0)],na.rm=TRUE)
	
}

write.csv(simp_mags_data_imp_dur_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_dur_90_all.csv")
write.csv(simp_mags_data_imp_dur_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_dur_90_W.csv")
write.csv(simp_mags_data_imp_dur_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_dur_90_AN.csv")
write.csv(simp_mags_data_imp_dur_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_dur_90_BN.csv")
write.csv(simp_mags_data_imp_dur_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_dur_90_D.csv")
write.csv(simp_mags_data_imp_dur_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_dur_90_C.csv")



simp_mags_data_imp_nmpks_90 <- simp_mags_data_90_imp
simp_mags_data_imp_nmpks_90_all <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="all")
simp_mags_data_imp_nmpks_90_W <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="W")
simp_mags_data_imp_nmpks_90_AN <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="AN")
simp_mags_data_imp_nmpks_90_BN <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="BN")
simp_mags_data_imp_nmpks_90_D <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="D")
simp_mags_data_imp_nmpks_90_C <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_imp_nmpks_90)){
	simp_mags_data_imp_nmpks_90_all$nmpks_nov[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$all$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_dec[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$all$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_jan[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$all$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_feb[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$all$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_mar[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$all$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_apr[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$all$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_mon3[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$all$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_mon6[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$all$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_hy[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$all$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$hy$numpeaks!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_nmpks_90_W$nmpks_nov[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$W$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_dec[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$W$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_jan[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$W$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_feb[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$W$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_mar[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$W$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_apr[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$W$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_mon3[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$W$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_mon6[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$W$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_hy[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$W$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$hy$numpeaks!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_nmpks_90_AN$nmpks_nov[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$AN$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_dec[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$AN$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_jan[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$AN$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_feb[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$AN$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_mar[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$AN$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_apr[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$AN$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_mon3[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$AN$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_mon6[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$AN$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_hy[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$AN$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$hy$numpeaks!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_nmpks_90_BN$nmpks_nov[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$BN$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_dec[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$BN$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_jan[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$BN$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_feb[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$BN$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_mar[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$BN$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_apr[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$BN$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_mon3[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$BN$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_mon6[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$BN$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_hy[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$BN$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$hy$numpeaks!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_nmpks_90_D$nmpks_nov[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$D$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_dec[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$D$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_jan[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$D$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_feb[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$D$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_mar[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$D$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_apr[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$D$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_mon3[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$D$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_mon6[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$D$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_hy[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$D$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$hy$numpeaks!=0)],na.rm=TRUE)
	
	
	simp_mags_data_imp_nmpks_90_C$nmpks_nov[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$C$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_dec[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$C$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_jan[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$C$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_feb[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$C$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_mar[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$C$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_apr[[i]]  <-   sd(simp_mags_data_imp_nmpks_90[[i]]$C$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_mon3[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$C$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_mon6[[i]]  <-  sd(simp_mags_data_imp_nmpks_90[[i]]$C$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_hy[[i]]  <- sd(simp_mags_data_imp_nmpks_90[[i]]$C$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$hy$numpeaks!=0)],na.rm=TRUE)
	
}

write.csv(simp_mags_data_imp_nmpks_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_nmpks_90_all.csv")
write.csv(simp_mags_data_imp_nmpks_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_nmpks_90_W.csv")
write.csv(simp_mags_data_imp_nmpks_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_nmpks_90_AN.csv")
write.csv(simp_mags_data_imp_nmpks_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_nmpks_90_BN.csv")
write.csv(simp_mags_data_imp_nmpks_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_nmpks_90_D.csv")
write.csv(simp_mags_data_imp_nmpks_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_SD_data_imp_nmpks_90_C.csv")

simp_mags_data_imp_nmpks_90_all <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="all")
simp_mags_data_imp_nmpks_90_W <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="W")
simp_mags_data_imp_nmpks_90_AN <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="AN")
simp_mags_data_imp_nmpks_90_BN <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="BN")
simp_mags_data_imp_nmpks_90_D <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="D")
simp_mags_data_imp_nmpks_90_C <- data.frame(gauge=names(simp_mags_data_imp_nmpks_90),nmpks_nov=NA, nmpks_dec=NA, nmpks_feb=NA,nmpks_mar=NA, nmpks_apr=NA,
		nmpks_mon3=NA, nmpks_mon6=NA, nmpks_hy=NA, yeartype="C")
for(i in 1:length(simp_mags_data_imp_nmpks_90)){
	simp_mags_data_imp_nmpks_90_all$nmpks_nov[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$all$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_dec[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$all$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_jan[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$all$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_feb[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$all$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_mar[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$all$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_apr[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$all$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_mon3[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$all$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_mon6[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$all$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_all$nmpks_hy[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$all$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$all$hy$numpeaks!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_nmpks_90_W$nmpks_nov[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$W$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_dec[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$W$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_jan[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$W$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_feb[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$W$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_mar[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$W$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_apr[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$W$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_mon3[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$W$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_mon6[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$W$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_W$nmpks_hy[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$W$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$W$hy$numpeaks!=0)],na.rm=TRUE)
	
	simp_mags_data_imp_nmpks_90_AN$nmpks_nov[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$AN$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_dec[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$AN$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_jan[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$AN$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_feb[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$AN$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_mar[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$AN$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_apr[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$AN$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_mon3[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$AN$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_mon6[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$AN$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_AN$nmpks_hy[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$AN$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$AN$hy$numpeaks!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_nmpks_90_BN$nmpks_nov[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$BN$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_dec[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$BN$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_jan[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$BN$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_feb[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$BN$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_mar[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$BN$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_apr[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$BN$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_mon3[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$BN$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_mon6[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$BN$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_BN$nmpks_hy[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$BN$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$BN$hy$numpeaks!=0)],na.rm=TRUE)
	
	
	
	simp_mags_data_imp_nmpks_90_D$nmpks_nov[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$D$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_dec[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$D$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_jan[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$D$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_feb[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$D$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_mar[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$D$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_apr[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$D$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_mon3[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$D$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_mon6[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$D$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_D$nmpks_hy[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$D$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$D$hy$numpeaks!=0)],na.rm=TRUE)
	
	
	simp_mags_data_imp_nmpks_90_C$nmpks_nov[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$C$nov$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$nov$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_dec[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$C$dec$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$dec$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_jan[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$C$jan$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$jan$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_feb[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$C$feb$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$feb$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_mar[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$C$mar$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$mar$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_apr[[i]]  <-   mean(simp_mags_data_imp_nmpks_90[[i]]$C$apr$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$apr$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_mon3[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$C$mon3$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$mon3$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_mon6[[i]]  <-  mean(simp_mags_data_imp_nmpks_90[[i]]$C$mon6$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$mon6$numpeaks!=0)],na.rm=TRUE)
	simp_mags_data_imp_nmpks_90_C$nmpks_hy[[i]]  <- mean(simp_mags_data_imp_nmpks_90[[i]]$C$hy$numpeaks[which( simp_mags_data_imp_nmpks_90[[i]]$C$hy$numpeaks!=0)],na.rm=TRUE)
	
}

write.csv(simp_mags_data_imp_nmpks_90_all, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_nmpks_90_all.csv")
write.csv(simp_mags_data_imp_nmpks_90_W, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_nmpks_90_W.csv")
write.csv(simp_mags_data_imp_nmpks_90_AN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_nmpks_90_AN.csv")
write.csv(simp_mags_data_imp_nmpks_90_BN, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_nmpks_90_BN.csv")
write.csv(simp_mags_data_imp_nmpks_90_D, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_nmpks_90_D.csv")
write.csv(simp_mags_data_imp_nmpks_90_C, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\simp_mag_data_imp_nmpks_90_C.csv")



COM90_avg_DOHY <- rep(NA, 93)
COM90_sd_DOHY <- rep(NA, 93)
gauge_COM90 <- rep(NA,93)
W_avg <- rep(NA,93)
W_sd <- rep(NA,93)
AN_avg <- rep(NA,93)
AN_sd <- rep(NA,93)
BN_avg <- rep(NA,93)
BN_sd <- rep(NA,93)
D_avg <- rep(NA,93)
D_sd <- rep(NA,93)
C_avg <- rep(NA,93)
C_sd <- rep(NA,93)

for(i in 1:93){
	gauge_COM90[[i]] <- strsplit(names(COM90_full_df)[[i+1]],"_")[[1]][[3]]
	gauge_COM90[[i]] <- strsplit(names(COM90_full_df)[[i+1]],"_")[[1]][[3]]
	if(gauge_COM90[[i]]%in%SacV_gauges$site_no){
		COM90_avg_DOHY[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear>=1970], na.rm=TRUE))
		COM90_sd_DOHY[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear>=1970], na.rm=TRUE))
		W_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="5")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)     )
		W_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="5")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)        )
		AN_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="4")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)    )
		AN_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="4")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)       )
		BN_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="3")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)    )
		BN_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="3")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)       )
		D_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="2")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)     )
		D_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="2")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)        )
		C_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="1")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)     )
		C_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SVI=="1")]&COM90_full_df$sthyyear>=1970],na.rm=TRUE)        )
		
	} else if(gauge_COM90[[i]]%in%SJV_gauges$site_no){   
		COM90_avg_DOHY[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear>=1989], na.rm=TRUE))
		COM90_sd_DOHY[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear>=1989], na.rm=TRUE))
		W_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="5")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE) )
		W_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="5")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE)    )
		AN_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="4")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE))
		AN_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="4")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE)   )
		BN_avg[[i]] <- round(mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="3")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE))
		BN_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="3")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE)   )
		D_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="2")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE) )
		D_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="2")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE)    )
		C_avg[[i]] <-round( mean(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="1")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE) )
		C_sd[[i]] <- round(sd(COM90_full_df[[i+1]][COM90_full_df$sthyyear%in%yeartype_old$sthyyear[which(yeartype_old$SJI=="1")]&COM90_full_df$sthyyear>=1989],na.rm=TRUE)    )
		
	}
}
COM90_magsd_imp <- data.frame(gauge =gauge_COM90, avg_DOHY=COM90_avg_DOHY, sd_DOHY= COM90_sd_DOHY,
		W_avg = W_avg,
		W_sd    =  W_sd    ,
		AN_avg  =  AN_avg  ,
		AN_sd   =  AN_sd   ,
		BN_avg  =  BN_avg  ,
		BN_sd   =  BN_sd   ,
		D_avg   =  D_avg   ,
		D_sd    =  D_sd    ,
		C_avg   =  C_avg   ,
		C_sd    =  C_sd    )

write.csv(COM90_magsd_imp, file="C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\Thesis_tables\\COM90_mag_sd_imp_updated.csv")