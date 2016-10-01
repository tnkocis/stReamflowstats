# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))	
	for(k in 1:length(spbatch)){
		table <- spbatch[[k]]$prep
		table$gague <- names(spbatch)[[k]]
		write.csv(table,paste("C:\\Users\\tiffn_000\\Documents\\date_files\\",names(spbatch)[[k]],".csv",sep=""))
	}
}	


files <- list.files("C:\\Users\\tiffn_000\\Documents\\date_files\\",full.names=TRUE)
tables <- vector("list",93)
for(i in 1:length(files)){
	tables[[i]]<-read.csv(files[[i]])
	names(tables)[[i]]<- tables[[i]]$gague[[1]]
}
datetable <- data.frame(gauge=names(tables),start_date=NA, end_date=NA,per_available=NA,per_missing=NA)
for(i in 1:length(tables)){
	st <- which(tables[[i]]$Available==1)[[1]]
	ed <- tail(which(tables[[i]]$Available==1),n=1)
	datetable$start_date[[i]] <- as.character(tables[[i]]$Date[st], format="%Y-%m-%d")
	datetable$end_date[[i]] <- as.character(tables[[i]]$Date[ed], format="%Y-%m-%d")
	totav <- sum(tables[[i]]$Available[st:ed],na.rm=TRUE)
	totdays <- (ed-st)+1
	datetable$per_available[[i]] <- round(totav/totdays*100,digits=0)
	datetable$per_missing[[i]] <- round(((totdays-totav)/totdays)*100,digits=0)
}

write.csv(datetable,"C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\manuscripts_from_thesis\\date_table_long.csv")
write.csv(datetable[,1:3],"C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\manuscripts_from_thesis\\date_table_short.csv")

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\zero_threshold_spbatch_",batchnum,".RData", sep=""))	
	for(k in 1:length(test_split)){
		table <- test_split[[k]]$all$hy
		table$gague <- names(test_split)[[k]]
		write.csv(table,paste("C:\\Users\\tiffn_000\\Documents\\zero_thresh\\hy_",names(test_split)[[k]],".csv",sep=""))
	}
}	

files <- list.files("C:\\Users\\tiffn_000\\Documents\\zero_thresh\\",full.names=TRUE)
tables <- vector("list",93)
for(i in 1:length(files)){
	tables[[i]]<-read.csv(files[[i]])
	names(tables)[[i]]<- tables[[i]]$gague[[1]]
}

longtermtable <- data.frame(gauge=names(tables),vol_acft_avg=NA)
for(i in 1:length(tables)){
	longtermtable$vol_acft_avg[[i]]<-mean(tables[[i]]$TotVolAbv_acft,na.rm=TRUE)
}

np <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\Manuscripts\\unshared\\SUBMISSION_orignially_edits_2_21_2016\\Appendecies\\Appendix_1\\data_magnitude_full_record.csv")
np_all_hy <- np[which(np$yeartype=="all"&np$period=="Hydrologic Year"),]

longtermtable$hmf <- np_all_hy$avg

longtermtable$perc <- (longtermtable$hmf/longtermtable$vol_acft_avg)*100


for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))	
	for(k in 1:length(test_split)){
		table <- test_split[[k]]$all$hy
		table$gague <- names(test_split)[[k]]
		write.csv(table,paste("C:\\Users\\tiffn_000\\Documents\\thresh_90\\hy_",names(test_split)[[k]],".csv",sep=""))
	}
}	

files90 <- list.files("C:\\Users\\tiffn_000\\Documents\\thresh_90\\",full.names=TRUE)
tables90 <- vector("list",93)
for(i in 1:length(files90)){
	tables90[[i]]<-read.csv(files90[[i]])
	names(tables90)[[i]]<- tables90[[i]]$gague[[1]]
}

comp <- vector("list",93)
names(comp) <- names(tables90)
for(i in 1L:length(tables90)){
	comp[[i]] <- tables90[[i]]$TotVolAbv_acft/tables[[i]]$TotVolAbv_acft
}

comptable <- data.frame(gauge=names(comp),avgperc=NA)
for(i in 1:length(comp)){
	comptable$avgperc[[i]] <- mean(comp[[i]],na.rm=TRUE)*100
}