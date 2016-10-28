# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

spbatch_93 <- vector("list",7)
for(i in 1:7){
	testnum2 <-i
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",i,".RData",sep=""))
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_base",".RData",sep=""))
	i <- testnum2
	spbatch_93[[i]] <- vector("list",length(which(datetable$gauge%in%names(spbatch))))
	spbatch_93[[i]] <- spbatch[which(names(spbatch)%in%datetable$gauge)]
}


spbatch_93_unlist <- spbatch_93[[1]]

for(i in 2:7){
spbatch_93_unlist <- append(spbatch_93_unlist,spbatch_93[[i]])
}

save("spbatch_93_unlist", file= "C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_93_spbatch.RData")