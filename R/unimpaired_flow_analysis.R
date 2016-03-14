# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################
unimp_13 <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\umimpaired_13.txt",header=TRUE)
unimp_13 <- unimp_13$Value
unimpaired_13 <- vector("list",length(unimp_13))
names(unimpaired_13) <- unimp_13
unimpaired_13_split <- vector("list",length(unimp_13))
names(unimpaired_13_split) <- unimp_13

for(z in 1:7){
	batchnum <- z
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\full_record_spbatch_",batchnum,".RData", sep=""))
	if(any(names(spbatch)%in%unimp_13)){
		un <- which(names(spbatch)%in%unimp_13)
		for(i in 1:length(un)){
			position <- which(names(unimpaired_13)==names(spbatch)[[un[[i]]]])
			unimpaired_13[[position]] <- spbatch[[un[[i]]]]
			unimpaired_13_split[[position]] <-test_split[[un[[i]]]]
		}
	}
}

unimpaired_13_data <- unimpaired_13
unimpaired_13_split_data <- unimpaired_13_split