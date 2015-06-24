# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


na.pad <- function(x,len){
	x[1:len]
}

paddf <- function(l,...){
	maxlen <- max(sapply(l,length))
	data.frame(lapply(l,na.pad,len=maxlen),...)
}
