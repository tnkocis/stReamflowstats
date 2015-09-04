# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################

magicCSV = function(startlist, targetdir){
	ff = function(x){ 
		if (class(x) == "list") 
			lapply(x, ff) 
		else if(class(x) == "data.frame") 
			NA
		else
			NULL
	}
	lnames = names(unlist(lapply(startlist, ff)))
	fnames = file.path(file.path(targetdir), paste0(gsub(".", "/", lnames, 
							fixed = TRUE), ".csv"))
	dirnames = unlist(lapply(fnames, dirname))
	varnames = paste0("startlist$", gsub(".", "$", lnames, fixed = TRUE))
	evalstrings = paste0('write.csv(', varnames, ', file ="', fnames, '")')
	exprs = lapply(evalstrings, function(x) parse(text = gsub("\\", "/", x, 
								fixed = TRUE)))
	
	suppressWarnings(lapply(dirnames, dir.create, recursive = TRUE))
	
	invisible(lapply(exprs, eval, envir = environment()))
}