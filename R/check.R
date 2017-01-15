# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################



		batchnum <- z
#		load( "C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_93_spbatch.RData")
		#		load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\functions_5_13_16",".RData", sep=""))
		
#		thresholdchoice <- y
#		thresholdtext <- paste(thresholdchoice,"%",sep="")
		
		#		for(k in 1:length(spbatch)){
		#			spbatch[[k]]$thresholds_maf <- thresholds(spbatch[[k]]$prep)
		#		}
		
		blah_peakflows <- vector("list", length(spbatch))
		blah_peakflowstats <- vector("list", length(spbatch))
		blah_peakflowsummary <- vector("list", length(spbatch))
		blah_peakflowmonthlystats <- vector("list", length(spbatch))
		blah_peakflowsdf <- vector("list", length(spbatch))
		for(k in 88:88){
			blah_peakflows[[k]] <- vector("list",1)
			names(blah_peakflows[[k]]) <- c("peakflows")
			blah_peakflows[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$HydroYear$Data))
			for(i in 1:length(spbatch[[k]]$HydroYear$Data)){
				blah_peakflows[[k]]$peakflows[[i]] <- edit_simplified_peakanalysis(input=spbatch[[k]]$HydroYear$Data[[i]],
						width=3, threshold=0, 
						thresholdname="0", mastertime="hy", Index=spbatch[[k]]$Index)
				
			}
			blah_peakflowstats[[k]] <- vector("list",length(blah_peakflows[[k]]$peakflows))
			for(i in 1:length(blah_peakflows[[k]]$peakflows))	{
				blah_peakflowstats[[k]][[i]] <- blah_peakflows[[k]]$peakflows[[i]][[2]]
			}
			blah_peakflowsummary[[k]] <- vector("list",length(blah_peakflows[[k]]$peakflows))
			for(i in 1:length(blah_peakflows[[k]]$peakflows))	{
				blah_peakflowsummary[[k]][[i]] <- blah_peakflows[[k]]$peakflows[[i]][[1]]
			}
			blah_peakflowmonthlystats[[k]] <- vector("list",length(blah_peakflows[[k]]$peakflows))
			for(i in 1:length(blah_peakflows[[k]]$peakflows))	{
				blah_peakflowmonthlystats[[k]][[i]] <- blah_peakflows[[k]]$peakflows[[i]][[3]]
			}
			blah_peakflowsdf[[k]] <- vector("list",3)
			names(blah_peakflowsdf[[k]]) <- c("pfstatsdf","pfsummarydf","pfmonthlystats")
			blah_peakflowsdf[[k]]$pfstatsdf <- do.call(rbind.data.frame,blah_peakflowstats[[k]])
			blah_peakflowsdf[[k]]$pfsummarydf <- do.call(rbind.data.frame,blah_peakflowsummary[[k]])
			blah_peakflowsdf[[k]]$pfmonthlystats <- do.call(rbind.data.frame,blah_peakflowmonthlystats[[k]])
			blah_peakflowsdf[[k]]$pfstatsdf$volday_is_zero <- rep(NA, length(blah_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv))
			for(i in 1:length(blah_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv)){
				if(blah_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv[[i]]==0){
					blah_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 1
				}else{
					blah_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 0
				}
			}
			blah_peakflowsdf[[k]]$pfstatsdf$volday_is_zero_cumsum <- cumsum(blah_peakflowsdf[[k]]$pfstatsdf$volday_is_zero)	
		}
		names(blah_peakflowsdf)<- names(spbatch)
		names(blah_peakflows)<- names(spbatch)
		names(blah_peakflowstats)<- names(spbatch)
		names(blah_peakflowsummary)<- names(spbatch)
		names(blah_peakflowmonthlystats)<- names(spbatch)
		blah_split <- vector("list", length(spbatch))
		for(k in 88:88){
			blah_split[[k]]<- peakflowanalysis_split(blah_peakflowsdf[[k]]$pfmonthlystats)
		}
		names(blah_split)<- names(spbatch)	
		
		
		edit_simplified_peakflowmags <- function(pfstatsdf, gauge, year){
			top <- vector("list", 6)
			for(i in 1:6){
				for(k in 1:15){
					mean_totvol_TAF <- mean(pfstatsdf[[i]][[k]]$TotVolAbv_acft[which(pfstatsdf[[i]][[k]]$TotVolAbv_acft!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])/1000
					mean_totdays <- mean(pfstatsdf[[i]][[k]]$TotDaysAbv[which(pfstatsdf[[i]][[k]]$TotDaysAbv!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])
					mean_numpeaks <- mean(pfstatsdf[[i]][[k]]$numpeaks[which(pfstatsdf[[i]][[k]]$numpeaks!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])
					frac_zero <- length(which(pfstatsdf[[i]][[k]]$numpeaks==0&pfstatsdf[[i]][[k]]$sthyyear>year))/length(pfstatsdf[[i]][[k]]$numpeaks[which(pfstatsdf[[i]][[k]]$sthyyear>year)])
					frac_nonzero <- 1-frac_zero
					num_zero <-length(which(pfstatsdf[[i]][[k]]$numpeaks==0&pfstatsdf[[i]][[k]]$sthyyear>year))
					num_nonzero <-length(which(pfstatsdf[[i]][[k]]$numpeaks!=0&pfstatsdf[[i]][[k]]$sthyyear>year))
					sd_totvol_TAF <- sd(pfstatsdf[[i]][[k]]$TotVolAbv_acft[which(pfstatsdf[[i]][[k]]$TotVolAbv_acft!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])/1000
					sd_totdays <- sd(pfstatsdf[[i]][[k]]$TotDaysAbv[which(pfstatsdf[[i]][[k]]$TotDaysAbv!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])
					sd_numpeaks <- sd(pfstatsdf[[i]][[k]]$numpeaks[which(pfstatsdf[[i]][[k]]$numpeaks!=0&pfstatsdf[[i]][[k]]$sthyyear>year)])
					
					meandf <- data.frame(mean_totvol_TAF =mean_totvol_TAF ,sd_totvol_TAF =sd_totvol_TAF ,
							mean_totdays=mean_totdays,sd_totdays=sd_totdays,
							mean_numpeaks=mean_numpeaks,sd_numpeaks=sd_numpeaks,
							frac_zero=frac_zero,
							frac_nonzero=frac_nonzero,
							num_zero=num_zero,
							num_nonzero=num_nonzero,
							gauge=gauge,
							styear=year)
					
					top[[i]][[k]] <- meandf	
				}
				names(top[[i]]) <- names(pfstatsdf[[i]])
			}
			names(top)<- names(pfstatsdf)
			return(top)
		}
		
		blah_peakflowmags_full_edit <- vector("list", length(spbatch))
		for(k in 88:88){
			blah_peakflowmags_full_edit[[k]] <- edit_simplified_peakflowmags(blah_split[[k]],names(blah_split)[[k]],1800)
			
		}
		names(blah_peakflowmags_full_edit) <- names(spbatch)
		
		
		
