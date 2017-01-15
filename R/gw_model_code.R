# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


# TODO: Add comment
# 
# Author: kmstone5
###############################################################################


# TODO: Add comment
# 
# Author: kmstone5
###############################################################################
thresholds <- function(input){
#remove if package
	if(!require(dplyr)){
		install.packages("dplyr")
		library(dplyr)
	}
#
	if (missing(input))
		stop("Input data is required.")
	
	discharge_maf <- input$Discharge_cfs*86400*2.29568411e-5*1e-6
	percentile_maf <- quantile(discharge_maf, probs=c(0.05,0.1,0.2,0.25,0.5,0.75,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99),na.rm=TRUE)
	names(percentile_maf) <- paste("P",c(5,10,20,25,50,75,90,91,92,93,94,95,96,97,98,99),"maf",sep="")
	percentile_maf <- as.list(percentile_maf)
	
	return(Percentile_maf=as.data.frame(percentile_maf))
	
}


for(k in 1:length(spbatch)){
	spbatch[[k]]$thresholds_maf <- thresholds(spbatch[[k]]$prep)
}

simp_mags_data_vol_90_all_flat <- test_split
save(simp_mags_data_vol_90_all_flat, file="C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_vol_data_for_gwmodel_90.RData")

save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_spbatch93_percentile_",90,".RData", sep=""))

for(y in 99:99){	
	threshchoice <- y
	for(z in 1:1){
		batchnum <- z
		load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_spbatch93_percentile_",90,".RData", sep=""))
#		load( "C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_base.RData")
		
		#		load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\functions_5_13_16",".RData", sep=""))
		
		thresholdchoice <- threshchoice
		thresholdtext <- paste(thresholdchoice,"%",sep="")
		
		#		for(k in 1:length(spbatch)){
		#			spbatch[[k]]$thresholds_maf <- thresholds(spbatch[[k]]$prep)
		#		}
		
		test_peakflows <- vector("list", length(spbatch))
		test_peakflowstats <- vector("list", length(spbatch))
		test_peakflowsummary <- vector("list", length(spbatch))
		test_peakflowmonthlystats <- vector("list", length(spbatch))
		test_peakflowsdf <- vector("list", length(spbatch))
		for(k in 1:length(spbatch)){
			test_peakflows[[k]] <- vector("list",1)
			names(test_peakflows[[k]]) <- c("peakflows")
			test_peakflows[[k]]$peakflows <- vector("list", length=length(spbatch[[k]]$HydroYear$Data))
			for(i in 1:length(spbatch[[k]]$HydroYear$Data)){
				test_peakflows[[k]]$peakflows[[i]] <- edit_simplified_peakanalysis(input=spbatch[[k]]$HydroYear$Data[[i]],
						width=3, threshold=(eval(parse(text=paste("spbatch[[k]]$thresholds_maf$P",thresholdchoice,"maf/(86400*2.29568411e-5*1e-6)",sep="")))), 
						thresholdname=thresholdtext, mastertime="hy", Index=spbatch[[k]]$Index)
				
			}
			test_peakflowstats[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
			for(i in 1:length(test_peakflows[[k]]$peakflows))	{
				test_peakflowstats[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[2]]
			}
			test_peakflowsummary[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
			for(i in 1:length(test_peakflows[[k]]$peakflows))	{
				test_peakflowsummary[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[1]]
			}
			test_peakflowmonthlystats[[k]] <- vector("list",length(test_peakflows[[k]]$peakflows))
			for(i in 1:length(test_peakflows[[k]]$peakflows))	{
				test_peakflowmonthlystats[[k]][[i]] <- test_peakflows[[k]]$peakflows[[i]][[3]]
			}
			test_peakflowsdf[[k]] <- vector("list",3)
			names(test_peakflowsdf[[k]]) <- c("pfstatsdf","pfsummarydf","pfmonthlystats")
			test_peakflowsdf[[k]]$pfstatsdf <- do.call(rbind.data.frame,test_peakflowstats[[k]])
			test_peakflowsdf[[k]]$pfsummarydf <- do.call(rbind.data.frame,test_peakflowsummary[[k]])
			test_peakflowsdf[[k]]$pfmonthlystats <- do.call(rbind.data.frame,test_peakflowmonthlystats[[k]])
			test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero <- rep(NA, length(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv))
			for(i in 1:length(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv)){
				if(test_peakflowsdf[[k]]$pfstatsdf$TotDaysAbv[[i]]==0){
					test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 1
				}else{
					test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero[[i]] <- 0
				}
			}
			test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero_cumsum <- cumsum(test_peakflowsdf[[k]]$pfstatsdf$volday_is_zero)	
		}
		names(test_peakflowsdf)<- names(spbatch)
		names(test_peakflows)<- names(spbatch)
		names(test_peakflowstats)<- names(spbatch)
		names(test_peakflowsummary)<- names(spbatch)
		names(test_peakflowmonthlystats)<- names(spbatch)
		test_split <- vector("list", length(spbatch))
		for(k in 1:length(spbatch)){
			test_split[[k]]<- peakflowanalysis_split(test_peakflowsdf[[k]]$pfmonthlystats)
		}
		names(test_split)<- names(spbatch)	
		
		test_peakflowmags_full <- vector("list", length(spbatch))
		for(k in 1:length(spbatch)){
			test_peakflowmags_full[[k]] <- simplified_peakflowmags(test_split[[k]],names(test_split)[[k]],1800)
			
		}
		names(test_peakflowmags_full) <- names(spbatch)
		
		test_peakflowmags_full_bind <- test_peakflowmags_full[[1]]
		for(k in 2:length(spbatch)){
			for(i in 1:6){
				for(l in 1:15){
					test_peakflowmags_full_bind[[i]][[l]] <- rbind.data.frame(test_peakflowmags_full_bind[[i]][[l]],
							test_peakflowmags_full[[k]][[i]][[l]])
				}
			}
		}
		
		
		#		for(i in 1:15){
		#			write.csv(test_peakflowmags_full_bind$all[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\all\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$all)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$W[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\W\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$W)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$AN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\AN\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$AN)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$BN[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\BN\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$BN)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$D[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\D\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$D)[[i]],".csv", sep=""))
		#			
		#			write.csv(test_peakflowmags_full_bind$C[[i]], file=paste("C:\\Users\\tiffn_000\\Documents\\percentile_99\\peakflow_mags\\full\\C\\batch_",batchnum,"_",
		#							names(test_peakflowmags_full_bind$C)[[i]],".csv", sep=""))
		#		}
		#		
		simp_mags_data_vol_99_flat <- test_split
		save(simp_mags_data_vol_99_flat,file="C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_vol_data_for_gwmodel_99.RData" )
		save.image(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_spbatch93_percentile_",thresholdchoice,".RData", sep=""))
		
	}	
}

thresholdsvect <- c(90:99)	
gwmodel_data2 <- vector("list", length(thresholdsvect))
for(r in 1:length(thresholdsvect)){
	percnum2 <- r
	load(paste("C:\\Users\\tiffn_000\\Documents\\workspaces\\kaweahREDO_percentile_",thresholdsvect[[percnum2]],"_spbatch_",1,".RData", sep=""))
	gwmodel_data2[[percnum2]] <- test_split
}
names(gwmodel_data2)<- thresholdsvect
save(gwmodel_data2, file="C:/Users/tiffn_000/Google Drive/KaweahREDO/vol_data_for_kaweah_gwmodel.RData")







#node_calc <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\SRA_Kathleen\\Data\\C2Vsim\\downstreamPODs.csv")
#node_calc <- node_calc[!is.na(node_calc$divert.calculation.with.vol90_mon6_all.gauge.data),]

node_calc <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\testgwmodel\\nodecalcs_code.csv")

df <- data.frame(node_IGW=node_calc$C2Vsim_river_nodes_fine_IGW[!is.na(node_calc$C2Vsim_river_nodes_fine_IGW)], year=1900, volume=NA)
df <- as.data.frame(t(df))
df[c(1:3),] <- NA

colnames(df) <- node_calc$C2Vsim_river_nodes_fine_IGW
node_calc$divert..calculation. <- as.character(node_calc$divert..calculation.)
gauges_list <- vector("list",93)
vol_90 <- read.csv("C:\\Users\\tiffn_000\\Google Drive\\SRA_Kathleen\\mags\\vol_90\\simp_mags_data_vol_90_all.csv", header=TRUE)
gauges <- paste("X",vol_90$gauge, sep="")
names(gauges_list) <- gauges

year_merge <- data.frame(year=seq(1900,2014,1))
df2 <- t(vol_90[,9])
colnames(df2) <- gauges
df2 <- as.data.frame(df2)
df2$Year <- 1900
vol_90$Xgauge <- gauges
test2 <- merge(df2,year_merge,by.x="Year",by.y="year",all.y=TRUE)
test2$year <- test2$Year
test2$Year <-NULL
test2$month <- NA


years <- c(1900:2014)
df3 <- data.frame(matrix(NA,nrow=length(years),ncol=length(node_calc$C2Vsim_river_nodes_fine_IGW)))
colnames(df3) <- node_calc$C2Vsim_river_nodes_fine_IGW
df3$year <- years

list_vol <- vector("list",6)
#load("C:\\Users\\tiffn_000\\Google Drive\\SRA_Kathleen\\vol_data_for_gwmodel.RData")


###########
load("C:\\Users\\tiffn_000\\Documents\\workspaces\\REDO_vol_data_for_gwmodel_90.RData")

for(k in 5:10){
	for(h in 1:length(simp_mags_data_vol_90_flat)){
		year_test <- merge(simp_mags_data_vol_90_flat[[h]]$all[[k]],year_merge,by.x="sthyyear",by.y="year",all.y=TRUE)
		year_test$gauge <- paste("X",names(simp_mags_data_vol_90_flat)[[h]],sep="")
		year_test$TotVolAbv_acft[is.na(year_test$TotVolAbv_acft)] <- 0
		gaugetext <- paste("test2$",year_test$gauge[[1]]," <- year_test$TotVolAbv_acft")
		test2$month <- unique(year_test$month)[!is.na(unique(year_test$month))]
		eval(parse(text=gaugetext))
	}
	df4 <- test2
	for(j in 1:length(years)){
		for(i in 1:length(node_calc$C2Vsim_river_nodes_fine_IGW)){
			year <- df4$year[[j]]
			t <- which(df4$year==year)
			test <- paste(paste("df4$",strsplit(node_calc$divert..calculation.[[i]],c("[$]"))[[1]][2:length(strsplit(node_calc$divert..calculation.[[i]],c("[$]"))[[1]])],sep=""),collapse="")
			df3[j,i] <- eval(parse(text=test))
			df3$month <-df4$month
		}
	}
	df3[df3<0] <- 0
	list_vol[[k-4]] <- df3
	
}

names(list_vol) <- c("November","December","January","February","March","April")
for(i in 1:6){
	for(j in 1:length(list_vol[[i]]$year)){
		list_vol[[i]]$hydrologic_year[[j]] <- paste(list_vol[[i]]$year[[j]],"-",list_vol[[i]]$year[[j]]+1, sep="")
	}
	list_vol[[i]]$year <- NULL
	list_vol[[i]] <-list_vol[[i]][seq(1,114,1),]
}

for(i in 1:6){
	write.csv(list_vol[[i]],paste("C:\\Users\\tiffn_000\\Google Drive\\REDOgwmodel_giorgos\\90\\",names(list_vol)[[i]],".csv",sep=""))
}



vol_mon6 <- list_vol[[1]][,1:42]+list_vol[[2]][,1:42]+list_vol[[3]][,1:42]+list_vol[[4]][,1:42]+list_vol[[5]][,1:42]+list_vol[[6]][,1:42]
vol_mon6$hyyear <-list_vol$November$hydrologic_year
vol_mon6$total <- rowSums(vol_mon6[,1:42])


save.image("C:\\Users\\kmstone5\\Google Drive\\REDOgwmodel_giorgos\\gwmodel_90.RData")


