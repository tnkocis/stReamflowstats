# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


library(dplyr)
library(ggplot2)

files_mag <- list.files(path="C:/Users/tiffn_000/Google Drive/Rachel_p/", pattern="full_")
mags <- vector("list", length(files_mag))
for(i in 1:length(mags)){
	table <- read.csv(paste("C:/Users/tiffn_000/Google Drive/Rachel_p/",files_mag[[i]],sep=""))
	tablename <- strsplit(files_mag[[i]],"[.]")[[1]][[1]]
	tablename <- strsplit(tablename,"_")
	if(tablename[[1]][[3]]=="nov"){
		period <- "November"
	} else if (tablename[[1]][[3]]=="dec"){
		period <- "December"
	} else if (tablename[[1]][[3]]=="jan"){
		period <- "January"
	} else if (tablename[[1]][[3]]=="feb"){
		period <- "February"
	} else if (tablename[[1]][[3]]=="mar"){
		period <- "March"
	} else if (tablename[[1]][[3]]=="apr"){
		period <- "April"
	} else if (tablename[[1]][[3]]=="mon3"){
		period <- "December to February"
	} else if (tablename[[1]][[3]]=="mon6"){
		period <- "November to April"
	} else if (tablename[[1]][[3]]=="hy"){
		period <- "Hydrologic Year"
	}
	table2 <- data.frame(gauge=table$gauge,yeartype=tablename[[1]][[2]],period=period,avg=table$mean_numpeaks,sd=table$sd_numpeaks,valtype=c("intraannual_frequency_numpeaks"))
	mags[[i]] <- table2
}
mags2 <- do.call(rbind.data.frame,mags)
write.csv(mags2,"C:/Users/tiffn_000/Google Drive/Rachel_p/simp_data_full_vol_90_intraannual_frequency.csv")

files_mag <- list.files(path="C:/Users/tiffn_000/Google Drive/Rachel_p/", pattern="postimp_")
mags <- vector("list", length(files_mag))
for(i in 1:length(mags)){
	table <- read.csv(paste("C:/Users/tiffn_000/Google Drive/Rachel_p/",files_mag[[i]],sep=""))
	tablename <- strsplit(files_mag[[i]],"[.]")[[1]][[1]]
	tablename <- strsplit(tablename,"_")
	if(tablename[[1]][[3]]=="nov"){
		period <- "November"
	} else if (tablename[[1]][[3]]=="dec"){
		period <- "December"
	} else if (tablename[[1]][[3]]=="jan"){
		period <- "January"
	} else if (tablename[[1]][[3]]=="feb"){
		period <- "February"
	} else if (tablename[[1]][[3]]=="mar"){
		period <- "March"
	} else if (tablename[[1]][[3]]=="apr"){
		period <- "April"
	} else if (tablename[[1]][[3]]=="mon3"){
		period <- "December to February"
	} else if (tablename[[1]][[3]]=="mon6"){
		period <- "November to April"
	} else if (tablename[[1]][[3]]=="hy"){
		period <- "Hydrologic Year"
	}
	table2 <- data.frame(gauge=table$gauge,yeartype=tablename[[1]][[2]],period=period,avg=table$mean_numpeaks,sd=table$sd_numpeaks,valtype=c("intraannual_frequency_numpeaks"))
	mags[[i]] <- table2
}
mags2 <- do.call(rbind.data.frame,mags)
write.csv(mags2,"C:/Users/tiffn_000/Google Drive/Rachel_p/simp_data_imp_vol_90_intraannual_frequency.csv")

##fracywf##

files_mag <- list.files(path="C:/Users/tiffn_000/Google Drive/Rachel_p/", pattern="full_")
mags <- vector("list", length(files_mag))
for(i in 1:length(mags)){
	table <- read.csv(paste("C:/Users/tiffn_000/Google Drive/Rachel_p/",files_mag[[i]],sep=""))
	tablename <- strsplit(files_mag[[i]],"[.]")[[1]][[1]]
	tablename <- strsplit(tablename,"_")
	if(tablename[[1]][[3]]=="nov"){
		period <- "November"
	} else if (tablename[[1]][[3]]=="dec"){
		period <- "December"
	} else if (tablename[[1]][[3]]=="jan"){
		period <- "January"
	} else if (tablename[[1]][[3]]=="feb"){
		period <- "February"
	} else if (tablename[[1]][[3]]=="mar"){
		period <- "March"
	} else if (tablename[[1]][[3]]=="apr"){
		period <- "April"
	} else if (tablename[[1]][[3]]=="mon3"){
		period <- "December to February"
	} else if (tablename[[1]][[3]]=="mon6"){
		period <- "November to April"
	} else if (tablename[[1]][[3]]=="hy"){
		period <- "Hydrologic Year"
	}
	table2 <- data.frame(gauge=table$gauge,yeartype=tablename[[1]][[2]],period=period,frac_zero=table$frac_zero,frac_nonzero=table$frac_nonzero,num_zero=table$num_zero, num_nonzero=table$num_nonzero,valtype=c("intERannual_frequency_fraction_of_years"))
	mags[[i]] <- table2
}
mags2 <- do.call(rbind.data.frame,mags)
write.csv(mags2,"C:/Users/tiffn_000/Google Drive/Rachel_p/simp_data_full_vol_90_intERannual_frequency.csv")

files_mag <- list.files(path="C:/Users/tiffn_000/Google Drive/Rachel_p/", pattern="postimp_")
mags <- vector("list", length(files_mag))
for(i in 1:length(mags)){
	table <- read.csv(paste("C:/Users/tiffn_000/Google Drive/Rachel_p/",files_mag[[i]],sep=""))
	tablename <- strsplit(files_mag[[i]],"[.]")[[1]][[1]]
	tablename <- strsplit(tablename,"_")
	if(tablename[[1]][[3]]=="nov"){
		period <- "November"
	} else if (tablename[[1]][[3]]=="dec"){
		period <- "December"
	} else if (tablename[[1]][[3]]=="jan"){
		period <- "January"
	} else if (tablename[[1]][[3]]=="feb"){
		period <- "February"
	} else if (tablename[[1]][[3]]=="mar"){
		period <- "March"
	} else if (tablename[[1]][[3]]=="apr"){
		period <- "April"
	} else if (tablename[[1]][[3]]=="mon3"){
		period <- "December to February"
	} else if (tablename[[1]][[3]]=="mon6"){
		period <- "November to April"
	} else if (tablename[[1]][[3]]=="hy"){
		period <- "Hydrologic Year"
	}
	table2 <- data.frame(gauge=table$gauge,yeartype=tablename[[1]][[2]],period=period,frac_zero=table$frac_zero,frac_nonzero=table$frac_nonzero,num_zero=table$num_zero, num_nonzero=table$num_nonzero,valtype=c("intERannual_frequency_fraction_of_years"))
	mags[[i]] <- table2
}
mags2 <- do.call(rbind.data.frame,mags)
write.csv(mags2,"C:/Users/tiffn_000/Google Drive/Rachel_p/simp_data_imp_vol_90_intERannual_frequency.csv")
















#setwd("C:/Users/tiffn_000/Google Drive/Manuscripts/redo_numbers/simp_data")
library(dplyr)
library(ggplot2)
inpath <- c("C:/Users/tiffn_000/Google Drive/Rachel_p/")
vol.frame <- read.csv(paste(inpath,"redo_simp_data_full_vol_90.csv", sep=""))
vol.frame[is.na(vol.frame)] <- 0 
dur.frame<- read.csv(paste(inpath,"simp_data_full_duration.csv", sep=""))
nmpks.frame<- read.csv(paste(inpath,"simp_data_full_intraannual_frequency.csv",sep=""))
imp.vol.frame<- read.csv(paste(inpath,"redo_simp_data_imp_vol_90.csv",sep=""))
imp.vol.frame[is.na(imp.vol.frame)] <- 0 
imp.dur.frame<- read.csv(paste(inpath,"simp_data_imp_duration.csv",sep=""))
imp.nmpks.frame<- read.csv(paste(inpath,"simp_data_imp_intraanual_frequency.csv",sep=""))
vol.frame <- vol.frame[,2:length(vol.frame)]
dur.frame<- dur.frame[,2:length(dur.frame)]
nmpks.frame<- nmpks.frame[,2:length(nmpks.frame)]
imp.vol.frame<- imp.vol.frame[,2:length(imp.vol.frame)]
imp.dur.frame<- imp.dur.frame[,2:length(imp.dur.frame)]
imp.nmpks.frame<- imp.nmpks.frame[,2:length(imp.nmpks.frame)]

# define gauges
imp.gauges = c(11447650, 11303500, 11186000)
unimp.gauges = c(11383500,11266500,11202001)
six.gauges = c(imp.gauges, unimp.gauges)
# bundle data
blanks = data.frame(gauge = six.gauges, yeartype = " ", period = NA, avg = NA, 
		sd = NA, valtype = NA)
full = do.call(rbind.data.frame, list(dur.frame, vol.frame,
				nmpks.frame)) %>% rbind.data.frame(blanks)
full["tag"] = "full" 
post = do.call(rbind.data.frame, list(imp.dur.frame, imp.vol.frame,
				imp.nmpks.frame)) %>% rbind.data.frame(blanks)
post["tag"] = "post-impairment"
alldat = rbind.data.frame(full, post) %>% filter(gauge %in% six.gauges)  
# format levels
alldat$yeartype <- factor(alldat$yeartype, levels = c("C", "D", "BN", "AN", 
				"W", " ", "all"))
levels(alldat$yeartype) = c("Critical", "Dry",
		"Below Normal","Above Normal","Wet"," ","All")
alldat$period <- factor(alldat$period, levels = c("November", "December", 
				"January", "February","March", "April", "December to February", 
				"November to April", "Hydrologic Year"))
# convert AF to cubic km
alldat[alldat$valtype %in% "vol AF", "avg"] = 1.23348e-6*
		alldat[alldat$valtype %in% "vol AF", "avg"]
alldat[alldat$valtype %in% "vol AF", "sd"] = 1.23348e-6*
		alldat[alldat$valtype %in% "vol AF", "sd"]  
levels(alldat$valtype)[levels(alldat$valtype) %in% "vol AF"] = "vol km3"
# add sd
alldat$ymin <- alldat$avg - alldat$sd
alldat$ymax <- alldat$avg + alldat$sd


my_barplot = function(d, yvar, monthly = TRUE, full = TRUE){
	if(yvar == "vol km3"){
		ylabel = expression(paste("High Magnitude Flow Volume ", (km^3)))
		tlabel = "Average Volume Above 90th Percentile\n"
		allcolor = "chartreuse4"    
	} else if(yvar == "duration_days"){
		ylabel = "Number of Days Above 90th Percentile\n"
		tlabel = "Average Number Of Days Above 90th Percentile"
		allcolor = "magenta"
	} else if(yvar == "intraannual_frequency_numpeaks"){
		ylabel = "Number of Days Above 90th Percentile\n"
		tlabel = "Average Number Of Peaks Above 90th Percentile"
		allcolor = "turquoise2"  
	} else {
		stop('value of argument "yvar" not recognized.')
	}
	if(monthly){
		p = c("November", "December", "January", "February", "March", "April")
		plabel = "Monthly (November to April) By Year Type"
	} else {
		p = c("December to February", "November to April", "Hydrologic Year")
		plabel = "3-Month Period, 6-Month Period, Hydrologic Year By Year Type"
	}
	if(full){
		rlabel = "Full Record of Available Data, Zero-Deflated"
	} else {
		rlabel = "Post-Impairment Record of Available Data, Zero-Deflated"
	}
	plottitle = eval(parse(text = paste0('ggtitle(expression(atop("', tlabel, 
							'", atop("', plabel, '", atop(italic("', rlabel, '"))))))')))
	subd = d[d$period %in% p & d$valtype %in% yvar,]
	ggplot(subd, aes(x = yeartype, fill = yeartype, y = avg)) + ylab(ylabel) +
			facet_grid(gauge ~ period, scales = "free_y") + xlab("\nYear Type") + 
			geom_bar(stat = "identity", color = "black") + #plottitle +
			geom_errorbar(aes(ymin = avg,ymax = ymax), width = 0.3)  +
			scale_x_discrete(labels = c("C", "D", "BN", "AN", "W"," ", "All"), 
					drop = FALSE) +
			scale_fill_manual(NULL, 
					values = c("Critical" = "lightcoral", "Dry" = "lemonchiffon", 
							"Below Normal" = "mediumaquamarine", "Above Normal" = "dodgerblue3", 
							"Wet" = "darkblue", " " = "white", "All" = allcolor), 
					labels = c("Critical\t", "Dry\t", "Below Normal\t", "Above Normal\t", 
							"Wet\t", "All\t")) + 
			guides(fill = guide_legend(reverse = FALSE, nrow = 1)) +
			theme(
					axis.text.x = element_text(color="black", size=10),
					axis.text.y = element_text(color="black", size=12),
					axis.title.x = element_text(color="black", size=14),
					axis.title.y = element_text(color="black", size=14),
#		title = element_text(color="black", size=rel(2)),
					legend.position = "bottom",
					legend.title = element_text(color="black", size=14),
					legend.text = element_text(color="black", size=12),
					strip.text = element_text(color="black", size=10),
					legend.key = element_rect(colour = 'black')
			)
}

outpath = "C:/Users/tiffn_000/Google Drive/Rachel_p/plots"

# full
imp.full = alldat %>% filter(gauge %in% imp.gauges, tag == "full")
lab.imp <- c("USGS 11447650\nSacramento River",
		"USGS 11303500\nSan Joaquin River",
		"USGS 11186000\n Kern River")
names(lab.imp) = paste(c(imp.gauges))
imp.full$gauge = factor(imp.full$gauge, levels = names(lab.imp))
levels(imp.full$gauge) = lab.imp

imp.full = filter(imp.full, gauge != "USGS 11186000\n Kern River")

ggsave(file.path(outpath, "imp_monthly_vol_full.png"), 
		my_barplot(imp.full, "vol km3", monthly = TRUE, full = TRUE), 
		width = 10, height = 6, units = "in")
 ggsave(file.path(outpath, "imp_bigperiod_vol_full.png"), 
 my_barplot(imp.full, "vol km3", monthly = FALSE, full = TRUE), 
 width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_monthly_dur_full.png"), 
# my_barplot(imp.full, "dur days", monthly = TRUE, full = TRUE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_bigperiod_dur_full.png"), 
# my_barplot(imp.full, "dur days", monthly = FALSE, full = TRUE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_monthly_nmpks_full.png"), 
# my_barplot(imp.full, "nmpks", monthly = TRUE, full = TRUE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_bigperiod_nmpks_full.png"), 
# my_barplot(imp.full, "nmpks", monthly = FALSE, full = TRUE), 
# width = 13.125, height = 9, units = "in")


# post-impairment
imp.post = alldat %>% filter(gauge %in% imp.gauges, tag == "post-impairment")
lab.imp <- c("USGS 11447650\nSacramento River\n1970 - Present",
		"USGS 11303500\nSan Joaquin River\n1989 - Present",
		"USGS 11186000\n Kern River\n1989 - Present")
names(lab.imp) = paste(c(imp.gauges))
imp.post$gauge = factor(imp.post$gauge, levels = names(lab.imp))
levels(imp.post$gauge) = lab.imp

imp.post = filter(imp.post, gauge != "USGS 11186000\n Kern River\n1989 - Present")

ggsave(file.path(outpath, "imp_monthly_vol_post.png"), 
		my_barplot(imp.post, "vol km3", monthly = TRUE, full = FALSE), 
		width = 10, height = 6, units = "in")
# ggsave(file.path(outpath, "imp_monthly_dur_post.png"), 
# my_barplot(imp.post, "dur days", monthly = TRUE, full = FALSE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_monthly_nmpks_post.png"), 
# my_barplot(imp.post, "nmpks", monthly = TRUE, full = FALSE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_bigperiod_dur_post.png"), 
# my_barplot(imp.post, "dur days", monthly = FALSE, full = FALSE), 
# width = 13.125, height = 9, units = "in")
 ggsave(file.path(outpath, "imp_bigperiod_vol_post.png"), 
 my_barplot(imp.post, "vol km3", monthly = FALSE, full = FALSE), 
 width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_bigperiod_nmpks_post.png"), 
# my_barplot(imp.post, "nmpks", monthly = FALSE, full = FALSE), 
# width = 13.125, height = 9, units = "in")



###ACREFEET###
library(dplyr)
library(ggplot2)
inpath <- c("C:/Users/tiffn_000/Google Drive/Rachel_p/")
vol.frame <- read.csv(paste(inpath,"redo_simp_data_full_vol_90.csv", sep=""))
vol.frame[is.na(vol.frame)] <- 0 
dur.frame<- read.csv(paste(inpath,"simp_data_full_vol_90_duration.csv", sep=""))
nmpks.frame<- read.csv(paste(inpath,"simp_data_full_vol_90_intraannual_frequency.csv",sep=""))
imp.vol.frame<- read.csv(paste(inpath,"redo_simp_data_imp_vol_90.csv",sep=""))
imp.vol.frame[is.na(imp.vol.frame)] <- 0 
imp.dur.frame<- read.csv(paste(inpath,"simp_data_imp_vol_90_duration.csv",sep=""))
imp.nmpks.frame<- read.csv(paste(inpath,"simp_data_imp_vol_90_intraannual_frequency.csv",sep=""))
vol.frame <- vol.frame[,2:length(vol.frame)]
dur.frame<- dur.frame[,2:length(dur.frame)]
nmpks.frame<- nmpks.frame[,2:length(nmpks.frame)]
imp.vol.frame<- imp.vol.frame[,2:length(imp.vol.frame)]
imp.dur.frame<- imp.dur.frame[,2:length(imp.dur.frame)]
imp.nmpks.frame<- imp.nmpks.frame[,2:length(imp.nmpks.frame)]

###change this to change which stations are plotted###
imp.gauges = c(11447650)
six.gauges = imp.gauges
# bundle data
blanks = data.frame(gauge = six.gauges, yeartype = " ", period = NA, avg = NA, 
		sd = NA, valtype = NA)
full = do.call(rbind.data.frame, list(dur.frame, vol.frame,
				nmpks.frame)) %>% rbind.data.frame(blanks)
full["tag"] = "full" 
post = do.call(rbind.data.frame, list(imp.dur.frame, imp.vol.frame,
				imp.nmpks.frame)) %>% rbind.data.frame(blanks)
post["tag"] = "post-impairment"
alldat = rbind.data.frame(full, post) %>% filter(gauge %in% six.gauges)  
# format levels
alldat$yeartype <- factor(alldat$yeartype, levels = c("C", "D", "BN", "AN", 
				"W", " ", "all"))
levels(alldat$yeartype) = c("Critical", "Dry",
		"Below Normal","Above Normal","Wet"," ","All")
alldat$period <- factor(alldat$period, levels = c("November", "December", 
				"January", "February","March", "April", "December to February", 
				"November to April", "Hydrologic Year"))
# convert AF to cubic km
alldat[alldat$valtype %in% "vol AF", "avg"] = 1e-6*
		alldat[alldat$valtype %in% "vol AF", "avg"]
alldat[alldat$valtype %in% "vol AF", "sd"] = 1e-6*
		alldat[alldat$valtype %in% "vol AF", "sd"]  
levels(alldat$valtype)[levels(alldat$valtype) %in% "vol AF"] = "vol MAF"
# add sd
alldat$ymin <- alldat$avg - alldat$sd
alldat$ymax <- alldat$avg + alldat$sd
##add station name
stationname <- read.csv(paste(inpath,"gauge_locations.csv",sep=""))
stationname <- data.frame(site=stationname$site_no, station_name=stationname$station_nm)
alldat <- merge(alldat,stationname,by.x="gauge",by.y="site")

library(grid)
my_barplot = function(d, yvar, monthly = TRUE, full = TRUE){
	if(yvar == "vol MAF"){
		ylabel = expression(paste("High Magnitude Flow Volume ", (MAF)))
		tlabel = "Average Magnitude (Volume) Above 90th Percentile\n"
		allcolor = "chartreuse4"    
	} else if(yvar == "duration_days"){
		ylabel = "Number of Days Above 90th Percentile\n"
		tlabel = "Average Number Of Days Above 90th Percentile"
		allcolor = "magenta"
	} else if(yvar == "intraannual_frequency_numpeaks"){
		ylabel = "Number of Days Above 90th Percentile\n"
		tlabel = "Average Number Of Peaks Above 90th Percentile"
		allcolor = "turquoise2"  
	} else {
		stop('value of argument "yvar" not recognized.')
	}
	if(monthly){
		p = c("November", "December", "January", "February", "March", "April")
		plabel = "Monthly (November to April) By Year Type"
	} else {
		p = c("December to February", "November to April", "Hydrologic Year")
		plabel = "3-Month Period, 6-Month Period, Hydrologic Year By Year Type"
	}
	if(full){
		rlabel = "Full Record of Available Data, Zero-Deflated"
	} else {
		rlabel = "Post-Impairment Record of Available Data, Zero-Deflated"
	}
#	if(yvar=="vol MAF" & full==TRUE){
		numgauges <- length(as.character(unique(d$gauge)))
		gauges2 <- rep(NA, length(numgauges))
		for(i in 1:length(gauges2)){
			if(" A " %in% as.character(unique(d$gauge))[[i]]){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," A ")[[1]][[1]]
			}else if(" BL " %in% as.character(unique(d$gauge))[[i]]){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," BL ")[[1]][[1]]
			}else if(" NR " %in% as.character(unique(d$gauge))[[i]]){
				gauges2[[i]] <-	strsplit(strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]," NR ")[[1]][[1]]
			}else{
				gauges2[[i]] <-	strsplit(as.character(unique(d$gauge))[[i]],"\n")[[1]][[2]]
			}
		}
		captext <- rep(NA, length(gauges2))
		for(i in 1:length(gauges2)){
			if(i == length(gauges2)){
				captext[[i]] <- paste("USGS ",as.character(unique(d$gauge))[[i]], " on the ", gauges2[[i]],".",sep="")
				
			}else {
				captext[[i]] <- paste("USGS ",as.character(unique(d$gauge))[[i]], " on the ", gauges2[[i]],", ",sep="")
			}
		}
		captext2 <- paste(captext, collapse=" ")
		cap = paste("Average magnitude (volume) of HMF over the full record of data by period and year type for ", numgauges, " sites in the Central Valley:",
				captext2,"\n Source: Kocis & Dahlke 2017", sep="")
#	}
	plottitle = eval(parse(text = paste0('ggtitle(expression(atop("', tlabel, 
							'", atop("', plabel, '", atop(italic("', rlabel, '"))))))')))
	subd = d[d$period %in% p & d$valtype %in% yvar,]
	ggplot(subd, aes(x = yeartype, fill = yeartype, y = avg))+ ylab(ylabel) +
			facet_grid(gauge ~ period, scales = "free_y") + xlab("\nYear Type") + 
			geom_bar(stat = "identity", color = "black") + #plottitle +
			geom_errorbar(aes(ymin = avg,ymax = ymax), width = 0.3)  +
			scale_x_discrete(labels = c("C", "D", "BN", "AN", "W"," ", "All"), 
					drop = FALSE) +
			scale_fill_manual(NULL, 
					values = c("Critical" = "lightcoral", "Dry" = "lemonchiffon", 
							"Below Normal" = "mediumaquamarine", "Above Normal" = "dodgerblue3", 
							"Wet" = "darkblue", " " = "white", "All" = allcolor), 
					labels = c("Critical\t", "Dry\t", "Below Normal\t", "Above Normal\t", 
							"Wet\t", "All\t")) + 
			guides(fill = guide_legend(reverse = FALSE, nrow = 1)) +
			theme(
					axis.text.x = element_text(color="black", size=10),
					axis.text.y = element_text(color="black", size=12),
					axis.title.x = element_text(color="black", size=14),
					axis.title.y = element_text(color="black", size=14),
#		title = element_text(color="black", size=rel(2)),
					legend.position = "bottom",
					legend.title = element_text(color="black", size=14),
					legend.text = element_text(color="black", size=12),
					strip.text = element_text(color="black", size=10),
					legend.key = element_rect(colour = 'black')
			)+
			labs(caption="cap")
}

outpath = "C:/Users/tiffn_000/Google Drive/Rachel_p/plots"

# full
imp.full = alldat %>% filter(gauge %in% imp.gauges, tag == "full")
lab.imp <- rep(NA, length(imp.gauges))
for(i in 1:length(imp.gauges)){
	lab.imp[[i]] <- paste("USGS ",imp.gauges[[i]],"\n",as.character(alldat$station_name[which(alldat$gauge==imp.gauges[[i]])[[1]]]),sep="")
}
names(lab.imp) = paste(c(imp.gauges))
imp.full$gauge = factor(imp.full$gauge, levels = names(lab.imp))
levels(imp.full$gauge) = lab.imp

imp.full = filter(imp.full, gauge != "USGS 11186000\n Kern River")

ggsave(file.path(outpath, "imp_monthly_vol_full_AF.png"), 
		my_barplot(imp.full, "vol MAF", monthly = TRUE, full = TRUE), 
		width = 10, height = 6, units = "in")
ggsave(file.path(outpath, "imp_bigperiod_vol_full_AF.png"), 
		my_barplot(imp.full, "vol MAF", monthly = FALSE, full = TRUE), 
		width = 13.125, height = 9, units = "in")
 ggsave(file.path(outpath, "imp_monthly_dur_full.png"), 
 my_barplot(imp.full, "duration_days", monthly = TRUE, full = TRUE), 
 width = 13.125, height = 9, units = "in")
 ggsave(file.path(outpath, "imp_bigperiod_dur_full.png"), 
 my_barplot(imp.full, "duration_days", monthly = FALSE, full = TRUE), 
 width = 13.125, height = 9, units = "in")
 ggsave(file.path(outpath, "imp_monthly_nmpks_full.png"), 
 my_barplot(imp.full, "intraannual_frequency_numpeaks", monthly = TRUE, full = TRUE), 
 width = 13.125, height = 9, units = "in")
 ggsave(file.path(outpath, "imp_bigperiod_nmpks_full.png"), 
 my_barplot(imp.full, "intraannual_frequency_numpeaks", monthly = FALSE, full = TRUE), 
 width = 13.125, height = 9, units = "in")


# post-impairment
imp.post = alldat %>% filter(gauge %in% imp.gauges, tag == "post-impairment")
lab.imp <- rep(NA, length(imp.gauges))
for(i in 1:length(imp.gauges)){
	lab.imp[[i]] <- paste("USGS ",imp.gauges[[i]],"\n",as.character(alldat$station_name[which(alldat$gauge==imp.gauges[[i]])[[1]]]),sep="")
}
names(lab.imp) = paste(c(imp.gauges))
imp.post$gauge = factor(imp.post$gauge, levels = names(lab.imp))
levels(imp.post$gauge) = lab.imp

imp.post = filter(imp.post, gauge != "USGS 11186000\n Kern River\n1989 - Present")

ggsave(file.path(outpath, "imp_monthly_vol_post_AF.png"), 
		my_barplot(imp.post, "vol MAF", monthly = TRUE, full = FALSE), 
		width = 10, height = 6, units = "in")
 ggsave(file.path(outpath, "imp_monthly_dur_post.png"), 
 my_barplot(imp.post, "duration_days", monthly = TRUE, full = FALSE), 
 width = 13.125, height = 9, units = "in")
 ggsave(file.path(outpath, "imp_monthly_nmpks_post.png"), 
 my_barplot(imp.post, "intraannual_frequency_numpeaks", monthly = TRUE, full = FALSE), 
 width = 13.125, height = 9, units = "in")
 ggsave(file.path(outpath, "imp_bigperiod_dur_post.png"), 
 my_barplot(imp.post, "duration_days", monthly = FALSE, full = FALSE), 
 width = 13.125, height = 9, units = "in")
ggsave(file.path(outpath, "imp_bigperiod_vol_post_AF.png"), 
		my_barplot(imp.post, "vol MAF", monthly = FALSE, full = FALSE), 
		width = 13.125, height = 9, units = "in")
 ggsave(file.path(outpath, "imp_bigperiod_nmpks_post.png"), 
 my_barplot(imp.post, "intraannual_frequency_numpeaks", monthly = FALSE, full = FALSE), 
 width = 13.125, height = 9, units = "in")








imp.full = alldat %>% filter(gauge %in% imp.gauges, tag == "full")
lab.imp <- c("USGS 11447650\nSacramento River",
		"USGS 11303500\nSan Joaquin River",
		"USGS 11186000\n Kern River")
names(lab.imp) = paste(c(imp.gauges))
imp.full$gauge = factor(imp.full$gauge, levels = names(lab.imp))
levels(imp.full$gauge) = lab.imp


ggsave(file.path(outpath, "imp_monthly_vol_full_wkern_AF.png"), 
		my_barplot(imp.full, "vol MAF", monthly = TRUE, full = TRUE), 
		width = 10, height = 6, units = "in")
ggsave(file.path(outpath, "imp_bigperiod_vol_full_wkern_AF.png"), 
		my_barplot(imp.full, "vol MAF", monthly = FALSE, full = TRUE), 
		width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_monthly_dur_full.png"), 
# my_barplot(imp.full, "dur days", monthly = TRUE, full = TRUE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_bigperiod_dur_full.png"), 
# my_barplot(imp.full, "dur days", monthly = FALSE, full = TRUE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_monthly_nmpks_full.png"), 
# my_barplot(imp.full, "nmpks", monthly = TRUE, full = TRUE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_bigperiod_nmpks_full.png"), 
# my_barplot(imp.full, "nmpks", monthly = FALSE, full = TRUE), 
# width = 13.125, height = 9, units = "in")


# post-impairment
imp.post = alldat %>% filter(gauge %in% imp.gauges, tag == "post-impairment")
lab.imp <- c("USGS 11447650\nSacramento River\n1970 - Present",
		"USGS 11303500\nSan Joaquin River\n1989 - Present",
		"USGS 11186000\n Kern River\n1989 - Present")
names(lab.imp) = paste(c(imp.gauges))
imp.post$gauge = factor(imp.post$gauge, levels = names(lab.imp))
levels(imp.post$gauge) = lab.imp


ggsave(file.path(outpath, "imp_monthly_vol_post_wkern_AF.png"), 
		my_barplot(imp.post, "vol MAF", monthly = TRUE, full = FALSE), 
		width = 10, height = 6, units = "in")
# ggsave(file.path(outpath, "imp_monthly_dur_post.png"), 
# my_barplot(imp.post, "dur days", monthly = TRUE, full = FALSE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_monthly_nmpks_post.png"), 
# my_barplot(imp.post, "nmpks", monthly = TRUE, full = FALSE), 
# width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_bigperiod_dur_post.png"), 
# my_barplot(imp.post, "dur days", monthly = FALSE, full = FALSE), 
# width = 13.125, height = 9, units = "in")
ggsave(file.path(outpath, "imp_bigperiod_vol_post_wkern_AF.png"), 
		my_barplot(imp.post, "vol MAF", monthly = FALSE, full = FALSE), 
		width = 13.125, height = 9, units = "in")
# ggsave(file.path(outpath, "imp_bigperiod_nmpks_post.png"), 
# my_barplot(imp.post, "nmpks", monthly = FALSE, full = FALSE), 
# width = 13.125, height = 9, units = "in")


###GIS STUFF##


full_all_mon6 <- read.csv("/Users/tiffnk/Google Drive/Manuscripts/redo_numbers/full_all_mon6.csv", stringsAsFactors=FALSE)
gaugeloc <- read.csv("/Users/tiffnk/Google Drive/Manuscripts/unshared/Thesis_tables/locations.txt", stringsAsFactors=FALSE)
gaugeloc <- gaugeloc[,c(3,5,6)]

unimp <- read.csv("/Users/tiffnk/Google Drive/Manuscripts/unshared/unimp_13.txt", stringsAsFactors=FALSE)

full_all_mon6_vol <- data.frame(meanvolTAF = full_all_mon6$mean_totvol_TAF, gauge=full_all_mon6$gauge)
full_all_mon6_vol <- merge(full_all_mon6_vol,gaugeloc, by.x="gauge",by.y="site_no", all.x=TRUE)
full_all_mon6_vol$meanvolm3 <- full_all_mon6_vol$meanvolTAF*1233481.84
full_all_mon6_vol$meanvolkm3 <- full_all_mon6_vol$meanvolTAF*0.00123348184
full_all_mon6_vol$class <- NA
for(i in 1:length(full_all_mon6_vol$meanvolm3)){
	if(full_all_mon6_vol$meanvolm3[[i]]>=3e9){
		full_all_mon6_vol$class[[i]] <- 10
	}
	else if(full_all_mon6_vol$meanvolm3[[i]]>=2e9){
		full_all_mon6_vol$class[[i]] <- 9
	}
	else if(full_all_mon6_vol$meanvolm3[[i]]>=1e9){
		full_all_mon6_vol$class[[i]] <- 8
	}
	else if(full_all_mon6_vol$meanvolm3[[i]]>=500e6){
		full_all_mon6_vol$class[[i]] <- 7
	}
	else if(full_all_mon6_vol$meanvolm3[[i]]>=250e6){
		full_all_mon6_vol$class[[i]] <- 6
	}
	else if(full_all_mon6_vol$meanvolm3[[i]]>=150e6){
		full_all_mon6_vol$class[[i]] <- 5
	}
	
	else if(full_all_mon6_vol$meanvolm3[[i]]>=50e6){
		full_all_mon6_vol$class[[i]] <- 4
	}
	else if(full_all_mon6_vol$meanvolm3[[i]]>=10e6){
		full_all_mon6_vol$class[[i]] <- 3
	}
	else if(full_all_mon6_vol$meanvolm3[[i]]>=1e6){
		full_all_mon6_vol$class[[i]] <- 2
	}
	else if(full_all_mon6_vol$meanvolm3[[i]]>=0){
		full_all_mon6_vol$class[[i]] <- 1
	}
}
full_all_mon6_vol$status <- NA
full_all_mon6_vol$status[which(full_all_mon6_vol$gauge%in%unimp$site_no)] <- "unimp"
full_all_mon6_vol$status[which(!full_all_mon6_vol$gauge%in%unimp$site_no)] <- "imp"
full_all_mon6_vol_unimp <- full_all_mon6_vol[which(full_all_mon6_vol$gauge%in%unimp$site_no),]
full_all_mon6_vol_imp <- full_all_mon6_vol[which(!full_all_mon6_vol$gauge%in%unimp$site_no),]

write.csv(full_all_mon6_vol, "/Users/tiffnk/Google Drive/Manuscripts/redo_numbers/full_all_mon6_vol.csv")
write.csv(full_all_mon6_vol_imp, "/Users/tiffnk/Google Drive/Manuscripts/redo_numbers/full_all_mon6_vol_imp.csv")
write.csv(full_all_mon6_vol_unimp, "/Users/tiffnk/Google Drive/Manuscripts/redo_numbers/full_all_mon6_vol_unimp.csv")



full_W_mon6 <- read.csv("/Users/tiffnk/Google Drive/Manuscripts/redo_numbers/full_W_mon6.csv", stringsAsFactors=FALSE)
gaugeloc <- read.csv("/Users/tiffnk/Google Drive/Manuscripts/unshared/Thesis_tables/locations.txt", stringsAsFactors=FALSE)
gaugeloc <- gaugeloc[,c(3,5,6)]

full_W_mon6_vol <- data.frame(meanvolTAF = full_W_mon6$mean_totvol_TAF, gauge=full_W_mon6$gauge)
full_W_mon6_vol <- merge(full_W_mon6_vol,gaugeloc, by.x="gauge",by.y="site_no", W.x=TRUE)
full_W_mon6_vol$meanvolm3 <- full_W_mon6_vol$meanvolTAF*1233481.84
full_W_mon6_vol$meanvolkm3 <- full_W_mon6_vol$meanvolTAF*0.00123348184
full_W_mon6_vol$class <- NA
for(i in 1:length(full_W_mon6_vol$meanvolm3)){
	if(full_W_mon6_vol$meanvolm3[[i]]>=3e9){
		full_W_mon6_vol$class[[i]] <- 10
	}
	else if(full_W_mon6_vol$meanvolm3[[i]]>=2e9){
		full_W_mon6_vol$class[[i]] <- 9
	}
	else if(full_W_mon6_vol$meanvolm3[[i]]>=1e9){
		full_W_mon6_vol$class[[i]] <- 8
	}
	else if(full_W_mon6_vol$meanvolm3[[i]]>=500e6){
		full_W_mon6_vol$class[[i]] <- 7
	}
	else if(full_W_mon6_vol$meanvolm3[[i]]>=250e6){
		full_W_mon6_vol$class[[i]] <- 6
	}
	else if(full_W_mon6_vol$meanvolm3[[i]]>=150e6){
		full_W_mon6_vol$class[[i]] <- 5
	}
	
	else if(full_W_mon6_vol$meanvolm3[[i]]>=50e6){
		full_W_mon6_vol$class[[i]] <- 4
	}
	else if(full_W_mon6_vol$meanvolm3[[i]]>=10e6){
		full_W_mon6_vol$class[[i]] <- 3
	}
	else if(full_W_mon6_vol$meanvolm3[[i]]>=1e6){
		full_W_mon6_vol$class[[i]] <- 2
	}
	else if(full_W_mon6_vol$meanvolm3[[i]]>=0){
		full_W_mon6_vol$class[[i]] <- 1
	}
}

full_W_mon6_vol$status <- NA
full_W_mon6_vol$status[which(full_W_mon6_vol$gauge%in%unimp$site_no)] <- "unimp"
full_W_mon6_vol$status[which(!full_W_mon6_vol$gauge%in%unimp$site_no)] <- "imp"
full_W_mon6_vol_unimp <- full_W_mon6_vol[which(full_W_mon6_vol$gauge%in%unimp$site_no),]
full_W_mon6_vol_imp <- full_W_mon6_vol[which(!full_W_mon6_vol$gauge%in%unimp$site_no),]

write.csv(full_W_mon6_vol, "/Users/tiffnk/Google Drive/Manuscripts/redo_numbers/full_W_mon6_vol.csv")
write.csv(full_W_mon6_vol_imp, "/Users/tiffnk/Google Drive/Manuscripts/redo_numbers/full_W_mon6_vol_imp.csv")
write.csv(full_W_mon6_vol_unimp, "/Users/tiffnk/Google Drive/Manuscripts/redo_numbers/full_W_mon6_vol_unimp.csv")

