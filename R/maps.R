# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


SacV_gauges <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/SacV_gauges.txt", header=TRUE)
SJV_gauges <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/SJV_gauges.txt", header=TRUE)
locations <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/locations.txt", header=TRUE)
unimp <-read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/unimp_13.txt", header=TRUE)
		
full_vol_all <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_vol_90_all.csv", header=TRUE)
full_vol_W <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_vol_90_W.csv", header=TRUE)
full_vol_AN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_vol_90_AN.csv", header=TRUE)
full_vol_BN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_vol_90_BN.csv", header=TRUE)
full_vol_D <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_vol_90_D.csv", header=TRUE)
full_vol_C <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_vol_90_C.csv", header=TRUE)

full_dur_all <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_dur_90_all.csv", header=TRUE)
full_dur_W <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_dur_90_W.csv", header=TRUE)
full_dur_AN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_dur_90_AN.csv", header=TRUE)
full_dur_BN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_dur_90_BN.csv", header=TRUE)
full_dur_D <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_dur_90_D.csv", header=TRUE)
full_dur_C <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_dur_90_C.csv", header=TRUE)

full_nmpks_all <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_nmpks_90_all.csv", header=TRUE)
full_nmpks_W <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_nmpks_90_W.csv", header=TRUE)
full_nmpks_AN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_nmpks_90_AN.csv", header=TRUE)
full_nmpks_BN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_nmpks_90_BN.csv", header=TRUE)
full_nmpks_D <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_nmpks_90_D.csv", header=TRUE)
full_nmpks_C <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_nmpks_90_C.csv", header=TRUE)

full_fracywf_all <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_90_all.csv", header=TRUE)
full_fracywf_W <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_90_W.csv", header=TRUE)
full_fracywf_AN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_90_AN.csv", header=TRUE)
full_fracywf_BN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_90_BN.csv", header=TRUE)
full_fracywf_D <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_90_D.csv", header=TRUE)
full_fracywf_C <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_90_C.csv", header=TRUE)

######

imp_vol_all <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_vol_90_all.csv", header=TRUE)
imp_vol_W <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_vol_90_W.csv", header=TRUE)
imp_vol_AN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_vol_90_AN.csv", header=TRUE)
imp_vol_BN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_vol_90_BN.csv", header=TRUE)
imp_vol_D <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_vol_90_D.csv", header=TRUE)
imp_vol_C <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_vol_90_C.csv", header=TRUE)

imp_dur_all <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_dur_90_all.csv", header=TRUE)
imp_dur_W <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_dur_90_W.csv", header=TRUE)
imp_dur_AN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_dur_90_AN.csv", header=TRUE)
imp_dur_BN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_dur_90_BN.csv", header=TRUE)
imp_dur_D <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_dur_90_D.csv", header=TRUE)
imp_dur_C <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_dur_90_C.csv", header=TRUE)

imp_nmpks_all <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_nmpks_90_all.csv", header=TRUE)
imp_nmpks_W <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_nmpks_90_W.csv", header=TRUE)
imp_nmpks_AN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_nmpks_90_AN.csv", header=TRUE)
imp_nmpks_BN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_nmpks_90_BN.csv", header=TRUE)
imp_nmpks_D <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_nmpks_90_D.csv", header=TRUE)
imp_nmpks_C <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mag_data_imp_nmpks_90_C.csv", header=TRUE)

imp_fracywf_all <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_imp_90_all.csv", header=TRUE)
imp_fracywf_W <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_imp_90_W.csv", header=TRUE)
imp_fracywf_AN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_imp_90_AN.csv", header=TRUE)
imp_fracywf_BN <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_imp_90_BN.csv", header=TRUE)
imp_fracywf_D <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_imp_90_D.csv", header=TRUE)
imp_fracywf_C <- read.csv("C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_tables/simp_mags_data_fracywf_imp_90_C.csv", header=TRUE)

allg <- unique(full_vol_all$gauge)
gauges <- data.frame(gauge=allg)
unimp <- data.frame(gauge=unimp$gauge,status="unimpaired")
gauges <- merge(gauges,unimp, by="gauge", all.x=TRUE)
gauges$status <- factor(gauges$status, levels=c("impaired", "unimpaired"))
gauges$status[is.na(gauges$status)] <- "impaired"


library(scales)
library(ggplot2)
library(ggmap)
library(Cairo)
library(rgdal)
library(dplyr)
library(maptools)
library(rgeos)
library(gpclib)
library(raster)

cv_huc <- readOGR(dsn = "C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_Maps", layer = "CV_huc")
cv_huc <- fortify(cv_huc)
cv_streams <- readOGR(dsn = "C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_Maps", layer = "CV_streams")
cv_streams <- fortify(cv_streams)
unimp_sites <- readOGR(dsn = "C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_Maps", layer = "unimpaired_final_thesis")
unimp_sites<- as.data.frame(unimp_sites)
cal_outline <- readOGR(dsn = "C:/Users/tiffn_000/Google Drive/Manuscripts/unshared/Thesis_Maps", layer = "cal_outline")

levels(cv_huc$group) <- c("Sacramento River Basin","San Joaquin River Basin", "Tulare Basin")
CP <- as(extent(-123.2, -117.9, 34.75, 41.5), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(cal_outline))

## Clip the map
cal_outline2_int <- gIntersection(cal_outline, CP, byid=TRUE)
cal_outline2 <- crop(cal_outline2_int,extent(extent(-123.199, -117.901, 34.7501, 41.4999)))
cal_outline2 <- fortify(cal_outline2_int, region="STATE_NAME")

leftover <- gDifference(CP, cal_outline2_int,byid=TRUE)
leftover <- gIntersection(leftover,CP)
leftover <- crop(leftover,extent(extent(-123.199, -117.901, 34.7501, 41.4999)))
leftover <- fortify(leftover)

cal2 <- get_map(location = c(-123.2,34.75,-117.9,41.5), source = "stamen",maptype=c("terrain-background"))
cal<- map_data("state")
cal <- subset(cal, region %in% c("california"))

vol_all_merge <- merge(full_vol_all,locations, by.x="gauge", by.y="gauge", all.x=TRUE)
vol_all_merge <- merge(vol_all_merge,gauges,by="gauge", all.x=TRUE)
test <- vol_all_merge
test$vol_hy_TAF <- cut(test$vol_hy_AF/1000, pretty(test$vol_hy_AF/1000,6, min.n=6))
breaks_hy_vol <- pretty(test$vol_hy_AF/1000,6, min.n=6)
maptest <- ggmap(cal2)+	
		geom_polygon(data=CP, aes(x=long, y=lat), alpha=0.6, fill="white")+
		geom_polygon(data=leftover, aes(x=long, y=lat), fill="white")+
		geom_polygon(data=cv_huc, aes(x=long, y=lat, group=group, fill=group), alpha=0.2)+
		geom_line(data=cv_streams, aes(x=long, y=lat, group=group), size=0.2,color="blue", alpha=0.2)+
#		geom_point(data=unimp_sites, aes(x=dec_long_v, y=dec_lat_va))+
		geom_point(data=test, aes(x=dec_long_v, y=dec_lat_va,size=vol_hy_TAF, color=vol_hy_TAF, shape=status))+
		scale_color_brewer(palette = "YlGnBu", direction=1,drop=FALSE) +
		scale_size_discrete(drop=FALSE)+
		geom_polygon(data=cal_outline2, aes(x=long, y=lat), alpha=0, color="black", size=0.2)+
		theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
				axis.text = element_blank(),
				axis.line = element_blank(),
				axis.ticks = element_blank(),
				axis.title = element_blank())

ggsave("C:/Users/tiffn_000/Desktop/maptest.png", maptest, width=5, height=6, units="in")
