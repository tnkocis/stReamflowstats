rm(list=ls())
library("ggplot2")
library("rethinking")
library("dplyr")
setwd("D:\\GB_Project\\Streamflow_Analysis\\Stream_Gauges_Raw_Data")

sacvalley <- as.list(read.csv("D:\\GB_Project\\Streamflow_Analysis\\Stream_Gauges_Raw_Data\\Sites_list_huc\\Sacramento_Valley.csv", header=TRUE))
sjvalley <- as.list(read.csv("D:\\GB_Project\\Streamflow_Analysis\\Stream_Gauges_Raw_Data\\Sites_list_huc\\SanJoaquin_Valley.csv", header=TRUE))

temp <- list.files(pattern="*.csv")

temp.re <- gsub(".csv","", temp)
#g<- rep("g", 23)
#names <- paste(g,temp.re, sep ="")

USGSgauges <- list()
for (i in 1:length(temp)) {
  USGSgauges[[i]] <- read.csv(temp[i])
  USGSgauges[[i]]["datetime"] <- as.Date(USGSgauges[[i]]$datetime, "%m/%d/%Y")
}

names(USGSgauges) <- temp.re

#expr = as.expression(parse(text=paste(names, "['datetime'] =
 #                                     as.Date(", names, "$datetime, '%m/%d/%Y')", sep="")))
#eval(expr)
preSFAnal <-  function(data5){
  
  for (i in 1:length(data5)){
    if (is.element("X01_00060_00003", names(data5[[i]]))== TRUE){data5[[i]]$discharge_cfs <- data5[[i]]$X01_00060_00003
    }else{}
    if(is.element("X02_00060_00003",names(data5[[i]]))== TRUE){data5[[i]]$discharge_cfs <- data5[[i]]$X02_00060_00003
    }else{}
  }
  return(data5)
}

SFAnalleap <- function(data, name){
  if (missing(data))
    stop("Data is required, dumbass.")
  minusleap <- list()
  for(i in 1:length(data)){
    data[[i]]$year <- as.numeric(format(data[[i]]$datetime,"%Y"))
    data[[i]]$month <- as.numeric(format(data[[i]]$datetime,"%m"))
    data[[i]]$day <- as.numeric(format(data[[i]]$datetime,"%d"))
    minusleap[[i]] <- filter(data[[i]], !(day==29 & month==2))
  }
  names(minusleap) <- name
  return(minusleap)
}

# SFAnal2 <- function(wint){
#     if (missing(wint))
#       stop("Winter list required, dumbass. You broke the function.")
#     years <-list()
#     for(i in 1:length(wint)){
#       years[[i]] <- seq(min(wint[[i]]$year), max(wint[[i]]$year), by=1)}
#     #works until here
#     wint_by_year <- list()
#     for (i in seq(1,length(years),1)){
#         wint_by_year[[i]] <- filter(wint[[i]], 
#                                     (for(n in seq(1,length(years[[i]]-1),1)){
#                                       ((month >9 & year == years[[n]])| (month <4 & year == years[[n]]+1))
#                                     })
#       }
#       names(wint_by_year)[[i]] <- years[[n]]+1
#       wint_by_year[[i]]["Discharge_ft3_day"] <- wint_by_year[[i]]$discharge_cfs*86400
#       wint_by_year[[i]]["Discharge_acft_day"] <- wint_by_year[[i]]$Discharge_ft3_day*(2.29568411e-5)
#       wint_by_year[[i]]["Discharge_acfte6_day"] <- wint_by_year[[i]]$Discharge_acft_day*1e-6
#     }
#     return(wint_by_year)
# }

SFAnalwint <- function(wint){
  if (missing(wint))
    stop("Winter list required, dumbass. You broke the function.")
  
  years <- seq(min(wint$year), max(wint$year), by=1)
  wint_by_year <- list()
  for (i in seq(1,length(years)-1,1)){
    wint_by_year[[i]] <- filter(wint, (month >9 & year == years[i])| (month <4 & year == years[i]+1))
    names(wint_by_year)[i] <- years[i]+1
    wint_by_year[[i]]["Discharge_ft3_day"] <- as.numeric(wint_by_year[[i]]$discharge_cfs)*86400
    wint_by_year[[i]]["Discharge_acft_day"] <- wint_by_year[[i]]$Discharge_ft3_day*(2.29568411e-5)
    wint_by_year[[i]]["Discharge_acfte6_day"] <- wint_by_year[[i]]$Discharge_acft_day*1e-6
  }
  return(wint_by_year)
}

SFAnalsummer <- function(summer){
  if (missing(summer))
    stop("Winter list required, dumbass. You broke the function.")
  
  years <- seq(min(summer$year), max(summer$year), by=1)
  summer_by_year <- list()
  for (i in seq(1,length(years)-1,1)){
    summer_by_year[[i]] <- filter(summer, (month >3 & year == years[i]+1)& (month <8 & year == years[i]+1))
    names(summer_by_year)[i] <- years[i]+1
    summer_by_year[[i]]["Discharge_ft3_day"] <- as.numeric(summer_by_year[[i]]$discharge_cfs)*86400
    summer_by_year[[i]]["Discharge_acft_day"] <- summer_by_year[[i]]$Discharge_ft3_day*(2.29568411e-5)
    summer_by_year[[i]]["Discharge_acfte6_day"] <- summer_by_year[[i]]$Discharge_acft_day*1e-6
  }
  return(summer_by_year)
}

SFAnalstats <- function(data){
  if (missing(data))
    stop("Data Required, dumbass. You broke the function.")
  total_Q_ft3 <- NULL
  mean_Q_ft3_day <- NULL
  median_Q_ft3_day<- NULL
  total_Q_acft <- NULL
  mean_Q_acft_day <- NULL
  median_Q_acft_day<- NULL
  total_Q_acfte6 <- NULL
  mean_Q_acfte6_day <- NULL
  median_Q_acfte6_day<- NULL
  for (i in 1:length(data)){ 
    total_Q_ft3 <- c(total_Q_ft3, sum(data[[i]][,"Discharge_ft3_day"], na.rm=TRUE))
    mean_Q_ft3_day <- c(mean_Q_ft3_day, mean(data[[i]][,"Discharge_ft3_day"], na.rm=TRUE))
    median_Q_ft3_day<- c(median_Q_ft3_day, median(data[[i]][,"Discharge_ft3_day"], na.rm=TRUE))
    total_Q_acft <- c(total_Q_acft, sum(data[[i]][,"Discharge_acft_day"], na.rm=TRUE))
    mean_Q_acft_day <- c(mean_Q_acft_day, mean(data[[i]][,"Discharge_acft_day"], na.rm=TRUE))
    median_Q_acft_day<- c(median_Q_acft_day, median(data[[i]][,"Discharge_acft_day"], na.rm=TRUE))
    total_Q_acfte6 <- c(total_Q_acfte6, sum(data[[i]][,"Discharge_acfte6_day"], na.rm=TRUE))
    mean_Q_acfte6_day <- c(mean_Q_acfte6_day, mean(data[[i]][,"Discharge_acfte6_day"], na.rm=TRUE))
    median_Q_acfte6_day<- c(median_Q_acfte6_day, median(data[[i]][,"Discharge_acfte6_day"], na.rm=TRUE))
  }
  yearstats <- list(year= names(data),
                    total_Q_ft3=total_Q_ft3, 
                    mean_Q_ft3_day = mean_Q_ft3_day,
                    median_Q_ft3_day = median_Q_ft3_day,
                    total_Q_acft = total_Q_acft,
                    mean_Q_acft_day = mean_Q_acft_day,
                    median_Q_acft_day = median_Q_acft_day,
                    total_Q_acfte6 = total_Q_acfte6,
                    mean_Q_acfte6_day = mean_Q_acfte6_day,
                    median_Q_acfte6_day = median_Q_acfte6_day)
  
  return(yearstats)
}

#processing begins here
USGSgauges2 <- preSFAnal(USGSgauges)
USGSgauges3 <- SFAnalleap(USGSgauges2, temp.re)
USGSgauges4 <- list()
for( i in seq(1, length(USGSgauges3),1)) {
  USGSgauges4[[i]] <- filter(USGSgauges3[[i]], ((max(year)-min(year))>2))
}

for (i in seq(length(USGSgauges4),1,-1)){
  if(nrow(USGSgauges4[[i]])<1)
    USGSgauges4[[i]]<- NULL
}

USGSgauges_wint <- list()
for (n in 1:length(USGSgauges4)){
  USGSgauges_wint[[n]] <- SFAnalwint(USGSgauges4[[n]])
}

USGSgauges_summer <- list()
for (n in 1:length(USGSgauges4)){
  USGSgauges_summer[[n]] <- SFAnalsummer(USGSgauges4[[n]])
}

names_gauges <- list()
for (i in 1:length(USGSgauges4)){
  names_gauges[i] <- as.character(USGSgauges4[[i]]$site_no[1])
}
  
names(USGSgauges_wint) <- names_gauges
names(USGSgauges_summer) <- names_gauges

yearsstats_wint <- list()
for (n in 1:length(USGSgauges_wint)){
  yearsstats_wint[[n]] <- SFAnalstats(USGSgauges_wint[[n]])
}
names(yearsstats_wint) <- names_gauges

yearsstats_summer <- list()
for (n in 1:length(USGSgauges_summer)){
  yearsstats_summer[[n]] <- SFAnalstats(USGSgauges_summer[[n]])
}
names(yearsstats_summer) <- names_gauges

sacvalley$site_no <- as.character(sacvalley$site_no)
sjvalley$site_no <- as.character(sjvalley$site_no)

USGSgauges_wint_sacvalley <- USGSgauges_wint[which(names(USGSgauges_wint) %in% sacvalley$site_no)]
USGSgauges_wint_sjvalley <- USGSgauges_wint[which(names(USGSgauges_wint) %in% sjvalley$site_no)]
USGSgauges_summer_sacvalley <- USGSgauges_summer[which(names(USGSgauges_summer) %in% sacvalley$site_no)]
USGSgauges_summer_sjvalley <- USGSgauges_summer[which(names(USGSgauges_summer) %in% sjvalley$site_no)]

yearsstats_wint_sacvalley <- yearsstats_wint[which(names(yearsstats_wint) %in% sacvalley$site_no)]
yearsstats_wint_sjvalley <- yearsstats_wint[which(names(yearsstats_wint) %in% sjvalley$site_no)]
yearsstats_summer_sacvalley <- yearsstats_summer[which(names(yearsstats_summer) %in% sacvalley$site_no)]
yearsstats_summer_sjvalley <- yearsstats_summer[which(names(yearsstats_summer) %in% sjvalley$site_no)]


#SVI
yearsstats_totalavgs <- function(summer, winter){
  if(missing(summer))
    return("Missing input summer data, you idiot.")
  if(missing(winter))
    return("Missing input winter data, you idiot.")
  
  wint_tot_q_ft3 <- sum(winter$total_Q_ft3, na.rm=TRUE)
  summ_tot_q_ft3 <- sum(summer$total_Q_ft3, na.rm=TRUE)
  years <- length(winter$year)-
    length(which(summer$total_Q_ft3==0))-
    length(which(is.na(summer$total_Q_ft3)))
  avg_year_q_ft3 <- ((wint_tot_q_ft3)+(summ_tot_q_ft3))/years
  avg_year_q_acft <- avg_year_q_ft3*0.0000229568409049
  avg_year_q_acfte6 <- avg_year_q_acft/1e6
  totals <- list(no_of_years=years,
                 avg_year_Q_ft3=avg_year_q_ft3,
                 avg_year_Q_acft=avg_year_q_acft,
                 avg_year_Q_acfte6=avg_year_q_acfte6)
  return(totals)
}

overall_stats_sacvalley <- list()
for(i in 1:length(yearsstats_summer_sacvalley)){
  overall_stats_sacvalley[[i]] <- yearsstats_totalavgs(yearsstats_summer_sacvalley[[i]], 
                                                       yearsstats_wint_sacvalley[[i]])
}
names(overall_stats_sacvalley) <- names(yearsstats_summer_sacvalley)

overall_stats_sjvalley <- list()
for(i in 1:length(yearsstats_summer_sjvalley)){
  overall_stats_sjvalley[[i]] <- yearsstats_totalavgs(yearsstats_summer_sjvalley[[i]], 
                                                       yearsstats_wint_sjvalley[[i]])
}
names(overall_stats_sjvalley) <- names(yearsstats_summer_sjvalley)
  

SVI.avgstart <- function(sacvalley){
  if(missing(sacvalley))
    return("Missing input data, you idiot.")
  SVI <- NULL
  for (i in 1:length(data)){ 
    SVI <- c(SVI, sum(data[[i]][,"Discharge_ft3_day"]))
  }
  yearstats <- list(year= names(data),
                    total_Q_ft3=total_Q_ft3, 
                    mean_Q_ft3_day = mean_Q_ft3_day,
                    median_Q_ft3_day = median_Q_ft3_day,
                    total_Q_acft = total_Q_acft,
                    mean_Q_acft_day = mean_Q_acft_day,
                    median_Q_acft_day = median_Q_acft_day,
                    total_Q_acfte6 = total_Q_acfte6,
                    mean_Q_acfte6_day = mean_Q_acfte6_day,
                    median_Q_acfte6_day = median_Q_acfte6_day)
  
  return(yearstats)
  return(SVI)
}
}

#processing ends here

# SFAnal <- function(data){
#   if (missing(data))
#     stop("Data is required, dumbass.")
# 
#   for(i in 1:length(data)){
#     data[[i]]$year <- as.numeric(format(data[[i]]$datetime,"%Y"))
#     data[[i]]$month <- as.numeric(format(data[[i]]$datetime,"%m"))
#     data[[i]]$day <- as.numeric(format(data[[i]]$datetime,"%d"))
#     winter[[i]] <- filter(data[[i]], !day==29 & month==2))
#   }
# #   data$year <- as.numeric(format(data$datetime,"%Y"))
# #   data$month <- as.numeric(format(data$datetime, "%m"))
# #   data$day <- as.numeric(format(data$datetime,"%d"))
# #   winter <- filter(data, !(day==29 & month==2))
#   
#   winteryears <- function(wint){
#     if (missing(wint))
#       stop("Winter list required, dumbass. You broke the function.")
#     
#     years <- seq(min(wint$year), max(wint$year), by=1)
#     wint_by_year <- list()
#     for (i in seq(1,length(years)-1,by=1)){
#         wint_by_year[[i]] <- filter(wint, (month >9 & year == years[i])| (month <4 & year == years[i]+1))
#         names(wint_by_year)[i] <- years[i]+1
#         wint_by_year[[i]]["Discharge_ft3_day"] <- wint_by_year[[i]]$discharge_cfs*86400
#         wint_by_year[[i]]["Discharge_acft_day"] <- wint_by_year[[i]]$Discharge_ft3_day*(2.29568411e-5)
#         wint_by_year[[i]]["Discharge_acfte6_day"] <- wint_by_year[[i]]$Discharge_acft_day*1e-6
#     }
#     return(wint_by_year)
#       }
#   data3 <- winteryears(winter)
#   return(data3)
# }




for (i in 1:length(USGSgauges))
  USGSgauges2 <- lapply(USGSgauges,SFAnal)

gAMRiv_Fair_Oaks$month <- months(gAMRiv_Fair_Oaks$datetime)

gAMRiv_Fair_Oaks$year <- as.numeric(format(gAMRiv_Fair_Oaks$datetime, "%Y"))
gAMRiv_Fair_Oaks$month <- as.numeric(format(gAMRiv_Fair_Oaks$datetime, "%m"))
gAMRiv_Fair_Oaks$day <- as.numeric(format(gAMRiv_Fair_Oaks$datetime, "%d"))
gAMRiv_Fair_Oaks <- filter(gAMRiv_Fair_Oaks, !(day==29 & month==2))
gAMRiv_Fair_Oaks_Wint <- filter(gAMRiv_Fair_Oaks, month %in% c(1,2,3,10,11,12))

years <- seq(min(gAMRiv_Fair_Oaks_Wint$year), max(gAMRiv_Fair_Oaks_Wint$year), by=1)
names2 <- rep("gAMRiv_Fair_Oaks_Wint",length.out=length(years))
years.name <- paste(names2,years,sep="")
for (i in seq(1,length(years),by=1))
  assign(years.name[i+1],filter(gAMRiv_Fair_Oaks_Wint, (month >9 & year == years[i])| (month <4 & year == years[i+1])))


for (i in seq_along(years.name))
    assign(years.name[i+1],`[[<-` (get(years.name[i+1]),"disch_vol_ft3_day", 
       value=((get(years.name[i+1])$X02_00060_00003)*86400)))
for (i in seq_along(years.name))
    assign(years.name[i+1],`[[<-` (get(years.name[i+1]),"disch_vol_acft_day", 
       value=((get(years.name[i+1])$X02_00060_00003)*86400*(2.29568411e-5))))

for (i in seq_along(years.name))
  assign(years.name[i+1],`[[<-` (get(years.name[i+1]),"disch_vol_acft_day_1e6", 
       value=((get(years.name[i+1])$X02_00060_00003)*86400*(2.29568411e-5)*1e-6)))

#make list of hydro year Q
wint_Q_ft3 <- list()
for (i in years.name[2:length(years.name)])
  wint_Q_ft3[[i]] <- sum(get(i)$disch_vol_ft3_day)

wint_Q_acft <-list()
for (i in years.name[2:length(years.name)])
  wint_Q_acft[[i]] <- sum(get(i)$disch_vol_acft_day)

wint_Q_acft_1e6 <-list()
for (i in years.name[2:length(years.name)])
  wint_Q_acft_1e6[[i]] <- sum(get(i)$disch_vol_acft_day_1e6)

wint_meanQ_acft_1e6 <-list()
for (i in years.name[2:length(years.name)])
  wint_meanQ_acft_1e6[[i]] <- mean(get(i)$disch_vol_acft_day_1e6)

wint_medianQ_acft_1e6 <-list()
for (i in years.name[2:length(years.name)])
  wint_medianQ_acft_1e6[[i]] <- median(get(i)$disch_vol_acft_day_1e6)

gAMRiv_Fair_Oaks_wint_Q_acft_1e6 <- do.call(rbind,wint_Q_acft_1e6)
hist(gAMRiv_Fair_Oaks_wint_Q_acft_1e6, breaks= 20, 
     xlab = "American River at Fair Oaks Winter Discharge (Acre-ft *1e6)") 
mtext("American River at Fair Oaks 1905-Present")


####################################
TotalQyearly <- list(years.name[i+1],sum(years.name[i+1]$X02_00060_00003))

q <- filter(gAMRiv_Fair_Oaks_Wint, (month >9 & year == (years[1]))| (month <  4 & year == (years[2])))

gAMRiv_Fair_Oaks_Wint1905 <- filter(gAMRiv_Fair_Oaks_Wint, (month > 9 & year == 1904) | (month < 4 & year == 1905))



TQ1904_5 <- sum(gAMRiv_Fair_Oaks_wint$X02_00060_00003)


# g11342000$dohy <- seq(1,131, by=1)
# dohy.seq <- seq(1,131,by=1)
# m <- map(
#   alist(
#     X02_00060_00003 ~ dnorm(mu, sigma),
#     mu <- a + b1*dohy +b2*dohy^2 +b3*dohy^3 + b4*dohy^4 + b5*dohy^5 +b6*dohy^6,
#     a ~ dnorm(0,100),
#     b1 ~ dnorm(0,10),
#     b2 ~ dnorm(0,10),
#     b3 ~ dnorm(0,10),
#     b4 ~ dnorm(0,10),
#     b5 ~ dnorm(0,10),
#     b6 ~ dnorm(0,10),
#     sigma ~ dunif(0,50)), data = g11342000)
# 
# mu <- link(m, data=list(dohy=dohy.seq))

gAMRiv_Fair_Oaks$datenum <- as.numeric(gAMRiv_Fair_Oaks$datetime)

# s <- as.data.frame(approx(gAMRiv_Fair_Oaks$datenum, gAMRiv_Fair_Oaks$X02_00060_00003, n=1e7))
# s$X02_00060_00003 <- s$y
# s$datetime <- as.Date(s$x,origin = "1970-01-01")

# gAMRiv_Fair_Oaks_wint$datenum <- as.numeric(gAMRiv_Fair_Oaks_wint$datetime)
# 
# d <- as.data.frame(approx(gAMRiv_Fair_Oaks_wint$datenum, gAMRiv_Fair_Oaks_wint$X02_00060_00003, n=1e4))
# d$X02_00060_00003 <- d$y
# d$datetime <- as.Date(d$x,origin = "1970-01-01")
# 
# nov2014 <- as.Date("11/01/2014", "%m/%d/%Y")
# jan2014 <- as.Date("1/1/2015", "%m/%d/%Y")
# 
# 
# flow800 <- rep(800, length(gAMRiv_Fair_Oaks_wint$datenum))
# flow800L <- as.data.frame(approx(gAMRiv_Fair_Oaks_wint$datenum, flow800, n=1e4))
# flow800L$datetime <- as.Date(flow800L$x,origin = "1970-01-01")
# flow800L$X02_00060_00003 <- flow800L$y
# 
# shade <- data.frame(x=flow800L$datetime, ymin=flow800L$X02_00060_00003,
#                     ymax=d$X02_00060_00003)
# 
# ggplot(shade) +
#   geom_point(aes(x, ymax, color=ymax)) +
#   geom_line(aes(x, ymax, color=ymax)) + 
#   scale_color_gradient(low="dodgerblue4", high="firebrick3")+
#   labs(title= "USGS Station American River at Fair Oaks")+
#   labs(x="Date")+
#   labs(y="Discharge (cfs)")+
#   labs(color="Discharge (cfs)")+
#   ylim(750,950)+
#   geom_line(aes(x,ymin,color=ymax))+
#   geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), 
#                fill="lightskyblue1", alpha=.4)
# 
# day <- seq(from=1, to=73, by=1)
# flow800Ls <- approxfun(gAMRiv_Fair_Oaks_wint$datenum, flow800)
# ds <- approxfun(day, gAMRiv_Fair_Oaks_wint$X02_00060_00003)
# 
# flint <- integrate(ds, min(day), max(day))
# cubft <- (flint$value - (800*73))*86400
# acft <- cubft*(2.29568e-5)
# 
# wint1905 <- list(seq(as.Date("1904/1/1"), as.Date("1905/3/31"), "days"))
