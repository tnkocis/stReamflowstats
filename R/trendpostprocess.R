# TODO: Add comment
# 
# Author: tiffn_000
###############################################################################


trend3monfilesdams <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\mon3\\dams")

trend3mondams <- vector("list", length(trend3monfilesdams))

for(i in 1:length(trend3monfilesdams)){
	trend3mondams[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\mon3\\dams\\",trend3monfilesdams[[i]],sep=""), header=TRUE, sep=",")

}

trend3mondamsdf <- do.call(rbind.data.frame,trend3mondams)

trend3mondams_w1_mnpks <- trend3mondamsdf[which(trend3mondamsdf$window==1&trend3mondamsdf$measure=="meanpeaksabv"),]
trend3mondams_w1_numpks <- trend3mondamsdf[which(trend3mondamsdf$window==1&trend3mondamsdf$measure=="numpeaksabv"),]
trend3mondams_w1_totdays <- trend3mondamsdf[which(trend3mondamsdf$window==1&trend3mondamsdf$measure=="totdaysabv"),]
trend3mondams_w1_totpkflw <- trend3mondamsdf[which(trend3mondamsdf$window==1&trend3mondamsdf$measure=="totpeakflwabv"),]
trend3mondams_w1_totvol <- trend3mondamsdf[which(trend3mondamsdf$window==1&trend3mondamsdf$measure=="totvolabv"),]

trend3mondams_w5_mnpks <- trend3mondamsdf[which(trend3mondamsdf$window==5&trend3mondamsdf$measure=="meanpeaksabv"),]
trend3mondams_w5_numpks <- trend3mondamsdf[which(trend3mondamsdf$window==5&trend3mondamsdf$measure=="numpeaksabv"),]
trend3mondams_w5_totdays <- trend3mondamsdf[which(trend3mondamsdf$window==5&trend3mondamsdf$measure=="totdaysabv"),]
trend3mondams_w5_totpkflw <- trend3mondamsdf[which(trend3mondamsdf$window==5&trend3mondamsdf$measure=="totpeakflwabv"),]
trend3mondams_w5_totvol <- trend3mondamsdf[which(trend3mondamsdf$window==5&trend3mondamsdf$measure=="totvolabv"),]

trend3mondams_w10_mnpks <- trend3mondamsdf[which(trend3mondamsdf$window==10&trend3mondamsdf$measure=="meanpeaksabv"),]
trend3mondams_w10_numpks <- trend3mondamsdf[which(trend3mondamsdf$window==10&trend3mondamsdf$measure=="numpeaksabv"),]
trend3mondams_w10_totdays <- trend3mondamsdf[which(trend3mondamsdf$window==10&trend3mondamsdf$measure=="totdaysabv"),]
trend3mondams_w10_totpkflw <- trend3mondamsdf[which(trend3mondamsdf$window==10&trend3mondamsdf$measure=="totpeakflwabv"),]
trend3mondams_w10_totvol <- trend3mondamsdf[which(trend3mondamsdf$window==10&trend3mondamsdf$measure=="totvolabv"),]

write.csv(trend3mondams_w1_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w1_mnpks.csv")
write.csv(trend3mondams_w1_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w1_numpks.csv")
write.csv(trend3mondams_w1_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w1_totdays.csv")
write.csv(trend3mondams_w1_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w1_totpkflw.csv")
write.csv(trend3mondams_w1_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w1_totvol.csv")

write.csv(trend3mondams_w5_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_mnpks.csv")
write.csv(trend3mondams_w5_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_numpks.csv")
write.csv(trend3mondams_w5_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_totdays.csv")
write.csv(trend3mondams_w5_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_totpkflw.csv")
write.csv(trend3mondams_w5_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w5_totvol.csv")

write.csv(trend3mondams_w10_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w10_mnpks.csv")
write.csv(trend3mondams_w10_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w10_numpks.csv")
write.csv(trend3mondams_w10_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w10_totdays.csv")
write.csv(trend3mondams_w10_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w10_totpkflw.csv")
write.csv(trend3mondams_w10_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\dams\\trend3mondams_w10_totvol.csv")

#################

trend3monfilesfull <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\mon3\\full")

trend3monfull <- vector("list", length(trend3monfilesfull))

for(i in 1:length(trend3monfilesfull)){
	trend3monfull[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\mon3\\full\\",trend3monfilesfull[[i]],sep=""), header=TRUE, sep=",")
	
}

trend3monfulldf <- do.call(rbind.data.frame,trend3monfull)

trend3monfull_w1_mnpks <- trend3monfulldf[which(trend3monfulldf$window==1&trend3monfulldf$measure=="meanpeaksabv"),]
trend3monfull_w1_numpks <- trend3monfulldf[which(trend3monfulldf$window==1&trend3monfulldf$measure=="numpeaksabv"),]
trend3monfull_w1_totdays <- trend3monfulldf[which(trend3monfulldf$window==1&trend3monfulldf$measure=="totdaysabv"),]
trend3monfull_w1_totpkflw <- trend3monfulldf[which(trend3monfulldf$window==1&trend3monfulldf$measure=="totpeakflwabv"),]
trend3monfull_w1_totvol <- trend3monfulldf[which(trend3monfulldf$window==1&trend3monfulldf$measure=="totvolabv"),]

trend3monfull_w5_mnpks <- trend3monfulldf[which(trend3monfulldf$window==5&trend3monfulldf$measure=="meanpeaksabv"),]
trend3monfull_w5_numpks <- trend3monfulldf[which(trend3monfulldf$window==5&trend3monfulldf$measure=="numpeaksabv"),]
trend3monfull_w5_totdays <- trend3monfulldf[which(trend3monfulldf$window==5&trend3monfulldf$measure=="totdaysabv"),]
trend3monfull_w5_totpkflw <- trend3monfulldf[which(trend3monfulldf$window==5&trend3monfulldf$measure=="totpeakflwabv"),]
trend3monfull_w5_totvol <- trend3monfulldf[which(trend3monfulldf$window==5&trend3monfulldf$measure=="totvolabv"),]

trend3monfull_w10_mnpks <- trend3monfulldf[which(trend3monfulldf$window==10&trend3monfulldf$measure=="meanpeaksabv"),]
trend3monfull_w10_numpks <- trend3monfulldf[which(trend3monfulldf$window==10&trend3monfulldf$measure=="numpeaksabv"),]
trend3monfull_w10_totdays <- trend3monfulldf[which(trend3monfulldf$window==10&trend3monfulldf$measure=="totdaysabv"),]
trend3monfull_w10_totpkflw <- trend3monfulldf[which(trend3monfulldf$window==10&trend3monfulldf$measure=="totpeakflwabv"),]
trend3monfull_w10_totvol <- trend3monfulldf[which(trend3monfulldf$window==10&trend3monfulldf$measure=="totvolabv"),]

write.csv(trend3monfull_w1_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w1_mnpks.csv")
write.csv(trend3monfull_w1_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w1_numpks.csv")
write.csv(trend3monfull_w1_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w1_totdays.csv")
write.csv(trend3monfull_w1_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w1_totpkflw.csv")
write.csv(trend3monfull_w1_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w1_totvol.csv")

write.csv(trend3monfull_w5_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w5_mnpks.csv")
write.csv(trend3monfull_w5_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w5_numpks.csv")
write.csv(trend3monfull_w5_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w5_totdays.csv")
write.csv(trend3monfull_w5_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w5_totpkflw.csv")
write.csv(trend3monfull_w5_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w5_totvol.csv")

write.csv(trend3monfull_w10_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w10_mnpks.csv")
write.csv(trend3monfull_w10_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w10_numpks.csv")
write.csv(trend3monfull_w10_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w10_totdays.csv")
write.csv(trend3monfull_w10_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w10_totpkflw.csv")
write.csv(trend3monfull_w10_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\3mon\\full\\trend3monfull_w10_totvol.csv")

#################

trendhyfilesfull <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\hy\\full")

trendhyfull <- vector("list", length(trendhyfilesfull))

for(i in 1:length(trendhyfilesfull)){
	trendhyfull[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\hy\\full\\",trendhyfilesfull[[i]],sep=""), header=TRUE, sep=",")
	
}

trendhyfulldf <- do.call(rbind.data.frame,trendhyfull)

trendhyfull_w1_mnpks <- trendhyfulldf[which(trendhyfulldf$window==1&trendhyfulldf$measure=="meanpeaksabv"),]
trendhyfull_w1_numpks <- trendhyfulldf[which(trendhyfulldf$window==1&trendhyfulldf$measure=="numpeaksabv"),]
trendhyfull_w1_totdays <- trendhyfulldf[which(trendhyfulldf$window==1&trendhyfulldf$measure=="totdaysabv"),]
trendhyfull_w1_totpkflw <- trendhyfulldf[which(trendhyfulldf$window==1&trendhyfulldf$measure=="totpeakflwabv"),]
trendhyfull_w1_totvol <- trendhyfulldf[which(trendhyfulldf$window==1&trendhyfulldf$measure=="totvolabv"),]

trendhyfull_w5_mnpks <- trendhyfulldf[which(trendhyfulldf$window==5&trendhyfulldf$measure=="meanpeaksabv"),]
trendhyfull_w5_numpks <- trendhyfulldf[which(trendhyfulldf$window==5&trendhyfulldf$measure=="numpeaksabv"),]
trendhyfull_w5_totdays <- trendhyfulldf[which(trendhyfulldf$window==5&trendhyfulldf$measure=="totdaysabv"),]
trendhyfull_w5_totpkflw <- trendhyfulldf[which(trendhyfulldf$window==5&trendhyfulldf$measure=="totpeakflwabv"),]
trendhyfull_w5_totvol <- trendhyfulldf[which(trendhyfulldf$window==5&trendhyfulldf$measure=="totvolabv"),]

trendhyfull_w10_mnpks <- trendhyfulldf[which(trendhyfulldf$window==10&trendhyfulldf$measure=="meanpeaksabv"),]
trendhyfull_w10_numpks <- trendhyfulldf[which(trendhyfulldf$window==10&trendhyfulldf$measure=="numpeaksabv"),]
trendhyfull_w10_totdays <- trendhyfulldf[which(trendhyfulldf$window==10&trendhyfulldf$measure=="totdaysabv"),]
trendhyfull_w10_totpkflw <- trendhyfulldf[which(trendhyfulldf$window==10&trendhyfulldf$measure=="totpeakflwabv"),]
trendhyfull_w10_totvol <- trendhyfulldf[which(trendhyfulldf$window==10&trendhyfulldf$measure=="totvolabv"),]

write.csv(trendhyfull_w1_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w1_mnpks.csv")
write.csv(trendhyfull_w1_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w1_numpks.csv")
write.csv(trendhyfull_w1_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w1_totdays.csv")
write.csv(trendhyfull_w1_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w1_totpkflw.csv")
write.csv(trendhyfull_w1_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w1_totvol.csv")

write.csv(trendhyfull_w5_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w5_mnpks.csv")
write.csv(trendhyfull_w5_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w5_numpks.csv")
write.csv(trendhyfull_w5_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w5_totdays.csv")
write.csv(trendhyfull_w5_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w5_totpkflw.csv")
write.csv(trendhyfull_w5_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w5_totvol.csv")

write.csv(trendhyfull_w10_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w10_mnpks.csv")
write.csv(trendhyfull_w10_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w10_numpks.csv")
write.csv(trendhyfull_w10_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w10_totdays.csv")
write.csv(trendhyfull_w10_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w10_totpkflw.csv")
write.csv(trendhyfull_w10_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\full\\trendhyfull_w10_totvol.csv")

##############
trendhyfilesdams <- dir("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\hy\\dams")

trendhydams <- vector("list", length(trendhyfilesdams))

for(i in 1:length(trendhyfilesdams)){
	trendhydams[[i]] <- read.csv(file=paste("C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\hy\\dams\\",trendhyfilesdams[[i]],sep=""), header=TRUE, sep=",")
	
}

trendhydamsdf <- do.call(rbind.data.frame,trendhydams)

trendhydams_w1_mnpks <- trendhydamsdf[which(trendhydamsdf$window==1&trendhydamsdf$measure=="meanpeaksabv"),]
trendhydams_w1_numpks <- trendhydamsdf[which(trendhydamsdf$window==1&trendhydamsdf$measure=="numpeaksabv"),]
trendhydams_w1_totdays <- trendhydamsdf[which(trendhydamsdf$window==1&trendhydamsdf$measure=="totdaysabv"),]
trendhydams_w1_totpkflw <- trendhydamsdf[which(trendhydamsdf$window==1&trendhydamsdf$measure=="totpeakflwabv"),]
trendhydams_w1_totvol <- trendhydamsdf[which(trendhydamsdf$window==1&trendhydamsdf$measure=="totvolabv"),]

trendhydams_w5_mnpks <- trendhydamsdf[which(trendhydamsdf$window==5&trendhydamsdf$measure=="meanpeaksabv"),]
trendhydams_w5_numpks <- trendhydamsdf[which(trendhydamsdf$window==5&trendhydamsdf$measure=="numpeaksabv"),]
trendhydams_w5_totdays <- trendhydamsdf[which(trendhydamsdf$window==5&trendhydamsdf$measure=="totdaysabv"),]
trendhydams_w5_totpkflw <- trendhydamsdf[which(trendhydamsdf$window==5&trendhydamsdf$measure=="totpeakflwabv"),]
trendhydams_w5_totvol <- trendhydamsdf[which(trendhydamsdf$window==5&trendhydamsdf$measure=="totvolabv"),]

trendhydams_w10_mnpks <- trendhydamsdf[which(trendhydamsdf$window==10&trendhydamsdf$measure=="meanpeaksabv"),]
trendhydams_w10_numpks <- trendhydamsdf[which(trendhydamsdf$window==10&trendhydamsdf$measure=="numpeaksabv"),]
trendhydams_w10_totdays <- trendhydamsdf[which(trendhydamsdf$window==10&trendhydamsdf$measure=="totdaysabv"),]
trendhydams_w10_totpkflw <- trendhydamsdf[which(trendhydamsdf$window==10&trendhydamsdf$measure=="totpeakflwabv"),]
trendhydams_w10_totvol <- trendhydamsdf[which(trendhydamsdf$window==10&trendhydamsdf$measure=="totvolabv"),]

write.csv(trendhydams_w1_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w1_mnpks.csv")
write.csv(trendhydams_w1_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w1_numpks.csv")
write.csv(trendhydams_w1_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w1_totdays.csv")
write.csv(trendhydams_w1_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w1_totpkflw.csv")
write.csv(trendhydams_w1_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w1_totvol.csv")

write.csv(trendhydams_w5_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w5_mnpks.csv")
write.csv(trendhydams_w5_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w5_numpks.csv")
write.csv(trendhydams_w5_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w5_totdays.csv")
write.csv(trendhydams_w5_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w5_totpkflw.csv")
write.csv(trendhydams_w5_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w5_totvol.csv")

write.csv(trendhydams_w10_mnpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w10_mnpks.csv")
write.csv(trendhydams_w10_numpks, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w10_numpks.csv")
write.csv(trendhydams_w10_totdays, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w10_totdays.csv")
write.csv(trendhydams_w10_totpkflw, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w10_totpkflw.csv")
write.csv(trendhydams_w10_totvol, file="C:\\Users\\tiffn_000\\Documents\\GIS\\Active_sites_final\\Data\\trends\\grouped\\hy\\dams\\trendhydams_w10_totvol.csv")

#################