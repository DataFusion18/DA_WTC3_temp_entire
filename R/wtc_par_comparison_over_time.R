# Compare the WTC1, WTC3 and WTC4 inside and outside PAR data to find out the transmission rate through the chamber wall
# Seems like the WTC-3 surface might got dusty over time which reduced the PAR from 2016 summer compared to 2009 summer
# read all 12 chamber inside PAR data
files <- list.files(path = "raw_data/WTC_CO2DROUGHT_CM_DATABASE_20090401_L2/Chamber flux/Hourly gapfilled fluxes", pattern = ".csv", full.names = TRUE)
temp <- lapply(files, fread, sep=",")
wtc1.par <- rbindlist( temp )
wtc1.par = data.frame(wtc1.par)
keeps <- c("DateTime", "Date","chamber", "PAR")
wtc1.par = wtc1.par[ , keeps, drop = FALSE]
wtc1.par$chamber = as.factor(wtc1.par$chamber)
wtc1.par$Date <- as.Date(wtc1.par$DateTime)
wtc1.par$DateTime <- as.POSIXct(wtc1.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
names(wtc1.par)[4] = c("PAR.inside")

# read met data for 2008 and 2009
wtc1.met.data.2008 <- read.csv("raw_data/WTC_CO2DROUGHT_CM_DATABASE_20090401_L2/Met Data/2008/HFE 30min Metdata 2008.csv", header=T)
wtc1.met.data.2009 <- read.csv("raw_data/WTC_CO2DROUGHT_CM_DATABASE_20090401_L2/Met Data/2009/HFE 30min Metdata 2009.csv", header=T)
wtc1.met.data = rbind(wtc1.met.data.2008,wtc1.met.data.2009)
keeps <- c("DateTime","PAR")
wtc1.met.data = wtc1.met.data[ , keeps, drop = FALSE]
wtc1.met.data$Date = as.Date(wtc1.met.data$DateTime)
wtc1.met.data = subset(wtc1.met.data, Date >= as.Date("2008-04-14") & Date <= as.Date("2009-03-06"))
wtc1.met.data$DateTime <- as.POSIXct(wtc1.met.data$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
wtc1.met.data$hour <- cut(wtc1.met.data$DateTime, breaks = "hour")

getmeans  <- function(wtc1.met.data) c(PAR = mean(wtc1.met.data$PAR,na.rm=TRUE))
wtc1.met.data <- ddply(wtc1.met.data, .(hour), getmeans)
wtc1.met.data$DateTime <- as.POSIXct(wtc1.met.data$hour, tz = "GMT")
wtc1.met.data$Date <- as.Date(wtc1.met.data$DateTime)
names(wtc1.met.data)[2] = c("PAR.outside")

# merge inside and outside PAR data for wtc1
wtc1.par.merge = merge(wtc1.par,wtc1.met.data[,c("DateTime","Date","PAR.outside")],by=c("DateTime","Date"),all=T)
wtc1.par.merge = wtc1.par.merge[complete.cases(wtc1.par.merge),]
wtc1.par.merge.sub = with( wtc1.par.merge , wtc1.par.merge[ hour( DateTime ) >= 11 & hour( DateTime ) <= 13 , ] )
wtc1.par.merge.sub = summaryBy(PAR.outside+PAR.inside ~ DateTime, data=wtc1.par.merge.sub, FUN=mean, na.rm=T)
names(wtc1.par.merge.sub)[2:3] = c("PAR.outside","PAR.inside")

wtc1.par.merge.sub$transmission = wtc1.par.merge.sub$PAR.inside / wtc1.par.merge.sub$PAR.outside * 100

# discard the data when transmission is greater than 100% ad less than 80% and find the mean transmission rate for WTC1 during 2008-2009
mean(wtc1.par.merge.sub$transmission)
mean(wtc1.par.merge.sub$transmission[wtc1.par.merge.sub$transmission<=100 & wtc1.par.merge.sub$transmission>=80])

#----------------------------------------------------------------------------------------------------------------
png("output/PAR_wtc1_data_all.png", units="px", width=1500, height=1500, res=150)
daterange=c(as.POSIXlt(min(wtc1.par.merge.sub$DateTime)),as.POSIXlt(max(wtc1.par.merge.sub$DateTime)))
par(mfrow=c(2,2), mar=c(5,4.5,3,1))
plot(wtc1.par.merge.sub$DateTime,wtc1.par.merge.sub$transmission, type="p",pch=20,lty=2,lwd=0.3,main=paste("All data"),
     xaxt="n",xlab="",ylab="Transmission Rate (%)")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b-%Y")
with(subset(wtc1.par.merge.sub, transmission <= 100 & transmission >= 80), 
     plot(DateTime,transmission, type="p",pch=20,lty=2,lwd=0.3,main=paste("Filtered transmission"),
          xaxt="n",xlab="",ylab="Transmission Rate (%)"))
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b-%Y")
plot(wtc1.par.merge.sub$DateTime,wtc1.par.merge.sub$PAR.inside, type="p",pch=20,lty=2,lwd=0.3,main=paste("Inside PAR"),
     xaxt="n",xlab="",ylab="Inside PAR")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b-%Y")
plot(wtc1.par.merge.sub$DateTime,wtc1.par.merge.sub$PAR.outside, type="p",pch=20,lty=2,lwd=0.3,main=paste("Outside PAR"),
     xaxt="n",xlab="",ylab="Outside PAR")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b-%Y")
dev.off()

# png("output/PAR_wtc1_data_all.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(3,4), mar=c(5,4.5,3,1))
# for (i in 1:nlevels(wtc1.par.merge.sub$chamber)) {
#   with(subset(wtc1.par.merge.sub, chamber %in% as.factor(wtc1.par.merge.sub$chamber[i])), 
#        plot(Date,transmission, type="p",pch='.',lty=2,lwd=0.3,main=paste(wtc1.par.merge.sub$chamber[i]),
#             xlab="",ylab="Transmission Rate (%)"))
# }
# dev.off()
# 
# png("output/PAR_wtc1_data_trans<100.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(3,4), mar=c(5,4.5,3,1))
# for (i in 1:nlevels(wtc1.par.merge.sub$chamber)) {
#   with(subset(wtc1.par.merge.sub, chamber %in% as.factor(wtc1.par.merge.sub$chamber[i]) & transmission <= 100 & transmission >= 80), 
#        plot(Date,transmission, type="p",pch='.',lty=2,lwd=0.3,main=paste(wtc1.par.merge.sub$chamber[i]),
#             xlab="",ylab="Transmission Rate (%)"))
# }
# dev.off()
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# # import WTC3 inside PAR data for all chambers till Feb 2013
# WTC01.par <- read.table("raw_data/WTC_PAR/WTC01_Table2_20130228.dat",header=T,sep=",")
# WTC01.par$chamber = as.factor("C01")
# WTC02.par <- read.table("raw_data/WTC_PAR/WTC02_Table2_20130228.dat",header=T,sep=",")
# WTC02.par$chamber = as.factor("C02")
# WTC03.par <- read.table("raw_data/WTC_PAR/WTC03_Table2_20130228.dat",header=T,sep=",")
# WTC03.par$chamber = as.factor("C03")
# WTC04.par <- read.table("raw_data/WTC_PAR/WTC04_Table2_20130228.dat",header=T,sep=",")
# WTC04.par$chamber = as.factor("C04")
# WTC05.par <- read.table("raw_data/WTC_PAR/WTC05_Table2_20130228.dat",header=T,sep=",")
# WTC05.par$chamber = as.factor("C05")
# WTC06.par <- read.table("raw_data/WTC_PAR/WTC06_Table2_20130228.dat",header=T,sep=",")
# WTC06.par$chamber = as.factor("C06")
# WTC07.par <- read.table("raw_data/WTC_PAR/WTC07_Table2_20130228.dat",header=T,sep=",")
# WTC07.par$chamber = as.factor("C07")
# WTC09.par <- read.table("raw_data/WTC_PAR/WTC09_Table2_20130228.dat",header=T,sep=",")
# WTC09.par$chamber = as.factor("C09")
# WTC10.par <- read.table("raw_data/WTC_PAR/WTC10_Table2_20130228.dat",header=T,sep=",")
# WTC10.par$chamber = as.factor("C10")
# WTC11.par <- read.table("raw_data/WTC_PAR/WTC11_Table2_20130228.dat",header=T,sep=",")
# WTC11.par$chamber = as.factor("C11")
# WTC12.par <- read.table("raw_data/WTC_PAR/WTC12_Table2_20130228.dat",header=T,sep=",")
# WTC12.par$chamber = as.factor("C12")
# 
# wtc3.par = rbind(WTC01.par,WTC02.par,WTC03.par,WTC04.par,WTC05.par,WTC06.par,WTC07.par,WTC09.par,WTC10.par,WTC11.par,WTC12.par)
# keeps <- c("TIMESTAMP", "chamber", "PPFD_Avg")
# wtc3.par = wtc3.par[ , keeps, drop = FALSE]
# names(wtc3.par) = c("DateTime", "chamber", "PAR")
# # 
# wtc3.par$DateTime = as.POSIXct(wtc3.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# wtc3.par = wtc3.par[complete.cases(wtc3.par), ]
# wtc3.par$Date = as.Date(wtc3.par$DateTime)
# wtc3.par$hour <- cut(wtc3.par$DateTime, breaks = "hour")
# wtc3.par$hour <- as.POSIXct(wtc3.par$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")

# import WTC3 inside PAR data for all chambers
files <- list.files(path = "raw_data/WTC_TEMP_CM_WTCMET", pattern = ".csv", full.names = TRUE)
temp <- lapply(files, fread, sep=",")
wtc3.par <- rbindlist( temp )

wtc3.par <- wtc3.par[ , c("DateTime","chamber","PPFD_Avg")]
names(wtc3.par) = c("DateTime", "chamber", "PAR")
wtc3.par = wtc3.par[complete.cases(wtc3.par),]
wtc3.par$DateTime <- as.POSIXct(wtc3.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# wtc3.par.flux = subset(wtc3.par.flux, DateTime >= as.Date("2013-03-01"))
wtc3.par$hour <- cut(wtc3.par$DateTime, breaks = "hour")
wtc3.par$Date <- as.Date(wtc3.par$DateTime)

# wtc3.par.sub = subset(wtc3.par, Date >= as.Date("2012-12-18") & Date <= as.Date("2013-02-12"))
wtc3.par$PAR = as.numeric(as.character(wtc3.par$PAR))

# WTC.par.sub = WTC.par
getmeans  <- function(wtc3.par) c(PAR = mean(wtc3.par$PAR,na.rm=TRUE))
wtc3.par.means <- ddply(wtc3.par, .(hour), getmeans)
wtc3.par.means$hour <- as.POSIXct(wtc3.par.means$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
names(wtc3.par.means) = c("DateTime","PAR.inside")

# for(i in chambers[c(1:7,9:12)]) { 
#   files <- list.files(path = paste0("raw_data/WTC_PAR/WTC3_PAR_inside/",i), pattern = ".dat", full.names=TRUE)
#   temp <- lapply(files, fread, sep=",")
#   wtc3.ch.par <- rbindlist( temp )
#   wtc3.ch.par = data.frame(wtc3.ch.par)
#   keeps <- c("TIMESTAMP", "PPFD_Avg")
#   wtc3.ch.par = wtc3.ch.par[ , keeps, drop = FALSE]
#   names(wtc3.ch.par) = c("DateTime", "PAR")
#   
#   wtc3.ch.par$DateTime = as.POSIXct(wtc3.ch.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
#   wtc3.ch.par = wtc3.ch.par[complete.cases(wtc3.ch.par), ]
#   wtc3.ch.par$Date = as.Date(wtc3.ch.par$DateTime)
#   wtc3.ch.par$hour <- cut(wtc3.ch.par$DateTime, breaks = "hour")
#   
#   wtc3.ch.par$PAR = as.numeric(wtc3.ch.par$PAR)
#   wtc3.ch.par$chamber = as.factor(i)
#   
#   getmeans  <- function(wtc3.ch.par) c(PAR = mean(wtc3.ch.par$PAR,na.rm=TRUE))
#   wtc3.ch.par.means <- ddply(wtc3.ch.par, .(chamber, hour), getmeans)
#   wtc3.ch.par.means$hour <- as.POSIXct(wtc3.ch.par.means$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
#   
#   if (i %in% chambers[1]) {
#     wtc3.par = wtc3.ch.par.means
#   } else {
#     wtc3.par = rbind(wtc3.par,wtc3.ch.par.means)
#   }
# }
# 
# 
# 
# files <- list.files(path = "raw_data/WTC_PAR/WTC01_Table2_20130123", pattern = ".dat", full.names = TRUE)
# temp <- lapply(files, fread, sep=",")
# wtc3.ch01.par <- rbindlist( temp )
# wtc3.ch01.par = data.frame(wtc3.ch01.par)
# keeps <- c("TIMESTAMP", "PPFD_Avg")
# wtc3.ch01.par = wtc3.ch01.par[ , keeps, drop = FALSE]
# names(wtc3.ch01.par) = c("DateTime", "PAR")
# wtc3.ch01.par$DateTime <- as.POSIXct(wtc3.ch01.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# wtc3.ch01.par$PAR = as.numeric(wtc3.ch01.par$PAR)
# wtc3.ch01.par = wtc3.ch01.par[complete.cases(wtc3.ch01.par),]
# wtc3.ch01.par$chamber = as.factor("C01")
# 
# wtc3.ch01.par.sub = with( wtc3.ch01.par , wtc3.ch01.par[ hour( DateTime ) >= 11 & hour( DateTime ) <= 13 , ] )
# wtc3.ch01.par.sub$hour <- cut(wtc3.ch01.par.sub$DateTime, breaks = "hour")
# wtc3.ch01.par.sub$hour <- as.POSIXct(wtc3.ch01.par.sub$hour,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# getmeans  <- function(wtc3.ch01.par.sub) c(PAR = mean(wtc3.ch01.par.sub$PAR,na.rm=TRUE))
# wtc3.ch01.par.sub <- ddply(wtc3.ch01.par.sub, .(hour), getmeans)
# wtc3.ch01.par.sub$DateTime <- as.POSIXct(wtc3.ch01.par.sub$hour, tz = "GMT")
# wtc3.ch01.par.sub$Date <- as.Date(wtc3.ch01.par.sub$DateTime)
# names(wtc3.ch01.par.sub)[2] = c("PAR.inside")
# 
# 
# files <- list.files(path = "raw_data/WTC_PAR/WTC12_Table2_20130228", pattern = ".dat", full.names = TRUE)
# temp <- lapply(files, fread, sep=",")
# wtc3.ch12.par <- rbindlist( temp )
# wtc3.ch12.par = data.frame(wtc3.ch12.par)
# keeps <- c("TIMESTAMP", "PPFD_Avg")
# wtc3.ch12.par = wtc3.ch12.par[ , keeps, drop = FALSE]
# names(wtc3.ch12.par) = c("DateTime", "PAR")
# wtc3.ch12.par$DateTime <- as.POSIXct(wtc3.ch12.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# wtc3.ch12.par$PAR = as.numeric(wtc3.ch12.par$PAR)
# wtc3.ch12.par = wtc3.ch12.par[complete.cases(wtc3.ch12.par),]
# wtc3.ch12.par$chamber = as.factor("C12")
# 
# wtc3.ch12.par.sub = with( wtc3.ch12.par , wtc3.ch12.par[ hour( DateTime ) >= 11 & hour( DateTime ) <= 13 , ] )
# wtc3.ch12.par.sub$hour <- cut(wtc3.ch12.par.sub$DateTime, breaks = "hour")
# wtc3.ch12.par.sub$hour <- as.POSIXct(wtc3.ch12.par.sub$hour,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# getmeans  <- function(wtc3.ch12.par.sub) c(PAR = mean(wtc3.ch12.par.sub$PAR,na.rm=TRUE))
# wtc3.ch12.par.sub <- ddply(wtc3.ch12.par.sub, .(hour), getmeans)
# wtc3.ch12.par.sub$DateTime <- as.POSIXct(wtc3.ch12.par.sub$hour, tz = "GMT")
# wtc3.ch12.par.sub$Date <- as.Date(wtc3.ch12.par.sub$DateTime)
# names(wtc3.ch12.par.sub)[2] = c("PAR.inside")


# # read WTC3 outside PAR data for all chambers
# wtc3.met.data = read.csv("raw_data/PAR_raw.txt", header=FALSE)
# colnames(wtc3.met.data) = c("DateTime","PAR")
# wtc3.met.data$DateTime = as.POSIXct(wtc3.met.data$DateTime,format="%d/%m/%Y %H:%M:%S",tz="GMT")
# wtc3.met.data$Date = as.Date(wtc3.met.data$DateTime)
# wtc3.met.data.sub = subset(wtc3.met.data, Date >= as.Date("2012-12-18") & Date <= as.Date("2013-02-12"))
# # par.data.sub = subset(par.data, Date > as.Date("2012-12-03") & Date < as.Date("2012-12-13"))
# wtc3.met.data.sub$hour = cut(wtc3.met.data.sub$DateTime, breaks = "hour")
# 
# # need to turn the datetime into hms
# getmeans = function(wtc3.met.data.sub) c(PAR = mean(wtc3.met.data.sub$PAR))
# wtc3.met.data.sub.means = ddply(wtc3.met.data.sub, .(hour), getmeans)
# wtc3.met.data.sub.means$hour = as.POSIXct(wtc3.met.data.sub.means$hour, tz = "GMT")
# names(wtc3.met.data.sub.means) = c("DateTime","PAR.outside")


# import ROS WS data for WTC3 outside PAR for all chambers
files <- list.files(path = "raw_data/ROS_metdata", pattern = ".dat", full.names = TRUE)
temp <- lapply(files, fread, sep=",")
ros.par <- rbindlist( temp )
ros.par = data.frame(ros.par)
keeps <- c("TIMESTAMP", "PPFD_Avg")
ros.par = ros.par[ , keeps, drop = FALSE]
names(ros.par) = c("DateTime", "PAR")

ros.par$DateTime = as.POSIXct(ros.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
ros.par = ros.par[complete.cases(ros.par), ]
ros.par$Date = as.Date(ros.par$DateTime)
ros.par$hour <- cut(ros.par$DateTime, breaks = "hour")
ros.par$hour <- as.POSIXct(ros.par$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")

ros.par.sub = subset(ros.par, Date >= as.Date(min(wtc3.par.means$DateTime)) & Date <= as.Date(max(wtc3.par.means$DateTime)))
# ros.par.sub = subset(ros.par, Date >= as.Date("2012-12-18") & Date <= as.Date("2013-02-11"))
ros.par.sub$PAR = as.numeric(as.character(ros.par.sub$PAR))

getmeans  <- function(ros.par.sub) c(PAR = mean(ros.par.sub$PAR,na.rm=TRUE))
ros.par.sub.means <- ddply(ros.par.sub, .(hour), getmeans)
ros.par.sub.means$hour <- as.POSIXct(ros.par.sub.means$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
names(ros.par.sub.means) = c("DateTime","PAR.outside")

# merge inside and outside PAR data for wtc3
wtc3.par.merge = merge(wtc3.par.means,ros.par.sub.means,by=c("DateTime"),all=T)
wtc3.par.merge = wtc3.par.merge[complete.cases(wtc3.par.merge),]
# wtc3.par.merge.sub = with( wtc3.par.merge , wtc3.par.merge[ hour( DateTime ) >= 11 & hour( DateTime ) <= 13 , ] )
wtc3.par.merge.sub = with( wtc3.par.merge , wtc3.par.merge[ hour( DateTime ) == 12 , ] )
# wtc3.par.merge.sub = summaryBy(PAR.outside+PAR.inside ~ DateTime, data=wtc3.par.merge.sub, FUN=mean, na.rm=T)
# names(wtc3.par.merge.sub)[2:3] = c("PAR.outside","PAR.inside")

wtc3.par.merge.sub$transmission = wtc3.par.merge.sub$PAR.inside / wtc3.par.merge.sub$PAR.outside * 100

# discard the data when transmission is greater than 100% ad less than 80% and find the mean transmission rate for WTC1 during 2008-2009
mean(wtc3.par.merge.sub$transmission)
mean(wtc3.par.merge.sub$transmission[wtc3.par.merge.sub$transmission <= 100])
max(wtc3.par.merge.sub$transmission[wtc3.par.merge.sub$transmission <= 100])

#----------------------------------------------------------------------------------------------------------------
png("output/PAR_wtc3_data_all.png", units="px", width=2000, height=1000, res=180)
daterange=c(as.POSIXlt(min(wtc3.par.merge.sub$DateTime)),as.POSIXlt(max(wtc3.par.merge.sub$DateTime)))
par(mfrow=c(2,2), mar=c(5,4.5,3,1))
plot(wtc3.par.merge.sub$DateTime,wtc3.par.merge.sub$transmission, type="p",pch=20,lty=2,lwd=0.3,main=paste("All data"),
     xaxt="n",xlab="",ylab="Transmission Rate (%)")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="2 week"), format="%d-%b-%Y")
with(subset(wtc3.par.merge.sub, transmission <= 100), 
     plot(DateTime,transmission, type="p",pch=20,lty=2,lwd=0.3,main=paste("Filtered data: Discard transmission > 100"),
     xaxt="n",xlab="",ylab="Transmission Rate (%)"))
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="2 week"), format="%d-%b-%Y")
plot(wtc3.par.merge.sub$DateTime,wtc3.par.merge.sub$PAR.inside, type="p",pch=20,lty=2,lwd=0.3,main=paste("Inside PAR"),
     xaxt="n",xlab="",ylab="Inside PAR")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="2 week"), format="%d-%b-%Y")
plot(wtc3.par.merge.sub$DateTime,wtc3.par.merge.sub$PAR.outside, type="p",pch=20,lty=2,lwd=0.3,main=paste("Outside PAR"),
     xaxt="n",xlab="",ylab="Outside PAR")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="2 week"), format="%d-%b-%Y")
dev.off()

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# read WTC4 inside PAR data for all chambers
files <- list.files(path = "raw_data/WTC4_PAR", pattern = ".dat", full.names = TRUE)
temp <- lapply(files, fread, sep=",")
wtc4.par <- rbindlist( temp )
wtc4.par = data.frame(wtc4.par)

keeps <- c("TIMESTAMP", "PPFD_Avg")
wtc4.par = wtc4.par[ , keeps, drop = FALSE]
names(wtc4.par) = c("DateTime", "PAR")
wtc4.par$DateTime <- as.POSIXct(wtc4.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
wtc4.par$PAR = as.numeric(wtc4.par$PAR)
wtc4.par = wtc4.par[complete.cases(wtc4.par),]
wtc4.par.sub = with( wtc4.par , wtc4.par[ hour( DateTime ) >= 11 & hour( DateTime ) <= 13 , ] )
wtc4.par.sub$hour <- cut(wtc4.par.sub$DateTime, breaks = "hour")
wtc4.par.sub$hour <- as.POSIXct(wtc4.par.sub$hour,format="%Y-%m-%d %H:%M:%S",tz="GMT")

getmeans  <- function(wtc4.par.sub) c(PAR = mean(wtc4.par.sub$PAR,na.rm=TRUE))
wtc4.par.sub <- ddply(wtc4.par.sub, .(hour), getmeans)
wtc4.par.sub$DateTime <- as.POSIXct(wtc4.par.sub$hour, tz = "GMT")
wtc4.par.sub$Date <- as.Date(wtc4.par.sub$DateTime)
names(wtc4.par.sub)[2] = c("PAR.inside")
wtc4.par.sub = subset(wtc4.par.sub, Date >= as.Date("2015-09-01") & Date <= as.Date("2016-11-24"))

# wtc4 outside PAR data from ROS WS
files <- list.files(path = "raw_data/ROS_metdata_wtc4", pattern = ".dat", full.names = TRUE)
temp <- lapply(files, fread, sep=",")
ros.par.wct4 <- rbindlist( temp )
ros.par.wct4 = data.frame(ros.par.wct4)
keeps <- c("TIMESTAMP", "PPFD_Avg")
ros.par.wct4 = ros.par.wct4[ , keeps, drop = FALSE]
names(ros.par.wct4) = c("DateTime", "PPFD_Avg")

ros.par.wct4$DateTime = as.POSIXct(ros.par.wct4$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
ros.par.wct4 = ros.par.wct4[complete.cases(ros.par.wct4), ]
ros.par.wct4$Date = as.Date(ros.par.wct4$DateTime)
ros.par.wct4$hour <- cut(ros.par.wct4$DateTime, breaks = "hour")
ros.par.wct4$hour <- as.POSIXct(ros.par.wct4$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")

# ros.par.sub = subset(ros.par, Date >= as.Date("2012-12-12") & Date <= as.Date("2014-05-26"))
ros.par.wct4.sub = subset(ros.par.wct4, Date >= as.Date("2015-09-01") & Date <= as.Date("2016-11-24"))
ros.par.wct4.sub$PPFD_Avg = as.numeric(as.character(ros.par.wct4.sub$PPFD_Avg))

getmeans  <- function(ros.par.wct4.sub) c(PPFD_Avg = mean(ros.par.wct4.sub$PPFD_Avg,na.rm=TRUE))
ros.par.wct4.sub.means <- ddply(ros.par.wct4.sub, .(hour), getmeans)
ros.par.wct4.sub.means$hour <- as.POSIXct(ros.par.wct4.sub.means$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
names(ros.par.wct4.sub.means)[1] = c("DateTime")
ros.par.wct4.sub.means = with( ros.par.wct4.sub.means , ros.par.wct4.sub.means[ hour( DateTime ) >= 11 & hour( DateTime ) <= 13 , ] )
names(ros.par.wct4.sub.means)[2] = c("PAR.outside")

wtc4.par.sub = merge(wtc4.par.sub[,c("DateTime","Date","PAR.inside")],ros.par.wct4.sub.means,by="DateTime")
wtc4.par.sub$transmission = wtc4.par.sub$PAR.inside / wtc4.par.sub$PAR.outside * 100

# discard the data when transmission is greater than 100% ad less than 80% and find the mean transmission rate for WTC1 during 2008-2009
mean(wtc4.par.sub$transmission)

#----------------------------------------------------------------------------------------------------------------
png("output/PAR_wtc4_data_all.png", units="px", width=2000, height=1000, res=180)
daterange=c(as.POSIXlt(min(wtc4.par.sub$DateTime)),as.POSIXlt(max(wtc4.par.sub$DateTime)))
par(mfrow=c(1,3), mar=c(5,4.5,3,1))
plot(wtc4.par.sub$DateTime,wtc4.par.sub$transmission, type="p",pch=20,lty=2,lwd=0.3,main=paste("All data"),
     xaxt="n",xlab="",ylab="Transmission Rate (%)")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b-%Y")
plot(wtc4.par.sub$DateTime,wtc4.par.sub$PAR.inside, type="p",pch=20,lty=2,lwd=0.3,main=paste("Inside PAR"),
          xaxt="n",xlab="",ylab="Inside PAR")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b-%Y")
plot(wtc4.par.sub$DateTime,wtc4.par.sub$PAR.outside, type="p",pch=20,lty=2,lwd=0.3,main=paste("Outside PAR"),
     xaxt="n",xlab="",ylab="Outside PAR")
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b-%Y")
dev.off()

#----------------------------------------------------------------------------------------------------------------
# plotting histograms for all WTC experiment PAR transmission rates
png("output/PAR_hist_wtc_data_all.png", units="px", width=2000, height=1000, res=180)
par(mfrow=c(2,2), mar=c(5,4.5,3,1))
x=wtc1.par.merge.sub$transmission[wtc1.par.merge.sub$transmission<100 & wtc1.par.merge.sub$transmission>75]
h = hist(x, main="WTC1 (2008-2009)", xlab="Transmission rates (%)", border="blue", col="green", las=1, xaxt='n', breaks=25)
axis(side=1, at=seq(0,100,5), labels=seq(0,100,5))
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
text(par('usr')[2]-15, par('usr')[4]-3, labels = paste("Transmission rate =", round(xfit[which(yfit == max(yfit))], 1), "%"))

x=wtc3.par.merge.sub$transmission[wtc3.par.merge.sub$transmission<100 & wtc3.par.merge.sub$transmission>75]
h = hist(x, main="WTC3 (2013-2014)", xlab="Transmission rates (%)", border="blue", col="green", las=1, xaxt='n', breaks=25)
axis(side=1, at=seq(0,100,5), labels=seq(0,100,5))
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
text(par('usr')[2]-5, par('usr')[4]-3, labels = paste("Transmission rate =", round(xfit[which(yfit == max(yfit))], 1), "%"))

x=wtc4.par.sub$transmission[wtc4.par.sub$transmission<100 & wtc4.par.sub$transmission>75]
h = hist(x, main="WTC4 (2015-2016)", xlab="Transmission rates (%)", border="blue", col="green", las=1, xaxt='n', breaks=25)
axis(side=1, at=seq(0,100,5), labels=seq(0,100,5))
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
text(par('usr')[2]-5, par('usr')[4]-3, labels = paste("Transmission rate =", round(xfit[which(yfit == max(yfit))], 1), "%"))
dev.off()
