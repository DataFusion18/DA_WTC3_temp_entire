# Process the met data from the start of the experiment (Dec 2012) to start of flux measurement (Sept 2013)

met.data <- read.csv("raw_data/met.data.raw.txt", header=FALSE)
colnames(met.data)<- c("DateTime","chamber","dewpoint","RH","Tair")
met.data$DateTime <- as.POSIXct(met.data$DateTime,format="%d/%m/%Y %H:%M:%S",tz="GMT")
# met.data$Date <- as.Date(met.data$DateTime)
met.data.sub = subset(met.data, DateTime < as.Date("2013-03-01"))
# met.data.sub = subset(met.data, DateTime < as.Date("2012-12-13"))
# met.data.sub$Hour <- hour(met.data.sub$DateTime)
met.data.sub$VPD <- RHtoVPD(RH=met.data.sub$RH,TdegC=met.data.sub$Tair)
met.data.sub$hour <- cut(met.data.sub$DateTime, breaks = "hour")
# met.data.sub$hour <- format(met.data.sub$DateTime, format='%H:%M:%S')
met.data.sub$chamber = as.character(met.data.sub$chamber)

met.data.sub$chamber = as.factor( ifelse(met.data.sub$chamber %in% as.character("ch01"), as.character("C01"), ifelse(met.data.sub$chamber %in% as.character("ch02"), as.character("C02"), 
                                   ifelse(met.data.sub$chamber %in% as.character("ch03"), as.character("C03"), ifelse(met.data.sub$chamber %in% as.character("ch04"), as.character("C04"), 
                                    ifelse(met.data.sub$chamber %in% as.character("ch05"), as.character("C05"), ifelse(met.data.sub$chamber %in% as.character("ch06"), as.character("C06"), 
                                     ifelse(met.data.sub$chamber %in% as.character("ch07"), as.character("C07"), ifelse(met.data.sub$chamber %in% as.character("ch08"), as.character("C08"), 
                                      ifelse(met.data.sub$chamber %in% as.character("ch09"), as.character("C09"), ifelse(met.data.sub$chamber %in% as.character("ch10"), as.character("C10"), 
                                       ifelse(met.data.sub$chamber %in% as.character("ch11"), as.character("C11"), as.character("C12")) )))))))))))
met.data.sub$chamber = as.factor(met.data.sub$chamber)

#----------------------------------------------------------------------------------------------------------------
# need to turn the datetime into hms
# met.data.sub$DateTime <- ymd_hms(met.data.sub$DateTime)
# met.data.sub$time <- format(met.data.sub$DateTime, format='%H:%M:%S')
getmeans  <- function(met.data.sub) c(RH = mean(met.data.sub$RH,na.rm=TRUE), VPD = mean(met.data.sub$VPD,na.rm=TRUE), Tair = mean(met.data.sub$Tair,na.rm=TRUE))

met.data.sub.means <- ddply(met.data.sub, .(chamber, hour), getmeans)
met.data.sub.means$hour <- as.POSIXct(met.data.sub.means$hour, tz = "GMT")

## A plot for one chamber
with(subset(met.data.sub.means, chamber %in% as.factor("C01")), plot(hour,Tair, type="l"))
with(subset(met.data.sub.means, chamber %in% as.factor("C01")), plot(hour,VPD, type="l"))

# ## A plot for each chamber
# xyplot(Tair ~ hour | chamber, data = met.data.sub.means, type = "l", scales = list(x = list(relation = "free", rot = 90)))


# met.data.sub$DateTime <- as.POSIXct(met.data.sub$DateTime,format="%d/%m/%Y %T",tz="GMT")
# met.data.sub$Date <- as.Date(met.data.sub$DateTime)
# met.data.sub$Hour <- hour(met.data.sub$DateTime)
# met.data.sub$minute <- minute(met.data.sub$DateTime)

# met.data.sub$VPD1 <- DewtoVPD(Tdew=met.data.sub$dewpoint,TdegC=met.data.sub$Tair)
# met.data.sub$VPD2 <- RHtoVPD(RH=met.data.sub$Rh,TdegC=met.data.sub$Tair)

#----------------------------------------------------------------------------------------------------------------
par.data <- read.csv("raw_data/PAR_raw.txt", header=FALSE)
colnames(par.data)<- c("DateTime","PAR")
par.data$DateTime <- as.POSIXct(par.data$DateTime,format="%d/%m/%Y %H:%M:%S",tz="GMT")
par.data$Date <- as.Date(par.data$DateTime)
par.data.sub = subset(par.data, Date > as.Date("2012-12-03") & Date < as.Date("2013-03-01"))
# par.data.sub = subset(par.data, Date > as.Date("2012-12-03") & Date < as.Date("2012-12-13"))
par.data.sub$hour <- cut(par.data.sub$DateTime, breaks = "hour")

# need to turn the datetime into hms
getmeans  <- function(par.data.sub) c(PAR = mean(par.data.sub$PAR))

par.data.sub.means <- ddply(par.data.sub, .(hour), getmeans)
par.data.sub.means$hour <- as.POSIXct(par.data.sub.means$hour, tz = "GMT")

## A plot for one chamber
plot(par.data.sub.means$hour, par.data.sub.means$PAR, type="l")

# ## daily average values
# par.data.sub.means.1 <- timeAverage(par.data.sub, avg.time = "hour")


#----------------------------------------------------------------------------------------------------------------
met.data.DectoMar = merge(met.data.sub.means,par.data.sub.means, by=c("hour"))
names(met.data.DectoMar) = c("DateTime","chamber","RH_al","VPD_al","Tair_al","PPFD_Avg")

# Filling the gaps in met data by taking the mean of previous and next day
for (i in 1:nlevels(chambers)) {
  met.data.DectoMar.1 = subset(met.data.DectoMar, chamber %in% as.factor(chambers[i])) 
  newdat <- data.frame(DateTime=with(met.data.DectoMar.1, seq(min(DateTime), max(DateTime), by='hour')))
  met.data.DectoMar.sub <- merge(met.data.DectoMar.1, newdat, by='DateTime', all=TRUE)
  
  index = complete.cases(met.data.DectoMar.sub)
  missing.times = met.data.DectoMar.sub$DateTime[!index] - as.difftime(1, unit="days")
  y1 = subset(met.data.DectoMar.sub, DateTime %in% missing.times)
  y1$DateTime = y1$DateTime + as.difftime(1, unit="days")
  missing.times = met.data.DectoMar.sub$DateTime[!index] + as.difftime(1, unit="days")
  y2 = subset(met.data.DectoMar.sub, DateTime %in% missing.times)
  y2$DateTime = y2$DateTime - as.difftime(1, unit="days")
  
  pp <- cbind(rbind.fill(list(y1, y2)))
  y <- summaryBy(RH_al+VPD_al+Tair_al+PPFD_Avg ~ DateTime+chamber, data=pp, FUN=c(mean),na.rm=T)
  names(y) = c("DateTime","chamber","RH_al","VPD_al","Tair_al","PPFD_Avg")
  
  met.data.DectoMar.sub[!index,] = y
  met.data.DectoMar.sub$PPFD_Avg[met.data.DectoMar.sub$PPFD_Avg <= 0] = 0 # Remove the negetive PAR values
  
  if (i == 1) {
    met.data.DectoMar.final = met.data.DectoMar.sub
  }
  if (i > 1) {
    met.data.DectoMar.final = rbind(met.data.DectoMar.final, met.data.DectoMar.sub)
  }
} 

# # Plot to check the gap-filling
# met.data.DectoMar.sub$Date <- as.Date(met.data.DectoMar.sub$DateTime)
# met.data.all.c00 = subset(met.data.DectoMar.sub, Date == as.Date("2013-02-27")) 
# with(subset(met.data.DectoMar.sub, Date == as.Date("2013-02-27")), plot(DateTime,Tair_al, type="p",lty=2,lwd=0.3))

#----------------------------------------------------------------------------------------------------------------
# import site weather data, take only Tair, format date stuff
files <- list.files(path = "raw_data/WTC_TEMP_CM_WTCMET", pattern = ".csv", full.names = TRUE)
temp <- lapply(files, fread, sep=",")
Tair <- rbindlist( temp )

met.data.hiev <- Tair[ , c("chamber","DateTime","Tair_al","RH_al","PPFD_Avg")]
met.data.hiev = subset(met.data.hiev, DateTime >= as.Date("2013-03-01"))
met.data.hiev$DateTime <- as.POSIXct(met.data.hiev$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# met.data.hiev$Date <- as.Date(met.data.hiev$DateTime)
met.data.hiev$VPD_al <- RHtoVPD(RH=met.data.hiev$RH_al,TdegC=met.data.hiev$Tair_al)
met.data.hiev$hour <- cut(met.data.hiev$DateTime, breaks = "hour")

# met.data.hiev = subset(met.data.hiev, DateTime > as.Date("2014-05-21"))

# need to turn the datetime into hms
getmeans  <- function(met.data.hiev) c(RH_al = mean(met.data.hiev$RH_al,na.rm=TRUE), VPD_al = mean(met.data.hiev$VPD_al,na.rm=TRUE), Tair_al = mean(met.data.hiev$Tair_al,na.rm=TRUE), PPFD_Avg = mean(met.data.hiev$PPFD_Avg,na.rm=TRUE))

met.data.hiev.means <- ddply(met.data.hiev, .(chamber, hour), getmeans)
met.data.hiev.means$hour <- as.POSIXct(met.data.hiev.means$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")

names(met.data.hiev.means)[2] = c("DateTime")
# met.data.hiev.means$DateTime <- as.POSIXct(met.data.hiev.means$DateTime, format="%Y-%m-%d %H:%M:%S",tz="GMT")

# statsNA(met.data.hiev.means$Tair_al)
# Filling the gaps in met data by taking the mean of previous and next day
for (i in 1:nlevels(chambers)) {
  met.data.hiev.means.sub = subset(met.data.hiev.means, chamber %in% as.factor(chambers[i])) 
  index = complete.cases(met.data.hiev.means.sub)
  missing.times = met.data.hiev.means.sub$DateTime[!index] - as.difftime(1, unit="days")
  y1 = subset(met.data.hiev.means.sub, DateTime %in% missing.times)
  y1$DateTime = y1$DateTime + as.difftime(1, unit="days")
  missing.times = met.data.hiev.means.sub$DateTime[!index] + as.difftime(1, unit="days")
  y2 = subset(met.data.hiev.means.sub, DateTime %in% missing.times)
  y2$DateTime = y2$DateTime - as.difftime(1, unit="days")
  
  pp <- cbind(rbind.fill(list(y1, y2)))
  y <- summaryBy(RH_al+VPD_al+Tair_al+PPFD_Avg ~ chamber+DateTime, data=pp, FUN=c(mean),na.rm=T)
  names(y) = c("chamber","DateTime","RH_al","VPD_al","Tair_al","PPFD_Avg")
  
  met.data.hiev.means.sub[!index,] = y
  met.data.hiev.means.sub$PPFD_Avg[met.data.hiev.means.sub$PPFD_Avg <= 0] = 0 # Remove the negetive PAR values
  
  if (i == 1) {
    met.data.hiev.means.final = met.data.hiev.means.sub
  }
  if (i > 1) {
    met.data.hiev.means.final = rbind(met.data.hiev.means.final, met.data.hiev.means.sub)
  }
}     

# Plot to check the gap-filling
with(subset(met.data.hiev.means.final, chamber %in% as.factor("C01")), plot(DateTime,Tair_al, type="l",lty=2,lwd=0.3))
with(subset(met.data.hiev.means.final, chamber %in% as.factor("C08")), plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3))

with(subset(met.data.hiev.means.final, chamber %in% as.factor("C01") & DateTime > as.Date("2013-06-01") & DateTime < as.Date("2013-06-10")), 
     plot(DateTime,Tair_al, type="l",lty=2,lwd=0.3))
with(subset(met.data.hiev.means.final, chamber %in% as.factor("C01") & DateTime > as.Date("2013-06-01") & DateTime < as.Date("2013-06-10")), 
     plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3))
   

# met.data.hiev.means.sub$Date = as.Date(met.data.hiev.means.sub$DateTime)
# subset(met.data.hiev.means.sub, Date == as.Date("2013-06-06"))

#----------------------------------------------------------------------------------------------------------------
met.data.all = rbind(met.data.DectoMar.final,met.data.hiev.means.final)
met.data.all$Date <- as.Date(met.data.all$DateTime)
met.data.all = subset(met.data.all, Date < "2014-05-27")

## Plot raw PAR data
png("output/11.PAR_raw_data.png", units="px", width=5000, height=2500, res=250)
par(mfrow=c(3,4), mar=c(5,4.5,3,1))
for (i in 1:nlevels(chambers)) {
  with(subset(met.data.all, chamber %in% as.factor(chambers[i])), 
       plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3,main=paste(chambers[i]),
            xlab="",ylab=expression("Leaf area"~(cm^2))))
}
dev.off()

# ## A plot for one chamber
# with(subset(met.data.all, chamber %in% as.factor("C07")), plot(DateTime,Tair_al, type="l",lty=2,lwd=0.3))
# # with(subset(met.data.all, chamber %in% as.factor("C09") & DateTime > as.Date("2013-07-01") & DateTime < as.Date("2013-09-01")), plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3))
# with(subset(met.data.all, chamber %in% as.factor("C11") & DateTime > as.Date("2013-04-01") & DateTime < as.Date("2013-09-01")), plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3))
# with(subset(met.data.all, chamber %in% as.factor("C12") & DateTime > as.Date("2013-04-01") & DateTime < as.Date("2013-09-01")), plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3))
# met.data.all.c00 = subset(met.data.all, chamber %in% as.factor("C11") & Date == as.Date("2012-12-16"))
# with(subset(met.data.DectoMar.sub, Date == as.Date("2013-02-27")), plot(DateTime,Tair_al, type="p",lty=2,lwd=0.3))
# met.data.all.sub = subset(met.data.all, chamber %in% as.factor("C07")) 


# Replace chamber 03 PAR with mean of chamber 01 and 02
met.data.all.c03 = subset(met.data.all, chamber %in% as.factor(chambers[03]) & DateTime > as.Date("2014-01-01")) 
met.data.all.c01 = subset(met.data.all, chamber %in% as.factor(chambers[01]) & DateTime > as.Date("2014-01-01")) 
met.data.all.c02 = subset(met.data.all, chamber %in% as.factor(chambers[02]) & DateTime > as.Date("2014-01-01")) 
met.data.all.c03$PPFD_Avg = (met.data.all.c01$PPFD_Avg + met.data.all.c02$PPFD_Avg)/2 # Remove the negetive PAR values
# plot(met.data.all.c10$DateTime,met.data.all.c10$PPFD_Avg, type="l",lty=2,lwd=0.3)

index.c03 = which(met.data.all$chamber %in% as.factor(chambers[03]) & met.data.all$DateTime > as.Date("2014-01-01"))
met.data.all[index.c03,] = met.data.all.c03
# with(subset(met.data.all, chamber %in% as.factor("C10")), plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3))

# Replace chamber 08 PAR with mean of chamber 06 and 07
met.data.all.c08 = subset(met.data.all, chamber %in% as.factor(chambers[08]) & DateTime > as.Date("2013-02-01") & DateTime < as.Date("2013-08-01")) 
met.data.all.c06 = subset(met.data.all, chamber %in% as.factor(chambers[06]) & DateTime > as.Date("2013-02-01") & DateTime < as.Date("2013-08-01")) 
met.data.all.c07 = subset(met.data.all, chamber %in% as.factor(chambers[07]) & DateTime > as.Date("2013-02-01") & DateTime < as.Date("2013-08-01")) 
met.data.all.c08$PPFD_Avg = (met.data.all.c06$PPFD_Avg + met.data.all.c07$PPFD_Avg)/2 # Remove the negetive PAR values
# plot(met.data.all.c10$DateTime,met.data.all.c10$PPFD_Avg, type="l",lty=2,lwd=0.3)

index.c08 = which(met.data.all$chamber %in% as.factor(chambers[08]) & met.data.all$DateTime > as.Date("2013-02-01") & met.data.all$DateTime < as.Date("2013-08-01"))
met.data.all[index.c08,] = met.data.all.c08
# with(subset(met.data.all, chamber %in% as.factor("C10")), plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3))

# Replace chamber 10 PAR with mean of chamber 11 and 12
met.data.all.c10 = subset(met.data.all, chamber %in% as.factor(chambers[10]) & DateTime > as.Date("2013-04-01") & DateTime < as.Date("2013-09-01")) 
met.data.all.c11 = subset(met.data.all, chamber %in% as.factor(chambers[11]) & DateTime > as.Date("2013-04-01") & DateTime < as.Date("2013-09-01")) 
met.data.all.c12 = subset(met.data.all, chamber %in% as.factor(chambers[12]) & DateTime > as.Date("2013-04-01") & DateTime < as.Date("2013-09-01")) 
met.data.all.c10$PPFD_Avg = (met.data.all.c11$PPFD_Avg + met.data.all.c12$PPFD_Avg)/2 # Remove the negetive PAR values
# plot(met.data.all.c10$DateTime,met.data.all.c10$PPFD_Avg, type="l",lty=2,lwd=0.3)

index.c10 = which(met.data.all$chamber %in% as.factor(chambers[10]) & met.data.all$DateTime > as.Date("2013-04-01") & met.data.all$DateTime < as.Date("2013-09-01"))
met.data.all[index.c10,] = met.data.all.c10
# with(subset(met.data.all, chamber %in% as.factor("C10")), plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3))

#----------------------------------------------------------------------------------------------------------------
keeps <- c("Date", "chamber", "T_treatment", "leafArea")
la.initial = cue.day[ , keeps, drop = FALSE]
names(la.initial)[4] = "leafarea"

la.final.sub = subset(la.final, Date < "2013-09-14" & Date >= "2012-12-04")
la.final.sub = la.final.sub[-4]

la.all = rbind(la.initial, la.final.sub)

# Gap-filled LA for chamber 07 during the period 14-09-2013 to 02-10-2013
la.all.c07 = subset(la.all, chamber %in% as.factor("C07"))
dates = data.frame(Date=unique(la.all$Date))
la.all.c07 <- merge(la.all.c07, dates, by.x='Date', by.y='Date', all.x=T, all.y=T)
la.all.c07$chamber = la.all.c07$chamber[1]
la.all.c07$T_treatment = la.all.c07$T_treatment[1]
la.all.c07$leafarea = na.approx(la.all.c07$leafarea)
# Merge the gap-filled data
toBeRemoved = which(la.all$chamber %in% as.factor("C07"))
la.all = la.all[-toBeRemoved,]
la.all = rbind(la.all, la.all.c07)



met.la.data.all = merge(met.data.all,la.all, by=c("Date", "chamber"), all=TRUE) 
# units: LA = cm^2; PPFV_Avg = mu mol m-2 s-1; Tair_al = deg C; RH_al = %; VPD_al = kPa
met.la.data.all = met.la.data.all[order(met.la.data.all$DateTime),]

# write csv file with daily inputs of GPP, Ra, LA
write.csv(met.la.data.all, file = "processed_data/met.la.data.all.04-10-2012.csv", row.names = FALSE)
met.la.data.all = subset(met.la.data.all, Date >= as.Date("2012-12-12"))
write.csv(met.la.data.all, file = "processed_data/met.la.data.all.csv", row.names = FALSE)

#----------------------------------------------------------------------------------------------------------------

# for (i in 1:nlevels(chambers)) {
#   with(subset(met.la.data.all, chamber %in% as.factor(chambers[i]) & Date > as.Date("2013-09-10") & Date < as.Date("2013-10-10")), 
#        plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3,main=paste(chambers[i]),
#             xlab="",ylab=expression("Leaf area"~(cm^2))))
# }

#----------------------------------------------------------------------------------------------------------------
# Do some plotting
png("output/9.Tair_data.png", units="px", width=5000, height=2500, res=250)
par(mfrow=c(3,4))
  for (i in 1:nlevels(chambers)) {
    with(subset(met.la.data.all, chamber %in% as.factor(chambers[i])), 
         plot(DateTime,Tair_al,type="l",lty=2,lwd=0.3,main=paste(chambers[i]),xlab="",
              ylab=expression("Air Temperature "(degree~C))))
  }
dev.off()

png("output/10.VPD_data.png", units="px", width=5000, height=2500, res=250)
par(mfrow=c(3,4), mar=c(5,4.5,3,1))
for (i in 1:nlevels(chambers)) {
  with(subset(met.la.data.all, chamber %in% as.factor(chambers[i])), 
       plot(DateTime,VPD_al, type="l",lty=2,lwd=0.3,main=paste(chambers[i]),
              xlab="",ylab="VPD (kPa)"))
}
dev.off()

png("output/11.PAR_data.png", units="px", width=5000, height=2500, res=250)
par(mfrow=c(3,4), mar=c(5,4.5,3,1))
for (i in 1:nlevels(chambers)) {
  with(subset(met.la.data.all, chamber %in% as.factor(chambers[i])), 
       plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3,main=paste(chambers[i]),
                   xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
}
dev.off()

png("output/12.LA_data.png", units="px", width=5000, height=2500, res=250)
par(mfrow=c(3,4), mar=c(5,4.5,3,1))
for (i in 1:nlevels(chambers)) {
  with(subset(met.la.data.all, chamber %in% as.factor(chambers[i])), 
       plot(Date,leafarea, type="l",lwd=3,main=paste(chambers[i]),
          xlab="Height (cm)",ylab=expression("Leaf area"~(cm^2))))
}
dev.off()

#----------------------------------------------------------------------------------------------------------------
# Merge daily LA data with modeled GPP values for both treatments
la.data.final <- summaryBy(leafarea+Tair_al ~ Date+T_treatment, data=met.la.data.all, FUN=c(mean,standard.error))
names(la.data.final)[3:6] = c("LA", "Tair", "LA_SE", "Tair_SE")
la.data.final = subset(la.data.final, Date >= as.Date("2012-12-12") & Date <= as.Date("2014-05-26"))

gpp.la.data.final = merge(gpp.data.final,la.data.final,by=c("Date", "T_treatment"), all=TRUE)

#----------------------------------------------------------------------------------------------------------------
#- plot W/A GPP and W/A LAI data over time
GPP_warmed = subset(gpp.la.data.final, gpp.la.data.final$T_treatment %in% as.factor("elevated"))
GPP_ambient = subset(gpp.la.data.final, gpp.la.data.final$T_treatment %in% as.factor("ambient"))
GPP_ratio = GPP_warmed
GPP_ratio$GPP = GPP_warmed$GPP / GPP_ambient$GPP
GPP_ratio$Date = as.POSIXlt(GPP_ratio$Date,tz="AEST")
daterange=c(as.POSIXlt(min(GPP_ratio$Date)),as.POSIXlt(max(GPP_ratio$Date)))

#- plot W/A LAI data over time
LA_warmed = subset(gpp.la.data.final, gpp.la.data.final$T_treatment %in% as.factor("elevated"))
LA_ambient = subset(gpp.la.data.final, gpp.la.data.final$T_treatment %in% as.factor("ambient"))
LA_ratio = LA_warmed
LA_ratio$LA = LA_warmed$LA / LA_ambient$LA
LA_ratio$Date = as.POSIXlt(LA_ratio$Date,tz="AEST")

png("output/1.GPP_LA_ratio.png", units="px", width=5000, height=2500, res=250)
par(mfrow=c(2,1), mar=c(5,4.5,3,1))
plot(GPP_ratio$GPP~as.Date(GPP_ratio$Date),data=GPP_ratio,xlab="",ylab="GPP ratio (Warmed/Ambient)",pch=1,
     type="p",lwd=3,cex=0.3,cex.lab=1)
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%d-%m-%Y")

plot(LA_ratio$LA~as.Date(LA_ratio$Date),data=LA_ratio,xlab="",ylab="LA ratio (Warmed/Ambient)",pch=1,
     type="p",lwd=3,cex=0.3,cex.lab=1)
axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%d-%m-%Y")
dev.off()

png("output/1.GPP_Tair_2013_Summer.png", units="px", width=5000, height=2500, res=250)
par(mfrow=c(2,1), mar=c(5,4.5,3,1))
plot(gpp.la.data.final$GPP~gpp.la.data.final$Tair,data=gpp.la.data.final,xlab=expression("Daily Temperature "(degree~C)),
     ylab=expression(GPP~"(g C "*d^"-1"*")"),pch=1,main=paste("Entire experiment"),
     type="p",lwd=3,cex=0.3,cex.lab=1)
gpp.la.data.final.sub = subset(gpp.la.data.final, Date >= as.Date("2012-12-12") & Date <= as.Date("2013-06-01"))
plot(gpp.la.data.final.sub$GPP~gpp.la.data.final.sub$Tair,data=gpp.la.data.final.sub,xlab=expression("Daily Temperature "(degree~C)),
     ylab=expression(GPP~"(g C "*d^"-1"*")"),pch=1,main=paste("Dec 2012 to May 2013"),
     type="p",lwd=3,cex=0.3,cex.lab=1)
dev.off()

png("output/9.1.Tair_2013_Summer.png", units="px", width=5000, height=2500, res=250)
par(mfrow=c(1,2), mar=c(5,4.5,3,1))
gpp.la.data.final.sub = subset(gpp.la.data.final, Date >= as.Date("2012-12-12") & Date <= as.Date("2013-03-01"))
plot(subset(gpp.la.data.final.sub$Tair, gpp.la.data.final.sub$T_treatment %in% as.factor("elevated"))~
       subset(gpp.la.data.final.sub$Tair, gpp.la.data.final.sub$T_treatment %in% as.factor("ambient")),xlab=expression(Ambient~daily~temperature~(degree*C)),
     ylab=expression(Warmed~daily~temperature~(degree*C)),pch=1,main=paste("2013 Summer Dec 2012 to Feb 2013"),
     type="p",lwd=3,cex=0.3,cex.lab=1)
x = c(10,30)
y = c(13,33)
segmentInf <- function(xs, ys){
  fit <- lm(ys~xs)
  abline(fit)
}
segmentInf(x,y)

met.la.data.all.sub = subset(met.la.data.all, Date >= as.Date("2013-01-01") & Date <= as.Date("2013-01-31") & chamber %in% as.factor(c("C01","C02")))
plot(subset(met.la.data.all.sub$Tair, met.la.data.all.sub$T_treatment %in% as.factor("elevated"))~
       subset(met.la.data.all.sub$Tair, met.la.data.all.sub$T_treatment %in% as.factor("ambient")),xlab=expression(Ambient~hourly~temperature~(degree*C)),
     ylab=expression(Warmed~hourly~temperature~(degree*C)),pch=1,main=paste("2013 January - Chamber 01 vs Chamber 02"),
     type="p",lwd=3,cex=0.3,cex.lab=1)
segmentInf(x,y)
dev.off()

# font.size = 12
# plots = list()
# pd <- position_dodge(0)
# plots[[1]] = ggplot(data=gpp.la.data.final.sub, aes(x=Date, y=Tair, group = interaction(T_treatment), colour=T_treatment)) + 
#   geom_point(position=pd) +
#   # geom_errorbar(position=pd, aes(ymin=GPP-GPP_SE, ymax=GPP+GPP_SE), colour="grey", width=1) +
#   geom_line(position=pd, data = gpp.la.data.final.sub, aes(x = Date, y = Tair, group = interaction(T_treatment), colour=T_treatment)) +
#   ylab(expression(Average~daily~temperature~(degree*C))) +
#   scale_x_date(date_labels="%d %b %y",date_breaks  ="2 week",limits = c(min(gpp.la.data.final.sub$Date), max(gpp.la.data.final.sub$Date))) +
#   labs(colour="Temperature") +
#   scale_color_manual(labels = c("ambient", "warmed"), values = c("blue", "red")) +
#   theme_bw() + 
#   theme(legend.title = element_text(colour="black", size=font.size)) +
#   theme(legend.text = element_text(colour="black", size = font.size)) +
#   theme(legend.position = c(0.2,0.85), legend.box = "horizontal") + theme(legend.key.height=unit(0.8,"line")) +
#   theme(legend.key = element_blank()) +
#   theme(text = element_text(size=font.size)) +
#   theme(axis.title.x = element_blank()) +
#   theme(axis.title.y = element_text(size = font.size, vjust=0.3)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
# 
# dev.off()
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# import site weather data, take only Tair, format date stuff
# i = chambers[1]
for(i in chambers) { 
  files <- list.files(path = paste0("raw_data/wtc3_soil_temperature/",i), pattern = ".dat", full.names=TRUE)
  temp <- lapply(files, fread, sep=",")
  soil.data.ch <- rbindlist( temp )
  soil.data.ch = data.frame(soil.data.ch)
  keeps <- c("TIMESTAMP", "SoilTemp_Avg.1.", "SoilTemp_Avg.2.")
  soil.data.ch = soil.data.ch[ , keeps, drop = FALSE]
  names(soil.data.ch) = c("DateTime", "SoilTemp_Avg.1.", "SoilTemp_Avg.2.")
  
  soil.data.ch$DateTime = as.POSIXct(soil.data.ch$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
  soil.data.ch = soil.data.ch[complete.cases(soil.data.ch), ]
  soil.data.ch$Date = as.Date(soil.data.ch$DateTime)
  soil.data.ch$hour <- cut(soil.data.ch$DateTime, breaks = "hour")
  
  soil.data.ch$SoilTemp_Avg.1. = as.numeric(soil.data.ch$SoilTemp_Avg.1.)
  soil.data.ch$SoilTemp_Avg.2. = as.numeric(soil.data.ch$SoilTemp_Avg.2.)
  soil.data.ch$SoilTemp = (soil.data.ch$SoilTemp_Avg.1. + soil.data.ch$SoilTemp_Avg.2.)/2
  soil.data.ch$chamber = as.factor(i)
  
  getmeans  <- function(soil.data.ch) c(SoilTemp = mean(soil.data.ch$SoilTemp,na.rm=TRUE))
  soil.data.ch.means <- ddply(soil.data.ch, .(chamber, hour), getmeans)
  soil.data.ch.means$hour <- as.POSIXct(soil.data.ch.means$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
  
  if (i %in% chambers[1]) {
    soil.data = soil.data.ch.means
  } else {
    soil.data = rbind(soil.data,soil.data.ch.means)
  }
}

names(soil.data)[2] = c("DateTime")
soil.data$SoilTemp[soil.data$SoilTemp <= 0] = NA # Remove the negetive PAR values
soil.data$Date = as.Date(soil.data$DateTime)
soil.data = subset(soil.data, Date <= as.Date("2014-05-26"))

met.la.soil.data.all = merge(met.la.data.all,soil.data, by=c("DateTime", "Date", "chamber"), all=TRUE) 

# xyplot(SoilTemp ~ Tair_al, data = met.la.soil.data.all, na.rm=T)
model.soil.temp = lm(SoilTemp ~ Tair_al, data = met.la.soil.data.all)
summary(model.soil.temp)

eq.soil = function(x){coefficients(model.soil.temp)[1] + coefficients(model.soil.temp)[2] * x }
index = complete.cases(met.la.soil.data.all$SoilTemp)
met.la.soil.data.all$SoilTemp[!index] = eq.soil(met.la.soil.data.all$Tair_al[!index])

# write csv file with daily inputs of Met data, GPP, Ra, LA
write.csv(met.la.soil.data.all, file = "processed_data/met.la.soil.data.all.csv", row.names = FALSE)

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# # Compare the WTC-3 PAR data with ROS weather station PAR data to find out the reason behind variation in PAR between two consecutive summers of 2013 and 2014
# # Seems like the WTC-3 surface might got dusty over time which reduced the PAR in 2014 summer compared to previous summer of 2013
# files <- list.files(path = "raw_data/ROS_metdata", pattern = ".dat", full.names = TRUE)
# temp <- lapply(files, fread, sep=",")
# ros.par <- rbindlist( temp )
# ros.par = data.frame(ros.par)
# keeps <- c("TIMESTAMP", "PPFD_Avg")
# ros.par = ros.par[ , keeps, drop = FALSE]
# names(ros.par) = c("DateTime", "PPFD_Avg")
# 
# ros.par$DateTime = as.POSIXct(ros.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# ros.par = ros.par[complete.cases(ros.par), ]
# ros.par$Date = as.Date(ros.par$DateTime)
# ros.par$hour <- cut(ros.par$DateTime, breaks = "hour")
# ros.par$hour <- as.POSIXct(ros.par$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
# 
# # ros.par.sub = subset(ros.par, Date >= as.Date("2012-12-12") & Date <= as.Date("2014-05-26"))
# ros.par.sub = subset(ros.par, Date >= as.Date("2012-12-18") & Date <= as.Date("2013-02-11"))
# ros.par.sub$PPFD_Avg = as.numeric(as.character(ros.par.sub$PPFD_Avg))
# 
# getmeans  <- function(ros.par.sub) c(PPFD_Avg = mean(ros.par.sub$PPFD_Avg,na.rm=TRUE))
# ros.par.sub.means <- ddply(ros.par.sub, .(hour), getmeans)
# ros.par.sub.means$hour <- as.POSIXct(ros.par.sub.means$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
# names(ros.par.sub.means)[1] = c("DateTime")
# 
# png("output/11.PAR_data_test.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(1,3), mar=c(5,4.5,3,1))
# for (i in c(1,12)) {
#   with(subset(met.la.data.all, chamber %in% as.factor(chambers[i])), 
#        plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3,main=paste(chambers[i]),
#             xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
# }
# plot(ros.par.sub.means$DateTime,ros.par.sub.means$PPFD_Avg, type="l",lty=2,lwd=0.3,main=paste("ROS_WS"),xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# dev.off()
# 
# daterange=c(as.POSIXlt(min(met.la.data.all$DateTime)),as.POSIXlt(max(met.la.data.all$DateTime)))
# png("output/11.PAR_data_test_C10.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(1,1), mar=c(5,4.5,3,1))
# for (i in c(10)) {
#   with(subset(met.la.data.all, chamber %in% as.factor(chambers[i])), 
#        plot(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#             xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%d-%b-%Y")
# }
# lines(ros.par.sub.means$DateTime,ros.par.sub.means$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#       xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# legend("topright",legend=paste(c("Chamber 10","ROS WS")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# dev.off()
# 
# png("output/11.PAR_data_test_C12.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(1,1), mar=c(5,4.5,3,1))
# for (i in c(12)) {
#   with(subset(met.la.data.all, chamber %in% as.factor(chambers[i])), 
#        plot(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#             xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%d-%b-%Y")
# }
# lines(ros.par.sub.means$DateTime,ros.par.sub.means$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#       xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# legend("topright",legend=paste(c("Chamber 12","ROS WS")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# dev.off()
# 
# #----------------------------------------------------------------------------------------------------------------
# met.la.data.all.sub = subset(met.la.data.all, Date >= as.Date("2012-12-12") & Date <= as.Date("2013-02-11"))
# 
# png("output/11.PAR_data_test.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(1,3), mar=c(5,4.5,3,1))
# for (i in c(1,12)) {
#   with(subset(met.la.data.all.sub, chamber %in% as.factor(chambers[i])), 
#        plot(DateTime,PPFD_Avg, type="l",lty=2,lwd=0.3,main=paste(chambers[i]),
#             xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
# }
# plot(ros.par.sub.means$DateTime,ros.par.sub.means$PPFD_Avg, type="l",lty=2,lwd=0.3,main=paste("ROS_WS"),xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# dev.off()
# 
# daterange=c(as.POSIXlt(min(met.la.data.all.sub$DateTime)),as.POSIXlt(max(met.la.data.all.sub$DateTime)))
# png("output/11.PAR_data_test_C1_ROS_WTC.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(1,2), mar=c(5,4.5,3,1))
# for (i in c(1)) {
#   with(subset(met.la.data.all.sub, chamber %in% as.factor(chambers[i])), 
#        plot(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#             xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="week"), format="%d-%b-%Y")
#   met.la.data.all.1 = subset(met.la.data.all.sub, chamber %in% as.factor(chambers[i]))
# }
# lines(ros.par.sub.means$DateTime,ros.par.sub.means$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#       xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# legend("topright",legend=paste(c("Chamber 1","ROS WS")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# 
# plot(ros.par.sub.means$PPFD_Avg,met.la.data.all.1$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("ROS PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# model10 <- lm(met.la.data.all.1$PPFD_Avg~ros.par.sub.means$PPFD_Avg)
# abline(model10,col="red",lwd=2)
# coef(model10)
# abline(0,1,col="blue",lwd=2)
# dev.off()
# 
# png("output/11.PAR_data_test_C10_ROS_WTC.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(1,2), mar=c(5,4.5,3,1))
# for (i in c(10)) {
#   with(subset(met.la.data.all.sub, chamber %in% as.factor(chambers[i])), 
#        plot(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#             xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="week"), format="%d-%b-%Y")
#   met.la.data.all.10 = subset(met.la.data.all.sub, chamber %in% as.factor(chambers[i]))
# }
# lines(ros.par.sub.means$DateTime,ros.par.sub.means$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#       xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# legend("topright",legend=paste(c("Chamber 10","ROS WS")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# 
# plot(ros.par.sub.means$PPFD_Avg,met.la.data.all.10$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("ROS PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# model10 <- lm(met.la.data.all.10$PPFD_Avg~ros.par.sub.means$PPFD_Avg)
# abline(model10,col="red",lwd=2)
# coef(model10)
# abline(0,1,col="blue",lwd=2)
# dev.off()
# 
# png("output/11.PAR_data_test_C12_ROS_WTC.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(1,2), mar=c(5,4.5,3,1))
# for (i in c(12)) {
#   with(subset(met.la.data.all.sub, chamber %in% as.factor(chambers[i])), 
#        plot(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#             xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="week"), format="%d-%b-%Y")
# }
# lines(ros.par.sub.means$DateTime,ros.par.sub.means$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#       xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# legend("topright",legend=paste(c("Chamber 12","ROS WS")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# 
# met.la.data.all.12 = subset(met.la.data.all.sub, chamber %in% as.factor("C12"))
# plot(ros.par.sub.means$PPFD_Avg,met.la.data.all.12$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("ROS PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# model12 <- lm(met.la.data.all.12$PPFD_Avg~ros.par.sub.means$PPFD_Avg)
# abline(model12,col="red",lwd=2)
# coef(model12)
# abline(0,1,col="blue",lwd=2)
# dev.off()
# 
# plot(met.la.data.all.10$PPFD_Avg,met.la.data.all.12$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("WTC10 PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC12 PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))

#----------------------------------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------------------------------
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
# WTC08.par <- read.table("raw_data/WTC_PAR/WTC08_Table2_20130228.dat",header=T,sep=",")
# WTC08.par$chamber = as.factor("C08")
# WTC09.par <- read.table("raw_data/WTC_PAR/WTC09_Table2_20130228.dat",header=T,sep=",")
# WTC09.par$chamber = as.factor("C09")
# WTC10.par <- read.table("raw_data/WTC_PAR/WTC10_Table2_20130228.dat",header=T,sep=",")
# WTC10.par$chamber = as.factor("C10")
# WTC11.par <- read.table("raw_data/WTC_PAR/WTC11_Table2_20130228.dat",header=T,sep=",")
# WTC11.par$chamber = as.factor("C11")
# WTC12.par <- read.table("raw_data/WTC_PAR/WTC12_Table2_20130228.dat",header=T,sep=",")
# WTC12.par$chamber = as.factor("C12")
# 
# WTC.par = rbind(WTC01.par,WTC02.par,WTC03.par,WTC04.par,WTC05.par,WTC06.par,WTC07.par,WTC08.par,WTC09.par,WTC10.par,WTC11.par,WTC12.par)
# keeps <- c("TIMESTAMP", "chamber", "PPFD_Avg")
# WTC.par = WTC.par[ , keeps, drop = FALSE]
# names(WTC.par) = c("DateTime", "chamber", "PPFD_Avg")
# # 
# WTC.par$DateTime = as.POSIXct(WTC.par$DateTime,format="%Y-%m-%d %H:%M:%S",tz="GMT")
# WTC.par = WTC.par[complete.cases(WTC.par), ]
# WTC.par$Date = as.Date(WTC.par$DateTime)
# WTC.par$hour <- cut(WTC.par$DateTime, breaks = "hour")
# WTC.par$hour <- as.POSIXct(WTC.par$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
# # 
# # ros.par.sub = subset(ros.par, Date >= as.Date("2012-12-12") & Date <= as.Date("2014-05-26"))
# WTC.par.sub = subset(WTC.par, Date >= as.Date("2012-12-18") & Date <= as.Date("2013-02-11"))
# WTC.par.sub$PPFD_Avg = as.numeric(as.character(WTC.par.sub$PPFD_Avg))
# 
# # WTC.par.sub = WTC.par
# getmeans  <- function(WTC.par.sub) c(PPFD_Avg = mean(WTC.par.sub$PPFD_Avg,na.rm=TRUE))
# WTC.par.sub.means <- ddply(WTC.par.sub, .(hour,chamber), getmeans)
# WTC.par.sub.means$hour <- as.POSIXct(WTC.par.sub.means$hour, format="%Y-%m-%d %H:%M:%S",tz="GMT")
# names(WTC.par.sub.means)[1] = c("DateTime")
# 
# # WTC.par.sub = subset(WTC.par.sub.means, DateTime >= as.Date("2012-12-18") & DateTime <= as.Date("2013-02-11"))
# daterange=c(as.POSIXlt(min(WTC.par.sub.means$DateTime)),as.POSIXlt(max(WTC.par.sub.means$DateTime)))
# 
# met.la.data.all.1 = subset(met.la.data.all.1, Date >= as.Date("2012-12-18") & Date <= as.Date("2013-02-11"))
# plots = list() 
# # png("output/11.PAR_data_test_C02_WTC.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(2,1), mar=c(5,4.5,3,1))
# plot(met.la.data.all.1$DateTime,met.la.data.all.1$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#      xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# for (i in c(2)) {
#   with(subset(WTC.par.sub.means, chamber %in% as.factor(chambers[i])),
#        lines(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#              xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="week"), format="%d-%b-%Y")
# }
# legend("topright",legend=paste(c("Chamber 2 inside","WTC outside")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# 
# WTC.par.sub.2 = subset(WTC.par.sub.means, chamber %in% as.factor("C02"))
# plot(met.la.data.all.1$PPFD_Avg,WTC.par.sub.2$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("WTC outside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC 2 inside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# model2 <- lm(WTC.par.sub.2$PPFD_Avg ~ poly(met.la.data.all.1$PPFD_Avg, 2))
# # points(ros.par.sub.means$PPFD_Avg, fitted(model10), col='red', pch=20)
# lines(sort(met.la.data.all.1$PPFD_Avg), fitted(model2)[order(met.la.data.all.1$PPFD_Avg)], col='red', type='l',lwd=2)
# # coef(model2)
# abline(0,1,col="blue",lwd=2)
# plots[[1]] = recordPlot()
# 
# # png("output/11.PAR_data_test_C05_WTC.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(2,1), mar=c(5,4.5,3,1))
# plot(met.la.data.all.1$DateTime,met.la.data.all.1$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#      xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# for (i in c(5)) {
#   with(subset(WTC.par.sub.means, chamber %in% as.factor(chambers[i])),
#        lines(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#              xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="week"), format="%d-%b-%Y")
# }
# legend("topright",legend=paste(c("Chamber 5 inside","WTC outside")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# 
# WTC.par.sub.5 = subset(WTC.par.sub.means, chamber %in% as.factor("C05"))
# plot(met.la.data.all.1$PPFD_Avg,WTC.par.sub.5$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("WTC outside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC 5 inside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# model5 <- lm(WTC.par.sub.5$PPFD_Avg ~ poly(met.la.data.all.1$PPFD_Avg, 2))
# # points(ros.par.sub.means$PPFD_Avg, fitted(model10), col='red', pch=20)
# lines(sort(met.la.data.all.1$PPFD_Avg), fitted(model5)[order(met.la.data.all.1$PPFD_Avg)], col='red', type='l',lwd=2)
# # coef(model5)
# abline(0,1,col="blue",lwd=2)
# plots[[2]] = recordPlot()
# 
# par(mfrow=c(2,1), mar=c(5,4.5,3,1))
# plot(met.la.data.all.1$DateTime,met.la.data.all.1$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#      xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# for (i in c(8)) {
#   with(subset(WTC.par.sub.means, chamber %in% as.factor(chambers[i])),
#        lines(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#              xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="week"), format="%d-%b-%Y")
# }
# legend("topright",legend=paste(c("Chamber 8 inside","WTC outside")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# 
# WTC.par.sub.8 = subset(WTC.par.sub.means, chamber %in% as.factor("C08"))
# plot(met.la.data.all.1$PPFD_Avg,WTC.par.sub.8$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("WTC outside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC 8 inside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# model8 <- lm(WTC.par.sub.12$PPFD_Avg ~ poly(met.la.data.all.1$PPFD_Avg, 2))
# # points(met.la.data.all.1$PPFD_Avg, fitted(model12), col='red', pch=20)
# lines(sort(met.la.data.all.1$PPFD_Avg), fitted(model8)[order(met.la.data.all.1$PPFD_Avg)], col='red', type='l',lwd=2)
# coef(model8)
# abline(0,1,col="blue",lwd=2)
# plots[[3]] = recordPlot()
# 
# # png("output/11.PAR_data_test_C10_WTC.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(2,1), mar=c(5,4.5,3,1))
# plot(met.la.data.all.1$DateTime,met.la.data.all.1$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#      xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# for (i in c(10)) {
#   with(subset(WTC.par.sub.means, chamber %in% as.factor(chambers[i])),
#        lines(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#              xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="week"), format="%d-%b-%Y")
# }
# legend("topright",legend=paste(c("Chamber 10 inside","WTC outside")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# 
# WTC.par.sub.10 = subset(WTC.par.sub.means, chamber %in% as.factor("C10"))
# plot(met.la.data.all.1$PPFD_Avg,WTC.par.sub.12$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("WTC outside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC 10 inside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# model10 <- lm(WTC.par.sub.12$PPFD_Avg ~ poly(met.la.data.all.1$PPFD_Avg, 2))
# # points(ros.par.sub.means$PPFD_Avg, fitted(model10), col='red', pch=20)
# lines(sort(met.la.data.all.1$PPFD_Avg), fitted(model10)[order(met.la.data.all.1$PPFD_Avg)], col='red', type='l',lwd=2)
# # coef(model10)
# abline(0,1,col="blue",lwd=2)
# plots[[4]] = recordPlot()
# 
# # png("output/11.PAR_data_test_C12_WTC.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(2,1), mar=c(5,4.5,3,1))
# plot(met.la.data.all.1$DateTime,met.la.data.all.1$PPFD_Avg,col='red',type="l",lty=2,lwd=0.3,xaxt="n",
#      xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# for (i in c(12)) {
#   with(subset(WTC.par.sub.means, chamber %in% as.factor(chambers[i])),
#        lines(DateTime,PPFD_Avg,col='blue',type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#              xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
#   axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="week"), format="%d-%b-%Y")
# }
# legend("topright",legend=paste(c("Chamber 12 inside","WTC outside")),col=c('blue','red'),lty=1,bty="n",cex=1.5,pt.cex=2)
# 
# WTC.par.sub.12 = subset(WTC.par.sub.means, chamber %in% as.factor("C12"))
# plot(met.la.data.all.1$PPFD_Avg,WTC.par.sub.12$PPFD_Avg,type="p",lty=1,main=paste("Compare PAR measurements"),
#      xlab=expression("WTC outside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})),ylab=expression("WTC 12 inside PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1})))
# model12 <- lm(WTC.par.sub.12$PPFD_Avg ~ poly(met.la.data.all.1$PPFD_Avg, 2))
# # points(met.la.data.all.1$PPFD_Avg, fitted(model12), col='red', pch=20)
# lines(sort(met.la.data.all.1$PPFD_Avg), fitted(model12)[order(met.la.data.all.1$PPFD_Avg)], col='red', type='l',lwd=2)
# coef(model12)
# abline(0,1,col="blue",lwd=2)
# plots[[5]] = recordPlot()
# 
# pdf(file = "output/11.WTC3_PAR_comparison.pdf")
# plots[[1]]
# plots[[2]]
# plots[[3]]
# plots[[4]]
# plots[[5]]
# dev.off()
# 
# WTC.par.sub.means.week = subset(WTC.par.sub.means, DateTime >= as.Date("2013-01-23") & DateTime <= as.Date("2013-01-30") )
# png("output/11.PAR_WTC_oneweek.png", units="px", width=5000, height=2500, res=250)
# par(mfrow=c(1,1), mar=c(5,4.5,3,1))
# with(subset(WTC.par.sub.means.week, chamber %in% as.factor(chambers[1])),
#      plot(DateTime,PPFD_Avg,col="red",type="l",lty=1,lwd=0.3,xaxt="n",main=paste("Compare PAR measurements"),
#            xlab="",ylab=expression("PAR"~(mu ~ mol ~ m^{-2} ~ s^{-1}))))
# for (i in c(2:12)) {
#   with(subset(WTC.par.sub.means.week, chamber %in% as.factor(chambers[i])),
#        lines(DateTime,PPFD_Avg,col=as.factor(chambers[i]),type="l",lty=1,lwd=0.3))
# }
# axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%d-%b-%Y")
# legend("topright",legend=paste(c(as.character(chambers))),col=as.factor(chambers),lty=1,bty="n",cex=1,pt.cex=1)
# dev.off()
