#- Read and process the LA dataset, return a dataframe of gapfilled LA data before the flux measurement starts.
# get the tree size data
la.count.raw <- read.csv("raw_data/WTC_TEMP_CM_TREE-HEIGHT-DIAMETER_20121120-20140527_L1_V2.CSV")
la.count.raw$chamber_n <- as.numeric(substr(la.count.raw$chamber,start=2,stop=3))
la.count.raw$DateTime <- as.Date(la.count.raw$DateTime)

#do some processing to get d2h
la.count <- base::subset(la.count.raw,select=c("DateTime","chamber_n","chamber","T_treatment","Water_treatment",
                                               "Total_Leaf_Number","Plant_height"))
# la.count <- subset(la.count, chamber_n %in% 11 & DateTime %in% as.Date("2013-12-24"))
la.count <- subset(la.count, chamber_n<=12 & DateTime <= as.Date("2013-12-24"))

# get the leaf size data
la.size <- read.csv("raw_data/WTC_TEMP_CM_BIOMASS-LEAFAREA_20130909_L1.csv")
la.size <- summaryBy(leafsize~chamber,data=la.size,FUN=max,na.rm=T,keep.names=T)

la.count <- merge(la.count, la.size, by="chamber")
la.count$leafarea = la.count$Total_Leaf_Number * la.count$leafsize/10000 # unit conversion to m2

keeps = c("DateTime","chamber","T_treatment","Plant_height","leafarea")
la.count = la.count[ , keeps, drop = FALSE]
names(la.count) = c("Date","chamber","T_treatment","height","leafarea")

#- get daily sums from the partitioned hourly data
cue.list <- returnCUE.day(dat=data.hr.p) # get daily sums from hourly data
cue.day <- cue.list[[1]]                # extract chamber values on each day
# with(subset(cue.day, chamber %in% as.factor("C12")), plot(Date,leafArea))

cue.day.sub = subset(cue.day, Date %in% as.Date(c("2013-09-17","2013-10-01","2013-10-15","2013-10-30","2013-11-12","2013-11-26","2013-12-10")))
keeps = c("Date","chamber","T_treatment","leafArea")
cue.day.sub = cue.day.sub[ , keeps, drop = FALSE]
names(cue.day.sub)[4] = c("leafarea")

cue.day.sub = merge(subset(la.count[c("Date","chamber","T_treatment","height")], Date %in% as.Date(c("2013-09-17","2013-10-01","2013-10-15","2013-10-30","2013-11-12","2013-11-26","2013-12-10"))),cue.day.sub,by=c("Date","chamber","T_treatment"),all=TRUE)
la.count.merged = rbind(la.count[!la.count$Date %in% as.Date(c("2013-09-17","2013-10-01","2013-10-15","2013-10-30","2013-11-12","2013-11-26","2013-12-10")), ],cue.day.sub)

png("output/8.LA_extimates.png", units="px", width=2000, height=1000, res=130)
par(mfrow=c(3,4), mar=c(5,4.5,3,1))
for (i in 1:nlevels(chambers)){
  # la.count.sub$X15 = na.spline(la.count.sub$X15)
  # la.count.sub$dh = la.count.sub$height*la.count.sub$X15
  # 
  # cue.day.sub = subset(cue.day, chamber %in% chambers[i])
  # keeps = c("Date","chamber","T_treatment","leafArea")
  # cue.day.sub = cue.day.sub[ , keeps, drop = FALSE]
  # 
  # plot(la.count.sub$DateTime,la.count.sub$leafarea)
  # plot(cue.day.sub$Date,cue.day.sub$leafArea)
  # 
  # keeps = c("DateTime","chamber","T_treatment","leafarea")
  # la.count.sub = la.count.sub[ , keeps, drop = FALSE]
  # 
  # la.count.sub = rbind(la.count.sub[c("DateTime","chamber_n","chamber","T_treatment","leafarea")],
  #                      cue.day.sub[c("DateTime","chamber_n","chamber","T_treatment","leafarea")],by=c("DateTime","chamber","T_treatment"))
  # la.count.sub$leafarea = na.spline(la.count.sub$leafarea)
  
  la.count.chamber = subset(la.count.merged, chamber %in% chambers[i])
  #- fit LA vs H & D equation  
  # la1 <- lm(leafarea~dh,data=la.count.chamber)
  # la1 <- lm(leafarea~log(height),data=la.count.chamber)
  # la1 <- lm(leafarea ~ 0 + height,data=la.count.chamber)
  la1 <- lm(leafarea ~ poly(height,3),data=la.count.chamber)
  summary(la1)
  visreg(la1, "height", line=list(col="red"), points=list(cex=1.5, pch=1), band=FALSE, rug=FALSE, main=paste(chambers[i]),
         xlab="Height (cm)",ylab=expression("Leaf area"~(cm^2)))
  # lines(x, predict(la1, data.frame(x=x)), col='red')
  # 
  # eq = function(x){coefficients(la1)[1] + coefficients(la1)[2] * (x) + coefficients(la1)[3] * (x^2) + coefficients(la1)[4] * (x^3)}
  la.count.sub = la.count.chamber
  la.count.sub$leafarea = predict(la1, data.frame(height=la.count.chamber$height))
  z = la.count.sub$leafarea <= 0
  la.count.sub$leafarea[z] <- la.count.chamber$leafarea[sum(z, na.rm=TRUE)]
  # la.count.sub$leafarea = la.count.sub$leafcount * la.count.sub$leafsize/100 # unit conversion to cm2
  # plot(la.count.sub$height,la.count.sub$leafarea)
  la.count.sub = subset(la.count.sub, Date <= as.Date("2013-09-17"))
  
  # ggplot(data=litterfall, aes(x=Date, y=litter, group = chamber, colour=chamber)) + 
  #   geom_point() +
  #   geom_line(data = litterfall, aes(x = Date, y = litter, group = chamber, colour=chamber)) +
  #   geom_smooth(data = litterfall, aes(x = Date, y = litter, group = chamber, colour=chamber), method="lm")
  # 
  
  # create dataframe for all days
  alldates <- rep(seq.Date(from=as.Date(range(la.count.sub$Date)[1]),to=as.Date(range(la.count.sub$Date)[2]),by="day"),1)
  chamber <- rep(chambers[i],length(alldates))
  datedf <- data.frame(chamber=chamber,Date=alldates)                                                                                             
  
  #merge data in with dataframe of all days
  la.count.sub <-merge(la.count.sub,datedf,all=T,by=c("chamber","Date"))
  la.count.sub$T_treatment <- rep(la.count.sub$T_treatment[1],length(alldates))
  la.count.sub$height <- na.approx(la.count.sub$height)
  la.count.sub$leafarea <- na.approx(la.count.sub$leafarea)
  
  if (i == 1) {
    la.final = la.count.sub
  } else {
    la.final = rbind(la.final,la.count.sub)
  }
}
dev.off()


#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# # subset by Date range of experiment
# met.data.la <- subset(met.data.raw[, c("chamber","Date","time","Tair_al","SoilTemp","PPFD_Avg")], Date  >= "2012-12-05" & Date  <= "2013-09-17")
# met.data.la$chamber = as.factor(met.data.la$chamber)
# met.data.la = merge(met.data, unique(height.dia[,c("chamber","T_treatment")]), by="chamber")

# met.data.la$period <- ifelse(met.data.la$PPFD_Avg>2,"Day","Night")


