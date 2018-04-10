# Read biomass
biomass.sub = subset(data.biomass, Date %in% as.Date(c("2013-09-17","2014-05-26")))

biomass.sub$biomass = biomass.sub$RM + biomass.sub$LM + biomass.sub$WM

biomass.sub = subset(biomass.sub, Date %in% as.Date("2014-05-26"))
biomass.sub[c(3:4),c("LM","RM","WM")] = biomass.sub[,c("LM","RM","WM")] / biomass.sub$biomass[c(1:2)]
biomass.sub[c(3:4),c("LM","RM","WM")]

#-------------------------------------------------------------------------------------
# Calculate foliage litterfall rate
# Script to process leaf litter data
litterfall = read.csv("raw_data/WTC_TEMP_CM_LEAFLITTER_20130913-20140528_L1.csv")
litterfall$startDate = as.Date(litterfall$startDate)
litterfall$collectionDate = as.Date(litterfall$collectionDate)
litterfall$Date <- (litterfall$startDate + ((litterfall$collectionDate - litterfall$startDate) / 2))
litterfall = subset(litterfall, Date >= as.Date("2013-09-14") & Date <= as.Date("2014-05-27"))
# litterfall = subset(litterfall, Date >= as.Date("2013-09-14") & Date <= as.Date("2014-02-12"))

# convert to data.table in place
litterfall = setDT(litterfall)
# dcast and do individual sums
litterfall.cast = dcast.data.table(litterfall, chamber ~ Date, value.var = 'litter', fun.aggregate = sum)
litterfall.cast <- melt(litterfall.cast, id.vars = "chamber")
litterfall.cast = merge(litterfall.cast, unique(treeMass[,c("chamber","T_treatment")]), all=TRUE)
# litterfall.cum.melt$chamber_type = as.factor( ifelse(litterfall.cum.melt$chamber %in% drought.chamb, "drought", "watered") )
names(litterfall.cast)[2:3] = c("Date","litter")
litterfall.cast$Date = as.Date(litterfall.cast$Date)
litterfall.cast = summaryBy(litter ~ Date+T_treatment, data=litterfall.cast, FUN=c(mean,standard.error))
names(litterfall.cast)[3:4] = c("litter","litter_SE")

# Foliage mass data
lm = data.biomass[,c("Date","T_treatment","LM","LM_SE")]
lm.litter = merge(lm,litterfall.cast, by=c("Date","T_treatment"), all = TRUE)
lm.litter[,c(3:4)] = na.spline(lm.litter[,c(3:4)])
lm.litter$litterrate = lm.litter$litter / lm.litter$LM / 14

mean(lm.litter$litterrate[which(lm.litter$T_treatment %in% as.factor("ambient"))], na.rm = T)
mean(lm.litter$litterrate[which(lm.litter$T_treatment %in% as.factor("elevated"))], na.rm = T)

