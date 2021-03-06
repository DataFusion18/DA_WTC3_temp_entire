# Script to process leaf litter data
litterfall = read.csv("raw_data/WTC_TEMP_CM_LEAFLITTER_20130913-20140528_L1.csv")
litterfall$startDate = as.Date(litterfall$startDate)
litterfall$collectionDate = as.Date(litterfall$collectionDate)
litterfall$startDate[litterfall$startDate == as.Date("1201-11-27")] = as.Date("2013-11-27")
litterfall$Date <- (litterfall$startDate + ((litterfall$collectionDate - litterfall$startDate) / 2))
# litterfall = subset(litterfall, Date >= as.Date("2013-09-14") & Date <= as.Date("2014-05-27"))
# litterfall = subset(litterfall, Date >= as.Date("2013-09-14") & Date <= as.Date("2014-02-12"))

# convert to data.table in place
litterfall = setDT(litterfall)
# dcast and do individual sums
litterfall.cast = dcast.data.table(litterfall, chamber ~ Date, value.var = 'litter', fun.aggregate = sum)

# cumsum to estimate dummy litter pool
litterfall.cum <- litterfall.cast[, as.list(cumsum(unlist(.SD))), by = chamber]
# # no cumsum to estimate litter flush
# litterfall.cum = litterfall.cast

litterfall.cast.melt <- melt(litterfall.cast, id.vars = "chamber")
litterfall.cast.melt = merge(litterfall.cast.melt, unique(treeMass[,c("chamber","T_treatment")]), all=TRUE)
# litterfall.cum.melt$chamber_type = as.factor( ifelse(litterfall.cum.melt$chamber %in% drought.chamb, "drought", "watered") )
names(litterfall.cast.melt)[2:3] = c("Date","litter")
litterfall.cast.melt$Date = as.Date(litterfall.cast.melt$Date)
litterfall.cast.melt$litter = litterfall.cast.melt$litter/14
litterfall.cast.melt = summaryBy(litter ~ Date+T_treatment, data=litterfall.cast.melt, FUN=c(mean,standard.error))
names(litterfall.cast.melt)[3:4] = c("litter","litter_SE")

# litterfall.cum.melt <- melt(litterfall.cum, id.vars = "chamber")
# litterfall.cum.melt = merge(litterfall.cum.melt, unique(treeMass[,c("chamber","T_treatment")]), all=TRUE)
# # litterfall.cum.melt$chamber_type = as.factor( ifelse(litterfall.cum.melt$chamber %in% drought.chamb, "drought", "watered") )
# names(litterfall.cum.melt)[2:3] = c("Date","litter")
# litterfall.cum.melt$Date = as.Date(litterfall.cum.melt$Date)
# litterfall.cum.melt = summaryBy(litter ~ Date+T_treatment, data=litterfall.cum.melt, FUN=c(mean,standard.error))
# names(litterfall.cum.melt)[3:4] = c("litter","litter_SE")


# litterfall.initial = data.frame(Date = rep(as.Date("2013-09-17"), 2),
#                                 T_treatment = rep(unique(data$T_treatment), each=1),
#                                 litter = rep(0.1,2),
#                                 litter_SE = rep(0.01,2))
# litterfall.cast.melt = rbind(litterfall.initial, litterfall.cast.melt)
litterfall.cast.melt$litter = litterfall.cast.melt$litter * c1 # unit conversion from gDM to gC
litterfall.cast.melt$litter_SE = litterfall.cast.melt$litter_SE * c1 # unit conversion from gDM to gC
