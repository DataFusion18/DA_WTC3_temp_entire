# Read biomass
biomass.sub = subset(data.biomass, Date %in% as.Date(c("2013-09-17","2014-05-26")))

biomass.sub$biomass = biomass.sub$RM + biomass.sub$LM + biomass.sub$WM

biomass.sub = subset(biomass.sub, Date %in% as.Date("2014-05-26"))
biomass.sub[c(3:4),c("LM","RM","WM")] = biomass.sub[,c("LM","RM","WM")] / biomass.sub$biomass[c(1:2)]
