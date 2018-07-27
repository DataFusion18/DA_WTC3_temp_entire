# Estimate the partitioning of the non-structural C among different organs using data from WTC-4 experiment
# on similar-sized seedlings of a related species (Eucalyptus parramattensis).

#  read the harvest WTC-4 TNC data
tnc.wtc4 <- read.xls("raw_data/Samples_WTC4_heatwave_10.10.2017.xlsx",1)
# wtc4.treatment = read.csv("raw_data/WTC_TEMP-PARRA_CM_TREE-HEIGHT-DIAMETER_20151028-20161124_L1.csv")
# names(tnc.wtc4)[3] = "chamber"
# tnc.wtc4 = merge(tnc.wtc4, wtc4.treatment[,c("chamber","T_treatment")], by="chamber")
keeps = c("Tissue","Chamber","Sugar..mg.g.","Starch..mg.g.","Whole.organ.total..g.")
tnc.wtc4 = tnc.wtc4[ , keeps, drop = FALSE]
tnc.wtc4$tnc..mg.g. = tnc.wtc4$Sugar..mg.g. + tnc.wtc4$Starch..mg.g.

# tnc.wtc4 <- summaryBy(tnc..mg.g. ~ Tissue, data=tnc.wtc4, FUN=c(mean,standard.error))
# tnc.wtc4$Tissue = as.character(tnc.wtc4$Tissue)
# tnc.wtc4$tnc.ratio = tnc.wtc4$tnc..mg.g..mean / sum(tnc.wtc4$tnc..mg.g..mean) * 100
# tnc.wtc4$tnc.ratio.SE = tnc.wtc4$tnc..mg.g..standard.error / tnc.wtc4$tnc..mg.g..mean * tnc.wtc4$tnc.ratio
# tnc.wtc4[c(6:7),1] = as.character(c("wood", "root"))
# tnc.wtc4[6,c(2:5)] = tnc.wtc4[2,c(2:5)] + tnc.wtc4[3,c(2:5)]
# tnc.wtc4[7,c(2:5)] = tnc.wtc4[1,c(2:5)] + tnc.wtc4[5,c(2:5)]
# tnc.wtc4 = tnc.wtc4[c(4,6,7),c(1,4,5)]
# # tnc.wtc4 <- summaryBy(tnc..mg.g. + Whole.organ.total..g. ~ Tissue, data=tnc.wtc4, FUN=c(mean,standard.error))

# There was no statistically significant difference in TNC partitioning across the treatments as John tested.
tnc.wtc4 <- summaryBy(Whole.organ.total..g. ~ Tissue, data=tnc.wtc4, FUN=c(mean,standard.error))
tnc.wtc4$Tissue = as.character(tnc.wtc4$Tissue)
tnc.wtc4$tnc.ratio = tnc.wtc4$Whole.organ.total..g..mean / sum(tnc.wtc4$Whole.organ.total..g..mean) * 100
tnc.wtc4$tnc.ratio.SE = tnc.wtc4$Whole.organ.total..g..standard.error / tnc.wtc4$Whole.organ.total..g..mean * tnc.wtc4$tnc.ratio

# tnc.wtc4 <- summaryBy(tnc..mg.g. ~ Tissue, data=tnc.wtc4, FUN=c(mean,standard.error))
# tnc.wtc4$Tissue = as.character(tnc.wtc4$Tissue)
# tnc.wtc4$tnc.ratio = tnc.wtc4$tnc..mg.g..mean / sum(tnc.wtc4$tnc..mg.g..mean) * 100
# tnc.wtc4$tnc.ratio.SE = tnc.wtc4$tnc..mg.g..standard.error / tnc.wtc4$tnc..mg.g..mean * tnc.wtc4$tnc.ratio
tnc.wtc4[c(6:7),1] = as.character(c("wood", "root"))
tnc.wtc4[6,c(2:5)] = tnc.wtc4[1,c(2:5)] + tnc.wtc4[5,c(2:5)]
tnc.wtc4[7,c(2:5)] = tnc.wtc4[2,c(2:5)] + tnc.wtc4[3,c(2:5)]
tnc.wtc4 = tnc.wtc4[c(4,6,7),c(1,4,5)]

# TNC partitioning according to Duan's experiment (used in Pot experiment)
carbohydrates.duan = read.csv("raw_data/Duan_carbohydrates.csv")
carbohydrates.duan = subset(carbohydrates.duan,CO2 == 400 & Temp == "Amb" & Water == "Well watered")
carbohydrates.duan$tnc = carbohydrates.duan$StarchW + carbohydrates.duan$SolSugW # Unit = mg of tnc per g of dry weight biomass
carbohydrates.duan$tnc = carbohydrates.duan$tnc / 10 # Unit = % of dry weight biomass
harvest.duan = read.csv("raw_data/Duan_harvest.csv")
harvest.duan = subset(harvest.duan,CO2 == 400 & Temp == "Amb" & Water == "Well watered")

leaf.tnc = subset(carbohydrates.duan,Organ == "Leaf") # Unit = % of dry weight leafmass
stem.tnc = subset(carbohydrates.duan,Organ == "Stem") # Unit = % of dry weight stemmass
root.tnc = subset(carbohydrates.duan,Organ == "Root") # Unit = % of dry weight rootmass

tnc.pot = data.frame(harvest.duan$Date,harvest.duan$LeafDW,leaf.tnc$tnc,harvest.duan$StemDW,stem.tnc$tnc,harvest.duan$RootDW,root.tnc$tnc)
names(tnc.pot) <- c("Date","leaf.C","leaf.tnc.C","stem.C","stem.tnc.C","root.C","root.tnc.C") 

tnc.pot$total.leaf.tnc.C = tnc.pot$leaf.tnc.C * tnc.pot$leaf.C / 100 # Unit = gC
tnc.pot$total.stem.tnc.C = tnc.pot$stem.tnc.C * tnc.pot$stem.C / 100 # Unit = gC
tnc.pot$total.root.tnc.C = tnc.pot$root.tnc.C * tnc.pot$root.C / 100 # Unit = gC

tnc.pot$leaf_to_all = tnc.pot$total.leaf.tnc.C / (tnc.pot$total.leaf.tnc.C + tnc.pot$total.stem.tnc.C + tnc.pot$total.root.tnc.C) * 100 # Unit = %
tnc.pot$stem_to_all = tnc.pot$total.stem.tnc.C / (tnc.pot$total.leaf.tnc.C + tnc.pot$total.stem.tnc.C + tnc.pot$total.root.tnc.C) * 100 # Unit = %
tnc.pot$root_to_all = tnc.pot$total.root.tnc.C / (tnc.pot$total.leaf.tnc.C + tnc.pot$total.stem.tnc.C + tnc.pot$total.root.tnc.C) * 100 # Unit = %

# tnc.pot <- summaryBy(tnc.pot ~ Organ, data=carbohydrates.duan, FUN=c(mean,standard.error))

# # unit conversion from g of tnc per g of dry weight biomass to gC in tnc per gC in plant biomass
# # 1 g of tnc has 0.4 gC and 1 g of dry weight biomass has 0.48 gC
# carbohydrates.duan$tnc = carbohydrates.duan$tnc * (0.4/c1) # Unit = gC in tnc / gC in plant biomass
# tnc.pot <- summaryBy(tnc ~ Organ, data=carbohydrates.duan, FUN=c(mean,standard.error))
# tnc.pot[,4] = tnc.pot[,2] / sum(tnc.pot[,2]) * 100
# tnc.pot[,3] = tnc.pot[,3] / tnc.pot[,2] * tnc.pot[,4]
# tnc.pot = tnc.pot[,-2]
# names(tnc.pot) = c("organ","tnc.SE","tnc.mean")
#-----------------------------------------------------------------------------------------
##### Total TNC calculation considering tree organ biomass partitioning
# leaf.tnc = subset(carbohydrates,Organ == "Leaf") # Unit = % of dry weight leafmass
# stem.tnc = subset(carbohydrates,Organ == "Stem") # Unit = % of dry weight stemmass
# root.tnc = subset(carbohydrates,Organ == "Root") # Unit = % of dry weight rootmass
# 
# tnc.pot = data.frame(leaf.tnc$tnc,stem.tnc$tnc,root.tnc$tnc)
# names(tnc.pot) <- c("leaf.tnc.C","stem.tnc.C","root.tnc.C") 
# 
# tnc.pot[7,] = colMeans(tnc.pot)
# tnc.pot[8,] = tnc.pot[7,] / rowSums(tnc.pot[7,]) * 100
# # tnc.pot = sapply(tnc.pot,function(x)sd(x)/sqrt(length(x)))


# Consider a linear change over time for TNC partitioning
tnc.partitioning = data.frame(matrix(ncol = 7, nrow = 531))
names(tnc.partitioning) = c("Date","foliage","wood","root","foliage_SE","wood_SE","root_SE")
# tnc.partitioning$Date = as.Date(as.Date("2013-09-17"):as.Date("2014-05-26"))
tnc.partitioning$Date = as.Date(as.Date("2012-12-12"):as.Date("2014-05-26"))

tnc.partitioning$foliage[1] <- mean(tnc.pot$leaf_to_all)/100 # TNC partitioning according to Duan's experiment (used in Pot experiment)
tnc.partitioning$wood[1] <- mean(tnc.pot$stem_to_all)/100 # TNC partitioning according to Duan's experiment (used in Pot experiment)
tnc.partitioning$root[1] <- mean(tnc.pot$root_to_all)/100 # TNC partitioning according to Duan's experiment (used in Pot experiment)
tnc.partitioning$foliage_SE[1] <- se(tnc.pot$leaf_to_all)/100 # TNC partitioning according to Duan's experiment (used in Pot experiment)
tnc.partitioning$wood_SE[1] <- se(tnc.pot$stem_to_all)/100 # TNC partitioning according to Duan's experiment (used in Pot experiment)
tnc.partitioning$root_SE[1] <- se(tnc.pot$root_to_all)/100 # TNC partitioning according to Duan's experiment (used in Pot experiment)

# tnc.partitioning[1,2:4] <- tnc.pot$tnc.mean/100 # TNC partitioning according to Duan's experiment (used in Pot experiment)
# tnc.partitioning[1,5:7] <- tnc.pot$tnc.SE/100 # TNC partitioning according to Duan's experiment (used in Pot experiment)
tnc.partitioning[nrow(tnc.partitioning),2:4] <- tnc.wtc4$tnc.ratio/100
tnc.partitioning[nrow(tnc.partitioning),5:7] <- tnc.wtc4$tnc.ratio.SE/100

for (i in 2:ncol(tnc.partitioning)) {
  tnc.partitioning[1:nrow(tnc.partitioning),i] = seq(tnc.partitioning[1,i], tnc.partitioning[nrow(tnc.partitioning),i], length.out = nrow(tnc.partitioning))
}

# tnc.partitioning = subset(tnc.partitioning, Date >= as.Date("2013-09-17") & Date <= as.Date("2014-02-12"))
