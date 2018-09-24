# Construct the definitive E. tereticornis allometry from the WTC3, GREAT, Sink-limited Pot & ROS experiments    

#----------------------------------------------------------------------------------------------------------------
# WTC3 experiment:  
# Import initial seedling data
harvest.wtc3 <- read.csv("raw_data/WTC_TEMP_CM_HARVEST-CANOPY_20140526-20140528_L1_v1.csv")
harvest.wtc3$WM = harvest.wtc3$BranchDM + harvest.wtc3$StemDM
data.harvest.wtc3 = summaryBy(TotalLeafDM+WM ~ chamber, data=harvest.wtc3, FUN=sum, na.rm=T)

# Read harvest rootmass data form WTC3 experiment
rootmass.harvest = read.csv("raw_data/WTC_TEMP_CM_HARVEST-ROOTS_20140529-20140606_L1_v1.csv")
data.harvest.wtc3 = merge(data.harvest.wtc3, rootmass.harvest[,c("chamber","RootDMtotal")], by=c("chamber"))

# Read H and D data from WTC3 experiment
data.harvest.wtc3 = merge(data.harvest.wtc3,subset(height.dia, Date %in% as.Date("2014-05-27")),by=c("chamber"))
data.harvest.wtc3$T_treatment = NULL
names(data.harvest.wtc3)[1:7] = c("treatment","LM","WM","RM","Date","D","H")
data.harvest.wtc3$exp = as.factor("wtc3")

# read the estimated biomass data
data.biomass.wtc3 = merge(rootmass, treeMass.sum, by = c("Date", "T_treatment"), all=TRUE)
# la.wtc3 = summaryBy(leafarea ~ Date+T_treatment, data=la.final, FUN=c(mean,standard.error))
# names(la.wtc3)[3:4] = c("LA","LA_SE")
# data.biomass.wtc3 = merge(data.biomass.wtc3,la.wtc3,by=c("Date", "T_treatment"))

height.dia.final$Date[height.dia.final$Date == as.Date("2014-05-27")] = as.Date("2014-05-26")
data.biomass.wtc3 = merge(data.biomass.wtc3,height.dia.final,by=c("Date", "T_treatment"),all=T)

keeps <- c("Date", "T_treatment", "LM","WM","RM","height","diameter")
data.biomass.wtc3 = data.biomass.wtc3[ , keeps, drop = FALSE]
names(data.biomass.wtc3)[6:7] = c("H","D")
data.biomass.wtc3$exp = as.factor("wtc3")
data.biomass.wtc3$datatype = as.factor("estimated")
data.biomass.wtc3$D2H = data.biomass.wtc3$D * data.biomass.wtc3$D * data.biomass.wtc3$H / 100 # unit conversion: mm2 to cm2
# data.biomass.wtc3$LM = data.biomass.wtc3$LM / 0.48
# data.biomass.wtc3$WM = data.biomass.wtc3$WM / 0.48
# data.biomass.wtc3$RM = data.biomass.wtc3$RM / 0.48


#----------------------------------------------------------------------------------------------------------------
# Sink-limited Pot experiment:  
# Import all harvest data
# Import initial seedling data
initial.harvest.pot <- read.csv("data_pot_experiment/seedling_initial.csv")
initial.harvest.pot$Date = as.Date("2013-01-21")
keeps <- c("Date", "leaf_mass", "wood_mass", "root_mass", "height", "diameter_15")
initial.harvest.pot = initial.harvest.pot[ , keeps, drop = FALSE]
initial.harvest.pot$volume = as.factor("initial")
names(initial.harvest.pot)[2:6] = c("LM","WM","RM","height","diameter")

# Import harvested seedling data for all different treatments
end.harvest.pot <- read.csv("data_pot_experiment/seedling_mass.csv")
end.harvest.pot$Date = as.Date("2013-05-21")
end.harvest.pot$RM = end.harvest.pot$fineroot + end.harvest.pot$coarseroot
keeps <- c("plot", "pot", "Date", "volume", "leafmass", "stemmass", "RM")
end.harvest.pot = end.harvest.pot[ , keeps, drop = FALSE]

# Import harvested seedling height and diameter
height.dia.pot = read.csv("data_pot_experiment/height.dia.harvest.pot.csv")
height.dia.pot$Date = as.Date(height.dia.pot$Date)
height.dia.pot = subset(height.dia.pot, Date %in% as.Date("2013-05-21"))

end.harvest.pot = merge(end.harvest.pot, height.dia.pot, by=c("plot","pot","Date","volume"), all=T)
end.harvest.pot = end.harvest.pot[ ,c(-1,-2)]
names(end.harvest.pot)[3:7] = c("LM","WM","RM","height","diameter")
end.harvest.pot$volume = as.factor(end.harvest.pot$volume)

data.harvest.pot = rbind(initial.harvest.pot, end.harvest.pot)
# data.harvest.pot = summaryBy(LM+WM+RM+height+diameter ~ Date+volume, data=data.harvest.pot, FUN=c(mean,standard.error), na.rm=T)
# names(data.harvest.pot)[3:12] = c("LM","SM","RM","H","D","LM_SE","SM_SE","RM_SE","H_SE","D_SE")
names(data.harvest.pot)[5:7] = c("H","D","volume")
data.harvest.pot$exp = as.factor("pot")
data.harvest.pot$treatment = as.factor( ifelse(data.harvest.pot$volume %in% as.factor("initial"), "initial", 
                                               ifelse(data.harvest.pot$volume %in% as.factor("5"), "vol5", 
                                               ifelse(data.harvest.pot$volume %in% as.factor("10"), "vol10", 
                                               ifelse(data.harvest.pot$volume %in% as.factor("15"), "vol15",
                                               ifelse(data.harvest.pot$volume %in% as.factor("20"), "vol20",
                                               ifelse(data.harvest.pot$volume %in% as.factor("25"), "vol25", "vol35")))))))
data.harvest.pot$volume = NULL

# # Import bi-weekly LM, SM, H, D and initial/harvest RM data
# Mleaf.data.pot = read.csv("processed_data_pot/Cleaf_weekly_data.csv") # Unit gC
# Mleaf.data.pot$Date = as.Date(Mleaf.data.pot$Date)
# Mstem.data.pot = read.csv("processed_data_pot/Cstem_weekly_data.csv") # Unit gC
# Mstem.data.pot$Date = as.Date(Mstem.data.pot$Date)
# Mroot.data.pot = read.csv("processed_data_pot/Croot_twice_data.csv") # Unit gC
# Mroot.data.pot$Date = as.Date(Mroot.data.pot$Date)
# LA.data.pot = read.csv("processed_data_pot/LA_daily_data.csv") # Unit m^2
# LA.data.pot$Date = as.Date(LA.data.pot$Date)
# height.dia.pot = read.csv("processed_data_pot/height.dia.final.csv") # Unit H = 
# height.dia.pot$Date = as.Date(height.dia.pot$Date)
# 
# data.biomass.pot = merge(Mleaf.data.pot,Mstem.data.pot,by=c("Date","volume"),all=T)
# data.biomass.pot = merge(data.biomass.pot,Mroot.data.pot,by=c("Date","volume"),all=T)
# data.biomass.pot = merge(data.biomass.pot,LA.data.pot,by=c("Date","volume"),all=T)
# data.biomass.pot = merge(data.biomass.pot,height.dia.pot,by=c("Date","volume"),all=T)
# names(data.biomass.pot)[3:10] = c("LM","LM_SE","SM","SM_SE","RM","RM_SE","LA","LA_SE")

#----------------------------------------------------------------------------------------------------------------
# GREAT experiment:   
# Import harvest data
data.harvest.great = read.csv("processed_data_great/data_harvest_great.csv") # Unit gC
data.harvest.great$exp = as.factor("great")
# data.harvest.great = summaryBy(Leafmass+Stemmass+Rootmass+Height+D ~ Date+Room+exp, data=data.harvest.great, FUN=c(mean,standard.error), na.rm=T)
# names(data.harvest.great)[4:13] = c("LM","SM","RM","H","D","LM_SE","SM_SE","RM_SE","H_SE","D_SE")
data.harvest.great = data.harvest.great[ ,c(-8,-9)]
names(data.harvest.great)[2:7] = c("room","H","D","WM","RM","LM")
# data.harvest.great$room = as.factor(data.harvest.great$room)
data.harvest.great$treatment = as.factor( ifelse(data.harvest.great$room == 1, "temp18", 
                                               ifelse(data.harvest.great$room == 2, "temp22.5", 
                                                      ifelse(data.harvest.great$room  == 3, "temp25", 
                                                             ifelse(data.harvest.great$room  == 4, "temp28.5",
                                                                    ifelse(data.harvest.great$room  == 5, "temp32", "temp35.5"))))))
data.harvest.great$room = NULL

# # Import bi-weekly modelled biomass and measured allometry data
# data.biomass.all.great = read.csv("processed_data_great/data_harvest_great.csv") # Unit gC
# data.biomass.all.great$Date = as.Date(data.biomass.all.great$Date)
# 
# data.biomass.great = summaryBy(Leafmass+Stemmass+Rootmass+Leafarea+Height+D ~ Date+Room, data=data.biomass.all.great, FUN=c(mean,standard.error))
# names(data.biomass.great)[3:14] = c("LM","SM","RM","LA","height","diameter","LM_SE","SM_SE","RM_SE","LA_SE","height_SE","diameter_SE")

#----------------------------------------------------------------------------------------------------------------
# ROS experiment:   
# Import measured allometry data
data.growth.ros = read.csv("data_ros/ROS_MD_PM_GROWTH_20111222-20130906_L1.csv") # Unit
plot_treenumber = data.growth.ros[c("plot", "treenumber", "plottype", "trt")]
plot_treenumber = unique(plot_treenumber)
# # consider only the Euc Tereticornis and well-watered tress
# data.growth.ros = subset(data.growth.ros,sp %in% as.factor("tereticornis") & trt %in% as.factor("water"))
# data.growth.ros$D = (data.growth.ros$diam.1+data.growth.ros$diam.2)/2/10 # unit conversion: mm to cm
# data.growth.ros$date = as.Date(data.growth.ros$date)
# keeps <- c("date", "plot", "ht", "D")
# data.growth.ros = data.growth.ros[ , keeps, drop = FALSE]
# data.growth.ros = data.growth.ros[complete.cases(data.growth.ros), ]
# data.growth.ros = data.growth.ros[!is.na(as.numeric(as.character(data.growth.ros$ht))),]
# data.growth.ros$ht = as.numeric(paste(data.growth.ros$ht))
# 
# height.dia.ros = summaryBy(ht+D ~ date+plot, data=data.growth.ros, FUN=c(mean,standard.error), na.rm=T)
# names(height.dia.ros)[1:6] = c("Date","plot","height","D","height_SE","D_SE")

# import harvest data
harvest.1.ros = read.csv("data_ros/ROS_MD_PM_HARVEST_20120401_L1.csv") # Unit 
harvest.2.ros = read.csv("data_ros/ROS_MD_PM_HARVEST_20120827_L1.csv") # Unit 
harvest.3.ros = read.csv("data_ros/ROS_MD_PM_HARVEST_20130215_L1.csv") # Unit 
harvest.4.ros = read.csv("data_ros/ROS_MD_PM_HARVEST_20130506-20130925.L1.csv") # Unit 
keeps <- c("date", "sp", "treenumber", "rootsDM", "stemDM", "branchDM", "leafDM", "ht", "diam.1", "diam.2")
harvest.1.ros = harvest.1.ros[ , keeps, drop = FALSE]
harvest.2.ros = harvest.2.ros[ , keeps, drop = FALSE]
harvest.3.ros = harvest.3.ros[ , keeps, drop = FALSE]
harvest.4.ros = harvest.4.ros[ , keeps, drop = FALSE]

harvest.ros = rbind(harvest.1.ros,harvest.2.ros,harvest.3.ros,harvest.4.ros)
harvest.ros = merge(harvest.ros, plot_treenumber, by="treenumber", all=F)

harvest.ros = subset(harvest.ros, sp %in% as.factor("eucalyptus tereticornis") & trt %in% as.factor("water"))
harvest.ros$ht = as.numeric(paste(harvest.ros$ht))
harvest.ros$diam.1 = as.numeric(paste(harvest.ros$diam.1))
harvest.ros$diam.2 = as.numeric(paste(harvest.ros$diam.2))

harvest.ros$D = (harvest.ros$diam.1+harvest.ros$diam.2)/2 # unit conversion: mm to cm
harvest.ros$WM = (harvest.ros$stemDM+harvest.ros$branchDM)/2 
# harvest.ros$LA = (harvest.ros$tenleafarea.1+harvest.ros$tenleafarea.2)/2/10000 # unit conversion: cm^2 to m^2
harvest.ros$exp = as.factor("ros")
keeps <- c("date", "plot", "ht", "D", "leafDM", "WM", "rootsDM", "exp")
data.harvest.ros = harvest.ros[ , keeps, drop = FALSE]
names(data.harvest.ros) = c("Date","plot","H","D","LM","WM","RM","exp")
# data.harvest.ros$treatment = as.factor(data.harvest.ros$treatment)

data.harvest.ros$treatment = as.factor( ifelse(data.harvest.ros$plot == 1, "plot1", 
                                        ifelse(data.harvest.ros$plot == 2, "plot2", 
                                        ifelse(data.harvest.ros$plot  == 3, "plot3", 
                                        ifelse(data.harvest.ros$plot  == 4, "plot4",
                                        ifelse(data.harvest.ros$plot  == 5, "plot5", 
                                        ifelse(data.harvest.ros$plot == 6, "plot6", 
                                        ifelse(data.harvest.ros$plot == 7, "plot7", 
                                        ifelse(data.harvest.ros$plot  == 8, "plot8", 
                                        ifelse(data.harvest.ros$plot  == 9, "plot9",
                                        ifelse(data.harvest.ros$plot  == 10, "plot10",        
                                        ifelse(data.harvest.ros$plot  == 11, "plot11", "plot12"))))))))))))
data.harvest.ros$plot = NULL

#----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# Plotting all harvest data
data.harvest.all = rbind(data.harvest.pot,data.harvest.great,data.harvest.ros,data.harvest.wtc3)
# data.harvest.all = rbind(data.harvest.pot,data.harvest.great,data.harvest.ros)
data.harvest.all$D2H = data.harvest.all$D * data.harvest.all$D * data.harvest.all$H / 100 # unit conversion: mm2 to cm2
data.harvest.all$datatype = as.factor("harvest")
# data.harvest.all[,c("LM","WM","RM")] = data.harvest.all[,c("LM","WM","RM")] * c1 # unit conversion: gDM to gC

plots = list() 
par(mfrow = c(2, 2))
plot(data.harvest.all$D,data.harvest.all$LM,col=data.harvest.all$exp,main="Diameter vs Leafmass", pch=1, xlab="Diameter (mm)", ylab="Leafmass (g DM)")
lines(data.biomass.wtc3$D,data.biomass.wtc3$LM,type="p", col="red", pch='.')
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)

plot(data.harvest.all$H,data.harvest.all$LM,col=data.harvest.all$exp,main="Height vs Leafmass", pch=1, xlab="Height (cm)", ylab="Leafmass (g DM)")
lines(data.biomass.wtc3$H,data.biomass.wtc3$LM,type="p", col="red", pch='.')
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)

plot(data.harvest.all$D2H,data.harvest.all$LM,col=data.harvest.all$exp,main="D2H vs Leafmass", pch=1, xlab="D2H (cm3)", ylab="Leafmass (g DM)")
lines(data.biomass.wtc3$D2H,data.biomass.wtc3$LM,type="p", col="red", pch='.')
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)
plots[[1]] = recordPlot()

par(mfrow = c(2, 2))
plot(data.harvest.all$D,data.harvest.all$WM,col=data.harvest.all$exp,main="Diameter vs Woodmass", pch=1, xlab="Diameter (mm)", ylab="Woodmass (g DM)")
lines(data.biomass.wtc3$D,data.biomass.wtc3$WM,type="p", col="red", pch='.')
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)

plot(data.harvest.all$H,data.harvest.all$WM,col=data.harvest.all$exp,main="Height vs Woodmass", pch=1, xlab="Height (cm)", ylab="Woodmass (g DM)")
lines(data.biomass.wtc3$H,data.biomass.wtc3$WM,type="p", col="red", pch='.')
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)

plot(data.harvest.all$D2H,data.harvest.all$WM,col=data.harvest.all$exp,main="D2H vs Woodmass", pch=1, xlab="D2H (cm3)", ylab="Woodmass (g DM)")
lines(data.biomass.wtc3$D2H,data.biomass.wtc3$WM,type="p", col="red", pch='.')
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)
plots[[2]] = recordPlot()

par(mfrow = c(2, 2))
plot(data.harvest.all$D,data.harvest.all$RM,col=data.harvest.all$exp,main="Diameter vs Rootmass", pch=1, xlab="Diameter (mm)", ylab="Rootmass (g DM)")
lines(data.biomass.wtc3$D,data.biomass.wtc3$RM,type="p", col="red", pch=20)
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)

plot(data.harvest.all$H,data.harvest.all$RM,col=data.harvest.all$exp,main="Height vs Rootmass", pch=1, xlab="Height (cm)", ylab="Rootmass (g DM)")
lines(data.biomass.wtc3$H,data.biomass.wtc3$RM,type="p", col="red", pch=20)
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)

plot(data.harvest.all$D2H,data.harvest.all$RM,col=data.harvest.all$exp,main="D2H vs Rootmass", pch=1, xlab="D2H (cm3)", ylab="Rootmass (g DM)")
lines(data.biomass.wtc3$D2H,data.biomass.wtc3$RM,type="p", col="red", pch=20)
legend('topleft', c("WTC3 estimated", "Pot", "Great", "ROS", "WTC3"), col=c("red",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1,y.intersp=0.5)
plots[[3]] = recordPlot()

pdf(file = "output/1.tree_attributes_tereticornis.pdf")
plots[[1]]
plots[[2]]
plots[[3]]
dev.off()

#-----------------------------------------------------------------------------------------
# Fit a linear regression by stem dia and height (ignoring temperature variation)
wm1 <- lm(log(WM) ~ log(D) + log(H), data=data.harvest.all)
# Fit a linear regression by stem dia, height and their interaction with temperature (including temperature effect)
wm2 <- lm(log(WM) ~ log(D2H), data=data.harvest.all)
wm3 <- lm(log(WM) ~ poly(log(D2H),3,raw=T), data=data.harvest.all)
wm3 <- lm(log(WM) ~ log(D) + log(H) + log(D) * log(H), data=data.harvest.all)

# Fit a linear regression for Rootmass by stem dia and height (ignoring temperature variation)
rm1 <- lm(log(RM) ~ log(D) + log(H), data=data.harvest.all)
# Fit a linear regression by stem dia, height and their interaction with temperature (including temperature effect)
rm2 <- lm(log(RM) ~ log(D2H), data=data.harvest.all)
# rm3 <- lm((RM) ~ poly((D2H),2), data=data.harvest.all)
rm3 <- lm(log(RM) ~ poly(log(D2H),3,raw=T), data=data.harvest.all)
rm3 <- lm(log(RM) ~ log(D) + log(H) + log(D) * log(H), data=data.harvest.all)

# Fit a linear regression for Leafmass by stem dia and height (ignoring temperature variation)
lm1 <- lm(log(LM) ~ log(D) + log(H), data=data.harvest.all)
# Fit a linear regression by stem dia, height and their interaction with temperature (including temperature effect)
lm2 <- lm(log(LM) ~ log(D2H), data=data.harvest.all)
# lm3 <- lm((LM) ~ poly((D2H),2), data=data.harvest.all)
lm3 <- lm(log(LM) ~ poly(log(D2H),3,raw=T), data=data.harvest.all)
lm3 <- lm(log(LM) ~ log(D) + log(H) + log(D) * log(H), data=data.harvest.all)

# # Fit a linear regression for Leafarea by stem dia and height (ignoring temperature variation)
# la1 <- lm(log(Leafarea) ~ log(D) + log(Height), data=data.harvest.all)
# # Fit a linear regression by stem dia, height and their interaction with temperature (including temperature effect)
# la2 <- lm(log(Leafarea) ~ log(D) + log(Height) + log(D) : temp + log(Height) : temp, data=data.harvest.all)


# Plot predictions
layout(matrix(c(1,2,0,0,3,4),3,2,byrow=TRUE), widths=c(1,1), heights=c(10,1,10))
visreg(wm1, "D", overlay=TRUE)
visreg(wm1, "H", overlay=TRUE)
visreg(wm2, "D2H", overlay=TRUE,main="Log Linear")
visreg(wm3, "D", overlay=TRUE,main="Log Quadratic")
plots[[5]] = recordPlot()

layout(matrix(c(1,2,0,0,3,4),3,2,byrow=TRUE), widths=c(1,1), heights=c(10,1,10))
visreg(rm1, "D", overlay=TRUE)
visreg(rm1, "H", overlay=TRUE)
visreg(rm2, "D2H", overlay=TRUE,main="Log Linear")
visreg(rm3, "D2H", overlay=TRUE,main="Log Quadratic")
plots[[6]] = recordPlot()

layout(matrix(c(1,2,0,0,3,4),3,2,byrow=TRUE), widths=c(1,1), heights=c(10,1,10))
visreg(lm1, "D", overlay=TRUE)
visreg(lm1, "H", overlay=TRUE)
visreg(lm2, "D2H", overlay=TRUE,main="Log Linear")
visreg(lm3, "D2H", overlay=TRUE,main="Log Quadratic")
plots[[4]] = recordPlot()

# layout(matrix(c(1,2,0,0,3,4),3,2,byrow=TRUE), widths=c(1,1), heights=c(10,1,10))
# visreg(la1, "D", overlay=TRUE)
# visreg(la1, "Height", overlay=TRUE)
# visreg(la2, "D", by="temp", overlay=TRUE,legend=FALSE)
# visreg(la2, "Height", by="temp", overlay=TRUE)
# plots[[4]] = recordPlot()

pdf(file = "output/2.model_comparison.pdf")
plots[[4]]; plots[[5]]; plots[[6]]
dev.off()


#-----------------------------------------------------------------------------------------
# Save model sumary and stat comparison
sink("output/3.model_comparison.txt")
cat("Leafmass models:\n----------------\n### Linear regression with H and D (all data log-transformed):"); summary(lm1)
cat("\n### Linear regression with D2H (all data log-transformed):"); summary(lm2)
cat("\n### Linear regression with H, D and interaction between them (all data log-transformed):"); summary(lm3)
cat("### Comparison between both models:\n")
AIC(lm1, lm2, lm3); BIC(lm1, lm2, lm3)

cat("\n\nWoodmass models:\n----------------\n### Linear regression with H and D (all data log-transformed):");  summary(wm1)
cat("\n### Linear regression with D2H (all data log-transformed):"); summary(wm2)
cat("\n### Linear regression with H, D and interaction between them (all data log-transformed):"); summary(wm3)
cat("### Comparison between both models:\n")
AIC(wm1, wm2, wm3); BIC(wm1, wm2, wm3)

cat("\n\nRootmass models:\n----------------\n### Linear regression with H and D (all data log-transformed):"); summary(rm1)
cat("\n### Linear regression with D2H (all data log-transformed):"); summary(rm2)
cat("\n### Linear regression with H, D and interaction between them (all data log-transformed):"); summary(rm3)
cat("### Comparison between both models:\n")
AIC(rm1, rm2, rm3); BIC(rm1, rm2, rm3)

# cat("\n\nLeafarea models:\n----------------\n### Linear regression ignoring temperature variation:"); summary(la1)
# cat("\n### Linear regression considering interaction with temperature:"); summary(la2)
# cat("### Comparison between both models:\n")
# AIC(la1, la2); BIC(la1, la2)
sink()
#-----------------------------------------------------------------------------------------
# Estimate the biomass from the fitted linear regression equation
# Estimate the leafmass from the fitted linear regression equation
# eq = function(x,y){exp(coefficients(lm1)[1] + coefficients(lm1)[2] * log(x)  + coefficients(lm1)[3] * log(y))}
# data.biomass.wtc3$LM.modelled = eq(data.biomass.wtc3$D, data.biomass.wtc3$H)
eq = function(x,y){exp(coefficients(lm3)[1] + coefficients(lm3)[2] * log(x)  + coefficients(lm3)[3] * log(y)  + coefficients(lm3)[4] * log(x) * log(y))}
data.biomass.wtc3$LM.modelled = eq(data.biomass.wtc3$D, data.biomass.wtc3$H)

# eq = function(x){exp(coefficients(lm3)[1] + coefficients(lm3)[2] * log(x)  + coefficients(lm3)[3] * log(x) * log(x))}
# eq = function(x){exp(coefficients(lm3)[1] + coefficients(lm3)[2] * log(x)  + coefficients(lm3)[3] * log(x) * log(x) + coefficients(lm3)[4] * log(x) * log(x) * log(x))}
# eq = function(x){(coefficients(lm3)[1] + coefficients(lm3)[2] * (x)  + coefficients(lm3)[3] * (x) * (x))}
# Calculate all seedling leafmass from height and diameter using the linear model
# data.biomass.wtc3$LM.modelled = eq(data.biomass.wtc3$D2H)

# Estimate the woodmass from the fitted linear regression equation
# eq = function(x,y){exp(coefficients(wm1)[1] + coefficients(wm1)[2] * log(x)  + coefficients(wm1)[3] * log(y))}
eq = function(x){exp(coefficients(wm3)[1] + coefficients(wm3)[2] * log(x)  + coefficients(wm3)[3] * log(x) * log(x) + coefficients(wm3)[4] * log(x) * log(x) * log(x))}
data.biomass.wtc3$WM.modelled = eq(data.biomass.wtc3$D2H)
# eq = function(x){(coefficients(wm3)[1] + coefficients(wm3)[2] * x  + coefficients(wm3)[3] * x * x)}
# Calculate all seedling woodmass from height and diameter using the linear model
# data.biomass.wtc3$WM.modelled = eq(data.biomass.wtc3$D,data.biomass.wtc3$H)
eq = function(x,y){exp(coefficients(wm3)[1] + coefficients(wm3)[2] * log(x)  + coefficients(wm3)[3] * log(y)  + coefficients(wm3)[4] * log(x) * log(y))}
data.biomass.wtc3$WM.modelled = eq(data.biomass.wtc3$D, data.biomass.wtc3$H)

# Estimate the rootmass from the fitted linear regression equation
# eq = function(x,y){exp(coefficients(rm1)[1] + coefficients(rm1)[2] * log(x)  + coefficients(rm1)[3] * log(y))}
eq = function(x){exp(coefficients(rm3)[1] + coefficients(rm3)[2] * log(x)  + coefficients(rm3)[3] * log(x) * log(x) + coefficients(rm3)[4] * log(x) * log(x) * log(x))}
data.biomass.wtc3$RM.modelled = eq(data.biomass.wtc3$D2H)
# eq = function(x){exp(coefficients(rm3)[1] + coefficients(rm3)[2] * log(x)  + coefficients(rm3)[3] * log(x) * log(x))}
# Calculate all seedling rootmass from height and diameter using the linear model
# data.biomass.wtc3$RM.modelled = eq(data.biomass.wtc3$D,data.biomass.wtc3$H)
eq = function(x,y){exp(coefficients(rm3)[1] + coefficients(rm3)[2] * log(x)  + coefficients(rm3)[3] * log(y)  + coefficients(rm3)[4] * log(x) * log(y))}
data.biomass.wtc3$RM.modelled = eq(data.biomass.wtc3$D, data.biomass.wtc3$H)


#-----------------------------------------------------------------------------------------
# Plotting all harvest data along with modelled biomass for Euc Tereticornis
par(mfrow = c(2, 2))
plot(data.harvest.all$D,data.harvest.all$LM,col=data.harvest.all$exp,main="Leafmass vs Diameter", pch=1, xlab="Diameter (mm)", ylab="Leafmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(D,LM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(D,LM,type="p", col="red", pch='+'))
lines(data.biomass.wtc3$D,data.biomass.wtc3$LM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

plot(data.harvest.all$H,data.harvest.all$LM,col=data.harvest.all$exp,main="Leafmass vs Height", pch=1, xlab="Height (cm)", ylab="Leafmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(H,LM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(H,LM,type="p", col="red", pch='+'))
lines(data.biomass.wtc3$H,data.biomass.wtc3$LM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

plot(data.harvest.all$D2H,data.harvest.all$LM,col=data.harvest.all$exp,main="Leafmass vs D2H", pch=1, xlab="D2H (cm3)", ylab="Leafmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(D2H,LM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(D2H,LM,type="p", col="red", pch='+'))
lines(data.biomass.wtc3$D2H,data.biomass.wtc3$LM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),plot(Date,LM,type="p", col="red", pch='o',main="Leafmass over time", xlab="Time", 
                                                                          ylim=c(0,max(data.biomass.wtc3$LM)), ylab="Leafmass (g DM)"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,LM,type="p", col="red", pch='+'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,LM.modelled,type="p", col="grey", pch='o'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,LM.modelled,type="p", col="grey", pch='+'))
# lines(data.biomass.wtc3$Date,data.biomass.wtc3$LM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 est-amb","WTC3 est-ele","WTC3 mod-amb","WTC3 mod-ele"), col=c("red","red","grey","grey"), pch=c('o','+','o','+'), bty='n', cex=1, y.intersp=0.75)
plots[[7]] = recordPlot()

par(mfrow = c(2, 2))
plot(data.harvest.all$D,data.harvest.all$WM,col=data.harvest.all$exp,main="Woodmass vs Diameter", pch=1, xlab="Diameter (mm)", ylab="Woodmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(D,WM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(D,WM,type="p", col="red", pch='+'))
# with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(D,WM.modelled,type="p", col="grey", pch='o'))
# with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(D,WM.modelled,type="p", col="grey", pch='+'))
lines(data.biomass.wtc3$D,data.biomass.wtc3$WM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

plot(data.harvest.all$H,data.harvest.all$WM,col=data.harvest.all$exp,main="Woodmass vs Height", pch=1, xlab="Height (cm)", ylab="Woodmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(H,WM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(H,WM,type="p", col="red", pch='+'))
lines(data.biomass.wtc3$H,data.biomass.wtc3$WM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

plot(data.harvest.all$D2H,data.harvest.all$WM,col=data.harvest.all$exp,main="Woodmass vs D2H", pch=1, xlab="D2H (cm3)", ylab="Woodmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(D2H,WM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(D2H,WM,type="p", col="red", pch='+'))
lines(data.biomass.wtc3$D2H,data.biomass.wtc3$WM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),plot(Date,WM,type="p", col="red", pch='o',main="Woodmass over time", xlab="Time", 
                                                                          ylim=c(0,max(data.biomass.wtc3$WM)), ylab="Woodmass (g DM)"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,WM,type="p", col="red", pch='+'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,WM.modelled,type="p", col="grey", pch='o'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,WM.modelled,type="p", col="grey", pch='+'))
# lines(data.biomass.wtc3$Date,data.biomass.wtc3$LM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 est-amb","WTC3 est-ele","WTC3 mod-amb","WTC3 mod-ele"), col=c("red","red","grey","grey"), pch=c('o','+','o','+'), bty='n', cex=1, y.intersp=0.75)
plots[[8]] = recordPlot()

par(mfrow = c(2, 2))
plot(data.harvest.all$D,data.harvest.all$RM,col=data.harvest.all$exp,main="Rootmass vs Diameter", pch=1, xlab="Diameter (mm)", ylab="Rootmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(D,RM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(D,RM,type="p", col="red", pch='+'))
lines(data.biomass.wtc3$D,data.biomass.wtc3$RM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

plot(data.harvest.all$H,data.harvest.all$RM,col=data.harvest.all$exp,main="Rootmass vs Height", pch=1, xlab="Height (cm)", ylab="Rootmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(H,RM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(H,RM,type="p", col="red", pch='+'))
lines(data.biomass.wtc3$H,data.biomass.wtc3$RM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

plot(data.harvest.all$D2H,data.harvest.all$RM,col=data.harvest.all$exp,main="Rootmass vs D2H", pch=1, xlab="D2H (cm3)", ylab="Rootmass (g DM)")
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(D2H,RM,type="p", col="red", pch='.'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(D2H,RM,type="p", col="red", pch='+'))
lines(data.biomass.wtc3$D2H,data.biomass.wtc3$RM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 estimated", "WTC3 modelled", "Pot", "Great", "ROS", "WTC3"), col=c("red","grey",unique(data.harvest.all$exp)), pch=19, bty='n', cex=1, y.intersp=0.75)

with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),plot(Date,RM,type="p", col="red", pch='o',main="Rootmass over time", xlab="Time", 
                                                                          ylim=c(0,max(data.biomass.wtc3$RM.modelled)), ylab="Rootmass (g DM)"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,RM,type="p", col="red", pch='+'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,RM.modelled,type="p", col="grey", pch='o'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,RM.modelled,type="p", col="grey", pch='+'))
# lines(data.biomass.wtc3$Date,data.biomass.wtc3$LM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 est-amb","WTC3 est-ele","WTC3 mod-amb","WTC3 mod-ele"), col=c("red","red","grey","grey"), pch=c('o','+','o','+'), bty='n', cex=1, y.intersp=0.75)
plots[[9]] = recordPlot()

pdf(file = "output/3.tree_attributes_tereticornis_final.pdf")
plots[[7]]
plots[[8]]
plots[[9]]
dev.off()
#-----------------------------------------------------------------------------------------
data.harvest.lm = subset(data.harvest.all[,c("Date","LM","H","D","exp","D2H")], exp %in% as.factor(c("pot","great","ros")))
data.harvest.lm = rbind(data.harvest.lm,subset(data.biomass.wtc3[,c("Date","LM","H","D","exp","D2H")], Date >= as.Date(c("2013-09-17"))))

# Fit a linear regression for Leafmass by stem dia and height (ignoring temperature variation)
lm1 <- lm(log(LM) ~ log(D) + log(H), data=data.harvest.lm)
summary(lm1)

# Estimate the leafmass from the fitted linear regression equation
eq = function(x,y){exp(coefficients(lm1)[1] + coefficients(lm1)[2] * log(x)  + coefficients(lm1)[3] * log(y))}
data.biomass.wtc3$LM.modelled = eq(data.biomass.wtc3$D, data.biomass.wtc3$H)


with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),plot(Date,LM,type="p", col="red", pch='o',main="Leafmass over time", xlab="Time", 
                                                                          ylim=c(0,max(data.biomass.wtc3$LM)), ylab="Leafmass (g DM)"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,LM,type="p", col="red", pch='+'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,LM.modelled,type="p", col="grey", pch='o'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,LM.modelled,type="p", col="grey", pch='+'))
# lines(data.biomass.wtc3$Date,data.biomass.wtc3$LM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 est-amb","WTC3 est-ele","WTC3 mod-amb","WTC3 mod-ele"), col=c("red","red","grey","grey"), pch=c('o','+','o','+'), bty='n', cex=1, y.intersp=0.75)


#-----------------------------------------------------------------------------------------
# Final WTC3 biomass
# Final leaf mass (Use allometry-based estimates till 2013-03-04, rest are geometry-based estimates)
data.biomass.wtc3$LM.final = NA
data.biomass.wtc3$LM.final[data.biomass.wtc3$Date <= "2013-06-26"] = subset(data.biomass.wtc3, Date <= "2013-06-26")$LM.modelled
data.biomass.wtc3$LM.final[data.biomass.wtc3$Date >= "2013-09-17"] = subset(data.biomass.wtc3, Date >= "2013-09-17")$LM
data.biomass.wtc3$LM.final[data.biomass.wtc3$T_treatment %in% as.factor("ambient")] = na.spline(subset(data.biomass.wtc3, T_treatment %in% as.factor("ambient"))$LM.final)
data.biomass.wtc3$LM.final[data.biomass.wtc3$T_treatment %in% as.factor("elevated")] = na.spline(subset(data.biomass.wtc3, T_treatment %in% as.factor("elevated"))$LM.final)

# Final wood mass (Use allometry-based estimates till 2013-03-04, rest are geometry-based estimates)
data.biomass.wtc3$WM.final = 0
data.biomass.wtc3$WM.final[data.biomass.wtc3$Date <= "2013-03-04"] = subset(data.biomass.wtc3, Date <= "2013-03-04")$WM.modelled
data.biomass.wtc3$WM.final[data.biomass.wtc3$Date > "2013-03-04"] = subset(data.biomass.wtc3, Date > "2013-03-04")$WM

# Final leaf mass (Use allometry-based estimates till 2013-03-04, rest are geometry-based estimates)
data.biomass.wtc3$RM.final = data.biomass.wtc3$RM

# Plotting all harvest data along with modelled biomass for Euc Tereticornis
par(mfrow = c(1, 1))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),plot(Date,LM,type="p", col="red", pch='o',main="Leafmass over time", xlab="Time",
                                                                          ylim=c(0,max(data.biomass.wtc3$LM)), ylab="Leafmass (g DM)"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,LM,type="p", col="red", pch='+'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,LM.final,type="p", col="grey", pch='o'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,LM.final,type="p", col="grey", pch='+'))
# lines(data.biomass.wtc3$Date,data.biomass.wtc3$LM.modelled,type="p", col="grey", pch=20)
legend('topleft', c("WTC3 est-amb","WTC3 est-ele","WTC3 mod-amb","WTC3 mod-ele"), col=c("red","red","grey","grey"), pch=c('o','+','o','+'), bty='n', cex=1, y.intersp=0.5)

png("output/2.Biomass_final_modelled.png", units="px", width=3000, height=2000, res=220)
par(mfrow = c(1, 2))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),plot(Date,LM.final,type="p", col="green", pch='o',ylab="Biomass (g DM)",ylim=c(1,max(data.biomass.wtc3$WM.final))))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,LM.final,type="p", col="green", pch='+'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,WM.final,type="p", col="grey", pch='o'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,WM.final,type="p", col="grey", pch='+'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,RM.final,type="p", col="red", pch='o'))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,RM.final,type="p", col="red", pch='+'))
legend('topleft', c("Amb-LM","Warm-LM","Amb-WM","Warm-WM","Amb-RM","Warm-RM"), col=c("green","green","grey","grey","red","red"), pch=c('o','+','o','+','o','+'), bty='n', cex=1, y.intersp=0.75)

with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),plot(Date,LM.final,type="p", col="green", pch='o', log="y",ylab="log (Biomass (g DM))",ylim=c(1,max(data.biomass.wtc3$WM.final))))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,LM.final,type="p", col="green", pch='+', log="y"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,WM.final,type="p", col="grey", pch='o', log="y"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,WM.final,type="p", col="grey", pch='+', log="y"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("ambient")),lines(Date,RM.final,type="p", col="red", pch='o', log="y"))
with(subset(data.biomass.wtc3,T_treatment %in% as.factor("elevated")),lines(Date,RM.final,type="p", col="red", pch='+', log="y"))
legend('topleft', c("Amb-LM","Warm-LM","Amb-WM","Warm-WM","Amb-RM","Warm-RM"), col=c("green","green","grey","grey","red","red"), pch=c('o','+','o','+','o','+'), bty='n', cex=1, y.intersp=0.75)
dev.off()

