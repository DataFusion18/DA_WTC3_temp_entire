# Script to read and plot stem height and diameter for various treatment cases 
# Read data form WTC3 experiment
height.dia.raw <- read.csv("raw_data/WTC_TEMP_CM_TREE-HEIGHT-DIAMETER_20121120-20140527_L1_V2.CSV") # units: Height = cm, dia = mm
height.dia.raw$DateTime = as.Date(height.dia.raw$DateTime)
flux <- read.csv("raw_data/WTC_TEMP_CM_WTCFLUX_20130914-20140526_L2_V2.csv")
chambers = unique(flux$chamber)

height.dia = data.frame(chamber = rep(chambers, each = length(unique(height.dia.raw$DateTime))),
                        Date = rep(unique(height.dia.raw$DateTime),length(chambers)),
                        T_treatment = character(length(chambers) * length(unique(height.dia.raw$DateTime))),
                        # W_treatment = character(length(chambers) * length(unique(height.dia.raw$DateTime))),
                        diameter = numeric(length(chambers) * length(unique(height.dia.raw$DateTime))),
                        height = numeric(length(chambers) * length(unique(height.dia.raw$DateTime))), stringsAsFactors=FALSE)
# height.dia = subset(height.dia, Date >= as.Date("2013-09-17") & Date <= as.Date("2014-05-27"))
height.dia = subset(height.dia, Date >= as.Date("2012-11-29") & Date <= as.Date("2014-05-27"))

for(i in 1:length(chambers)) {
  height.dia.sub = subset(height.dia.raw, chamber %in% as.factor(chambers[i]))
  # height.dia.sub = subset(height.dia, chamber %in% as.factor(chambers[i]) & Water_treatment %in% as.factor("control"))
  if (i==11) {  # Remove the reference measurements made on Chamber 11
    height.dia.sub.1 = subset(height.dia.sub, Stem_number %in% 1)
    height.dia.sub.1.2 = subset(height.dia.sub, Stem_number %in% 1.2)
    height.dia.sub.1[height.dia.sub.1$DateTime >= as.Date("2013-12-24"),"Plant_height"] = height.dia.sub.1.2[,"Plant_height"]
    height.dia.sub = height.dia.sub.1
  }
  keeps <- c("chamber", "DateTime", "T_treatment", "Plant_height", "X15", "X65")
  # keeps <- c("chamber", "DateTime", "T_treatment", "Water_treatment", "Plant_height", "X15", "X65")
  height.dia.sub = height.dia.sub[ , keeps, drop = FALSE]
  
  D.15 <- lm(X15 ~ X65, data=height.dia.sub)
  # visreg(D.15, "X65", overlay=TRUE)
  # summary(D.15)
  eq.D = function(x){coefficients(D.15)[1] + coefficients(D.15)[2] * x }
  
  index = complete.cases(height.dia.sub$X15)
  height.dia.sub$X15[!index] = eq.D(height.dia.sub$X65[!index])
  
  # height.dia.sub$X15 = eq.D(height.dia.sub$X65)
  # height.dia.sub = subset(height.dia.sub, DateTime >= as.Date("2013-09-14") & DateTime <= as.Date("2014-05-27"))
  # height.dia.sub = height.dia.sub[!is.na(height.dia.sub$X65),]
  keeps <- c("chamber", "DateTime", "T_treatment", "X15", "Plant_height")
  height.dia.sub = height.dia.sub[ , keeps, drop = FALSE]
  names(height.dia.sub) <- c("chamber","Date","T_treatment","diameter","height")
  height.dia.sub$T_treatment = as.character(height.dia.sub$T_treatment)
  
  # height.dia[(1+(i-1)*length(unique(height.dia$Date))) : (i*length(unique(height.dia$Date))), 
  #            c("T_treatment","W_treatment","diameter","height")] = height.dia.sub[,c("T_treatment","W_treatment","diameter","height")]
  height.dia[(1+(i-1)*length(unique(height.dia$Date))) : (i*length(unique(height.dia$Date))), 
             c("T_treatment","diameter","height")] = height.dia.sub[,c("T_treatment","diameter","height")]
}
height.dia$T_treatment = as.factor(height.dia$T_treatment)
# height.dia$W_treatment = as.factor(height.dia$W_treatment)

# Average the ambient and elevated temperature treatments considering the drought/watered treatment seperated from the start of the experiment
# n=3 for whole period, considering only the well-watered treatment for both ambient and warmed treatments
drought.chamb = unique(height.dia.raw$chamber[ height.dia.raw$Water_treatment %in% as.factor("drydown")])
height.dia$chamber_type = as.factor( ifelse(height.dia$chamber %in% drought.chamb, "drought", "watered") )
height.dia.final <- summaryBy(height+diameter ~ Date+T_treatment+chamber_type, data=height.dia, FUN=c(mean,standard.error))
names(height.dia.final)[4:7] = c("height", "diameter", "height_SE", "diameter_SE")
height.dia.final = subset(height.dia.final, chamber_type %in% as.factor("watered"))
height.dia.final$chamber_type = NULL

# height.dia.final <- summaryBy(height+diameter ~ Date+T_treatment, data=height.dia, FUN=c(mean,standard.error))
# names(height.dia.final)[3:6] = c("height", "diameter", "height_SE", "diameter_SE")
height.dia.final = subset(height.dia.final, Date >= as.Date("2012-12-12") & Date <= as.Date("2014-05-27"))

#----------------------------------------------------------------------------------------------------------------
# Plot H and D for WTC3 treatments
plots = list()
pd <- position_dodge(3)
plots[[1]] = ggplot(height.dia.final, aes(x=Date, y=diameter, group = T_treatment, colour=T_treatment)) + 
  geom_point(position=pd) +
  geom_errorbar(position=pd, aes(ymin=diameter-diameter_SE, ymax=diameter+diameter_SE), colour="grey", width=2) +
  # geom_ribbon(data = data.biomass, aes(ymin=WM-WM_SE, ymax=WM+WM_SE), linetype=2, alpha=0.1,size=0.1) +
  # geom_line(data = data.biomass, aes(x = Date, y = WM, group = interaction(T_treatment,chamber_type), colour=T_treatment, linetype=chamber_type)) + 
  ylab(expression("Diameter"~"(mm)")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="2 month",limits = c(min(height.dia.final$Date)-2, max(height.dia.final$Date)+2)) +
  labs(colour="Treatment") +
  scale_color_manual(labels = c("Ambient", "Elevated"), values = c("blue", "red")) +
  theme_bw() +
  theme(legend.title = element_text(colour="black", size=font.size)) +
  theme(legend.text = element_text(colour="black", size = font.size)) +
  theme(legend.position = c(0.2,0.8), legend.box = "horizontal") + theme(legend.key.height=unit(0.8,"line")) +
  theme(legend.key = element_blank()) +
  theme(text = element_text(size=font.size)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = font.size, vjust=0.3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plots[[2]] = ggplot(height.dia.final, aes(x=Date, y=height, group = T_treatment, colour=T_treatment)) + 
  geom_point(position=pd) +
  geom_errorbar(position=pd, aes(ymin=height-height_SE, ymax=height+height_SE), colour="grey", width=1) +
  # geom_ribbon(data = data.biomass, aes(ymin=WM-WM_SE, ymax=WM+WM_SE), linetype=2, alpha=0.1,size=0.1) +
  # geom_line(data = data.biomass, aes(x = Date, y = WM, group = interaction(T_treatment,chamber_type), colour=T_treatment, linetype=chamber_type)) + 
  ylab(expression("Height"~"(cm)")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="2 month",limits = c(min(height.dia.final$Date)-2, max(height.dia.final$Date)+2)) +
  labs(colour="Treatment") +
  scale_color_manual(labels = c("Ambient", "Elevated"), values = c("blue", "red")) +
  theme_bw() +
  theme(legend.title = element_text(colour="black", size=font.size)) +
  theme(legend.text = element_text(colour="black", size = font.size)) +
  theme(legend.position = c(0.2,0.8), legend.box = "horizontal") + theme(legend.key.height=unit(0.8,"line")) +
  theme(legend.key = element_blank()) +
  theme(text = element_text(size=font.size)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = font.size, vjust=0.3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf(file = "output/3.tree_H_D_final.pdf")
do.call(grid.arrange,  plots)
dev.off()

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
