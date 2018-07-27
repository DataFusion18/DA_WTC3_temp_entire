# Script to read the modelled GPP and calculate Rabove (aboveground respiration)   
#  read the daily gpp data from Dushan's model
gpp.data.amb <- read.csv("processed_data/daily_c_ambient.csv")
gpp.data.amb$T_treatment = as.factor("ambient")
gpp.data.ele <- read.csv("processed_data/daily_c_warmed.csv")
gpp.data.ele$T_treatment = as.factor("elevated")
gpp.data = rbind(gpp.data.amb,gpp.data.ele)
gpp.data$Date <- as.POSIXct(gpp.data$Date,format="%Y-%m-%d")
# gpp.data$Date <- as.Date(gpp.data$Date)

gpp.data.final <- summaryBy(gCarbon_tree ~ Date+T_treatment, data=gpp.data, FUN=c(mean,standard.error))
names(gpp.data.final)[3:4] = c("GPP", "GPP_SE")
gpp.data.final = subset(gpp.data.final, Date >= as.Date("2012-12-12") & Date <= as.Date("2014-05-26"))
gpp.data.final$Date <- as.Date(gpp.data.final$Date)+1

#----------------------------------------------------------------------------------------------------------------
#- plot GPP, Ra, and LA data over time for various treatments
font.size = 12
plots = list()
pd <- position_dodge(0)
plots[[1]] = ggplot(data=gpp.data.final, aes(x=Date, y=GPP, group = interaction(T_treatment), colour=T_treatment)) + 
  geom_point(position=pd) +
  geom_errorbar(position=pd, aes(ymin=GPP-GPP_SE, ymax=GPP+GPP_SE), colour="grey", width=1) +
  geom_line(position=pd, data = gpp.data.final, aes(x = Date, y = GPP, group = interaction(T_treatment), colour=T_treatment)) +
  ylab(expression(GPP~"(g C "*d^"-1"*")")) +
  scale_x_date(date_labels="%b %y",date_breaks  ="2 month",limits = c(min(gpp.data.final$Date), max(gpp.data.final$Date))) +
  labs(colour="Temperature") +
  scale_color_manual(labels = c("ambient", "warmed"), values = c("blue", "red")) +
  theme_bw() + ggtitle("Modelled GPP") +
  theme(legend.title = element_text(colour="black", size=font.size)) +
  theme(legend.text = element_text(colour="black", size = font.size)) +
  theme(legend.position = c(0.2,0.85), legend.box = "horizontal") + theme(legend.key.height=unit(0.8,"line")) +
  theme(legend.key = element_blank()) +
  theme(text = element_text(size=font.size)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(size = font.size, vjust=0.3)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# plots[[2]] = ggplot(data=data, aes(x=Date, y=Ra, group = interaction(T_treatment), colour=T_treatment)) + 
#   geom_point(position=pd) +
#   geom_errorbar(position=pd, aes(ymin=Ra-Ra_SE, ymax=Ra+Ra_SE), colour="grey", width=1) +
#   geom_line(position=pd, data = data, aes(x = Date, y = Ra, group = interaction(T_treatment), colour=T_treatment)) +
#   ylab(expression(R[above]~"(g C "*d^"-1"*")")) +
#   scale_x_date(date_labels="%b %y",date_breaks  ="1 month",limits = c(min(data$Date), max(data$Date))) +
#   labs(colour="Temperature") +
#   scale_color_manual(labels = c("ambient", "warmed"), values = c("blue", "red")) +
#   theme_bw() + ggtitle("Partitioned Rabove from flux measurement") +
#   theme(legend.title = element_text(colour="black", size=font.size)) +
#   theme(legend.text = element_text(colour="black", size = font.size)) +
#   theme(legend.position = c(0.2,0.85), legend.box = "horizontal") + theme(legend.key.height=unit(0.8,"line")) +
#   theme(legend.key = element_blank()) +
#   theme(text = element_text(size=font.size)) +
#   theme(axis.title.x = element_blank()) +
#   theme(axis.title.y = element_text(size = font.size, vjust=0.3)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

png("output/1.GPP_over_time_total.png", units="px", width=2000, height=1000, res=130)
do.call(grid.arrange,  plots)
dev.off()

do.call(grid.arrange,  plots)
