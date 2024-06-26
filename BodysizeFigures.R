library(ggplot2)
library(sjPlot)
library(patchwork)
library(see)
library(tidyr)
library(reshape2)
library(plyr)
library(viridis)
library(car)
library(nlme)
library(lme4)
library(lmerTest)
#library(MuMIn)

#Read data
setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.sub= read.csv("BodySize_sub.csv")
bs.unmatched= read.csv("BodySize_unmatched.csv" )
bs.all= read.csv("BodySize_wClim_plusNiwot.csv" )
clim.seas=read.csv("NiwotSeasClimFilled.csv" )

#---------
#Figure 1a. Historic elevation gradient all sites
dodge <- position_dodge(width = 100)
jdodge <- position_jitterdodge(dodge.width = 100, jitter.width=100)

#Violin plot
bs.unmatched$SexTime= paste(bs.unmatched$Sex, bs.unmatched$time, sep="")
bs.unmatched$group= paste(bs.unmatched$Species, bs.unmatched$elev, bs.unmatched$Sex, bs.unmatched$time, sep="")

bs.unmatched$Species= factor(bs.unmatched$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

elev.plot= ggplot(data=bs.unmatched[bs.unmatched$time=="historic",], aes(x=elev, y = Mean_Femur, group= SexTime, color=time, fill=time)) +
  facet_wrap(Species~., scales="free", ncol=1)+
  geom_point(position=jdodge, aes(shape=Sex))+
  theme_bw()+ geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  geom_violin(aes(group=group),alpha=0.6, width=400, position=dodge, scale="width")+
  theme_modern(legend.position = "none")+
  scale_fill_manual(values= c("cadetblue"))+
  scale_color_manual(values= c("cadetblue"))+
  scale_shape_manual(values=c(21,24,25))+
  xlab("Elevation (m)")+
  ylab("Femur length (mm)")+ 
  guides(color = FALSE, shape = FALSE)

#change scales
#https://stackoverflow.com/questions/42588238/setting-individual-y-axis-limits-with-facet-wrap-not-with-scales-free-y

#add mean and se
bs.sum= ddply(bs.unmatched, c("Species", "elev", "Sex","time","SexTime"), summarise,
              N    = length(Mean_Femur),
              mean = mean(Mean_Femur),
              sd   = sd(Mean_Femur) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$N)

elev.plot.hist= elev.plot + 
  geom_errorbar(data=bs.sum[bs.sum$time=="historic",], position=position_dodge(width = 100), aes(x=elev, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=bs.sum[bs.sum$time=="historic",], position=position_dodge(width = 100), aes(x=elev, y = mean, shape=Sex), size=3, col="black")

#---------
#Figure 1b. Current elevation gradient all sites
dodge <- position_dodge(width = 100)
jdodge <- position_jitterdodge(dodge.width = 100, jitter.width=100)

#Violin plot
bs.all$SexTime= paste(bs.all$Sex, bs.all$time, sep="")
bs.all$group= paste(bs.all$Species, bs.all$elev, bs.all$Sex, bs.all$time, sep="")

bs.all$Species= factor(bs.all$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#drop 3414 since no males?
bs.all= bs.all[-which(bs.all$Species=="M. boulderensis" & bs.all$elev==3414),]

elev.plot= ggplot(data=bs.all[bs.all$time=="current",], aes(x=elev, y = Mean_Femur, group= SexTime, color=time, fill=time)) +
  facet_wrap(Species~., scales="free", ncol=1)+
  geom_point(position=jdodge, aes(shape=Sex))+
  theme_bw()+ geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  geom_violin(aes(group=group),alpha=0.6, width=400, position=dodge, scale="width")+
  theme_modern(legend.position = "none")+
  scale_fill_manual(values= c("darkorange"))+
  scale_color_manual(values= c("darkorange"))+
  scale_shape_manual(values=c(21,24,25))+
  xlab("Elevation (m)")+
  ylab("Femur length (mm)")+ 
  guides(color = FALSE, shape = FALSE)

#add mean and se
bs.sum= ddply(bs.all, c("Species", "elev", "Sex","time","SexTime"), summarise,
              N    = length(Mean_Femur),
              mean = mean(Mean_Femur),
              sd   = sd(Mean_Femur) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$N)

elev.plot.current= elev.plot + 
  geom_errorbar(data=bs.sum[bs.sum$time=="current",], position=position_dodge(width = 100), aes(x=elev, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=bs.sum[bs.sum$time=="current",], position=position_dodge(width = 100), aes(x=elev, y = mean, shape=Sex), size=3, col="black")

#---------
#Figure 1c. Elevation gradient anomaly

#violin plot for anomaly
bs.all$SexTime= paste(bs.all$Sex, bs.all$time, sep="")
bs.all$group= paste(bs.all$Species, bs.all$elev, bs.all$Sex, bs.all$time, sep="")

bs.all$Species= factor(bs.all$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#plot mean and se
bs.sum= ddply(bs.all, c("Species", "elev", "Sex","time","SexTime"), summarise,
              N    = length(Femur.anom),
              mean = mean(Femur.anom, na.rm=T),
              sd   = sd(Femur.anom, na.rm=T) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$N)

anom.plot= ggplot(data=bs.sum, aes(x=elev, y = mean, group= SexTime, color=time, fill=time)) +
  facet_wrap(Species~., scales="free_x", ncol=1)+
  geom_point( aes(shape=Sex), size=3, col="black")+
  theme_bw()+ 
  #geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+ #DROP SMOOTH
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  theme_modern()+
  scale_fill_manual(values= c("darkorange","cadetblue"))+
  scale_color_manual(values= c("darkorange","cadetblue"))+
  scale_shape_manual(values=c(21,24,25))+
  xlab("Elevation (m)")+
  ylab("Femur length anomaly (mm)")
  
anom.plot= anom.plot + geom_hline(yintercept=0, color="gray")

anom.plot= anom.plot + 
  geom_errorbar(data=bs.sum, aes(x=elev, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  xlim(1700,4000)

#Save figure 1
setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("Fig1_SizeByElevTime.pdf",height = 15, width = 15)
elev.plot.hist + elev.plot.current + anom.plot + 
  plot_layout(widths = c(1, 1, 0.8))
dev.off()

#---------------------
#Figure 2. Climate trends

elevs.sites= c("NOAA", "A1","B1","C1","D1")
elevs= c(1672, 2134, 2591, 3048, 3566)

clim.seas$elev= elevs[match(clim.seas$Site, elevs.sites)]

clim.seas$filled[clim.seas$filled<0.25]=0
clim.seas$filled[clim.seas$filled>0.25]=1
clim.seas$filled= factor(clim.seas$filled)

clim.plot= ggplot(data=clim.seas, aes(x=Year, y = Mean, color=factor(elev)))+ 
  facet_wrap(~Seas)+
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(method='lm')+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  ylab("Mean Temperature (C)")+
  scale_shape_manual(values=c(16,1))

#or use anaomaly?
clim.plot.anom= ggplot(data=clim.seas, aes(x=Year, y = Mean.anom, color=factor(elev)))+ 
  facet_wrap(~Seas)+
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(method='lm')+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  ylab("Mean Temperature (C)")+
  scale_shape_manual(values=c(16,1))

#Save figure
pdf("Fig_Climate.pdf",height = 6, width = 8)
clim.plot
dev.off()

#---
# Analyze temporal trends

#add elevation
clim.seas$elev= elevs[match(clim.seas$Site, elevs.sites)]

mod.lm <- lm(Mean~Year*elev, data = clim.seas[which(clim.seas$Seas=="spring"),])
mod.lm <- lm(Mean~Year*elev, data = clim.seas[which(clim.seas$Seas=="summer"),])

anova(mod.lm)

#---------------------
#Figure. Temperature anomalies

#add mean and se
bs.all$SexElev=paste(bs.all$Sex, bs.all$elev, sep="")
bs.all.sum= ddply(bs.all, c("Species", "elev", "Year"), summarise, #combine across sex "Sex","SexElev"
                  N    = length(Mean_Femur),
                  mean = mean(Mean_Femur),
                  std   = sd(Mean_Femur),
                  mean.anom = mean(Femur.anom),
                  std.anom   = sd(Femur.anom),
                  Tspr.anom = mean(Tspr.mean.anom),
                  Tsum.anom.prev = mean(Tsum.mean.anom.prev),
                  Tmo.anom= mean(Tmo.anom),
                  Tspr.mean = mean(Tspr.mean),
                  Tsum.prev = mean(Tsum.mean),
                  Tmo= mean(Mean.mo)
                  #, springdd.anom = mean(springdd.anom)
)
bs.all.sum$se= bs.all.sum$std / sqrt(bs.all.sum$N)
bs.all.sum$se.anom= bs.all.sum$std.anom / sqrt(bs.all.sum$N)

bs.all.sum$Species= factor(bs.all.sum$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

bs.all.sum$time="historic"
bs.all.sum$time[which(as.numeric(bs.all.sum$Year)>2000)]<-"current"
bs.all.sum$SexTime= paste(bs.all.sum$Sex, bs.all.sum$time, sep="") 
bs.all.sum$SexTimeElev= paste(bs.all.sum$Sex, bs.all.sum$time, bs.all.sum$elev, sep="") 
bs.all.sum$SexTimeElev= paste(bs.all.sum$Sex, bs.all.sum$time, bs.all.sum$elev, sep="") 

# New facet label names
t.labs <- c("Spring", "Previous Summer","Month prev")
names(t.labs) <- c("Tspr.mean", "Tsum.prev","Tmo")

#Mean not anomaly
bs.tplot= bs.all.sum[,c("Species","elev","Year","mean.anom","se.anom","time","Tspr.mean","Tsum.prev")] #"Sex","SexElev", #drop month prev ,"Tmo"

#temp vars in long format
bs.tplot.long= melt(bs.tplot, na.rm = FALSE, value.name = "Temperature", id = c("Species","elev","Year","mean.anom","se.anom","time")) #"Sex","SexElev",
#order seasonally
bs.tplot.long$variable <- factor(bs.tplot.long$variable, levels=c("Tsum.prev","Tspr.mean"))

plot.Temps.all=ggplot(data=bs.tplot.long, aes(x=Temperature, y = mean.anom, group= elev, color=factor(elev)) )+
  facet_grid(Species~variable, scales="free", 
             labeller = labeller(variable = t.labs))+
  geom_point(size=3, aes(shape=time, fill=factor(elev)))+ #size= sd.anom, #, fill=factor(ifelse(time=="historic", NA, elev))
  theme_bw()+ geom_smooth(method="lm", se=FALSE) +
  #geom_errorbar( aes(ymin=mean.anom-se.anom, ymax=mean.anom+se.anom), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d(name="Elevation (m)")+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Temperature (C)") +ylab("Femur length anomaly (mm)")

#-------------
#plot subset in main text

bs.tplot.long= bs.tplot.long[bs.tplot.long$Species %in% c("E. simplex","M. sanguinipes"),]

plot.Temps=ggplot(data=bs.tplot.long, aes(x=Temperature, y = mean.anom, group= elev, color=factor(elev)) )+
  facet_grid(Species~variable, scales="free", 
             labeller = labeller(variable = t.labs))+
  geom_point(size=3, aes(shape=time, fill=factor(elev)))+ #size= sd.anom, #, fill=factor(ifelse(time=="historic", NA, elev))
  theme_bw()+ geom_smooth(method="lm", se=FALSE) +
  #geom_errorbar( aes(ymin=mean.anom-se.anom, ymax=mean.anom+se.anom), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d(name="Elevation (m)")+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Temperature (C)") +ylab("Femur length anomaly (mm)")

pdf("Fig_SizeByTemp_simple.pdf",height = 8, width = 8)
plot.Temps
dev.off()

#---------------------
#Phenology
#See PhenologyAnalysis
