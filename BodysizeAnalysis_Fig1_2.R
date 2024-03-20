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
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.sub= read.csv("BodySize_sub.csv")
bs.unmatched= read.csv("BodySize_unmatched.csv" )
bs.all= read.csv("BodySize_wClim_plusNiwot.csv" )
clim.seas=read.csv("NiwotSeasClimFilled.csv" )

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")

#-------------------
#Figure 1. Historic and current elevation gradient all sites
dodge <- position_dodge(width = 100)
jdodge <- position_jitterdodge(dodge.width = 100, jitter.width=100)

#Violin plot
bs.all$SexTime= paste(bs.all$Sex, bs.all$time, sep="")
bs.all$group= paste(bs.all$Species, bs.all$elev, bs.all$Sex, bs.all$time, sep="")

bs.all$Species= factor(bs.all$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#rename species timing
stv<- c("nymphal diapauser","early season","late season")
bs.all$SpTiming<- stv[match(bs.all$SpTiming, c("nymph","early","late"))]
bs.all$SpTiming<- factor(bs.all$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

#order species
bs.all$SpTord[bs.all$Species %in% c("E. simplex","A. clavatus","C. pellucida")]<- 1 
bs.all$SpTord[bs.all$Species %in% c("X. corallipes","M. boulderensis","M. sanguinipes")]<- 2

#PLOT ALL
elev.plot= ggplot(data=bs.all[bs.all$Species==species,], aes(x=elev, y = Mean_Femur, group= SexTime, color=time, fill=time)) +
  facet_grid(SpTord~SpTiming)+ #, scales="free_x")+ 
  geom_point(position=jdodge, aes(shape=Sex))+
  theme_bw()+ geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  geom_violin(aes(group=group),alpha=0.6, width=400, position=dodge, scale="width")+
  scale_fill_manual(values= c("cadetblue","darkorange"))+
  scale_color_manual(values= c("cadetblue","darkorange"))+
  scale_shape_manual(values=c(21,24,25))+
  xlab("Elevation (m)")+
  ylab("Femur length (mm)")+ 
  guides(color = FALSE, shape = FALSE)+
  theme(strip.text.y = element_blank())+
  ylim(8,17) ###CHECK OUTLIERS

#add mean and se
bs.sum= ddply(bs.all, c("Species", "elev", "SpTiming", "SpTord", "Sex","time","SexTime"), summarise,
              N    = length(Mean_Femur),
              mean = mean(Mean_Femur),
              sd   = sd(Mean_Femur) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$N)

#make dataframe with labels
sdf= data.frame(x=-1, y=1, lab=specs, SpTord=c(1,2,1,2,1,2), SpTiming=c(stv[c(1,1,2,2,3,3)]), elev=1768, vjust=1)
sdf$SpTiming<- factor(sdf$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

plot_fe<- function(species) {
elev.plot= ggplot(data=bs.all[bs.all$Species==species,], aes(x=elev, y = Mean_Femur, group= SexTime, color=time, fill=time)) +
  geom_point(position=jdodge, aes(shape=Sex))+
  theme_bw()+ geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+
  geom_violin(aes(group=group),alpha=0.6, width=400, position=dodge, scale="width")+
  scale_fill_manual(values= c("cadetblue","darkorange"))+
  scale_color_manual(values= c("cadetblue","darkorange"))+
  scale_shape_manual(values=c(21,24,25))+
  xlab("Elevation (m)")+
  ylab("Femur length (mm)")+
  labs(title=species)

 elev.plot + 
  geom_errorbar(data=bs.sum[bs.sum$Species==species,], position=position_dodge(width = 100), aes(x=elev, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=bs.sum[bs.sum$Species==species,], position=position_dodge(width = 100), aes(x=elev, y = mean, shape=Sex), size=3, col="black")
   #+geom_label(label=species, x=2000, y=12, color="black", label.size=0.5)
}

# #combine in patchwork
# spec_patch <- plot_fe(specs[1]) +plot_fe(specs[3]) +plot_fe(specs[5]) +
#   plot_fe(specs[2]) +plot_fe(specs[4]) +plot_fe(specs[6]) + 
#   plot_layout(ncol = 3, guides="collect") & ylab(NULL) & xlab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 0))
# #fix collecting axis  , axis_titles = "collect"
# 
# # Use the tag label as an x-axis label
# spec_patch <- wrap_elements(panel = spec_patch) +
#   labs(tag = "Elevation (m)") +
#   theme(
#     plot.tag = element_text(size = rel(1)),
#     plot.tag.position = "bottom"
#   )+
#   labs(tag = "Femur length (mm)") +
#   theme(
#     plot.tag = element_text(size = rel(1), angle = 90),
#     plot.tag.position = "left"
#   )

spec_patch <- tsr.mod.fig + plot_fe(specs[1]) +plot_fe(specs[3]) +plot_fe(specs[5]) +
  plot_fe(specs[2]) +plot_fe(specs[4]) +plot_fe(specs[6]) + 
  plot_layout(guides="collect", design=
                "111
                234
                567") #& ylab(NULL) & xlab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 0))

#get from _Fig3.R code
#update model plots
tsr.mod.fig<- tsr.mod.fig+ theme_bw()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  ylab("Femur length (mm)")+ xlab("scaled Elevation (m)")

#save
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("Fig1_both.pdf",height = 8, width = 10)
spec_patch
#spec_patch
dev.off()

#---------------------
#Figure 2. Climate trends

elevs.sites= c("NOAA", "A1","B1","C1","D1")
elevs= c(1672, 2134, 2591, 3048, 3566)

clim.seas$elev= elevs[match(clim.seas$Site, elevs.sites)]

clim.seas$filled[clim.seas$filled<0.25]=0
clim.seas$filled[clim.seas$filled>0.25]=1
clim.seas$filled= factor(clim.seas$filled)

clim.plot= ggplot(data=clim.seas[clim.seas$Seas=="spring",], aes(x=Year, y = Mean, color=factor(elev)))+ 
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(method='lm',aes(fill=factor(elev)))+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  scale_fill_viridis_d()+
  ylab("Spring temperature (C)")+
  scale_shape_manual(values=c(16,1))+ 
  guides(color="none", fill = "none", shape="none")

#climate anomaly
clim.plot.anom= ggplot(data=clim.seas[clim.seas$Seas=="spring",], aes(x=Year, y = Mean.anom, color=factor(elev)))+ 
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(se=FALSE,method='lm')+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  ylab("Spring temperature anomaly (C)")+
  scale_shape_manual(values=c(16,1))+ 
  guides(fill = "none")

#Save figure 2
pdf("Fig2_Climate.pdf",height = 6, width = 8)
clim.plot + clim.plot.anom + plot_annotation(tag_levels = 'a')
dev.off()

#---
#Summer plots
clim.plot.sum= ggplot(data=clim.seas[clim.seas$Seas=="summer",], aes(x=Year, y = Mean, color=factor(elev)))+ 
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(method='lm',aes(fill=factor(elev)))+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  scale_fill_viridis_d()+
  ylab("Summer temperature (C)")+
  scale_shape_manual(values=c(16,1))+ 
  guides(color="none", fill = "none", shape="none")

#climate anomaly
clim.plot.anom.sum= ggplot(data=clim.seas[clim.seas$Seas=="summer",], aes(x=Year, y = Mean.anom, color=factor(elev)))+ 
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(se=FALSE,method='lm')+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  ylab("Summer temperature anomaly (C)")+
  scale_shape_manual(values=c(16,1))+ 
  guides(fill = "none")

#Save figure 2 sup
pdf("Fig2_Climate_sum.pdf",height = 6, width = 8)
clim.plot.sum + clim.plot.anom.sum + plot_annotation(tag_levels = 'a')
dev.off()

#---
# Analyze temporal trends

#add elevation
clim.seas$elev= elevs[match(clim.seas$Site, elevs.sites)]
##order elevation
#clim.seas$elev= factor(clim.seas$elev, order=TRUE, levels=c(1672, 2134,2591,3048,3566))  
  
#scale
clim.scaled <- transform(clim.seas,
                       elev_cs=scale(elev),
                       Year_cs=scale(Year)
)

mod.lm <- lm(Mean.anom~Year_cs*elev_cs, data = clim.scaled[which(clim.scaled$Seas=="spring"),])
mod.lm <- lm(Mean.anom~Year_cs*elev_cs, data = clim.scaled[which(clim.scaled$Seas=="summer"),])

anova(mod.lm)
plot_model(mod.lm, type = "pred", terms = c("elev_cs", "Year_cs"), show.data=FALSE)
