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
library(car)
library(coefplot2)
library(tidyverse)
library(broom)
library(performance)

#Read data
#setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperBodysize/data/")
bs.unmatched= read.csv("AlexanderBodySize_all.csv")
bs.all= read.csv("AlexanderBodySize_wClimate.csv")
clim.seas=read.csv("NiwotClimateFilled.csv")
dat.all= read.csv("HopperData_Sept2019.csv")
repro= read.csv("Levy_FemaleGradientDataGrasshopper.csv")

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")

#---------------------
#Figure 1. Climate trends

elevs.sites= c("NOAA", "A1","B1","C1","D1")
elevs= c(1672, 2134, 2591, 3048, 3566)

clim.seas$elev= elevs[match(clim.seas$Site, elevs.sites)]

clim.seas$filled[clim.seas$filled<0.25]=0
clim.seas$filled[clim.seas$filled>0.25]=1
clim.seas$filled= factor(clim.seas$filled)

#---
#growing season plot

clim.plot.gs= ggplot(data=clim.seas[clim.seas$Seas=="gs",], aes(x=Year, y = Mean, color=factor(elev)))+ 
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(method='lm',aes(fill=factor(elev)))+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  scale_fill_viridis_d()+
  ylab("Growing season temperature (°C)")+
  scale_shape_manual(values=c(16,1))+ 
  guides(color="none", fill = "none", shape="none")

#climate anomaly
clim.plot.anom.gs= ggplot(data=clim.seas[clim.seas$Seas=="gs",], aes(x=Year, y = Mean.anom, color=factor(elev)))+ 
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(se=FALSE,method='lm')+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  ylab("Growing season temperature anomaly (°C)")+
  scale_shape_manual(values=c(16,1))+ 
  guides(fill = "none")

#Save climate figure
#setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperBodysize/figures/")

#Save figure 1
pdf("Fig1_Climate_gs.pdf",height = 6, width = 8)
clim.plot.gs + clim.plot.anom.gs + plot_annotation(tag_levels = 'a')
dev.off()

#---
#Spring plots
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

pdf("Fig_Climate_spr.pdf",height = 6, width = 8)
clim.plot + clim.plot.anom + plot_annotation(tag_levels = 'a')
dev.off()

#---
#Summer plots
clim.plot.sum= ggplot(data=clim.seas[clim.seas$Seas=="summer",], aes(x=Year, y = Mean, color=factor(elev)))+ 
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(method='lm',aes(fill=factor(elev)))+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  scale_fill_viridis_d()+
  ylab("Summer temperature (°C)")+
  scale_shape_manual(values=c(16,1))+ 
  guides(color="none", fill = "none", shape="none")

#climate anomaly
clim.plot.anom.sum= ggplot(data=clim.seas[clim.seas$Seas=="summer",], aes(x=Year, y = Mean.anom, color=factor(elev)))+ 
  geom_line()+geom_point(aes(shape=filled))+geom_smooth(se=FALSE,method='lm')+
  theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+
  ylab("Summer temperature anomaly (°C)")+
  scale_shape_manual(values=c(16,1))+ 
  guides(fill = "none")

#Save figure sup
pdf("FigED2_Climate_sum.pdf",height = 6, width = 8)
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
mod.lm <- lm(Mean.anom~Year_cs*elev_cs, data = clim.scaled[which(clim.scaled$Seas=="gs"),])

anova(mod.lm)
plot_model(mod.lm, type = "pred", terms = c("elev_cs", "Year_cs"), show.data=FALSE)

#===============================================
#Figure 2. Historic and current elevation gradient all sites
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

#drop two outlier A.clavatus for plotting
bs.all<- bs.all[-which(bs.all$Species=="A. clavatus" & bs.all$Mean_Femur<8.3),]

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
    theme_bw()+ geom_smooth(method="lm", se=FALSE, aes(lty=Sex), show.legend = FALSE)+
    geom_violin(aes(group=group),alpha=0.6, width=400, position=dodge, scale="width")+
    scale_fill_manual(values= c("darkorange", "cadetblue"))+
    scale_color_manual(values= c("darkorange", "cadetblue"))+
    scale_shape_manual(values=c(21,24,25))+
    xlab("Elevation (m)")+
    ylab("Femur length (mm)")+
    labs(title=substitute(italic(x), list(x=species)))
  
  elev.plot + 
    geom_errorbar(data=bs.sum[bs.sum$Species==species,], position=position_dodge(width = 100), aes(x=elev, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
    geom_point(data=bs.sum[bs.sum$Species==species,], position=position_dodge(width = 100), aes(x=elev, y = mean, shape=Sex), size=3, col="black")
  #+geom_label(label=species, x=2000, y=12, color="black", label.size=0.5)
}

#combine in patchwork
spec_patch <- plot_fe(specs[1]) +plot_fe(specs[3]) +plot_fe(specs[5]) +
  plot_fe(specs[2]) +plot_fe(specs[4]) +plot_fe(specs[6]) +
  plot_layout(ncol = 3, guides="collect") & ylab(NULL) & xlab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 0))

spec_patch<- wrap_elements(spec_patch)+
  plot_annotation(title = "   nymphal diapauser           early season                     late season",
                  caption= "Elevation (m)") &
  theme(plot.title = element_text(size = rel(1.5) ),
        plot.caption = element_text(vjust = 1, hjust = 0.5, size = rel(1.5) ) )

spec_patch<- wrap_elements(spec_patch)+
  labs(tag = "Femur length (mm)") +
  theme(
    plot.tag = element_text(size = rel(1.5), angle = 90),
    plot.tag.position = "left"
  )

#save
pdf("Fig2_both.pdf",height = 8, width = 10)
spec_patch
dev.off()

#===============================================
#analysis

#combined model
bs.sub1= bs.all[,c("Mean_Femur","Femur.anom","Year","time","elev","Sex","Species","Sites","SpTiming",
                   "Tspr.mean","Tspr.mean.anom","Tsum.mean.prev","Tsum.mean.anom.prev",
                   "Tsum.mean","Tsum.mean.anom",
                   "Tgs.mean.prev","Tgs.mean.anom.prev","Tgs.mean","Tgs.mean.anom",
                   "Mean.mo","Tmo.anom")] 
bs.sub1= na.omit(bs.sub1)
#check drops

bs.scaled <- transform(bs.sub1,
                       Tspr_cs=scale(Tspr.mean),
                       Tsum_cs=scale(Tsum.mean.prev),
                       Tsum_cur_cs=scale(Tsum.mean),
                       Tgs_cs=scale(Tgs.mean),
                       Tgs_prev_cs=scale(Tgs.mean.prev),
                       elev_cs=scale(elev),
                       Tspr.anom_cs=scale(Tspr.mean.anom),
                       Tsum.anom_cs=scale(Tsum.mean.anom.prev),
                       Tsum.cur.anom_cs=scale(Tsum.mean.anom),
                       Tgs.anom_cs=scale(Tgs.mean.anom),
                       Tgs.prev.anom_cs=scale(Tgs.mean.anom.prev),
                       Mean.mo_cs=scale(Mean.mo),
                       Tmo.anom_cs= scale(Tmo.anom)
                       #, springdd.anom_cs=scale(springdd.anom),
                       #t28d_cs= scale(t_28d),
                       #t28.anom_cs= scale(t28.anom),
)

bs.scaled$SpTiming<- factor(bs.scaled$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

bs.scaled$Species= factor(bs.scaled$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))
bs.scaled$time= factor(bs.scaled$time, order=TRUE, levels=c("historic","current"))

#make elevation ordered factor
#bs.scaled$elev_cs= factor(bs.scaled$elev_cs, ordered=TRUE )

#---------------
#TSR initial slopes
#historic
mod.lmer <- lmer(Mean_Femur~elev_cs*Sex*SpTiming +
                   (1|Year:Species),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled[which(bs.scaled$time=="historic"),]) 
tsr.mod.fig= plot_model(mod.lmer, type = "pred", terms = c("elev_cs", "Sex","SpTiming"), show.data=FALSE, title="")

#modern
mod.lmer.cur <- lmer(Mean_Femur~elev_cs*Sex*SpTiming +
                       (1|Year:Species),
                     REML = FALSE,
                     na.action = 'na.omit', data = bs.scaled[which(bs.scaled$time=="current"),]) 
tsr.mod.fig.cur= plot_model(mod.lmer.cur, type = "pred", terms = c("elev_cs", "Sex","SpTiming"), show.data=FALSE, title="")

#update model plots
tsr.mod.fig<- tsr.mod.fig+ theme_bw()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  ylab("Femur length (mm)")+ xlab("scaled Elevation (m)")

anova(mod.lmer)

#by species
stats= array(data=NA, dim=c(length(specs),3,6),
             dimnames=list(specs, c("elev","sex","elev:sex"), c("coef","SE","Sum Sq","NumDF","F value","Pr(>F)")) ) 

for(spec.k in 1:length(specs)){
  
  mod.lmer <- lmer(Mean_Femur~elev_cs*Sex +
                     (1|Year:Sites),
                   REML = FALSE, na.action = 'na.fail', 
                   data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),]) 
  
  stats[spec.k,,]=cbind(coef(summary(mod.lmer))[2:nrow(coef(summary(mod.lmer))),1:2],
                        as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")])
} #end loop specs 

stat.mat= rbind(stats[1,,],stats[2,,],stats[3,,],stats[4,,],stats[5,,],stats[6,,] )
stat.mat= as.data.frame(stat.mat)
stat.mat=cbind(rep(specs, each=3), stat.mat)
stat.mat$sig=""
stat.mat$sig[stat.mat$p.value<0.05]="*"
stat.mat$sig[stat.mat$p.value<0.01]="**"
stat.mat$sig[stat.mat$p.value<0.001]="***"

stat.mat[,c(2:4,6:7)]= round(stat.mat[,c(2:4,6:7)],2)
stat.mat$var= rownames(stat.mat)

#setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write_csv( stat.mat, 'species_slope.csv')

#--------------
#Fig analysis

#time model
mod.lmer <- lmer(Femur.anom~time*elev_cs*Sex*SpTiming +
                   (1|Year:Species),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled)
plot_model(mod.lmer, type = "pred", terms = c("elev_cs","time", "SpTiming"), show.data=TRUE)

#time model without sex
time.mod.lmer <- lmer(Femur.anom~time*elev_cs*SpTiming +
                        (1|Year:Species),
                      REML = FALSE,
                      na.action = 'na.omit', data = bs.scaled)
time.mod.fig= plot_model(time.mod.lmer, type = "pred", terms = c("elev_cs","time", "SpTiming"), show.data=FALSE, title="")

#--------------
#time + climate model 
#spring temp or previous summer temp
#drop sex
tc.mod.lmer <- lmer(Femur.anom~Tspr.anom_cs*time*elev_cs*SpTiming +
                      (1|Year:Species),
                    REML = FALSE, na.action = 'na.fail', 
                    data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
tc.mod.fig= plot_model(tc.mod.lmer, type = "pred", terms = c("Tspr.anom_cs","elev_cs","time","SpTiming"), show.data=FALSE, title="")

tc.mod.lmer.sum <- lmer(Femur.anom~Tsum.anom_cs*time*elev_cs*SpTiming +
                          (1|Year:Species),
                        REML = FALSE, na.action = 'na.fail', 
                        data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
tc.mod.fig.sum= plot_model(tc.mod.lmer.sum, type = "pred", terms = c("elev_cs","Tsum.anom_cs","time","SpTiming"), show.data=TRUE)

tc.mod.lmer.sum.cur <- lmer(Femur.anom~Tsum.cur.anom_cs*time*elev_cs*SpTiming +
                              (1|Year:Species),
                            REML = FALSE, na.action = 'na.fail', 
                            data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
tc.mod.fig.sum.cur= plot_model(tc.mod.lmer.sum.cur, type = "pred", terms = c("elev_cs","Tsum.cur.anom_cs","time","SpTiming"), show.data=TRUE)

tc.mod.lmer.m <- lmer(Femur.anom~Tmo.anom_cs*time*elev_cs*SpTiming +
                        (1|Year:Species),
                      REML = FALSE, na.action = 'na.fail', 
                      data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
tc.mod.fig.m= plot_model(tc.mod.lmer.m, type = "pred", terms = c("elev_cs","Tmo.anom_cs","time","SpTiming"), show.data=TRUE)

tc.mod.lmer.gs <- lmer(Femur.anom~Tgs.anom_cs*time*elev_cs*SpTiming +
                         (1|Year:Species),
                       REML = FALSE, na.action = 'na.fail', 
                       data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
tc.mod.fig.gs= plot_model(tc.mod.lmer.gs, type = "pred", terms = c("elev_cs","Tgs.anom_cs","time","SpTiming"), show.data=TRUE)

tc.mod.lmer.gs.prev <- lmer(Femur.anom~Tgs.prev.anom_cs*time*elev_cs*SpTiming +
                              (1|Year:Species),
                            REML = FALSE, na.action = 'na.fail', 
                            data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
tc.mod.fig.gs.prev= plot_model(tc.mod.lmer.gs.prev, type = "pred", terms = c("elev_cs","Tgs.prev.anom_cs","time","SpTiming"), show.data=TRUE)

#----------
#climate model
#spring
c.mod.lmer <- lmer(Femur.anom~Tspr.anom_cs*elev_cs*SpTiming +
                     (1|Year:Species),
                   REML = FALSE, na.action = 'na.fail', 
                   data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
clim.mod.fig= plot_model(c.mod.lmer, type = "pred", terms = c("Tspr.anom_cs","elev_cs","SpTiming"), show.data=FALSE, title="")

#summer
c.mod.lmer.sum <- lmer(Femur.anom~Tsum.anom_cs*elev_cs*SpTiming +
                         (1|Year:Species),
                       REML = FALSE, na.action = 'na.fail', 
                       data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
clim.mod.fig.sum= plot_model(c.mod.lmer.sum, type = "pred", terms = c("Tsum.anom_cs","elev_cs","SpTiming"), show.data=FALSE, title="")

c.mod.lmer.sum.cur <- lmer(Femur.anom~Tsum.cur.anom_cs*elev_cs*SpTiming +
                             (1|Year:Species),
                           REML = FALSE, na.action = 'na.fail', 
                           data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
clim.mod.fig.sum.cur= plot_model(c.mod.lmer.sum.cur, type = "pred", terms = c("Tsum.cur.anom_cs","elev_cs","SpTiming"), show.data=FALSE, title="")

#growing season
c.mod.lmer.gs <- lmer(Femur.anom~Tgs.anom_cs*elev_cs*SpTiming +
                        (1|Year:Species),
                      REML = FALSE, na.action = 'na.fail', 
                      data = bs.scaled) 
clim.mod.fig.gs= plot_model(c.mod.lmer.gs, type = "pred", terms = c("Tgs.anom_cs","elev_cs","SpTiming"), show.data=FALSE, title="")

c.mod.lmer.gs.prev <- lmer(Femur.anom~Tgs.prev.anom_cs*elev_cs*SpTiming +
                             (1|Year:Species),
                           REML = FALSE, na.action = 'na.fail', 
                           data = bs.scaled) 
clim.mod.fig.gs.prev= plot_model(c.mod.lmer.gs.prev, type = "pred", terms = c("Tgs.prev.anom_cs","elev_cs","SpTiming"), show.data=FALSE, title="")

#spring and summer
c.mod.lmer.ss <- lmer(Femur.anom~Tspr.anom_cs*Tsum.anom_cs*elev_cs*SpTiming +
                        (1|Year:Species),
                      REML = FALSE, na.action = 'na.fail', 
                      data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
plot_model(c.mod.lmer.ss, type = "pred", terms = c("Tspr.anom_cs","elev_cs","SpTiming"), show.data=FALSE)
plot_model(c.mod.lmer.ss, type = "pred", terms = c("Tsum.anom_cs","elev_cs","SpTiming"), show.data=FALSE)
clim.mod.fig.ss=plot_model(c.mod.lmer.ss, type = "pred", terms = c("Tspr.anom_cs","elev_cs","Tsum.anom_cs","SpTiming"), show.data=FALSE)

#month before
c.mod.lmer.m <- lmer(Femur.anom~Tmo.anom_cs*elev_cs*SpTiming +
                       (1|Year:Species),
                     REML = FALSE, na.action = 'na.fail', 
                     data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
clim.mod.fig.m= plot_model(c.mod.lmer.m, type = "pred", terms = c("Tmo.anom_cs","elev_cs","SpTiming"), show.data=FALSE, title="")

#--------

#update model plots
time.mod.fig<- time.mod.fig+ theme_bw()+
  scale_color_viridis_d(name="time period")+
  scale_fill_viridis_d(name="time period")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled Elevation (m)")

clim.mod.fig<- clim.mod.fig+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled spring temperature anomally (°C)")

clim.mod.fig.sum<- clim.mod.fig.sum+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled summer temperature anomally (°C)")

clim.mod.fig.sum.cur<- clim.mod.fig.sum.cur+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled current summer temperature anomally (°C)")

clim.mod.fig.m<- clim.mod.fig.m+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled developmental temperature anomally (°C)")

clim.mod.fig.gs<- clim.mod.fig.gs+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled growing season temperature anomally (°C)")

clim.mod.fig.gs.prev<- clim.mod.fig.gs.prev+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled previous growing season temperature anomally (°C)")

#add zero lines to model plots
time.mod.fig= time.mod.fig+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)
clim.mod.fig= clim.mod.fig+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)
clim.mod.fig.sum= clim.mod.fig.sum+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)
clim.mod.fig.sum.cur= clim.mod.fig.sum.cur+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)
clim.mod.fig.m= clim.mod.fig.m+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)
clim.mod.fig.gs= clim.mod.fig.gs+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)
clim.mod.fig.gs.prev= clim.mod.fig.gs.prev+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)

#--------------
#Figure 3
#Anom plot without sex

#plot mean and se
bs.sum= ddply(bs.all, c("Species", "SpTiming", "elev", "time"), summarise,
              N    = length(Femur.anom),
              mean = mean(Femur.anom, na.rm=T),
              sd   = sd(Femur.anom, na.rm=T) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$N)

#rename species timing
bs.sum$SpTiming<- factor(bs.sum$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

#order species
bs.sum$SpTord[bs.sum$Species %in% c("E. simplex","A. clavatus","C. pellucida")]<- 1 
bs.sum$SpTord[bs.sum$Species %in% c("X. corallipes","M. boulderensis","M. sanguinipes")]<- 2

#bs.sum[bs.sum$time=="current",]
anom.plot= ggplot(data=bs.sum, aes(x=elev, y = mean, color=time, fill=time)) + 
  facet_grid(SpTord~SpTiming)+
  geom_point(position=dodge, size=3)+ #, col="black"
  theme_bw()+ 
  # geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+ 
  theme(axis.title=element_text(size=16))+
  theme(strip.text.y = element_blank())+ 
  scale_fill_manual(values= c("darkorange","cadetblue"))+
  scale_color_manual(values= c("darkorange","cadetblue"))+
  xlab("Elevation (m)")+
  ylab("Femur length anomaly (mm)") 

anom.plot= anom.plot + 
  geom_errorbar(data=bs.sum, position=dodge, aes(x=elev, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  xlim(1700,4000)

#add species names to panels
#make dataframe with labels
sdf= data.frame(x=2500, y=1, lab=specs, SpTord=c(1,2,1,2,1,2), SpTiming=c(stv[c(1,1,2,2,3,3)]), elev=1768, vjust=1, SexTime="Fcurrent", time="current")
sdf$SpTiming<- factor(sdf$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

anom.plot= anom.plot + geom_text(aes(x, y, label=lab), data=sdf, color="black")

#add zero lines
anom.plot= anom.plot+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)

#plot together
pdf("Fig3Anom_elev.pdf",height = 8, width = 8)
time.mod.fig + anom.plot +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#===============================================
#Figure 4

#add mean and se
bs.all$SexElev=paste(bs.all$Sex, bs.all$elev, sep="")
bs.all.sum= ddply(bs.all, c("Species", "elev", "Year","SpTiming"), summarise, #combine across sex "Sex","SexElev"
                  N    = length(Mean_Femur),
                  mean = mean(Mean_Femur),
                  std   = sd(Mean_Femur),
                  mean.anom = mean(Femur.anom),
                  std.anom   = sd(Femur.anom),
                  Tspr.anom = mean(Tspr.mean.anom),
                  Tsum.anom.prev = mean(Tsum.mean.anom.prev),
                  Tsum.anom = mean(Tsum.mean.anom),
                  Tgs.anom = mean(Tgs.mean.anom),
                  Tgs.prev.anom = mean(Tgs.mean.anom.prev),
                  Tmo.anom= mean(Tmo.anom),
                  Tspr.mean = mean(Tspr.mean),
                  Tsum.mean = mean(Tsum.mean),
                  Tsum.prev = mean(Tsum.mean.prev),
                  Tgs = mean(Tgs.mean),
                  Tgs.prev = mean(Tgs.mean.prev),
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
bs.tplot= bs.all.sum[,c("Species","elev","Year","SpTiming","mean.anom","se.anom","time","Tspr.mean","Tsum.prev","Tsum.mean","Tmo", "Tgs", "Tgs.prev")] #"Sex","SexElev",

#order species
bs.tplot$SpTord[bs.tplot$Species %in% c("E. simplex","A. clavatus","C. pellucida")]<- 1 
bs.tplot$SpTord[bs.tplot$Species %in% c("X. corallipes","M. boulderensis","M. sanguinipes")]<- 2

#=====================
#Figure 4 growing season

plot.Temps.gs=ggplot(data=bs.tplot, aes(x=Tgs, y = mean.anom, group= elev, color=factor(elev)) )+
  facet_grid(SpTord~SpTiming)+ #, scale="free_x"
  geom_point(size=3, aes(shape=time, fill=factor(elev)))+ #size= sd.anom, #, fill=factor(ifelse(time=="historic", NA, elev))
  theme_bw()+
  theme(strip.text.y = element_blank())+ 
  geom_smooth(method="lm", se=FALSE) +
  #geom_errorbar( aes(ymin=mean.anom-se.anom, ymax=mean.anom+se.anom), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d(name="Elevation (m)")+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Growing season temperature (°C)") +ylab("Femur length anomaly (mm)")

#add species names to panels
sdf$x<- 12
plot.Temps.gs= plot.Temps.gs + geom_text(aes(x, y, label=lab), data=sdf)

#plot together
pdf("Fig4_gs.pdf",height = 8, width = 8)
clim.mod.fig.gs + plot.Temps.gs +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#Figure 4 equivalent for spring

plot.Temps.all=ggplot(data=bs.tplot, aes(x=Tspr.mean, y = mean.anom, group= elev, color=factor(elev)) )+
  facet_grid(SpTord~SpTiming)+ #, scale="free_x"
  geom_point(size=3, aes(shape=time, fill=factor(elev)))+ #size= sd.anom, #, fill=factor(ifelse(time=="historic", NA, elev))
  theme_bw()+
  theme(strip.text.y = element_blank())+ 
  geom_smooth(method="lm", se=FALSE) +
  #geom_errorbar( aes(ymin=mean.anom-se.anom, ymax=mean.anom+se.anom), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d(name="Elevation (m)")+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Spring temperature (C)") +ylab("Femur length anomaly (mm)")

#add species names to panels
#make dataframe with labels
sdf= data.frame(x=-1, y=1, lab=specs, SpTord=c(1,2,1,2,1,2), SpTiming=c(stv[c(1,1,2,2,3,3)]), elev=1768, vjust=1)
sdf$SpTiming<- factor(sdf$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

plot.Temps.all= plot.Temps.all + geom_text(aes(x, y, label=lab), data=sdf)

#add zero lines
plot.Temps.all= plot.Temps.all+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)

#plot together
pdf("Fig4_spring.pdf",height = 8, width = 8)
clim.mod.fig + plot.Temps.all +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#ED Figure 3: Figure 4 equivalent for summer

plot.Temps.sum=ggplot(data=bs.tplot, aes(x=Tsum.prev, y = mean.anom, group= elev, color=factor(elev)) )+
  facet_grid(SpTord~SpTiming)+ #, scale="free_x"
  geom_point(size=3, aes(shape=time, fill=factor(elev)))+ #size= sd.anom, #, fill=factor(ifelse(time=="historic", NA, elev))
  theme_bw()+
  theme(strip.text.y = element_blank())+ 
  geom_smooth(method="lm", se=FALSE) +
  #geom_errorbar( aes(ymin=mean.anom-se.anom, ymax=mean.anom+se.anom), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d(name="Elevation (m)")+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Summer temperature (°C)") +ylab("Femur length anomaly (mm)")

#add species names to panels
sdf$x<- 12
plot.Temps.sum= plot.Temps.sum + geom_text(aes(x, y, label=lab), data=sdf)

#plot together
pdf("FigED3_summer.pdf",height = 8, width = 8)
clim.mod.fig.sum + plot.Temps.sum +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#Figure 4 equivalent for current summer

plot.Temps.sum.cur=ggplot(data=bs.tplot, aes(x=Tsum.mean, y = mean.anom, group= elev, color=factor(elev)) )+
  facet_grid(SpTord~SpTiming)+ #, scale="free_x"
  geom_point(size=3, aes(shape=time, fill=factor(elev)))+ #size= sd.anom, #, fill=factor(ifelse(time=="historic", NA, elev))
  theme_bw()+
  theme(strip.text.y = element_blank())+ 
  geom_smooth(method="lm", se=FALSE) +
  #geom_errorbar( aes(ymin=mean.anom-se.anom, ymax=mean.anom+se.anom), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d(name="Elevation (m)")+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Current summer temperature (°C)") +ylab("Femur length anomaly (mm)")

#add species names to panels
sdf$x<- 12
plot.Temps.sum.cur= plot.Temps.sum.cur + geom_text(aes(x, y, label=lab), data=sdf)

#plot together
pdf("Fig4_summer_cur.pdf",height = 8, width = 8)
clim.mod.fig.sum.cur + plot.Temps.sum.cur +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#ED Figure 4: Figure 4 equivalent for month before adulthood

plot.Temps.m=ggplot(data=bs.tplot, aes(x=Tmo, y = mean.anom, group= elev, color=factor(elev)) )+
  facet_grid(SpTord~SpTiming)+ #, scale="free_x"
  geom_point(size=3, aes(shape=time, fill=factor(elev)))+ #size= sd.anom, #, fill=factor(ifelse(time=="historic", NA, elev))
  theme_bw()+
  theme(strip.text.y = element_blank())+ 
  geom_smooth(method="lm", se=FALSE) +
  #geom_errorbar( aes(ymin=mean.anom-se.anom, ymax=mean.anom+se.anom), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d(name="Elevation (m)")+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Developmental temperature (°C)") +ylab("Femur length anomaly (mm)")

#add species names to panels
sdf$x<- 12
plot.Temps.m= plot.Temps.m + geom_text(aes(x, y, label=lab), data=sdf)

#plot together
pdf("FigED4_month.pdf",height = 8, width = 8)
clim.mod.fig.m + plot.Temps.m +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#Figure 4 equivalent for previous growing season

plot.Temps.gs.prev=ggplot(data=bs.tplot, aes(x=Tgs.prev, y = mean.anom, group= elev, color=factor(elev)) )+
  facet_grid(SpTord~SpTiming)+ #, scale="free_x"
  geom_point(size=3, aes(shape=time, fill=factor(elev)))+ #size= sd.anom, #, fill=factor(ifelse(time=="historic", NA, elev))
  theme_bw()+
  theme(strip.text.y = element_blank())+ 
  geom_smooth(method="lm", se=FALSE) +
  #geom_errorbar( aes(ymin=mean.anom-se.anom, ymax=mean.anom+se.anom), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d(name="Elevation (m)")+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Growing season temperature (°C)") +ylab("Femur length anomaly (mm)")

#add species names to panels
sdf$x<- 12
plot.Temps.gs.prev= plot.Temps.gs.prev + geom_text(aes(x, y, label=lab), data=sdf)

#plot together
pdf("Fig4_gs_prev.pdf",height = 8, width = 8)
clim.mod.fig.gs.prev + plot.Temps.gs.prev +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#Format ANOVAs

#for(mod.k in c(1,3,4)){ #spring
#for(mod.k in c(2,3,5)){ #summer
#  for(mod.k in c(7,3,8)){ #month before temperature
#    for(mod.k in c(9,3,10)){ #gs temperature
for(mod.k in c(11,3,12)){ #gs prev temperature
  
  #time + climate
  if(mod.k==1) mod.lmer<- tc.mod.lmer
  if(mod.k==2) mod.lmer<- tc.mod.lmer.sum  
  if(mod.k==7) mod.lmer<- tc.mod.lmer.m
  if(mod.k==9) mod.lmer<- tc.mod.lmer.gs
  if(mod.k==11) mod.lmer<- tc.mod.lmer.gs.prev
  #time
  if(mod.k==3) mod.lmer<- time.mod.lmer
  #climate
  if(mod.k==4) mod.lmer<- c.mod.lmer
  if(mod.k==5) mod.lmer<- c.mod.lmer.sum
  if(mod.k==6) mod.lmer<- c.mod.lmer.ss
  if(mod.k==8) mod.lmer<- c.mod.lmer.m
  if(mod.k==10) mod.lmer<- c.mod.lmer.gs
  if(mod.k==12) mod.lmer<- c.mod.lmer.gs.prev
  
  aov1= tidy(anova(mod.lmer))
  aov1$sig=""
  aov1$sig[aov1$p.value<0.05]="*"
  aov1$sig[aov1$p.value<0.01]="**"
  aov1$sig[aov1$p.value<0.001]="***"
  aov1[,c(2:3,5:7)]=round( aov1[,c(2:3,5:7)],2)
  
  if(mod.k %in% c(1,2,7,9,11)){ terms=aov1$term; stats=aov1[c(5:8)]; aov.tab= aov1[,c(1,4)]} 
  
  if(mod.k %in% c(3,4,5,6,8,10,12)){
    match1=match(aov1$term, terms)
    aov.add<-as.data.frame(matrix(NA, nrow=length(terms), ncol=4))
    aov.add[match1,]= aov1[,5:8]
    names(aov.add)=names(aov1[,5:8])
    aov.tab= cbind(aov.tab,aov.add) }
  
} #end loop

#add time+ climate data
aov.tab= cbind(aov.tab, stats)

#setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
#write_csv(aov.tab, 'anovas.csv')
#write_csv(aov.tab, 'anovas_summer.csv')
#write_csv(aov.tab, 'anovas_mo.csv')
#write_csv(aov.tab, 'anovas_gs.csv')
write_csv(aov.tab, 'anovas_gs_prev.csv')

coef(mod.lmer) #random effects
plot_model(mod.lmer, type = "slope")

#compare AICs
summary(time.mod.lmer)$AICtab

#c current year: gs best by aic
summary(c.mod.lmer)$AICtab
summary(c.mod.lmer.sum.cur)$AICtab
summary(c.mod.lmer.gs)$AICtab
summary(c.mod.lmer.m)$AICtab

#c prev year
summary(c.mod.lmer.sum)$AICtab
summary(c.mod.lmer.gs.prev)$AICtab

#tc past summer best
#tc current year
summary(tc.mod.lmer)$AICtab
summary(tc.mod.lmer.sum.cur)$AICtab
summary(tc.mod.lmer.gs)$AICtab
summary(tc.mod.lmer.m)$AICtab

#tc prev year
summary(tc.mod.lmer.sum)$AICtab
summary(tc.mod.lmer.gs.prev)$AICtab

#===============================================
#PHENOLOGY ANALYSIS

#find unique spsiteyr
dat= dat.all[duplicated(dat.all$spsiteyear)==FALSE, c("species","year","site","spsiteyear","doy_adult","gdd_adult")]

#match to body size data
bs.sub= bs.all

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
gp= c("Eritettix simplex","Xanthippus corallipes","Aeropedellus clavatus","Melanoplus boulderensis","Camnula pellucida","Melanoplus sanguinipes")
bs.sub$gp= gp[match(bs.sub$Species, specs)]
bs.sub$spsiteyear= paste(bs.sub$Sites,bs.sub$Year,bs.sub$gp,sep="")
#drop sites without phenology data
bs.sub=subset(bs.sub, bs.sub$Sites %in% c("A1","B1","C1","CHA") )

#subset
#subset to species
dat= subset(dat, dat$species %in% gp)
#subset to sites
dat= subset(dat, dat$site %in% c("A1","B1","C1","CHA") )

#match phenology to body size
match1= match(bs.sub$spsiteyear, dat$spsiteyear)
#check
unmatched= unique(bs.sub$spsiteyear[is.na(match1)])

unique(dat.all[which(dat.all$species=="Melanoplus sanguinipes"),"spsiteyear"])

#--------
#estimate of doy_adult, gdd_adult
bs1= merge(bs.sub, dat,
           by.x = "spsiteyear", by.y = "spsiteyear", all.x="TRUE")
names(bs1)[which(names(bs1)=="doy_adult.y")]= "doy_adult"

#estimate doy anomaly
bs1$SpecElevSex= paste(bs1$Species, bs1$elev, bs1$Sex, sep="")
bs.doy.m= aggregate(bs1[,c("SpecElevSex","doy_adult","gdd_adult")], list(bs1$SpecElevSex), FUN=mean, na.rm=TRUE)
names(bs.doy.m)[1]<-"SpecElevSex"
match1= match(bs1$SpecElevSex, bs.doy.m$SpecElevSex)
bs1$doy.anom= bs1$doy_adult - bs.doy.m$doy_adult[match1]
bs1$gdd.anom= bs1$gdd_adult - bs.doy.m$gdd_adult[match1]
bs1$Species= factor(bs1$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#plot body size means per year
agg.size.yr= aggregate(bs1[,c("Mean_Femur","doy_adult","gdd_adult","Femur.anom","doy.anom","gdd.anom")], by=list(bs1$Species, bs1$Sites, bs1$Year, bs1$elev, bs1$time, bs1$SpTiming), FUN="mean", na.rm = TRUE)
names(agg.size.yr)[1:6]=c("Species", "Sites", "year", "elev","timeperiod","SpTiming")

#rename species timing
#stv<- c("nymphal diapauser","early season","late season")
#agg.size.yr$SpTiming<- stv[match(agg.size.yr$SpTiming, c("nymph","early","late"))]
agg.size.yr$SpTiming<- factor(agg.size.yr$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

#order species
agg.size.yr$SpTord[agg.size.yr$Species %in% c("E. simplex","A. clavatus","C. pellucida")]<- 1 
agg.size.yr$SpTord[agg.size.yr$Species %in% c("X. corallipes","M. boulderensis","M. sanguinipes")]<- 2

#plot
plot.doy= ggplot(data=agg.size.yr, aes(x=doy.anom, y=Femur.anom, color=factor(elev), shape=timeperiod))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_grid(SpTord~SpTiming)+
  xlab("Day of year of adulthood anomaly")+ ylab("Femur length anomaly (mm)")+
  theme(strip.text.y = element_blank())+ 
  scale_color_viridis_d()+ labs(color="Elevation (m)",shape = "time")

#add species names to panels
#make dataframe with labels
sdf= data.frame(x=-1, y=1, lab=specs, SpTord=c(1,2,1,2,1,2), SpTiming=c(stv[c(1,1,2,2,3,3)]), elev=1768, vjust=1)
sdf$SpTiming<- factor(sdf$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))
sdf$timeperiod<- "current"

plot.doy= plot.doy + geom_text(aes(x, y, label=lab), data=sdf)

#add zero lines
plot.doy= plot.doy+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)+
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5)

#-------------
#analyze
bs.scaled <- transform(bs1,
                       elev_cs=scale(elev)
)
bs.scaled$Species= factor(bs.scaled$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#rename species timing
bs.scaled$SpTiming<- factor(bs.scaled$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

mod.lmer <- lmer(Femur.anom~doy.anom*elev_cs*SpTiming + #include time? #drop sex *Sex
                   (1|Year:Species),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled)

phen.mod.fig<- plot_model(mod.lmer, type = "pred", terms = c("doy.anom","elev_cs","SpTiming"), show.data=FALSE, title="")
#plot_model(mod.lmer, type = "pred", terms = c("doy.anom","Tspr.anom"), show.data=TRUE)

#update model plots
phen.mod.fig<- phen.mod.fig+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("Day of year of adulthood anomaly")

#add zero lines to model plots
phen.mod.fig= phen.mod.fig+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)

#plot together
pdf("FigED5_SizeDoy_surveys.pdf",height = 8, width = 8)
phen.mod.fig + plot.doy +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#-----
#model
mod.lmer <- lmer(Femur.anom~doy.anom*Sex*Species + #include time?
                   (1|Year:Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs1)

plot_model(mod.lmer, type = "pred", terms = c("doy.anom","Sex","Species"), show.data=TRUE)
plot_model(mod.lmer, type = "slope")

#========================================
#Figure 5: All specimen data

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperBodysize/data/")
bs.all= read.csv("AlexanderBodySize_wClimate.csv")

#estimate anomaly
bs.all$SpecElevSex= paste(bs.all$Species, bs.all$elev, bs.all$Sex, sep="")
bs.all$SexElev= paste(bs.all$Sex, bs.all$elev, sep="")
bs.doy.m= aggregate(bs.all[,c("SpecElevSex","doy_spec","Tspr.mean.anom","Tsum.mean.anom.prev")], list(bs.all$SpecElevSex), FUN=mean, na.rm=TRUE)
names(bs.doy.m)[1]<-"SpecElevSex"
match1= match(bs.all$SpecElevSex, bs.doy.m$SpecElevSex)
bs.all$doy.anom= bs.all$doy_spec - bs.doy.m$doy_spec[match1]
#bs.all$dd.anom= bs.all$dd_collect - bs.doy.m$dd_collect[match1]

#order by seasonal timing
bs.all$Species= factor(bs.all$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#remove early pellucida from 2006 
bs.all= bs.all[-which(bs.all$doy_spec<160 & bs.all$Species=="C. pellucida"),]

#group by year
#add mean and se
bs.phen= ddply(bs.all, c("Species", "SpTiming", "elev", "Sites","timeperiod","Year", "Tspr.mean.anom", "Tsum.mean.anom.prev"), summarise,
               N    = length(Mean_Femur),
               mean.femur = mean(Mean_Femur),
               sd.femur   = sd(Mean_Femur), 
               mean.femur.anom = mean(Femur.anom),
               sd.femur.anom  = sd(Femur.anom),
               mean.doy = mean(doy_spec),
               sd.doy   = sd(doy_spec), 
               mean.doy.anom = mean(doy.anom),
               sd.doy.anom  = sd(doy.anom),
               mean.Tspr.anom = mean(Tspr.mean.anom),
               sd.Tspr.anom  = sd(Tspr.mean.anom),
               mean.Tsum.anom = mean(Tsum.mean.anom.prev),
               sd.Tsum.anom  = sd(Tsum.mean.anom.prev)
)
bs.phen$se.femur.anom= bs.phen$sd.femur.anom / sqrt(bs.phen$N)
bs.phen$se.doy.anom= bs.phen$sd.doy.anom / sqrt(bs.phen$N)

#rename species timing
stv<- c("nymphal diapauser","early season","late season")
bs.phen$SpTiming<- stv[match(bs.phen$SpTiming, c("nymph","early","late"))]
bs.phen$SpTiming<- factor(bs.phen$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

#order species
bs.phen$SpTord[bs.phen$Species %in% c("E. simplex","A. clavatus","C. pellucida")]<- 1 
bs.phen$SpTord[bs.phen$Species %in% c("X. corallipes","M. boulderensis","M. sanguinipes")]<- 2

#restrict to years with N>=4

#all species
bs.phen.yr.plot= ggplot(data=bs.phen[bs.phen$N>=4,], aes(x=mean.doy.anom, y=mean.femur.anom, color=factor(elev), shape=timeperiod, group=elev))+   
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_grid(SpTord~SpTiming)+ #, scales="free"
  theme(strip.text.y = element_blank())+ 
  xlab("Day of year of adulthood anomaly")+ ylab("Femur length anomaly (mm)")+
  scale_color_viridis_d()+ labs(color="Elevation (m)",shape = "time")

#add species names to panels
#make dataframe with labels
sdf= data.frame(x=-10, y=1, lab=specs, SpTord=c(1,2,1,2,1,2), SpTiming=c(stv[c(1,1,2,2,3,3)]), elev=1768, vjust=1, timeperiod="current")
sdf$SpTiming<- factor(sdf$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

bs.phen.yr.plot= bs.phen.yr.plot + geom_text(aes(x, y, label=lab), data=sdf)

#add zeros
bs.phen.yr.plot= bs.phen.yr.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)+ 
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5)

#-------
#analyze
bs.scaled <- transform(bs.all,
                       elev_cs=scale(elev)
)
bs.scaled$Species= factor(bs.scaled$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#rename species timing
bs.scaled$SpTiming<- factor(bs.scaled$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

mod.lmer <- lmer(Femur.anom~doy.anom*elev_cs*SpTiming + #include time? #drop sex *Sex
                   (1|Year:Species),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled)

phen.mod.fig<- plot_model(mod.lmer, type = "pred", terms = c("doy.anom","elev_cs","SpTiming"), show.data=FALSE, title="")
#plot_model(mod.lmer, type = "pred", terms = c("doy.anom","Tspr.anom"), show.data=TRUE)

#update model plots
phen.mod.fig<- phen.mod.fig+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("Day of year of adulthood anomaly")

#add zero lines to model plots
phen.mod.fig= phen.mod.fig+ geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5)

#plot together
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperBodysize/figures/")
pdf("Figure5_phenology.pdf",height = 8, width = 8)
phen.mod.fig + bs.phen.yr.plot +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#---------
anova(mod.lmer)

aov1= tidy(anova(mod.lmer))
aov1$sig=""
aov1$sig[aov1$p.value<0.05]="*"
aov1$sig[aov1$p.value<0.01]="**"
aov1$sig[aov1$p.value<0.001]="***"
aov1[,c(2:3,5:7)]=round( aov1[,c(2:3,5:7)],2)

write_csv( aov1, 'phenology_doy_anova.csv')

#===============================================
#Figure ED 6-7: Fitness analysis

repro.l<- melt(repro[,c("ID","Species","Site","Femur_mm","Elevation_m","EggMass","ClutchMass_g","NOvarioles","PropFunctOvarioles")], id.vars = c("ID","Species","Site","Femur_mm","Elevation_m"))
#order species
repro.l$Species<- factor(repro.l$Species, order=TRUE, levels=c("A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))
#Rename order variables
rvar<- c("EggMass","ClutchMass_g","NOvarioles","PropFunctOvarioles")
rvar.lab<- c("Egg mass (g)","Clutch mass (g)","N ovarioles","Prop Funct Ovarioles")
repro.l$variable<- rvar.lab[match(repro.l$variable, rvar)]
repro.l$variable<- factor(repro.l$variable, order=TRUE, levels=c("Egg mass (g)","Clutch mass (g)","N ovarioles","Prop Funct Ovarioles"))
#Rename variables

repro.plot= ggplot(data=repro.l, aes(y=value, x = Femur_mm, color=factor(Elevation_m) )) + 
  geom_point()+facet_grid(variable~Species, scales="free", switch="y")+geom_smooth(method="lm", aes(fill=factor(Elevation_m)))+theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+ scale_fill_viridis_d()+
  xlab("Femur length (mm)")+ylab("")+
  theme(legend.position="bottom")+ guides(fill="none")

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperBodysize/figures/")
pdf("FigED6_Repro.pdf",height = 8, width = 8)
repro.plot
dev.off()

ggplot(data=repro.l, aes(y=value, x = Elevation_m, color=Femur_mm)) + 
  geom_point()+facet_grid(variable~Species, scales="free")+geom_smooth(method="lm")+theme_bw()

#models
mod.lmer1 <- lm(EggMass ~Femur_mm*Elevation_m*Species,
                na.action = 'na.omit', data = repro)

mod.lmer2 <- lm(ClutchMass_g ~Femur_mm*Elevation_m*Species,
                na.action = 'na.omit', data = repro)

mod.lmer3 <- lm(NOvarioles ~Femur_mm*Elevation_m*Species,
                na.action = 'na.omit', data = repro)

mod.lmer4 <- lm(PropFunctOvarioles ~Femur_mm*Elevation_m*Species,
                na.action = 'na.omit', data = repro)

pmod1<- plot_model(mod.lmer1, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=FALSE, title = "", axis.title = c(title = "Egg mass (g)") )
pmod2<- plot_model(mod.lmer2, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=FALSE, title = "", axis.title = c(title = "Clutch mass (g)") )
pmod3<- plot_model(mod.lmer3, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=FALSE, title = "", axis.title = c(title = "N ovarioles") )
pmod4<- plot_model(mod.lmer4, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=FALSE, title = "", axis.title = c(title = "Prop Funct Ovarioles") )

pmod1$facet$params$nrow=1
pmod2$facet$params$nrow=1
pmod3$facet$params$nrow=1
pmod4$facet$params$nrow=1

#update model plots
pmod1<- pmod1+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")
pmod2<- pmod2+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
pmod3<- pmod3+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
pmod4<- pmod4+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

pmods<- pmod1 + pmod2 + pmod3 + pmod4 +plot_layout(ncol = 1) + 
  plot_layout(guides = "collect")  & xlab(NULL) & theme(plot.margin = margin(5.5, 5.5, 5.5, 0))
#+ plot_annotation(tag_levels = 'A')

# Use the tag label as an x-axis label
pmods <- wrap_elements(panel = pmods) +
  labs(tag = "Femur length (mm)") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )

#plot together
pdf("FigED7_Repro_mod.pdf",height = 8, width = 8)
pmods
dev.off()

anova(mod.lmer)

clim.mod.fig.sum<- clim.mod.fig.sum+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled Summer temperature anomally (C)")

#sp species
specs= c("A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes") 
mod1=lm(ClutchMass_g~Femur_mm + factor(Elevation_m), data=repro[repro$Species==specs[4],])
summary(mod1)
#Clutch mass of all but clavatus increase significantly with femur length
