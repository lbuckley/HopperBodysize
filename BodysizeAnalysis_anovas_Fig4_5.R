library(car)
library(nlme)
library(lme4)
library(lmerTest)
library(patchwork)
library(ggplot2)
library(sjPlot)
library(plyr)
library(see)
library(coefplot2)
#library(MuMIn)
library(tidyverse)
library(broom)
library(performance)

setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.all= read.csv("BodySize_wClim_plusNiwot.csv" )

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
#---------------------------
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

#rename species timing
stv<- c("nymphal diapauser","early season","late season")
bs.scaled$SpTiming<- stv[match(bs.scaled$SpTiming, c("nymph","early","late"))]
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

setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
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

# #check collinearity of temp
# mod.lmer <- lmer(Femur.anom~Tspr.anom_cs+Tsum.anom_cs+elev_cs +
#                    (1|Year:Sites),
#                  REML = FALSE, na.action = 'na.fail', 
#                  data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
# 
# check_model(mod.lmer)
# check_collinearity(mod.lmer)
#low if use temperature anomalies 

#--------
#Other plots
#https://lmudge13.github.io/sample_code/mixed_effects.html
tab_model(mod.lmer)

plot_model(mod.lmer, show.values = TRUE)
plot_model(mod.lmer, type = "re")
plot_model(mod.lmer, type = "slope")
plot_model(mod.lmer, type = "resid")
plot_model(mod.lmer, type = "diag")
plot_model(mod.lmer, type = "int")

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
#Anom plot without sex

#plot mean and se
bs.sum= ddply(bs.all, c("Species", "SpTiming", "elev", "time"), summarise,
              N    = length(Femur.anom),
              mean = mean(Femur.anom, na.rm=T),
              sd   = sd(Femur.anom, na.rm=T) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$N)

#rename species timing
stv<- c("nymphal diapauser","early season","late season")
bs.sum$SpTiming<- stv[match(bs.sum$SpTiming, c("nymph","early","late"))]
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
setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("Fig3Anom_noSex.pdf",height = 8, width = 8)
time.mod.fig + anom.plot +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#------------
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

#rename species timing
stv<- c("nymphal diapauser","early season","late season")
bs.tplot$SpTiming<- stv[match(bs.tplot$SpTiming, c("nymph","early","late"))]
bs.tplot$SpTiming<- factor(bs.tplot$SpTiming, order=TRUE, levels=c("nymphal diapauser","early season","late season"))

#order species
bs.tplot$SpTord[bs.tplot$Species %in% c("E. simplex","A. clavatus","C. pellucida")]<- 1 
bs.tplot$SpTord[bs.tplot$Species %in% c("X. corallipes","M. boulderensis","M. sanguinipes")]<- 2

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
pdf("Fig4.pdf",height = 8, width = 8)
clim.mod.fig + plot.Temps.all +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#same plot for summer

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
pdf("Fig4_summer.pdf",height = 8, width = 8)
clim.mod.fig.sum + plot.Temps.sum +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#same plot for current summer

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
#same plot for month before adulthood

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
pdf("Fig4_month.pdf",height = 8, width = 8)
clim.mod.fig.m + plot.Temps.m +plot_layout(ncol = 1, heights=c(1,2) )+ 
  plot_annotation(tag_levels = 'a')
dev.off()

#=====================
#same plot for growing season

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
#same plot for previous growing season

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

setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
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


