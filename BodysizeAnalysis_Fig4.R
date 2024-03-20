library(stringr)

#relate to phenology

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.all= read.csv("BodySize_wClim_plusNiwot.csv" )

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperPhenSynch/data/")
dat.all= read.csv("HopperData_Sept2019_forPhenOverlap.csv")

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
stv<- c("nymphal diapauser","early season","late season")
agg.size.yr$SpTiming<- stv[match(agg.size.yr$SpTiming, c("nymph","early","late"))]
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
stv<- c("nymphal diapauser","early season","late season")
bs.scaled$SpTiming<- stv[match(bs.scaled$SpTiming, c("nymph","early","late"))]
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
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("SizeDoy_surveys.pdf",height = 8, width = 8)
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

#-----

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("SizeDoy_surveys.pdf",height = 12, width = 12)
plot.doy / plot.gdd
dev.off()

#========================================
#All specimen data

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

#plot doy vs size 
plot.doy.size= ggplot(data=bs.all, aes(x=doy.anom, y=Femur.anom, color=factor(elev), shape=Sex, group=SexElev))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(Species~., scales="free")+
  theme(legend.position = "bottom")+
  xlab("Day of year of adulthood anomaly")+ ylab("Femur length anomally (mm)")+
  scale_color_viridis_d(name="Elevation (m)")

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("SizeDoy_museum.pdf",height = 8, width = 10)
plot.doy.size #| plot.gdd.size
dev.off()

#------
#group by year?

#add mean and se
bs.phen= ddply(bs.all, c("Species", "elev", "Sites","Sex", "SexElev", "timeperiod","Year", "Tspr.mean.anom", "Tsum.mean.anom.prev"), summarise,
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

bs.phen$SexElevTime= paste(bs.phen$Sex, bs.phen$elev, bs.phen$timeperiod, sep="")

bs.phen.yr.plot= ggplot(data=bs.phen, aes(x=mean.doy.anom, y=mean.femur.anom, color=factor(elev), shape=Sex, group=SexElev))+   
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(Species~., scales="free")+ #, scales="free"
  theme(legend.position = "bottom")+
  xlab("Day of year of adulthood anomaly")+ ylab("Femur length anomaly (mm)")+
  scale_color_viridis_d(name="Elevation (m)")
#include time period? #lty=timeperiod, group=SexElev

#bs.phen.yr.plot= bs.phen.yr.plot + 
#  geom_errorbar(data=bs.phen, aes(x=mean.doy.anom, y=mean.femur.anom, ymin=mean.femur.anom-se.femur.anom, ymax=mean.femur.anom+se.femur.anom), width=0, col="black", lty="solid")+
#  geom_errorbar(data=bs.phen, aes(x=mean.doy.anom, y=mean.femur.anom, xmin=mean.doy.anom-se.doy.anom, xmax=mean.doy.anom+se.doy.anom), width=0, col="black", lty="solid")

#simplify

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
stv<- c("nymphal diapauser","early season","late season")
bs.scaled$SpTiming<- stv[match(bs.scaled$SpTiming, c("nymph","early","late"))]
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
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("Fig4.pdf",height = 8, width = 8)
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

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write_csv( aov1, 'phenology_doy_anova.csv')
