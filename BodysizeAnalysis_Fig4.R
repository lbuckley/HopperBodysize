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
agg.size.yr= aggregate(bs1[,c("Mean_Femur","doy_adult","gdd_adult","Femur.anom","doy.anom","gdd.anom")], by=list(bs1$Species, bs1$Sites, bs1$Year, bs1$elev, bs1$time), FUN="mean", na.rm = TRUE)
names(agg.size.yr)[1:5]=c("Species", "Sites", "year", "elev","timeperiod")

#plot relationship
plot.doy= ggplot(data=agg.size.yr, aes(x=doy_adult, y=Mean_Femur, color=Species, shape=timeperiod, group=Species))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(Sites~., scales="free")+
theme(legend.position = "bottom")+
  xlab("Day of year of adulthood")+ ylab("Femur length (mm)")

plot.gdd= ggplot(data=agg.size.yr, aes(x=gdd_adult, y=Mean_Femur, color=Species, shape=timeperiod, group=Species))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(Sites~., scales="free")+
  theme(legend.position = "bottom")+
  xlab("GDDs at adulthood")+ ylab("Femur length (mm)")

#model
mod.lmer <- lmer(Femur.anom~doy.anom*Sex*Species + #include time?
                   (1|Year:Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs1)

mod.lmer <- lmer(Femur.anom~gdd.anom*Sex*Species + #include time?
                   (1|Year:Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs1)

plot_model(mod.lmer, type = "pred", terms = c("doy.anom","Sex","Species"), show.data=TRUE)
plot_model(mod.lmer, type = "slope")

#-----
#means by time period
agg.size= aggregate(bs1[,c("Mean_Femur","doy_adult","gdd_adult")], by=list(bs1$Species, bs1$Sites, bs1$elev, bs1$time), FUN="mean", na.rm = TRUE)
names(agg.size)[1:4]=c("Species", "Sites", "elev","timeperiod")

#subtract means
agg.size$SpSiTp= paste(agg.size$Species, agg.size$Sites, agg.size$time, sep="")
agg.size.yr$SpSiTp= paste(agg.size.yr$Species, agg.size.yr$Sites, agg.size.yr$time, sep="")
agg.size.yr$SpSi= paste(agg.size.yr$Species, agg.size.yr$Sites, sep="")

match1= match(agg.size.yr$SpSiTp, agg.size$SpSiTp)
agg.size.yr$Mean_Femur_diff= agg.size.yr$Mean_Femur - agg.size$Mean_Femur[match1]
agg.size.yr$doy_adult_diff= agg.size.yr$doy_adult - agg.size$doy_adult[match1]
agg.size.yr$gdd_adult_diff= agg.size.yr$gdd_adult - agg.size$gdd_adult[match1]

#plot
plot.doy.diff= ggplot(data=agg.size.yr, aes(x=doy_adult_diff, y=Mean_Femur_diff, color=Species, shape=timeperiod, group=SpSi, lty=Sites))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  theme(legend.position = "bottom")+
  xlab("Delta Day of year of adulthood")+ ylab("Delta Femur length (mm)")
  
plot.gdd.diff= ggplot(data=agg.size.yr, aes(x=gdd_adult_diff, y=Mean_Femur_diff, color=Species, shape=timeperiod, group=SpSi, lty=Sites))+
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  theme(legend.position = "bottom")+
  xlab("Delta GDDs at adulthood")+ ylab("Delta Femur length (mm)")

#estimate mean phenology
agg.size2= aggregate(bs1[,c("Mean_Femur","doy_adult")], by=list(bs1$Species), FUN="mean", na.rm = TRUE)

#-------------------
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("SizeByPhenology_bySite.pdf",height = 12, width = 12)
plot.doy / plot.gdd
dev.off()

pdf("SizeByPhenology_Diff.pdf",height = 6, width = 12)
plot.doy.diff + plot.gdd.diff
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

ggplot(data=bs.all, aes(x=doy_spec, y=Femur.anom, color=factor(elev), shape=Sex, group=SexElev))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(Species~., scales="free")+
  theme(legend.position = "bottom")+
  xlab("Day of year of adulthood anomaly")+ ylab("Femur length anomally (mm)")+
  scale_color_viridis_d()

#plot doy vs size 
plot.doy.size= ggplot(data=bs.all, aes(x=doy.anom, y=Femur.anom, color=factor(elev), shape=Sex, group=SexElev))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(Species~., scales="free")+
  theme(legend.position = "bottom")+
  xlab("Day of year of adulthood anomaly")+ ylab("Femur length anomally (mm)")+
  scale_color_viridis_d(name="Elevation (m)")

# plot.gdd.size= ggplot(data=bs.all, aes(x=dd.anom, y=Mean_Femur, color=factor(elev), shape=Sex, group=SexElev))+ 
#   geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
#   facet_wrap(Species~., scales="free")+
#   theme(legend.position = "bottom")+
#   xlab("Day of year of adulthood anomaly")+ ylab("Femur length (mm)")+
#   scale_color_viridis_d()

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

#all species
bs.phen.yr.plot= ggplot(data=bs.phen, aes(x=mean.doy.anom, y=mean.femur.anom, color=factor(elev), shape=timeperiod, group=elev))+   
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_grid(SpTord~SpTiming)+ #, scales="free"
  theme(strip.text.y = element_blank())+ 
  xlab("Day of year of adulthood anomaly")+ ylab("Femur length anomaly (mm)")+
  scale_color_viridis_d(name="Elevation (m)")

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
  plot_annotation(tag_levels = 'A')
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

#-------
#species individually
#ANOVA output

stat= c("Sum Sq","NumDF","F value","Pr(>F)")

mod.lmer <- lmer(Femur.anom~doy.anom*elev*Sex + #include time?
                   (1|Year:Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])

vars= rownames(anova(mod.lmer))

stats= array(data=NA, dim=c(length(specs),length(vars),4),
             dimnames=list(specs,vars, stat) ) 

modplots <- vector('list', length(specs))
slopeplots <- vector('list', length(specs))

for(spec.k in 1:length(specs)){
  
  mod.lmer <- lmer(Femur.anom~doy.anom*elev*Sex + #include time?
                     (1|Year:Sites),
                   REML = FALSE,
                   na.action = 'na.omit', data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])
  
  stats[spec.k,,1:4]=as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")]
  
  #plot output
  message(spec.k)
  modplots[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="pred",terms=c("doy.anom","elev"), show.data=TRUE, title=specs[spec.k])
    print(p1)
  })
  
  slopeplots[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="slope", title=specs[spec.k])
    print(p1)
  })
  
} #end loop specs 

#save figure
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_phenology.pdf",height = 12, width = 12)
(modplots[[1]] | modplots[[4]]) / (modplots[[2]] | modplots[[5]]) / (modplots[[3]] | modplots[[6]])
dev.off()

pdf("SlopePlots_phenology.pdf",height = 12, width = 12)
(slopeplots[[1]] | slopeplots[[4]]) / (slopeplots[[2]] | slopeplots[[5]]) / (slopeplots[[3]] | slopeplots[[6]])
dev.off()

lmer.sig= stats[,,4]
lmer.sig[lmer.sig < 0.05] <- "*"

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write.csv(lmer.sig, "ModSig_phenology.csv")

#add gdd anomalies

#estimate season length? Using Niwot snowtel data? See https://github.com/lbuckley/HopperPhenology/blob/master/ClimateExtremes_20May2022.R.

