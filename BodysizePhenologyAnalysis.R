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

#FIX 2012.5

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
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs1)

mod.lmer <- lmer(Femur.anom~gdd.anom*Sex*Species + #include time?
                   (1|Year/Sites),
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

#-------------------
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("SizeByPhenology_bySite.pdf",height = 12, width = 12)
plot.doy / plot.gdd
dev.off()

pdf("SizeByPhenology_Diff.pdf",height = 6, width = 12)
plot.doy.diff + plot.gdd.diff
dev.off()

#----------------------------

# #analysis with C1 data
# fdir= "/Volumes/GoogleDrive/My Drive/AlexanderResurvey/DataForAnalysis/"
# 
# #load climate data
# setwd( paste(fdir, "climate", sep="") )   
# clim= read.csv("AlexanderClimateAll_filled_May2022.csv")
# sort(unique(clim$Year))
# # add years: 1931, 1941, 1947, 1948, 1949, 1950, 2022
# clim.all= read.csv("AlexanderClimateAll.csv")
# sort(unique(clim.all$Year))
# inds= which(clim.all$Year %in% c(1961, 1962, 1963, 1964, 1979, 1981) )
# clim.add= clim.all[inds,]
# clim.add$sjy= paste(clim.add$Site, clim.add$Julian, clim.add$Year, sep="_")
# 
# clim.bind= clim[1:nrow(clim.add),]
# clim.bind[]=NA
# clim.bind[,c("Site","Julian","Year","Max","Min","Mean","sjy")]= clim.add[,c("Site","Julian","Year","Max","Min","Mean","sjy")]
# clim.all= rbind(clim, clim.bind)
# 
# #cummulative degree days
# #cumsum within groups
# clim.sum = clim.all %>% group_by(Year,Site) %>% arrange(Julian) %>% mutate(cdd_sum = cumsum(dd_sum),cdd_june = cumsum(dd_june),cdd_july = cumsum(dd_july),cdd_aug = cumsum(dd_aug),cdd_early = cumsum(dd_early),cdd_mid = cumsum(dd_mid),cdd_ac = cumsum(dd_ac),cdd_mb = cumsum(dd_mb),cdd_ms = cumsum(dd_ms) ) 
# 
# #rough spring (May + June) averages
# clim.ave= clim.all[which(clim.all$Julian %in% 121:181),]
# clim.ave= aggregate(clim.ave[,c("Julian","Max","Mean")], list(clim.ave$Site, clim.ave$Year), FUN="mean", na.rm=TRUE)
# names(clim.ave)[1:2]=c("Site","Year")
# #use C1
# clim.ave= clim.ave[which(clim.ave$Site=="C1"),]
# 
# #add clim C1 to bs.all
# match1= match(bs.all$Year, clim.ave$Year)
# matched= which(!is.na(match1))
# 
# bs.all$MeanC1[matched]= clim.ave$Mean[match1[matched]] 

#------------------------
# #load yearly data
# setwd("/Volumes/GoogleDrive/My Drive/AlexanderResurvey/DataForAnalysis/climate/")
# clim.yr= read.csv("AlexanderYearlyClimate.csv")
# 
# #match climate data
# match1= match(bs.all$year, clim.yr$year)
# matched= which(!is.na(match1))
# 
# bs.all$MeanClim[matched]= clim.yr$Mean[match1[matched]] 
# bs.all$dd[matched]= clim.yr$dd[match1[matched]] 
# bs.all$dd_early[matched]= clim.yr$dd_early[match1[matched]] 
# bs.all$SpringPre[matched]= clim.yr$SpringPre[match1[matched]] 
# bs.all$SpringSnow[matched]= clim.yr$SpringSnow[match1[matched]] 

#========================================

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

#plot doy vs size 
plot.doy.size= ggplot(data=bs.all, aes(x=doy.anom, y=Femur.anom, color=factor(elev), shape=Sex, group=SexElev))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(Species~., scales="free")+
  theme(legend.position = "bottom")+
  xlab("Day of year of adulthood anomaly")+ ylab("Femur length (mm)")+
  scale_color_viridis_d()
#group by year?

# plot.gdd.size= ggplot(data=bs.all, aes(x=dd.anom, y=Mean_Femur, color=factor(elev), shape=Sex, group=SexElev))+ 
#   geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
#   facet_wrap(Species~., scales="free")+
#   theme(legend.position = "bottom")+
#   xlab("Day of year of adulthood anomaly")+ ylab("Femur length (mm)")+
#   scale_color_viridis_d()

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("SizeDoy_museum.pdf",height = 10, width = 12)
plot.doy.size | plot.gdd.size
dev.off()

#-------
#analyze
bs.scaled <- transform(bs.all,
                       elev_cs=scale(elev)
)
bs.scaled$Species= factor(bs.scaled$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

mod.lmer <- lmer(Femur.anom~doy.anom*elev_cs*Sex*Species + #include time?
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled)

mod.lmer <- lmer(Femur.anom~doy.anom*Tspr.mean.anom*Tsum.mean.anom.prev*elev_cs*Sex*Species+
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled)

plot_model(mod.lmer, type = "pred", terms = c("doy.anom","elev_cs","Sex","Species"), show.data=TRUE)
#plot_model(mod.lmer, type = "pred", terms = c("doy.anom","Tspr.anom"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("doy.anom","elev_cs","Sex"), show.data=TRUE)

mod.lmer <- lmer(Femur.anom~dd.anom*elev_cs*Sex*Species + #include time?
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled)

plot_model(mod.lmer, type = "pred", terms = c("dd.anom","elev_cs","Sex","Species"), show.data=TRUE)
plot_model(mod.lmer, type = "slope")
anova(mod.lmer)

#-------
#species individually
#ANOVA output

stat= c("Sum Sq","NumDF","F value","Pr(>F)")

mod.lmer <- lmer(Femur.anom~doy.anom*elev*Sex + #include time?
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])

vars= rownames(anova(mod.lmer))

stats= array(data=NA, dim=c(length(specs),length(vars),4),
             dimnames=list(specs,vars, stat) ) 

modplots <- vector('list', length(specs))
slopeplots <- vector('list', length(specs))

for(spec.k in 1:length(specs)){
  
  mod.lmer <- lmer(Femur.anom~doy.anom*elev*Sex + #include time?
                     (1|Year/Sites),
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




