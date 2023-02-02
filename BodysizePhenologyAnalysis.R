library(stringr)

#relate to phenology

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.all= read.csv("BodySize_wClim.csv")

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperPhenSynch/data/")
dat.all= read.csv("HopperData_Sept2019_forPhenOverlap.csv")

#find unique spsiteyr
dat= dat.all[duplicated(dat.all$spsiteyear)==FALSE, c("species","year","site","spsiteyear","doy_adult","gdd_adult")]

#match to body size data
bs.sun= bs.all
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

#plot body size means per year
agg.size.yr= aggregate(bs1[,c("Mean_Femur","doy_adult","gdd_adult")], by=list(bs1$Species, bs1$Sites, bs1$Year, bs1$elev, bs1$time), FUN="mean", na.rm = TRUE)
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

### FIX
#estimate doy anomaly
bs1$SpecElevSex= paste(bs1$Species, bs1$elev, bs1$Sex, sep="")
bs.doy.m= aggregate(bs1[,c("SpecElevSex","doy_adult","gdd_adult")], list(bs1$SpecElevSex), FUN=mean, na.rm=TRUE)
names(bs.doy.m)[1]<-"SpecElevSex"
match1= match(bs1$SpecElevSex, bs.doy.m$SpecElevSex)
bs1$doy.anom= bs1$doy_adult - bs.doy.m$doy_adult[match1]
bs1$gdd.anom= bs1$gdd_adult - bs.doy.m$gdd_adult[match1]

mod.lmer <- lmer(Femur.anom~doy_spec*Sex*Species + #include time?
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs1)

plot_model(mod.lmer, type = "pred", terms = c("doy_spec","Sex","Species"), show.data=TRUE)
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

#-----
#plot as change body size, change phenology between historic and current
agg= aggregate(bs1[,c("Mean_Femur","doy_adult","gdd_adult")], by=list(bs1$Species, bs1$Sites, bs1$time, bs1$elev), FUN="mean", na.rm = TRUE)
names(agg)[1:4]=c("Species", "Sites", "time", "elev")

#compare historic and current
dm <- melt(agg, measure.vars = c("Mean_Femur","doy_adult","gdd_adult"))
agg.w= dcast(dm, Species + Sites + elev ~ variable+time, mean, value.var = "value")

#differences
agg.w$d.size= agg.w$Mean_Femur_current - agg.w$Mean_Femur_historic
agg.w$d.doy= agg.w$doy_adult_current - agg.w$doy_adult_historic
agg.w$d.gdd= agg.w$gdd_adult_current - agg.w$gdd_adult_historic

#code nymphal diapause
agg.w$winter<- "egg"
agg.w$winter[which(agg.w$Species %in% c("E. simplex", "X. corallipes"))]<- "nymphal"

#plot
ggplot(data=agg.w, aes(x=d.doy, y=d.size))+geom_point(aes(color=Species,shape=Sites))+ 
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0) #+geom_smooth(method="lm")

ggplot(data=agg.w, aes(x=d.gdd, y=d.size))+geom_point(aes(color=Species,shape=Sites, fill=winter, size=1, stroke=3))+ 
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)+scale_shape_manual(values=c(21,22,24))
#nyphal diapausers respond differently

#-------------------
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("SizeByPhenology_bySite.pdf",height = 12, width = 12)
plot.doy / plot.gdd
dev.off()

pdf("SizeByPhenology_Diff.pdf",height = 6, width = 12)
plot.doy.diff + plot.gdd.diff
dev.off()

#----------------------------
#Use date to assess temp, size relationship for all specimens
bs.all$Year= as.numeric(as.character(bs.all$Year))
bs.all$Year[which(bs.all$Year==1048)]<- 1948
bs.all$Year[which(bs.all$Year==1049)]<- 1949
bs.all$Year[which(bs.all$Year==1058)]<- 1958
bs.all$Year[which(bs.all$Year==1059)]<- 1959
bs.all$Year[which(bs.all$Year==1060)]<- 1960

#gdds and temps of season

fdir= "/Volumes/GoogleDrive/My Drive/AlexanderResurvey/DataForAnalysis/"

#load climate data
setwd( paste(fdir, "climate", sep="") )   
clim= read.csv("AlexanderClimateAll_filled_May2022.csv")
sort(unique(clim$Year))
# add years: 1931, 1941, 1947, 1948, 1949, 1950, 2022
clim.all= read.csv("AlexanderClimateAll.csv")
sort(unique(clim.all$Year))
inds= which(clim.all$Year %in% c(1961, 1962, 1963, 1964, 1979, 1981) )
clim.add= clim.all[inds,]
clim.add$sjy= paste(clim.add$Site, clim.add$Julian, clim.add$Year, sep="_")

clim.bind= clim[1:nrow(clim.add),]
clim.bind[]=NA
clim.bind[,c("Site","Julian","Year","Max","Min","Mean","sjy")]= clim.add[,c("Site","Julian","Year","Max","Min","Mean","sjy")]
clim.all= rbind(clim, clim.bind)

#cummulative degree days
#cumsum within groups
clim.sum = clim.all %>% group_by(Year,Site) %>% arrange(Julian) %>% mutate(cdd_sum = cumsum(dd_sum),cdd_june = cumsum(dd_june),cdd_july = cumsum(dd_july),cdd_aug = cumsum(dd_aug),cdd_early = cumsum(dd_early),cdd_mid = cumsum(dd_mid),cdd_ac = cumsum(dd_ac),cdd_mb = cumsum(dd_mb),cdd_ms = cumsum(dd_ms) ) 

#rough spring (May + June) averages
clim.ave= clim.all[which(clim.all$Julian %in% 121:181),]
clim.ave= aggregate(clim.ave[,c("Julian","Max","Mean")], list(clim.ave$Site, clim.ave$Year), FUN="mean", na.rm=TRUE)
names(clim.ave)[1:2]=c("Site","Year")
#use C1
clim.ave= clim.ave[which(clim.ave$Site=="C1"),]

#add clim C1 to bs.all
match1= match(bs.all$Year, clim.ave$Year)
matched= which(!is.na(match1))

bs.all$MeanC1[matched]= clim.ave$Mean[match1[matched]] 

#------------------------
#load yearly data
setwd("/Volumes/GoogleDrive/My Drive/AlexanderResurvey/DataForAnalysis/climate/")
clim.yr= read.csv("AlexanderYearlyClimate.csv")

#match climate data
match1= match(bs.all$year, clim.yr$year)
matched= which(!is.na(match1))

bs.all$MeanClim[matched]= clim.yr$Mean[match1[matched]] 
bs.all$dd[matched]= clim.yr$dd[match1[matched]] 
bs.all$dd_early[matched]= clim.yr$dd_early[match1[matched]] 
bs.all$SpringPre[matched]= clim.yr$SpringPre[match1[matched]] 
bs.all$SpringSnow[matched]= clim.yr$SpringSnow[match1[matched]] 

#------------------------
#Violin plot by climate
dodge <- position_dodge(width = 1)
jdodge <- position_jitterdodge(dodge.width = 1, jitter.width=1)
bs.all$SexElev= paste(bs.all$Sex, bs.all$elev, paste="")
bs.all$SexElevYr= paste(bs.all$Sex, bs.all$elev, bs.all$year, paste="")
#pick variable
bs.all$clim.var = bs.all$MeanC1
bs.all.sum$clim.var = bs.all.sum$MeanC1

#MeanC1, Mean, dd, dd_early, SpringPre, SpringSnow
size.clim.c1= ggplot(data=bs.all, aes(x=clim.var, y = Mean_Femur, group= SexElev, color=factor(elev), fill=factor(elev) ))+
  facet_wrap(Species~., scales="free")+
  geom_point(position=jdodge, aes(shape=Sex))+
  theme_bw()+ geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  geom_violin(aes(group=SexElevYr),alpha=0.6, width=0.5, position=dodge, scale="width")+
  theme_modern()+
  scale_shape_manual(values=c(21,24,25))+
  ylab("Femur length (mm)")

#add mean and se
bs.all.sum= ddply(bs.all, c("Species", "elev", "Sex","Year","SexElev"), summarise,
                  N    = length(Mean_Femur),
                  mean = mean(Mean_Femur),
                  sd   = sd(Mean_Femur),
                  MeanC1= mean(MeanC1),
                 # Mean= mean(Mean),
                  dd= mean(dd),
                  dd_early= mean(dd_early),
                  SpringPre= mean(SpringPre),
                  SpringSnow= mean(SpringSnow)  )
bs.all.sum$se= bs.all.sum$sd / sqrt(bs.all.sum$mean)

vclim= size.clim.c1 + 
  geom_errorbar(data=bs.all.sum, position=position_dodge(width = 1), aes(x=clim.var, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=bs.all.sum, position=position_dodge(width = 1), aes(x=clim.var, y = mean, shape=Sex), size=3, col="black")

pdf("Size_by_Clim_violin.pdf",height = 12, width = 12)
vclim
dev.off()

#-------
#plot means

# "MeanC1","Mean","dd","dd_early","SpringPre","SpringSnow"
plot.c2=ggplot(data=bs.all.sum, aes(x=MeanC1, y = mean, group= SexElev, shape=Sex, color=factor(elev) ))+
  facet_wrap(Species~., scales="free")+
  geom_point(size=3)+
  theme_bw()+ geom_smooth(method="lm", se=FALSE)+
  geom_errorbar( aes(ymin=mean-se, ymax=mean+se), width=0, col="black")+
  scale_color_brewer(palette = "Spectral")+ scale_y_continuous(trans='log')

pdf("SizeMean_by_Clim.pdf",height = 12, width = 12)
plot.c2
dev.off()  

#-----------------
#use collection dates
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/SpecimenData/")
mus1= read.csv("AlexanderSpecimens2021.csv")

#match bs to museum code
mus1$Barcode= mus1$SpecimenCode
mus1$Barcode= as.numeric(sub("UCMC ", "", mus1$Barcode))
bs.all$Barcode= as.numeric(bs.all$Barcode)

#match
match1= match(bs.all$Barcode, mus1$Barcode)
matched= which(!is.na(match1))
bs.all$DateCollected[matched]= mus1$DateCollected[na.omit(match1)]

dates= as.data.frame(str_split(bs.all$DateCollected, "/", simplify = TRUE))
names(dates)= c("month","day","year")
dates$month= as.numeric(dates$month)
dates$day= as.numeric(dates$day)
dates$year= as.numeric(dates$year)
dates$year[which(dates$year>20)]<- dates$year[which(dates$year>20)]+1900
dates$year[which(dates$year<20)]<- dates$year[which(dates$year<20)]+2000
bs.all$month= dates$month
bs.all$day= dates$day
bs.all$year= dates$year
bs.all$timeperiod="current"
bs.all$timeperiod[bs.all$Year<1990]="historic"

tmp <- as.Date(paste(bs.all$day, bs.all$month, bs.all$year, sep="/"), format = "%d/%m/%Y")
bs.all$doy_spec= as.numeric(format(tmp, "%j"))

#drop low value for C. pellucida
bs.all= bs.all[-which(bs.all$doy_spec==55),]

#estimate anomaly
bs.all$SpecElevSex= paste(bs.all$Species, bs.all$elev, bs.all$Sex, sep="")
bs.doy.m= aggregate(bs.all[,c("SpecElevSex","doy_spec")], list(bs.all$SpecElevSex), FUN=mean)
names(bs.doy.m)[1]<-"SpecElevSex"
match1= match(bs.all$SpecElevSex, bs.doy.m$SpecElevSex)
bs.all$doy.anom= bs.all$doy_spec - bs.doy.m$doy_spec[match1]

#plot doy vs size 
plot.doy.size= ggplot(data=bs.all, aes(x=doy.anom, y=Mean_Femur, color=factor(elev), shape=Sex, group=SexElev))+ 
  geom_point(size=3)+geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(Species~., scales="free")+
  theme(legend.position = "bottom")+
  xlab("Day of year of adulthood")+ ylab("Femur length (mm)")+
  scale_color_viridis_d()
#group by year?

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("SizeDoy_museum.pdf",height = 12, width = 12)
plot.doy.size
dev.off()

#analyze
mod.lmer <- lmer(Femur.anom~doy_spec*Sex*Species + #include time?
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.all)

plot_model(mod.lmer, type = "pred", terms = c("doy_spec","Sex","Species"), show.data=TRUE)
plot_model(mod.lmer, type = "slope")


