#relate to phenology

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperPhenSynch/data/")
dat.all= read.csv("HopperData_Sept2019_forPhenOverlap.csv")

#find unique spsiteyr
dat= dat.all[duplicated(dat.all$spsiteyear)==FALSE, c("species","year","site","spsiteyear","doy_adult","gdd_adult")]

#match to body size data
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
