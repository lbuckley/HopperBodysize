#LOAD PHENOLOGY DATA
#setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperPhenSynch/data/")
#dat.all= read.csv("HopperData_Sept2019_forPhenOverlap.csv")

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
dat= read.csv("Nufio_Resurvey_Final.csv")
dat= dat[,1:14]

#drop leading trailing white spaces
dat$Elevation..m. = trimws(dat$Elevation..m.)
elevs= unique(dat$Elevation..m.)
inds= which(dat$Elevation..m.==elevs[2])
dat$Elevation..m.[inds]<-"2191"
dat$Elevation..m.= as.numeric(dat$Elevation..m.)

dat$Species = trimws(dat$Species) 
#fix spelling
dat$Species[which(dat$Species=="Erittetix simplex")]<-"Eritettix simplex"
dat$Species[which(dat$Species=="Melanoplus bouderensis")]<-"Melanoplus boulderensis"

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
spec.full= c("Eritettix simplex","Xanthippus corallipes","Aeropedellus clavatus","Melanoplus boulderensis",
             "Camnula pellucida","Melanoplus sanguinipes")
dat= dat[which(dat$Species %in% spec.full),]

#------------------------------------------------
#ESTIMATE ADULTHOOD BASED ON DI

dates=as.Date(dat$Date, format= "%m/%d/%y")
dat$Year= as.numeric(format(dates,'%Y'))
dat$Year[which(dat$Year==2058)]=1958
dat$Year[which(dat$Year==2059)]=1959
dat$Year[which(dat$Year==2060)]=1960
#change B1 elevation from 2577 to 2591
dat$Elevation..m.[which(dat$Elevation..m.==2577)]=2591

#combine A1 subsites
dat.a1= subset(dat, dat$Elevation..m. %in% c(2191,2200,2202) )
dat.a1$Species=factor(dat.a1$Species)
dat.a1.agg= aggregate(dat.a1[,8:14], by=list(dat.a1$Date, dat.a1$Species, dat.a1$Year, dat.a1$Ordinal.date), FUN="sum" )
names(dat.a1.agg)[1:4]=c("Date","Species","Year","Ordinal.date")
dat.a1.agg$Elevation..m.=2195

#add A1 sites back to other
dat.a1.agg$spsiteyear= paste(dat.a1.agg$Elevation..m., dat.a1.agg$Year, dat.a1.agg$Species, sep="_")
columns= names(dat.a1.agg)
#combine
dat$spsiteyear= paste(dat$Elevation..m., dat$Year, dat$Species, sep="_")
dat.c= rbind(dat.a1.agg, dat[,columns])
#subset to matching elevations
dat.c= subset(dat.c, dat.c$Elevation..m. %in% c(1752,2195,2591,3048) )

#Calculate development index
dat.c$DI=0
inds=which(dat.c$N_total>0)  
dat.c$DI[inds]= (dat.c$N_Instar1[inds] +dat.c$N_Instar2[inds]*2 +dat.c$N_Instar3[inds]*3 +dat.c$N_Instar4[inds]*4 +dat.c$N_Instar5[inds]*5 +dat.c$N_adults[inds]*6)/dat.c$N_total[inds]

#Indexed calculation
combs= unique(dat.c$spsiteyear)

#days to predict over
doys= 150:265

#make matrix to store output
dout= data.frame(spsiteyear=combs, doy_adult= rep(NA, length(combs)),gdd_adult= rep(NA, length(combs)) ) 

for(k in 1:length(combs)){
  dats= subset(dat.c, dat.c$spsiteyear==combs[k])
  
  #require at least 4 data points
  if(nrow(dats)>=4) { 
    #doy
    doys= seq(min(dats$Ordinal.date), max(dats$Ordinal.date+7),5)
    
    spl<- smooth.spline(x=dats$Ordinal.date, y=dats$DI)
    pred.spl<- predict(spl, doys)
    #extract point where almost all adults DI>5.5
    dout[k,2]= doys[which.max(pred.spl$y>5.5)]
    
    # #gdd
    # #restrict to observed gdds
    # gdds= seq(min(dats$cdd_sumfall), max(dats$cdd_sumfall+50),10)
    # 
    # spl<- smooth.spline(x=dats$cdd_sumfall, y=dats$DI)
    # pred.spl<- predict(spl, gdds)
    # #extract point where almost all adults DI>5.5
    # dout[k,3]= gdds[which.max(pred.spl$y>5.5)]
    
  } #end check length
  
} #end combs

#add estimate back to df
dat.c$doy_adult= dout[match(dat.c$spsiteyear, dout$spsiteyear),"doy_adult"]
#dat$gdd_adult= dout[match(dat$spsiteyear, dout$spsiteyear),"gdd_adult"]

#----------------------------------
#find unique spsiteyr
phen= dat.c[duplicated(dat.c$spsiteyear)==FALSE, c("Species","Year","Elevation..m.","spsiteyear","doy_adult")]

#match to body size data
gp= c("Eritettix simplex","Xanthippus corallipes","Aeropedellus clavatus","Melanoplus boulderensis","Camnula pellucida","Melanoplus sanguinipes")
bs.sub$gp= gp[match(bs.sub$Species, specs)]
bs.sub$spsiteyear= paste(bs.sub$elev,bs.sub$Year,bs.sub$gp,sep="_")

#drop sites without phenology data
bs.sub=subset(bs.sub, bs.sub$Sites %in% c("A1","B1","C1","Chautauqua Mesa") )

#subset
phen= subset(phen, phen$Species %in% gp)

#match phenology to body size
match1= match(bs.sub$spsiteyear, phen$spsiteyear)
matched= which(!is.na(match1))
#check
unmatched= unique(bs.sub$spsiteyear[is.na(match1)])
#add phenology
bs.sub$doy_adult<- NA
bs.sub$doy_adult[matched]= phen$doy_adult[match1[matched]]

#plot relationship
plot.doy= ggplot(data=bs.sub, aes(x=doy_adult, y=Mean_Femur, shape=Species, color=Year))+ 
  geom_point()+geom_smooth(method="lm")+theme_bw()+
  facet_grid(Species~Sites, scales="free")+ theme(legend.position = "bottom")
#, group_by=spsiteyear

plot.doy= ggplot(data=bs.sub, aes(x=doy_adult, y=Mean_Femur, shape=Species, color=Year))+ 
  geom_point()+geom_smooth(method="lm")+theme_bw()+
  facet_grid(.~Sites, scales="free")+ theme(legend.position = "bottom")

#stats
#make species ordered
bs.sub$Species= factor(bs.sub$Species, ordered=TRUE, levels= specs)
#make species timing numeric factor
bs.sub$sptiming= match(bs.sub$Species, specs)

mod1= lm(Mean_Femur~doy_adult*Sites*Species, data=bs.sub)
mod1= lm(Mean_Femur~doy_adult*elev*sptiming, data=bs.sub)
anova(mod1)

#-----
#plot as change body size, change phenology?
agg= aggregate(bs.sub[,c("Mean_Femur","doy_adult")], by=list(bs.sub$Species, bs.sub$Sites, bs.sub$time, bs.sub$elev), FUN="mean", na.rm = TRUE)
names(agg)[1:4]=c("Species", "Sites", "time", "elev")

#compare historic and current
dm <- melt(agg, measure.vars = c("Mean_Femur","doy_adult")) #,"gdd_adult"
agg.w= dcast(dm, Species + Sites + elev ~ variable+time, mean, value.var = "value")

#differences
agg.w$d.size= agg.w$Mean_Femur_current - agg.w$Mean_Femur_historic
agg.w$d.doy= agg.w$doy_adult_current - agg.w$doy_adult_historic
#agg.w$d.gdd= agg.w$gdd_adult_current - agg.w$gdd_adult_historic

#plot
ggplot(data=agg.w, aes(x=d.doy, y=d.size))+geom_point(aes(color=Sites,shape=Species))+ 
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)+ylim(-1,1) #+geom_smooth(method="lm")

ggplot(data=agg.w, aes(x=d.gdd, y=d.size))+geom_point(aes(shape=Sites, color=Species))+ 
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)

#stats
mod1= lm(d.size~d.doy+Sites+Species, data=agg.w)

#----------------
#analyze differences in phenology and size from mean for species at site

agg= aggregate(bs.sub[,c("Mean_Femur","doy_adult")], by=list(bs.sub$Species, bs.sub$Sites, bs.sub$elev), FUN="mean", na.rm = TRUE)
names(agg)[1:3]=c("Species", "Sites", "elev")

agg$spsite= paste(agg$elev, agg$Species, sep="_")
bs.sub$spsite= paste(bs.sub$elev, bs.sub$Species, sep="_")
match1= match(bs.sub$spsite, agg$spsite)
bs.sub$ave.size= agg$Mean_Femur[match1]
bs.sub$ave.phen= agg$doy_adult[match1]

#differences
bs.sub$d.size= bs.sub$Mean_Femur-bs.sub$ave.size
bs.sub$d.doy= bs.sub$doy_adult-bs.sub$ave.phen

#mean phen and size
agg.ps= aggregate(bs.sub[,c("d.size","d.doy")], by=list(bs.sub$Species, bs.sub$Sites, bs.sub$elev, bs.sub$Year), FUN="mean", na.rm = TRUE)
names(agg.ps)[1:4]=c("Species", "Sites", "elev","year")

#plot
ggplot(data=bs.sub, aes(x=d.doy, y=d.size))+ 
  geom_point(aes(color=time))+theme_bw()+geom_smooth(method="lm")+
  facet_grid(Species~Sites, scales="free")+ theme(legend.position = "bottom")

#by year
plot.doy= ggplot(data=agg.ps, aes(x=d.doy, y=d.size, color=year))+ 
  geom_point()+theme_bw()+geom_smooth(method="lm")+
  facet_grid(Species~Sites, scales="free")+ theme(legend.position = "bottom")

#stats
#make species ordered
bs.sub$Species= factor(bs.sub$Species, ordered=TRUE, levels= specs)
#make species timing numeric factor
bs.sub$sptiming= match(bs.sub$Species, specs)

mod1= lm(d.size~d.doy*Sites*Species, data=bs.sub)
mod1= lm(d.size~d.doy*elev*Species, data=bs.sub)
anova(mod1)

plot_model(mod1, type="pred",terms=c("d.doy","Sites","Species"), show.data=TRUE)
plot_model(mod1, type="pred",terms=c("d.doy","elev","Species"), show.data=TRUE)

#E. simplex: bigger earlier at high elevation site
#X. corallipes: bigger earlier at low elevation, bigger later at high elevation
#A. clavatus: little change
#M. bouldernsis and C. pellucida: bigger later at low elevation
#M. sanguinipes: bigger earlier







