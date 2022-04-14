library(ggplot2)
library(sjPlot)
library(patchwork)
library(see)
library(tidyr)
library(reshape2)
library(plyr)
library(viridis)
library(car)
#library(MuMIn)

#load data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
#bs= read.csv("GrasshopperSize_processed.csv")
bs= read.csv("GrasshopperSize_processed_checked2020.csv")

#take out potentially duplicate Levy F data
#bs= bs[-which(bs$Project_info=="Levy" & bs$Sex=="F"),]
#Use Levy data rather than Egg Collection data
bs= bs[-which(bs$Project_info=="EggCollectionScanned"),]

#drop Buckley data since measured more coarsely
bs= bs[-which(bs$Project_info=="BuckleyPhys"),]

#----------
#Summer 2021 Additions
#Note from Cesar to "Look for Connor Data" to replace NAs
#Check elevation for meters
#Check whether "Allie Remeasure", "Gabe Body size" is duplicate; Doesn't seem to be 

#REPLACE A. clavatus survey
bs= bs[-which(bs$Project_info=="A. clavatus survey"),]

#Add new specimens
bs.add= read.csv("GrasshopperSize_add_Summer2021.csv")
#combine
bs= rbind(bs, bs.add[,1:15])

#CHECK DATA
#Fix species names
sort(unique(bs$Species))
bs[which(bs$Species=="A. Clavatus" ),"Species"]="A. clavatus"

#Write out data
write.csv(bs, "BodySize_all_Apr2022.csv")
#----------

#add time period
#assign year for delineation
bs$Year[bs$Year=="historic"]=1900
bs$Year[bs$Year=="current"]=2010
bs$time="historic"
bs$time[which(as.numeric(bs$Year)>2000)]<-"current"

#add elevation
sites= read.csv("HopperSites.csv") 

#align names
bs$Sites=trimws(bs$Sites)

bs$Sites[grep("Eldorado", bs$Sites)]<-"Eldorado Trail OSMP/ S. mesa trail"
bs$Sites[grep("Chautauqua", bs$Sites)]<-"Chautauqua Mesa"
bs$Sites[grep("B1", bs$Sites)]<-"B1"
bs$Sites[grep("C1", bs$Sites)]<-"C1"
bs$Sites[grep("D1", bs$Sites)]<-"Niwot Ridge (D1)"
bs$Sites[grep("Chicken Ranch", bs$Sites)]<-"Chicken Ranch Gulch"
bs$Sites[grep("Baldy", bs$Sites)]<-"Baldy Mountain" #Check whether two Baldys

bs$Sites[bs$Sites=="Ft. Collins"]<-"Kingfisher"
bs$Sites[bs$Sites=="Red Fox Hills"]<-"Red Fox"
bs$Sites[bs$Sites=="Rollins pass"]<-"Rollin's Pass"

#combine Rollin's and Mt. Evans sites?
bs$Sites[bs$Sites=="Rollin's Pass (above treeline)"]<-"Rollin's Pass"
#sites may not be comparable?
bs$Sites[bs$Sites=="Mt. Goliath, Mt Evans"]<-"Summit lake"
bs$Sites[bs$Sites=="Summit Lake (above treeline)"]<-"Summit lake"

#add elevation
bs$elev= sites$elevation[match(bs$Sites, sites$Site)]

#add reported elevations
elevs= rowMeans(bs[,c("Elevation_low","Elevation_upper")],na.rm=TRUE)
inds= which(!is.na(elevs))
bs$elev[inds]=elevs[inds]

#subset elevations
elevs.keep= c(1768,2134,2591,3048,3414, 3566, 3901)
bs.sub= subset(bs, bs$elev %in% elevs.keep)
#bs.sub=bs

#species by seasonal timing
specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
#specs= c("A. clavatus","C. pellucida","E. simplex","M. boulderensis","M. sanguinipes","X. corallipes") 

bs.sub= subset(bs.sub, bs.sub$Species %in% specs)

##drop data without historic , current match
#bs.sub= bs.sub[-which(bs.sub$Species=="A. clavatus" & bs.sub$elev %in%c(3414,3901) ),]
#bs.sub= bs.sub[-which(bs.sub$Species=="X. corallipes" & bs.sub$elev %in%c(1768) ),]
#bs.sub= bs.sub[-which(bs.sub$Species=="M. sanguinipes" & bs.sub$elev %in%c(3048,3566) ),]

#order factors
bs.sub$Sex= factor(bs.sub$Sex, order=TRUE, levels=c("F","M"))
bs.sub$Species= factor(bs.sub$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#explore data
#full table
tab= with(bs.sub, table(Species, elev, Sex, time))
#counts by species and sites
with(bs.sub, table(Species, time, Sites))

#combine
tab1= cbind(tab[,1,,1],tab[,1,,2],tab[,2,,1],tab[,2,,2],tab[,3,,1],tab[,3,,2],tab[,4,,1],tab[,4,,2],tab[,5,,1],tab[,5,,2],tab[,6,,1],tab[,6,,2],tab[,7,,1],tab[,7,,2])
#write out
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/")
write.csv(tab1,"Counts.csv")

#------
#scatter plot
#by elevation
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/")
pdf("Size_by_ElevTime.pdf",height = 12, width = 12)
ggplot(data=bs.sub, aes(x=elev, y = Mean_Femur, color=time, shape=factor(Sex))) + 
    facet_wrap(Species~., scales="free")+geom_point(size=2)+
    theme_bw()+ geom_smooth(method="lm", aes(lty=Sex) )+
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  scale_shape_manual(values = c(16, 21))
dev.off()
  
#find means
mu <- ddply(bs.sub, c("Sex","Species","elev","time"), summarise, femur.groupmean=mean(Mean_Femur))

#FIGURE 1- density plots
#density plots
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/")
pdf("DenPlots.pdf",height = 12, width = 12)
ggplot(data=bs.sub, aes(x=Mean_Femur, color=time,lty=Sex))+ 
  facet_grid(Species~elev, scales="free")+geom_density(aes(fill=time, alpha=0.3))+theme_bw()+
  geom_vline(data=mu, aes(xintercept=femur.groupmean, color=time,lty=Sex), size=0.3)
  #+scale_fill_manual(values=c("orange","blue"))
dev.off()

#combined model
bs.sub1= bs.sub[,c("Mean_Femur","time","elev","Sex","Species")]
bs.sub1= na.omit(bs.sub1)

mod1= lm(Mean_Femur~time*elev*Sex*Species, data=bs.sub1, na.action = "na.fail")
plot_model(mod1, type="pred",terms=c("elev","time","Species","Sex"), show.data=TRUE)
Anova(mod1, type=3)
# dredge(mod1) #full model most support

#-------
#By species
#ANOVA output
stat= c("sumsq","df","F","p")
vars= c("Intercept","time","elev","sex","time:elev","time:sex","elev:sex","time:elev:sex","residuals")

stats= array(data=NA, dim=c(length(specs),9,4),
             dimnames=list(specs,vars,stat) ) 

#FIGURE 2- model output

modplots <- vector('list', length(specs))

for(spec.k in 1:length(specs)){

  mod1= lm(Mean_Femur~time*elev*Sex, data=bs.sub[which(bs.sub$Species==specs[spec.k]),])
  stats[spec.k,,]=as.matrix(Anova(mod1, type="III"))
    
    #plot output
   message(spec.k)
   modplots[[spec.k]] <- local({
     spec.k <- spec.k
     p1 <- plot_model(mod1, type="pred",terms=c("elev","time","Sex"), show.data=TRUE,
                      title=specs[spec.k], legend.title = "period",
                      axis.title=c("elevation (m)","femur length (mm)"))
     print(p1)
   })
   
} #end loop specs 

#save figure
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/")
pdf("ModPlots.pdf",height = 12, width = 12)
(modplots[[1]] | modplots[[4]]) / (modplots[[2]] | modplots[[5]]) / (modplots[[3]] | modplots[[6]])
dev.off()

#save output
#round
stats[,,4]= signif(stats[,,4],3)
#stats.all=rbind(stats[1,,],stats[2,,],stats[3,,],stats[4,,],stats[5,,],stats[6,,])
#write.csv(stats[,,4],"Table1.csv")

#save coefficients
#mo.p=summary(mod1)$coefficients
#signif(), round()

# perform a two-sample Kolmogorov-Smirnov test
bs.sub1= subset(bs.sub, bs.sub$Species=="E. simplex" )
bs.sub2= subset(bs.sub1, bs.sub1$Sites==c("Chautauqua Mesa") )
bs.sub2= subset(bs.sub2, bs.sub2$Sex==c("M") )

hist= subset(bs.sub2, bs.sub2$time==c("historic") )
curr= subset(bs.sub2, bs.sub2$time==c("current") )

ks.test(hist$Mean_Femur,curr$Mean_Femur)

#=======================================
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
#bs.sub=subset(bs.sub, bs.sub$Sites %in% c("A1","B1","C1") )

#subset
dat= subset(dat, dat$species %in% gp)

#match phenology to body size
match1= match(bs.sub$spsiteyear, dat$spsiteyear)
#check
unmatched= unique(bs.sub$spsiteyear[is.na(match1)])

unique(dat.all[which(dat.all$species=="Eritettix simplex"),"spsiteyear"])

#estimate of doy_adult, gdd_adult
bs1= merge(bs.sub, dat,
      by.x = "spsiteyear", by.y = "spsiteyear", all.x="TRUE")
names(bs1)[which(names(bs1)=="doy_adult.y")]= "doy_adult"

#plot relationship
plot.doy= ggplot(data=bs1, aes(x=doy_adult, y=Mean_Femur, shape=species, color=Year))+ 
  geom_point()+geom_smooth(method="lm")+theme_bw()+
facet_grid(Species~Sites, scales="free")+ theme(legend.position = "bottom")
#, group_by=spsiteyear

plot.gdd=ggplot(data=bs1, aes(x=gdd_adult, y=Mean_Femur, shape=species, color=Year))+ 
  geom_point()+geom_smooth(method="lm")+theme_bw()+
  facet_grid(Species~Sites, scales="free")+ theme(legend.position = "bottom")

#plot together
plot.doy+plot.gdd

#------
#stats
mod1= lm(Mean_Femur~doy_adult*species*site, data=bs1)

#-----
#plot as change body size, change phenology?
agg= aggregate(bs1[,c("Mean_Femur","doy_adult","gdd_adult")], by=list(bs1$Species, bs1$Sites, bs1$time, bs1$elev), FUN="mean", na.rm = TRUE)
names(agg)[1:4]=c("Species", "Sites", "time", "elev")

#compare historic and current
dm <- melt(agg, measure.vars = c("Mean_Femur","doy_adult","gdd_adult"))
agg.w= dcast(dm, Species + Sites + elev ~ variable+time, mean, value.var = "value")

#differences
agg.w$d.size= agg.w$Mean_Femur_current - agg.w$Mean_Femur_historic
agg.w$d.doy= agg.w$doy_adult_current - agg.w$doy_adult_historic
agg.w$d.gdd= agg.w$gdd_adult_current - agg.w$gdd_adult_historic

#plot
ggplot(data=agg.w, aes(x=d.doy, y=d.size))+geom_point(aes(shape=Species,color=Sites))+ 
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0) #+geom_smooth(method="lm")

ggplot(data=agg.w, aes(x=d.gdd, y=d.size))+geom_point(aes(shape=Sites, color=Species))+ 
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)


