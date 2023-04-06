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

#add collected 2021 grasshoppers 
#40.06193, -105.37569 2279m elevation, 5.9 miles west of Boulder on Sunshine Canyon Drive
bs.meas= read.csv("GrasshopperFemurLength_SunshineCanyon_2021_format.csv")
#combine
bs= rbind(bs, bs.meas)

#CHECK DATA
#Fix species names
sort(unique(bs$Species))
bs[which(bs$Species=="A. Clavatus" ),"Species"]="A. clavatus"

#Add Sunshine Canyon speciemns
sun.dat= read.csv("GrasshopperFemurLength_SunshineCanyon_2021.csv")
sun.dat$Year="2021"
sun.dat$Sites="Sunshine Canyon"
sun.dat$Project_info= "collection"
sun.dat$Specimen_No=NA; sun.dat$Barcode=NA;sun.dat$Mass=NA;sun.dat$Elevation_low=NA;sun.dat$Elevation_upper=NA
sun.dat$Sex[sun.dat$Sex=="Male"]="M"; sun.dat$Sex[sun.dat$Sex=="Female"]="F"

inds= match(names(bs),names(sun.dat))
sun.dat1= sun.dat[inds]

bs= rbind(bs, sun.dat1)

#----------
#Summer 2022 additions 

#M. dodgei study
bs.add= read.csv("Alexander_Mdodgei_transcribed.csv")
#combine
bs= rbind(bs, bs.add)

#Sunshine collections
bs.add= read.csv("BodySize_SunshineCollection_20May2022.csv")
#combine
bs= rbind(bs, bs.add)

#Chicken Ranch Gulch Collections
#check elevation
bs.add= read.csv("EsimplexFemurLength_ChickenRanchGulch_2022.csv")
#combine
bs= rbind(bs, bs.add)

#----
#Museum specimens
bs.add= read.csv("MuseumMeasure2022.csv")
#restrict to measured
bs.add= bs.add[which(bs.add$now_meas==1),]
bs.add$Project_info= "MuseumMeasure2022"
bs.add$Specimen_No= NA
bs.add$Elevation_low= bs.add$elev
bs.add$Elevation_upper= NA

#sep Stage.Sex
bs.add$Sex="M"
bs.add$Sex[grep("female", bs.add$Stage.Sex)]="F"

bs.add1= bs.add[,c("SpeciesName","Sites","year", "Project_info","Sex","Specimen_No","Barcode","R_femur1","R_femur2","L_femur1","L_femur2") ]
bs.add1$Mean_Femur= rowMeans(bs.add1[,c("L_femur1","L_femur2","R_femur1","R_femur2")], na.rm=T)
bs.add1$Mass=NA
bs.add1= cbind(bs.add1, bs.add$Elevation_low, bs.add$Elevation_upper)
#update species names
bs.add1$SpeciesName= gsub("clavatus", "A. clavatus", bs.add1$SpeciesName)
bs.add1$SpeciesName= gsub("conspersa", "A. conspersa", bs.add1$SpeciesName)
bs.add1$SpeciesName= gsub("simplex", "E. simplex", bs.add1$SpeciesName)
bs.add1$SpeciesName= gsub("sanguinipes", "M. sanguinipes", bs.add1$SpeciesName)

#update names
colnames(bs.add1)= c("Species","Sites","Year","Project_info","Sex","Specimen_No","Barcode","R_Femur1","R_Femur2","L_Femur1","L_Femur2","Mean_Femur","Mass","Elevation_low","Elevation_upper")

#combine
bs= rbind(bs, bs.add1)

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

#Write out data
write.csv(bs, "BodySize_all.csv")

#order factors
bs$Sex= factor(bs$Sex, order=TRUE, levels=c("F","M"))

#----------------
#subset data to matches

#species by seasonal timing
#add A. conspersa? "A. conspersa", 
specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
#specs= c("A. clavatus","C. pellucida","E. simplex","M. boulderensis","M. sanguinipes","X. corallipes") 

bs.sub= subset(bs, bs$Species %in% specs)
bs.sub$Species= factor(bs.sub$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#drop very small pellucida
bs.sub= bs.sub[-which(bs.sub$Mean_Femur<5.1),]

#subset elevations
elevs.keep= c(1768,2042,2134,2317,2591,3048,3414,3505, 3566, 3901)
bs.sub= subset(bs.sub, bs.sub$elev %in% elevs.keep)

# Drop few samples of X. corallipes chat
bs.sub= bs.sub[-which(bs.sub$Species=="X. corallipes" & bs.sub$elev %in%c(1768) ),]

#store data from focal elevations even if no match
bs.unmatched= bs.sub

##drop data without historic , current match
# A. clavatus sunshine, chicken ranch gulch
bs.sub= bs.sub[-which(bs.sub$Species=="A. clavatus" & bs.sub$elev %in%c(2042,2317) ),]
# X. corallipes chat, sunshine, chicken ranch gulch
bs.sub= bs.sub[-which(bs.sub$Species=="X. corallipes" & bs.sub$elev %in%c(1768,2042,2317) ),]
# M. sanguinipes C1, D1, sunshine
bs.sub= bs.sub[-which(bs.sub$Species=="M. sanguinipes" & bs.sub$elev %in%c(2317,3048,3566) ),]
#M. boulderensis Rollin's Pass, sunshine canyon, chicken ranch gulch
bs.sub= bs.sub[-which(bs.sub$Species=="M. boulderensis" & bs.sub$elev %in%c(2042,2317,3414) ),]
#C. pellucida sunshine
bs.sub= bs.sub[-which(bs.sub$Species=="C. pellucida" & bs.sub$elev %in%c(2317,3566) ),] #3048

#explore data
#full table
tab= with(bs.sub, table(Species, elev, Sex, time))
#counts by species and sites
with(bs.sub, table(Species, time, Sites))

#combine
tab1= cbind(tab[,1,,1],tab[,1,,2],tab[,2,,1],tab[,2,,2],tab[,3,,1],tab[,3,,2],tab[,4,,1],tab[,4,,2],tab[,5,,1],tab[,5,,2],tab[,6,,1],tab[,6,,2],tab[,7,,1],tab[,7,,2])
#write out
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/")
write.csv(tab1,"Counts_Sep2022.csv")

#Use date to assess temp, size relationship for all specimens
bs.all$Year= as.numeric(as.character(bs.all$Year))
bs.all$Year[which(bs.all$Year==1048)]<- 1948
bs.all$Year[which(bs.all$Year==1049)]<- 1949
bs.all$Year[which(bs.all$Year==1058)]<- 1958
bs.all$Year[which(bs.all$Year==1059)]<- 1959
bs.all$Year[which(bs.all$Year==1060)]<- 1960

#Write out data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
write.csv(bs.sub, "BodySize_sub.csv")

bs.all= bs.sub
#------
#Violin plot
bs.sub$SexTime= paste(bs.sub$Sex, bs.sub$time, sep="")
bs.sub$group= paste(bs.sub$Species, bs.sub$elev, bs.sub$Sex, bs.sub$time, sep="")
dodge <- position_dodge(width = 100)
jdodge <- position_jitterdodge(dodge.width = 100, jitter.width=100)

vplot= ggplot(data=bs.sub, aes(x=elev, y = Mean_Femur, group= SexTime, color=time, fill=time)) +
  facet_wrap(Species~., scales="free")+
  geom_point(position=jdodge, aes(shape=Sex))+
  theme_bw()+ geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  geom_violin(aes(group=group),alpha=0.6, width=400, position=dodge, scale="width")+
  theme_modern()+
  scale_fill_manual(values= c("darkorange","cadetblue"))+
  scale_color_manual(values= c("darkorange","cadetblue"))+
  scale_shape_manual(values=c(21,24,25))+
  xlab("Elevation (m)")+
  ylab("Femur length (mm)")

#add mean and se
bs.sum= ddply(bs.sub, c("Species", "elev", "Sex","time","SexTime"), summarise,
              N    = length(Mean_Femur),
              mean = mean(Mean_Femur),
              sd   = sd(Mean_Femur) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$N)

vplot= vplot + 
  geom_errorbar(data=bs.sum, position=position_dodge(width = 100), aes(x=elev, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=bs.sum, position=position_dodge(width = 100), aes(x=elev, y = mean, shape=Sex), size=3, col="black")

pdf("Size_by_ElevTime_violin.pdf",height = 12, width = 12)
vplot
dev.off()

#All sites
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("Size_by_ElevTime_Unmatched.pdf",height = 12, width = 12)
ggplot(data=bs.unmatched, aes(x=elev, y = Mean_Femur, color=time, shape=factor(Sex))) + 
  facet_wrap(Species~., scales="free")+geom_point(size=2)+
  theme_bw()+ geom_smooth(method="lm", aes(lty=Sex) )+
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  scale_shape_manual(values = c(16, 21))
dev.off()

#Violin plot
bs.unmatched$SexTime= paste(bs.unmatched$Sex, bs.unmatched$time, sep="")
bs.unmatched$group= paste(bs.unmatched$Species, bs.unmatched$elev, bs.unmatched$Sex, bs.unmatched$time, sep="")

vplot= ggplot(data=bs.unmatched, aes(x=elev, y = Mean_Femur, group= SexTime, color=time, fill=time)) +
  facet_wrap(Species~., scales="free")+
  geom_point(position=jdodge, aes(shape=Sex))+
  theme_bw()+ geom_smooth(method="lm", se=FALSE, aes(lty=Sex))+
  theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
  #scale_shape_manual(values = c(16, 21))+
  geom_violin(aes(group=group),alpha=0.6, width=400, position=dodge, scale="width")+
  theme_modern()+
  scale_fill_manual(values= c("darkorange","cadetblue"))+
  scale_color_manual(values= c("darkorange","cadetblue"))+
  scale_shape_manual(values=c(21,24,25))+
  xlab("Elevation (m)")+
  ylab("Femur length (mm)")

#add mean and se
bs.sum= ddply(bs.unmatched, c("Species", "elev", "Sex","time","SexTime"), summarise,
              N    = length(Mean_Femur),
              mean = mean(Mean_Femur),
              sd   = sd(Mean_Femur) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$N)

vplot= vplot + 
  geom_errorbar(data=bs.sum, position=position_dodge(width = 100), aes(x=elev, y=mean, ymin=mean-se, ymax=mean+se), width=0, col="black")+
  geom_point(data=bs.sum, position=position_dodge(width = 100), aes(x=elev, y = mean, shape=Sex), size=3, col="black")

pdf("Size_by_ElevTime_violin_Unmatched.pdf",height = 12, width = 12)
vplot
dev.off()

#-------
# perform a two-sample Kolmogorov-Smirnov test
bs.sub1= subset(bs.sub, bs.sub$Species=="E. simplex" )
bs.sub2= subset(bs.sub1, bs.sub1$Sites==c("Chautauqua Mesa") )
bs.sub2= subset(bs.sub2, bs.sub2$Sex==c("M") )

hist= subset(bs.sub2, bs.sub2$time==c("historic") )
curr= subset(bs.sub2, bs.sub2$time==c("current") )

ks.test(hist$Mean_Femur,curr$Mean_Femur)

#---------------------
# #species summary
# "X. corallipes"   
# time:elev= now smaller at higher elevations
#   
# "M. boulderensis" 
# time:elev:Sex= female bigger low, smaller high; males smaller low
#   
# "C. pellucida"    
# time:elev: smaller at low elevation
#
# larger at low elevation for nymphal diapausers: E. simplex, X. corallipes (smaller high)
# smaller at low elevation: C.pellucida
# M. boulderensis: female bigger low, smaller high; males smaller low

#=======================================
