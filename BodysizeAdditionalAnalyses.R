library(ggplot2)
library(reshape)

#Compare wet and dry size

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
wd= read.csv("WetDrySize_Mboulderensis.csv")
wd$Femur= rowMeans(wd[,c("RFemur1","RFemur2","LFemur1","LFemur2")] )

#to wide format
wdw= wd[,c("Condition","Sex","Specimen","Femur")]
wdw= reshape(wdw, idvar = c("Specimen","Sex"), timevar = "Condition", direction = "wide")

ggplot(data=wdw, aes(x=Femur.fresh, y = Femur.dry, color=Sex)) + 
  geom_point()+geom_abline(slope=1, intercept=0)
  
mod1=lm(Femur.dry~0 + Femur.fresh, data=wdw)

#======================
#Fitness analysis

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/AdditionalData/")
repro= read.csv("Levy_Female Gradient Data Grasshopper.csv")

ggplot(data=repro, aes(x=Femur_mm, y = ClutchMass_g, color=factor(Elevation_m))) + 
  geom_point()+facet_wrap(Species~., scales="free")+geom_smooth(method="lm")+theme_bw()

specs= c("A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes") 
mod1=lm(ClutchMass_g~Femur_mm + factor(Elevation_m), data=repro[repro$Species==specs[4],])
summary(mod1)
#Clutch mass of all but clavatus increase significantly with femur length

#====================
#Allometry

#load data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs= read.csv("GrasshopperSize_processed_checked2020.csv")

#just Buckley data with femur and mass
bs= bs[which(bs$Project_info=="BuckleyPhys"),]

#subset to species
specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
bs= subset(bs, bs$Species %in% specs)

ggplot(data=bs, aes(x=Mean_Femur, y = Mass, color=Sites, shape=Sex)) + 
  geom_point()+facet_grid(Species~., scales="free")+geom_smooth(method="lm")+theme_bw()

#====================================
#Inventory of historic specimens

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/SpecimenData/")
mus= read.csv("AlexanderSpecimens2021.csv")

#fix elevation
mus$Elevation_m_low= as.numeric(mus$Elevation_m_low)
mus$Elevation_m_up= as.numeric(mus$Elevation_m_up)
mus$elev= mus$Elevation_m_low

inds= !is.na(mus$Elevation_m_up)
mus$elev[inds]= (mus$Elevation_m_low[inds]+mus$Elevation_m_up[inds])/2

mus1= mus[,c("SpeciesName","LocalityCode","DateCollected","Elevation..m.","elev")]

#mus1= mus1[which(mus1$elev %in% c(2195, 2591, 3048) ),]

#species
specs=c("simplex","corallipes","clavatus","dodgei","pellucida","sanguinipes")

mus1= mus1[which(mus1$SpeciesName %in% specs),]

#sites
locs= c("us_co_boulder.2S.bureau.","us_co_uofcolo.a-1.7000","us_co_uofcolo.a-1.7200","us_co_uofcolo.a-1.2195","us_co_uofcolo.b-1.8500",
"us_co_uofcolo.b-1.W.8500", "us_co_uofcolo.b-1.1.5w","us_co_uofcolo.d-1.12280", 
"us_co_uofcolo.d-1.3499","us_co_uofcolo.c-1.10000","us_co_uofcolo.c-1.elk.10000","us_co_elk.meadows.mrs.3048",
"us_co_rollins.pass.11700", "us_co_rollins.pass.rd.9.2mi", "us_co_mt.evans.12300.sumL.2N",
"us_co_mt.evans.12750.sumL", "us_co_mt.evans.12100.goliath")

mus1= mus1[which(mus1$LocalityCode %in% locs),]

#dates
dates= as.Date(mus1$DateCollected, format="%m/%d/%y")
mus1$year= as.numeric( format(dates, format="%Y") )
#fix 19xx years
mus1$year[mus1$year>2017]= mus1$year[mus1$year>2017]-1000
#historic, cuurent
mus1$period= "initial"
mus1$period[mus1$year>1990]= "resurvey"

#summary
table(mus1[,c("SpeciesName","Elevation..m.","period")]) #"LocalityCode",
#more historical data available
#Chataqua? Can't find. Maybe us_co_boulder.2S.bureau.
#A1: simplex, sanguinipes
#B1: simplex, sanguinipes
#C1: pellucida
#Rollins: clavatus
#Mt. Evans: ?



