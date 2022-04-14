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

#Inventory 2021
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
locs= c("us_co_boulder.chau.5800","us_co_boulder.2S.bureau.","us_co_uofcolo.a-1.7000","us_co_uofcolo.a-1.7200","us_co_uofcolo.a-1.2195","us_co_uofcolo.b-1.8500",
"us_co_uofcolo.b-1.W.8500", "us_co_uofcolo.b-1.1.5w","us_co_uofcolo.d-1.12280", 
"us_co_uofcolo.d-1.3499","us_co_uofcolo.c-1.10000","us_co_uofcolo.c-1.elk.10000","us_co_elk.meadows.mrs.3048",
"us_co_rollins.pass.11700", "us_co_rollins.pass.rd.9.2mi", "us_co_mt.evans.12300.sumL.2N",
"us_co_mt.evans.12750.sumL", "us_co_mt.evans.12100.goliath")
#mus1= mus1[which(mus1$LocalityCode %in% locs),]

#dates
dates= as.Date(mus1$DateCollected, format="%m/%d/%y")
mus1$year= as.numeric( format(dates, format="%Y") )
#fix 19xx years
inds= which(mus1$year>2017)
mus1$year[inds]= mus1$year[inds]-1000
#historic, current
mus1$period= "initial"
mus1$period[mus1$year>1990]= "resurvey"

#summary
table(mus1[,c("SpeciesName","Elevation..m.","period")]) #"LocalityCode",

tab1= table(mus1[,c("LocalityCode","SpeciesName","period")]) 

#subset to locations with >50 specimens
tab.sub= tab1[,,1]
tab.rs= rowSums(tab.sub)
tab.sub= tab.sub[which(tab.rs>50),]

#Figure out specimens to measure
#bs$Barcode
#mus$SpecimenCode

#Historical
#Chat
mc1= mus1[which(mus1$LocalityCode=="us_co_boulder.chau.5800" & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
#sanguinipes: 13 measured, 46 available

#A1
mc2= mus1[which(mus1$LocalityCode=="us_co_uofcolo.a-1.7000" & mus1$SpeciesName=="corallipes" & mus1$period=="initial"),]
mc3= mus1[which(mus1$LocalityCode=="us_co_uofcolo.a-1.7000" & mus1$SpeciesName=="clavatus" & mus1$period=="initial"),]
mc4= mus1[which(mus1$LocalityCode=="us_co_uofcolo.a-1.7000" & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
#corallipes: 20 measured, 61 available
#clavatus: 11 measured, 22 available
#sanguinipes: 12 measured, 40 available

#B1
mc= mus1[which(mus1$LocalityCode %in% c("us_co_uofcolo.b-1.8500","us_co_uofcolo.b-1.W.8500") & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
mc= mus1[which(mus1$LocalityCode %in% c("us_co_uofcolo.b-1.8500","us_co_uofcolo.b-1.W.8500") & mus1$SpeciesName=="simplex" & mus1$period=="initial"),]
#sanguinipes: 16 measured, 63 available
#simplex: 15 f measured, 77 available

#C1
mc= mus1[which(mus1$LocalityCode %in% c("us_co_uofcolo.c-1.10000") & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
mc= mus1[which(mus1$LocalityCode %in% c("us_co_uofcolo.c-1.10000") & mus1$SpeciesName=="pellucida" & mus1$period=="initial"),]
#pellucida: 39 measured, 58 available
#sanguinipes: 0 measured, 26 available

#D1
mc= mus1[which(mus1$LocalityCode %in% c("us_co_niwot.alpine") & mus1$SpeciesName=="sanguinipes" & mus1$period=="initial"),]
#some historic sanguinipes available
 
#Mt. Evans
mc= mus1[which(mus1$LocalityCode %in% c("us_co_mt.evans.11500.tline","us_co_mt.evans.12100.goliath","us_co_mt.evans.12800.sumL") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#clavatus: 31 hist measured 
#boulderensis: 47 current measured, 30 hist measured
#more current and historical clavatus and boulderensis to measure
#historical sanguinipes to measure, collect current?

#Rollins: 
mc= mus1[which(mus1$LocalityCode %in% c("us_co_rollins.pass.11700","us_co_rollins.pass.rd.15.2mi","us_co_rollins.pass.rd.E.11000","us_co_rollins.pass.rd.E.11200") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#more historical boulderensis, clavatus, and sanguinipes to measure

#Sunshine
mc= mus1[which(mus1$LocalityCode %in% c("us_co_sunshine-fourmile.can","us_co_sunshine.can.rd.6600"," us_co_sunshine.can.rd.7600","us_co_sunshine.can.rd.9.3.9000") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#check out modern?

#Chicken Gulch Ranch
mc= mus1[which(mus1$LocalityCode %in% c("us_co_chicken.ranch.gulch.6700") ),]
table(mc[,c("SpeciesName","LocalityCode","period")])
#many historic and some modern available
#check out modern?

#other potential sites for future collections
loc=c("us_co_baldy.mtn.10800-11000","us_co_buchanan.E1080","us_co_buchanan.E11300+","us_co_greenhorn.mtn.12300-1240","us_co_mont.alto.8500","us_co_rmnp.little.horse.8700","us_co_pikes.pk.12900-13000","us_co_pikes.pk.12900.SW")
mc= mus1[which(mus1$LocalityCode %in% c(loc) ),]
table(mc[,c("SpeciesName","LocalityCode","period")])

#====================
#Inventory 2022
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
mus= read.csv("BodySize_all_Apr2022.csv")

#species
specs=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
mus1= mus[which(mus$Species %in% specs),]

#historic, current
mus1$period<- "initial"
mus1$period[mus1$Year>1990]= "resurvey"

#summary
table(mus1[,c("period","elev","Species")]) 
table(mus1[,c("Species","period","Sites")])

table(mus1[,c("Sites","elev","Species")])

#subset to locations with >50 specimens
tab.sub= tab1[,,1]
tab.rs= rowSums(tab.sub)
tab.sub= tab.sub[which(tab.rs>50),]

#Add modern specimens?
#A. clavatus
#Baldy mountain: 60 initial
#Buchanan Pass: 30 initial
#Chicken  Ranch Gulch: 30 initial
#Mt. Evans: 192 initial, 21 resurvey
#Sunshine: 45 initial

#E. simplex
#Sunshine: 10 initial

#M. boulderensis
#Chicken Ranch Gulch: 30

#M. sanguinipes (low priority?)
#D1: 18 initial

#X. corallipes
#Baldy mountain: 31 initial
#Chicken Ranch Gulch: 50 initial
#Sunshine Canyon: 49 initial

#COLLECTING
#Sunshine (2317m): A. clavatus, E. simplex, X. corallipes
#Baldy (3322m): A. clavatus, X. corallipes
#Chicken Ranch Gulch (no longer viable?, 2042m): A. clavatus, M. boulderensis, X. corallipes
#Mount Evans (3505, 3688, 3883, 3993 initial only, 33901 31 initial, 75 recent): A. clavatus
#Buchanan Pass (tough to get to?): A. clavatus

#Baldy Mountain	Boulder County	39.988	-105.61	3290
#Up road 128 from Nederland

#MUSEUM CHECK
# Baldy Mountain: boulderensis
# Mt. evans: boulderensis other than summit lake, sanguinipes
# Pikes peak: clavatus, boulderensis (Ask Julian to collect modern?)
# Sunshine: boulderensis, sanguinipes






