library(ggplot2)
library(reshape)
library(reshape2)
library(nlme)
library(lme4)

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

#model
rvars<- c("EggMass","NOvarioles","NFunctOvarioles","PropFunctOvarioles","ClutchMass_g")
y<- rvars[5]
mod.lmer <- lm(EggMass ~Femur_mm*Elevation_m*Species,
                 na.action = 'na.omit', data = repro)
#plot_model(mod.lmer, type = "pred", terms = c("Femur_mm","Species"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=TRUE)

repro.l<- melt(repro, id.vars = c("ID","Species","Site","Femur_mm","Elevation_m"))

ggplot(data=repro.l, aes(y=value, x = Femur_mm, color=factor(Elevation_m))) + 
  geom_point()+facet_grid(variable~Species, scales="free")+geom_smooth(method="lm")+theme_bw()

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
