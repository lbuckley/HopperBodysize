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

repro.l<- melt(repro[,c("ID","Species","Site","Femur_mm","Elevation_m","EggMass","ClutchMass_g","NOvarioles","PropFunctOvarioles")], id.vars = c("ID","Species","Site","Femur_mm","Elevation_m"))
#order species
repro.l$Species<- factor(repro.l$Species, order=TRUE, levels=c("A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))
#Rename order variables
rvar<- c("EggMass","ClutchMass_g","NOvarioles","PropFunctOvarioles")
rvar.lab<- c("Egg mass (g)","Clutch mass (g)","N ovarioles","Prop Funct Ovarioles")
repro.l$variable<- rvar.lab[match(repro.l$variable, rvar)]
repro.l$variable<- factor(repro.l$variable, order=TRUE, levels=c("Egg mass (g)","Clutch mass (g)","N ovarioles","Prop Funct Ovarioles"))
#Rename variables

repro.plot= ggplot(data=repro.l, aes(y=value, x = Femur_mm, color=factor(Elevation_m) )) + 
  geom_point()+facet_grid(variable~Species, scales="free", switch="y")+geom_smooth(method="lm", aes(fill=factor(Elevation_m)))+theme_bw()+
  scale_color_viridis_d(name="elevation (m)")+ scale_fill_viridis_d()+
  xlab("Femur length (mm)")+ylab("")+
  theme(legend.position="bottom")+ guides(fill="none")
  
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("FigRepro.pdf",height = 8, width = 8)
repro.plot
dev.off()

ggplot(data=repro.l, aes(y=value, x = Elevation_m, color=Femur_mm)) + 
  geom_point()+facet_grid(variable~Species, scales="free")+geom_smooth(method="lm")+theme_bw()

#models
mod.lmer1 <- lm(EggMass ~Femur_mm*Elevation_m*Species,
               na.action = 'na.omit', data = repro)

mod.lmer2 <- lm(ClutchMass_g ~Femur_mm*Elevation_m*Species,
               na.action = 'na.omit', data = repro)

mod.lmer3 <- lm(NOvarioles ~Femur_mm*Elevation_m*Species,
               na.action = 'na.omit', data = repro)

mod.lmer4 <- lm(PropFunctOvarioles ~Femur_mm*Elevation_m*Species,
               na.action = 'na.omit', data = repro)

pmod1<- plot_model(mod.lmer1, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=FALSE, title = "", axis.title = c(title = "Egg mass (g)") )
pmod2<- plot_model(mod.lmer2, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=FALSE, title = "", axis.title = c(title = "Clutch mass (g)") )
pmod3<- plot_model(mod.lmer3, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=FALSE, title = "", axis.title = c(title = "N ovarioles") )
pmod4<- plot_model(mod.lmer4, type = "pred", terms = c("Femur_mm","Elevation_m", "Species"), show.data=FALSE, title = "", axis.title = c(title = "Prop Funct Ovarioles") )

pmod1$facet$params$nrow=1
pmod2$facet$params$nrow=1
pmod3$facet$params$nrow=1
pmod4$facet$params$nrow=1

#update model plots
pmod1<- pmod1+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")
pmod2<- pmod2+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
pmod3<- pmod3+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
pmod4<- pmod4+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

pmods<- pmod1 + pmod2 + pmod3 + pmod4 +plot_layout(ncol = 1) + 
  plot_layout(guides = "collect")  & xlab(NULL) & theme(plot.margin = margin(5.5, 5.5, 5.5, 0))
#+ plot_annotation(tag_levels = 'A')

# Use the tag label as an x-axis label
pmods <- wrap_elements(panel = pmods) +
  labs(tag = "Femur length (mm)") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )

#plot together
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Nov2023/")
pdf("FigRepro_mod.pdf",height = 8, width = 8)
pmods
dev.off()

anova(mod.lmer)

clim.mod.fig.sum<- clim.mod.fig.sum+ theme_bw()+
  scale_color_viridis_d(name="elevation")+
  scale_fill_viridis_d(name="elevation")+
  ylab("Femur length anomaly (mm)")+ xlab("scaled Summer temperature anomally (C)")

#sp species
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
