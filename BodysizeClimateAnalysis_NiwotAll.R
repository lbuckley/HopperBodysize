library(car)
library(nlme)
library(lme4)
library(lmerTest)
library(patchwork)
library(ggplot2)
library(sjPlot)
library(plyr)
library(see)
library(coefplot2)
#library(MuMIn)
library(tidyverse)
library(broom)
library(performance)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.all= read.csv("BodySize_wClim_plusNiwot.csv" )

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
#---------------------------
#analysis

#combined model
bs.sub1= bs.all[,c("Mean_Femur","Femur.anom","Year","time","elev","Sex","Species","Sites","SpTiming",
                   "Tspr.mean","Tspr.mean.anom","Tsum.mean.prev","Tsum.mean.anom.prev","Mean.mo","Tmo.anom")] 
bs.sub1= na.omit(bs.sub1)
#check drops

bs.scaled <- transform(bs.sub1,
                       Tspr_cs=scale(Tspr.mean),
                       Tsum_cs=scale(Tsum.mean.prev),
                       elev_cs=scale(elev),
                       Tspr.anom_cs=scale(Tspr.mean.anom),
                       Tsum.anom_cs=scale(Tsum.mean.anom.prev),
                       Mean.mo_cs=scale(Mean.mo),
                       Tmo.anom_cs= scale(Tmo.anom)
                       #, springdd.anom_cs=scale(springdd.anom),
                       #t28d_cs= scale(t_28d),
                       #t28.anom_cs= scale(t28.anom),
)

bs.scaled$Species= factor(bs.scaled$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))
bs.scaled$SpTiming= factor(bs.scaled$SpTiming, order=TRUE, levels=c("nymph","early","late"))
bs.scaled$time= factor(bs.scaled$time, order=TRUE, levels=c("historic","current"))

#make elevation ordered factor
#bs.scaled$elev_cs= factor(bs.scaled$elev_cs, ordered=TRUE )

#---------------
#TSR initial slopes

mod.lmer <- lmer(Mean_Femur~elev_cs*Sex*Species +
                   (1|Year:Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled[which(bs.scaled$time=="historic"),]) 

mod.lmer <- lmer(Mean_Femur~elev_cs*Sex*SpTiming +
                   (1|Year:Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled[which(bs.scaled$time=="historic"),]) 

#sp species
stats= array(data=NA, dim=c(length(specs),3,6),
             dimnames=list(specs, c("elev","sex","elev:sex"), c("coef","SE","Sum Sq","NumDF","F value","Pr(>F)")) ) 

for(spec.k in 1:length(specs)){
  
  mod.lmer <- lmer(Mean_Femur~elev_cs*Sex +
                     (1|Year:Sites),
                   REML = FALSE, na.action = 'na.fail', 
                   data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),]) 
  
  stats[spec.k,,]=cbind(coef(summary(mod.lmer))[2:nrow(coef(summary(mod.lmer))),1:2],
                           as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")])
} #end loop specs 

stat.mat= rbind(stats[1,,],stats[2,,],stats[3,,],stats[4,,],stats[5,,],stats[6,,] )
stat.mat= as.data.frame(stat.mat)
stat.mat=cbind(rep(specs, each=3), stat.mat)
stat.mat$sig=""
stat.mat$sig[stat.mat$p.value<0.05]="*"
stat.mat$sig[stat.mat$p.value<0.01]="**"
stat.mat$sig[stat.mat$p.value<0.001]="***"

stat.mat[,c(2:4,6:7)]= round(stat.mat[,c(2:4,6:7)],2)
stat.mat$var= rownames(stat.mat)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write_csv( stat.mat, 'species_slope.csv')

#--------------
#time model

mod.lmer <- lmer(Femur.anom~time*elev_cs*Sex*SpTiming +
                   (1|Year:Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled)
plot_model(mod.lmer, type = "pred", terms = c("elev_cs","time", "SpTiming"), show.data=TRUE)

#time model without sex
mod.lmer <- lmer(Femur.anom~time*elev_cs*SpTiming +
                   (1|Year:Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled)

summary(mod.lmer)$AICtab

check_model(mod.lmer)
check_collinearity(mod.lmer)

aov1= tidy(anova(mod.lmer))
aov1$sig=""
aov1$sig[aov1$p.value<0.05]="*"
aov1$sig[aov1$p.value<0.01]="**"
aov1$sig[aov1$p.value<0.001]="***"
aov1[,c(2:3,5:7)]=round( aov1[,c(2:3,5:7)],2)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write_csv( aov1, 'time_anova.csv')

plot_model(mod.lmer, show.values = TRUE)
plot_model(mod.lmer, type = "slope", rm.terms = "Sex")

#--------------
#time + climate model 
#spring temp or previous summer temp
#drop sex

mod.lmer <- lmer(Femur.anom~Tspr.anom_cs*elev_cs*time*SpTiming +
                   (1|Year:Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
mod.fig= plot_model(mod.lmer, type = "pred", terms = c("Tspr.anom_cs","elev_cs","time","SpTiming"), show.data=TRUE)

mod.lmer.sum <- lmer(Femur.anom~Tsum_cs*elev_cs*time*SpTiming +
                    (1|Year:Sites),
                  REML = FALSE, na.action = 'na.fail', 
                  data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
mod.sum= plot_model(mod.lmer.sum, type = "pred", terms = c("elev_cs","Tsum_cs","time","SpTiming"), show.data=TRUE)

#----------
#drop time period for figure?
mod.lmer <- lmer(Femur.anom~Tspr.anom_cs*elev_cs*SpTiming +
                   (1|Year:Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]

plot_model(mod.lmer, type = "pred", terms = c("Tspr.anom_cs","elev_cs","SpTiming"), show.data=FALSE)
#----------

aov1= tidy(anova(mod.lmer))
aov1$sig=""
aov1$sig[aov1$p.value<0.05]="*"
aov1$sig[aov1$p.value<0.01]="**"
aov1$sig[aov1$p.value<0.001]="***"
aov1[,c(2:3,5:7)]=round( aov1[,c(2:3,5:7)],2)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write_csv( aov1, 'time_climate_anova.csv')

summary(mod.lmer)$AICtab
coef(mod.lmer) #random effects
plot_model(mod.lmer, type = "slope")

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim_combined.pdf",height = 12, width = 12)
mod.fig
dev.off()

# #check collinearity of temp
# mod.lmer <- lmer(Femur.anom~Tspr.anom_cs+Tsum.anom_cs+elev_cs +
#                    (1|Year:Sites),
#                  REML = FALSE, na.action = 'na.fail', 
#                  data = bs.scaled) #[-which(bs.scaled$Species=="X. corallipes"),]
# 
# check_model(mod.lmer)
# check_collinearity(mod.lmer)
#low if use temperature anomalies 

# #species month
#Mean.mo_cs

#--------
#Other plots
#https://lmudge13.github.io/sample_code/mixed_effects.html
tab_model(mod.lmer)

plot_model(mod.lmer, show.values = TRUE)
plot_model(mod.lmer, type = "re")
plot_model(mod.lmer, type = "slope")
plot_model(mod.lmer, type = "resid")
plot_model(mod.lmer, type = "diag")
plot_model(mod.lmer, type = "int")

#library(remef)
