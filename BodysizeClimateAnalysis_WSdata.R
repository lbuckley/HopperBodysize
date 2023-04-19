library(car)
library(nlme)
library(lme4)
library(lmerTest)
library(patchwork)
library(ggplot2)
library(sjPlot)
library(plyr)
#library(MuMIn)

#try out weather station temperature data

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.all= read.csv("BodySize_wClim_plusWS.csv" )

env.vars= c(
  "Tspr","Tsum","springdd","Tspr.anom","Tsum.anom","springdd.anom","Tsum.prev","Tsum.anom.prev","t_28d","t28.anom",    
  "Tspr.min.anom.bould","Tsum.min.anom.bould","Tspr.mean.anom.bould","Tsum.mean.anom.bould","Tspr.max.anom.bould",       
"Tsum.max.anom.bould","Tspr.min.anom.bould.prev","Tsum.min.anom.bould.prev", 
"Tspr.mean.anom.bould.prev","Tsum.mean.anom.bould.prev","Tspr.max.anom.bould.prev","Tsum.max.anom.bould.prev", 
"Tspr.min.C1.anom","Tspr.mean.C1.anom","Tspr.max.C1.anom","Tsum.min.C1.anom",     
"Tsum.mean.C1.anom","Tsum.max.C1.anom","Tspr.min.C1.anom.prev","Tspr.mean.C1.anom.prev",   
"Tspr.max.C1.anom.prev","Tsum.min.C1.anom.prev","Tsum.mean.C1.anom.prev","Tsum.max.C1.anom.prev",
"tmin_28d.C1","tmean_28d.C1","tmax_28d.C1","tmin_28d.C1.anom","tmean_28d.C1.anom","tmax_28d.C1.anom"         
)    

bs.all <- transform(bs.all,
                       elev_cs=scale(elev) )

bs.all$Species= factor(bs.all$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#make elevation ordered factor
bs.all$elev_cs= factor(bs.all$elev_cs, ordered=TRUE)

#assess AICs
mod.aic= matrix(NA, nrow=length(env.vars), ncol=4)
mod.aic= as.data.frame(mod.aic)
mod.aic[,1]= env.vars

#loop through environmental variables
for(env.ind in 1:length(env.vars)){

bs.scaled= bs.all
bs.scaled$env.var= bs.scaled[,env.vars[env.ind]]
bs.scaled$env_cs= scale(bs.scaled$env.var)

#trim variables and drop NAs
bs.scaled= bs.scaled[,c("Femur.anom","env_cs","elev_cs","time","Sex","Species","Year","Sites")]
bs.scaled= na.omit(bs.scaled)
mod.aic[env.ind,2]=nrow(bs.scaled)

#time + climate model 
mod.lmer <- lmer(Femur.anom~env_cs*elev_cs*time*Sex*Species +
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled[-which(bs.scaled$Species=="X. corallipes"),]) 

#drop elevation
mod.lmer.noelev <- lmer(Femur.anom~env_cs*time*Sex*Species +
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled[-which(bs.scaled$Species=="X. corallipes"),]) 


mod.aic[env.ind,3]= AIC(mod.lmer)
mod.aic[env.ind,4]= AIC(mod.lmer.noelev)

} #end loop env vars

#-----------
anova(mod.lmer)
summary(mod.lmer)$coefficients
summary(mod.lmer)$AICtab
coef(mod.lmer)

plot_model(mod.lmer, type = "re")
plot_model(mod.lmer, type = "slope")
plot_model(mod.lmer, type = "resid")
plot_model(mod.lmer, type = "diag")

plot_model(mod.lmer, type = "pred", terms = c("env_cs","time"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("env_cs","Species"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("env_cs","elev_cs","time"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("env_cs","elev_cs","time", "Species"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("env_cs","time", "Sex", "Species"), show.data=TRUE)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim_combined.pdf",height = 12, width = 12)
plot_model(mod.lmer, type = "pred", terms = c("env_cs", "elev_cs","Species","Sex"), show.data=TRUE)
dev.off()
