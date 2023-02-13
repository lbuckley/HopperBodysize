library(car)
library(nlme)
library(lme4)
library(lmerTest)
library(patchwork)
library(ggplot2)
library(sjPlot)
library(plyr)
#library(MuMIn)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.all= read.csv("BodySize_wClim.csv")

#add mean and se
bs.all.sum= ddply(bs.all, c("Species", "elev", "Sex","Year","SexElev"), summarise,
                  N    = length(Mean_Femur),
                  mean = mean(Mean_Femur),
                  std   = sd(Mean_Femur),
                  mean.anom = mean(Femur.anom),
                  std.anom   = sd(Femur.anom),
                  Tspr.anom = mean(Tspr.anom),
                  Tsum.anom = mean(Tsum.anom),
                  springdd.anom = mean(springdd.anom)
                  )
bs.all.sum$se= bs.all.sum$std / sqrt(bs.all.sum$N)
bs.all.sum$se.anom= bs.all.sum$std.anom / sqrt(bs.all.sum$N)

bs.all.sum$Species= factor(bs.all.sum$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

bs.all.sum$time="historic"
bs.all.sum$time[which(as.numeric(bs.all.sum$Year)>2000)]<-"current"
bs.all.sum$SexTime= paste(bs.all.sum$Sex, bs.all.sum$time, sep="") 
bs.all.sum$SexTimeElev= paste(bs.all.sum$Sex, bs.all.sum$time, bs.all.sum$elev, sep="") 

#PLOT
#make historic data hollow #also sd.anom, Tspr.anom, Tsum.anom, springdd.anom
plot.Tspr=ggplot(data=bs.all.sum, aes(x=Tspr.anom, y = mean, group= SexElev, color=factor(elev)) )+
  facet_wrap(Species~., scales="free")+
  geom_point(size=3, aes(shape=Sex, fill=factor(ifelse(time=="historic", NA, elev)) ))+ #size= sd.anom, 
  theme_bw()+ geom_smooth(method="lm", se=FALSE) +
  geom_errorbar( aes(ymin=mean-se, ymax=mean+se), width=0, col="black")+
  scale_shape_manual(values = c(21,24,25))+
  scale_fill_viridis_d(na.value=NA, guide="none")+
  scale_color_viridis_d()+
  #scale_color_brewer(palette = "Spectral") +
  xlab("Temperature anomaly (C)") +ylab("Femur size (mm)")
#+ scale_y_continuous(trans='log')

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim_time.pdf",height = 8, width = 8)
plot.Tspr
dev.off()

#---------------------------
#analysis

#combined model
bs.sub1= bs.all[,c("Mean_Femur","Femur.anom","Year","time","elev","Sex","Species","Sites","Tspr","Tspr.anom","Tsum","Tsum.anom","springdd","springdd.anom")] #,"springdd"
bs.sub1= na.omit(bs.sub1)
#check drops

bs.scaled <- transform(bs.sub1,
                       Tspr_cs=scale(Tspr),
                       elev_cs=scale(elev),
                       Tspr.anom_cs=scale(Tspr.anom),
                       Tsum.anom_cs=scale(Tsum.anom),
                       springdd.anom_cs=scale(springdd.anom)
)

bs.scaled$Species= factor(bs.scaled$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#time model
bs.sub1$elev_cs= scale(bs.sub1$elev)
bs.sub1$Species= factor(bs.sub1$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

mod.lmer <- lmer(Femur.anom~time*elev_cs*Sex*Species +
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.sub1) #[-which(bs.sub1$Species=="X. corallipes"),]

plot_model(mod.lmer, type = "pred", terms = c("elev_cs","time", "Species","Sex"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("elev_cs","time"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("elev_cs","time", "Species"), show.data=TRUE)

#time + climate model
mod.lmer <- lmer(Femur.anom~Tspr.anom_cs*elev_cs*time*Sex*Species +
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled[-which(bs.scaled$Species=="X. corallipes"),]) 

#limited model with sex
mod.lmer <- lmer(Femur.anom~Tspr.anom+time +
                   time:elev_cs +time:Sex +time:elev_cs:Sex+ 
                   Tspr.anom:elev_cs +Tspr.anom:Sex +Tspr.anom:elev_cs:Sex+
                   Tspr.anom:Species+time:Species +
                   time:elev_cs:Species +time:Sex:Species +time:elev_cs:Sex:Species+ 
                   Tspr.anom:elev_cs:Species +Tspr.anom:Sex:Species +Tspr.anom:elev_cs:Sex:Species+
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled)  #drop [-which(bs.scaled$Species=="X. corallipes"),]?

#split by Sex?
#size in a year: determined by time period (vary by elevation, Sex, species), climate, 

anova(mod.lmer)
summary(mod.lmer)$coefficients
summary(mod.lmer)$AICtab
coef(mod.lmer)

plot_model(mod.lmer, type = "re")
plot_model(mod.lmer, type = "slope")
plot_model(mod.lmer, type = "resid")
plot_model(mod.lmer, type = "diag")

plot_model(mod.lmer, type = "pred", terms = c("Tspr.anom_cs","elev_cs","Species"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("Tspr.anom_cs","elev_cs","time", "Species"), show.data=TRUE)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim_combined.pdf",height = 12, width = 12)
plot_model(mod.lmer, type = "pred", terms = c("Tspr.anom_cs", "elev_cs","Species","Sex"), show.data=TRUE)
dev.off()

#============================
#By species time

stat= c("Sum Sq","NumDF","F value","Pr(>F)")

mod.lmer <- lmer(Femur.anom~time*elev_cs*Sex +
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.sub1[which(bs.sub1$Species==specs[spec.k]),]) 

vars= rownames(anova(mod.lmer))

stats= array(data=NA, dim=c(length(specs),length(vars),4),
             dimnames=list(specs,vars, stat) ) 

modplots <- vector('list', length(specs))
slopeplots <- vector('list', length(specs))

for(spec.k in 1:length(specs)){
  
  mod.lmer <- lmer(Femur.anom~time*elev_cs*Sex +
                     (1|Year/Sites),
                   REML = FALSE, na.action = 'na.fail', 
                   data = bs.sub1[which(bs.sub1$Species==specs[spec.k]),]) 
  
  stats[spec.k,,1:4]=as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")]
  
  #plot output
  message(spec.k)
  modplots[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="pred",terms=c("elev_cs","time"), show.data=TRUE, title=specs[spec.k])
    print(p1)
  })
  
  slopeplots[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="slope", title=specs[spec.k])
    print(p1)
  })
  
} #end loop specs 

#save figure
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_time.pdf",height = 12, width = 12)
(modplots[[1]] | modplots[[4]]) / (modplots[[2]] | modplots[[5]]) / (modplots[[3]] | modplots[[6]])
dev.off()

pdf("SlopePlots_time.pdf",height = 12, width = 12)
(slopeplots[[1]] | slopeplots[[4]]) / (slopeplots[[2]] | slopeplots[[5]]) / (slopeplots[[3]] | slopeplots[[6]])
dev.off()

lmer.sig= stats[,,4]
lmer.sig[lmer.sig < 0.05] <- "*"

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write.csv(lmer.sig, "ModSig_time.csv")

#-------
#By Species time + climate model
#ANOVA output
stat= c("Sum Sq","NumDF","F value","Pr(>F)")

mod.lmer <- lmer(Femur.anom~Tspr.anom_cs*elev_cs+Sex+time +(1|Year/Sites),
                   REML = FALSE,
                   na.action = 'na.omit',
                   data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])

vars= rownames(anova(mod.lmer))

stats= array(data=NA, dim=c(length(specs),length(vars),4),
             dimnames=list(specs,vars, stat) ) 

modplots <- vector('list', length(specs))
slopeplots <- vector('list', length(specs))

for(spec.k in 1:length(specs)){
  
  #mod.lmer <- lmer(Mean_Femur~Tspr.anom_cs*sd.anom*elev_cs*time*Sex + (1|Year/Sites), #(1|Year/Sites)
  #                 REML = FALSE,
  #                 na.action = 'na.omit', data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])
  # mod.lmer <- lmer(Femur.anom~Tspr.anom+time +
  #                    time:elev_cs +time:Sex +time:elev_cs:Sex+ 
  #                    Tspr.anom:elev_cs +Tspr.anom:Sex +Tspr.anom:elev_cs:Sex+
  #                    (1|Year/Sites),
  #                  REML = FALSE, na.action = 'na.fail', 
  #                  data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),]) 
  # mod.lmer.f <- lmer(Mean_Femur~Tspr.anom_cs*elev_cs*time +(1|Year/Sites),
  #                                  REML = FALSE,
  #                                  na.action = 'na.omit',
  #                    data = bs.scaled[which(bs.scaled$Species==specs[spec.k] & bs.scaled$Sex=="F"),])

  mod.lmer <- lmer(Femur.anom~Tspr.anom_cs*elev_cs+Sex+time +(1|Year/Sites),
                     REML = FALSE,
                     na.action = 'na.omit',
                     data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])
  
  stats[spec.k,,1:4]=as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")]
  
  #plot output
  message(spec.k)
  modplots[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="pred",terms=c("Tspr.anom_cs","elev_cs"), show.data=TRUE, title=specs[spec.k])
    print(p1)
  })
  
  slopeplots[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="slope", title=specs[spec.k])
    print(p1)
  })
  
} #end loop specs 

#save figure
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim.pdf",height = 12, width = 12)
(modplots[[1]] | modplots[[4]]) / (modplots[[2]] | modplots[[5]]) / (modplots[[3]] | modplots[[6]])
dev.off()

pdf("SlopePlots_clim.pdf",height = 12, width = 12)
(slopeplots[[1]] | slopeplots[[4]]) / (slopeplots[[2]] | slopeplots[[5]]) / (slopeplots[[3]] | slopeplots[[6]])
dev.off()

lmer.sig= stats[,,4]
lmer.sig[lmer.sig < 0.05] <- "*"

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write.csv(lmer.sig, "ModSig.csv")

#generally bigger through time (evolution) but larger with temperature (plasticity)?
#time effects: clavatus, pellucida
#temp*time*sex: simplex



