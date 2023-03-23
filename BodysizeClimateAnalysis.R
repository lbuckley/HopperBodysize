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

#violin plot for anomaly
bs.all$SexTime= paste(bs.all$Sex, bs.all$time, sep="")
bs.all$group= paste(bs.all$Species, bs.all$elev, bs.all$Sex, bs.all$time, sep="")
dodge <- position_dodge(width = 100)
jdodge <- position_jitterdodge(dodge.width = 100, jitter.width=100)

bs.all$Species= factor(bs.all$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

vplot= ggplot(data=bs.all, aes(x=elev, y = Femur.anom, group= SexTime, color=time, fill=time)) +
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
bs.sum= ddply(bs.all, c("Species", "elev", "Sex","time","SexTime"), summarise,
              N    = length(Femur.anom),
              mean = mean(Femur.anom),
              sd   = sd(Femur.anom) )
bs.sum$se= bs.sum$sd / sqrt(bs.sum$mean)

vplot= vplot + 
 geom_point(data=bs.sum, position=position_dodge(width = 100), aes(x=elev, y = mean, shape=Sex), size=3, col="black")

pdf("Size_by_ElevTime_violin_anomally.pdf",height = 12, width = 12)
vplot
dev.off()

#----------
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
bs.sub1= bs.all[,c("Mean_Femur","Femur.anom","Year","time","elev","Sex","Species","Sites","Tspr","Tspr.anom","Tsum","Tsum.anom","springdd","springdd.anom","t_28d","t28.anom","Tsum.prev","Tsum.anom.prev")] #,"springdd"
bs.sub1= na.omit(bs.sub1)
#check drops

bs.scaled <- transform(bs.sub1,
                       Tspr_cs=scale(Tspr),
                       elev_cs=scale(elev),
                       Tspr.anom_cs=scale(Tspr.anom),
                       Tsum.anom_cs=scale(Tsum.anom),
                       springdd.anom_cs=scale(springdd.anom),
                       t28d_cs= scale(t_28d),
                       t28.anom_cs= scale(t28.anom),
                       Tsum.prev_cs=scale(Tsum.prev),
                       Tsum.anom.prev_cs=scale(Tsum.anom.prev)
)

bs.scaled$Species= factor(bs.scaled$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#time model
bs.sub1$elev_cs= scale(bs.sub1$elev)
bs.sub1$Species= factor(bs.sub1$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

mod.lmer <- lmer(Femur.anom~time*elev_cs*Sex*Species +
                   (1|Year/Sites),
                 REML = FALSE,
                 na.action = 'na.omit', data = bs.scaled[-which(bs.scaled$Species=="X. corallipes"),]) #bs.scaled[-which(bs.scaled$Species=="X. corallipes"),]

plot_model(mod.lmer, type = "pred", terms = c("elev_cs","time","Sex", "Species"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("elev_cs","time"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("elev_cs","time", "Sex"), show.data=TRUE)

#time + climate model
bs.scaled$env.var= bs.scaled$Tspr.anom_cs
#bs.scaled$env.var= bs.scaled$t28.anom_cs

mod.lmer <- lmer(Femur.anom~env.var*elev_cs*time*Sex*Species +
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled[-which(bs.scaled$Species=="X. corallipes"),]) 

# #limited model with sex
# mod.lmer <- lmer(Femur.anom~env.var+time +
#                    time:elev_cs +time:Sex +time:elev_cs:Sex+ 
#                    env.var:elev_cs +env.var:Sex +env.var:elev_cs:Sex+
#                    env.var:Species+time:Species +
#                    time:elev_cs:Species +time:Sex:Species +time:elev_cs:Sex:Species+ 
#                    env.var:elev_cs:Species +env.var:Sex:Species +env.var:elev_cs:Sex:Species+
#                    (1|Year/Sites),
#                  REML = FALSE, na.action = 'na.fail', 
#                  data = bs.scaled)  #drop [-which(bs.scaled$Species=="X. corallipes"),]?

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

plot_model(mod.lmer, type = "pred", terms = c("env.var","time"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("env.var","Species"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("env.var","elev_cs","time"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("env.var","elev_cs","time", "Species"), show.data=TRUE)
plot_model(mod.lmer, type = "pred", terms = c("env.var","time", "Sex", "Species"), show.data=TRUE)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim_combined.pdf",height = 12, width = 12)
plot_model(mod.lmer, type = "pred", terms = c("env.var", "elev_cs","Species","Sex"), show.data=TRUE)
dev.off()

#============================
#By species time

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
stat= c("Sum Sq","NumDF","F value","Pr(>F)")

mod.lmer <- lmer(Femur.anom~time*elev_cs+Sex +
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.sub1) 

vars= rownames(anova(mod.lmer))
coef.names= names(fixef(mod.lmer))

stats= array(data=NA, dim=c(length(specs),length(vars),4),
             dimnames=list(specs,vars, stat) ) 
coefs= array(data=NA, dim=c(length(specs),length(coef.names)-1),
                dimnames=list(specs, coef.names[2:length(coef.names)] )) 
AICs= rep(NA, length(specs))
BICs= rep(NA, length(specs))

modplots <- vector('list', length(specs))
slopeplots <- vector('list', length(specs))

for(spec.k in 1:length(specs)){
  
  # mod.lmer <- lmer(Femur.anom~time*elev_cs*Sex +
  #                    (1|Year/Sites),
  #                  REML = FALSE, na.action = 'na.fail', 
  #                  data = bs.sub1[which(bs.sub1$Species==specs[spec.k]),]) 
  
  mod.lmer <- lmer(Femur.anom~time*elev_cs+Sex +
                     (1|Year/Sites),
                   REML = FALSE, na.action = 'na.fail', 
                   data = bs.sub1[which(bs.sub1$Species==specs[spec.k]),]) 
  
  stats[spec.k,,1:4]=as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")]
  coefs[spec.k,]= fixef(mod.lmer)[2:(length(vars)+1)]
  AICs[spec.k]= AIC(mod.lmer)
  BICs[spec.k]= BIC(mod.lmer)
  
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

#-------
#By Species time + climate model

#ANOVA output
stat= c("Sum Sq","NumDF","F value","Pr(>F)")

mod.lmer <- lmer(Femur.anom~ time*elev_cs+env.var+env.var:elev_cs+Sex + #env.var*elev_cs+Sex+time 
                   (1|Year/Sites),
                   REML = FALSE,
                   na.action = 'na.omit',
                   data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])

vars= rownames(anova(mod.lmer))
coef.names= names(fixef(mod.lmer))

stats.env= array(data=NA, dim=c(length(specs),3, length(vars),4),
             dimnames=list(specs,c("spr","sprmonth","prevsum"),vars, stat) ) 
coefs.env= array(data=NA, dim=c(length(specs),3, length(coef.names)-1),
             dimnames=list(specs, c("spr","sprmonth","prevsum"), coef.names[2:length(coef.names)] )) 
AICs.env= matrix(NA, length(specs),3)
BICs.env= matrix(NA, length(specs),3)

modplots.env <- vector('list', length(specs))
modplots.env1 <- vector('list', length(specs))
slopeplots.env <- vector('list', length(specs))

#mod.lmer <- lmer(Mean_Femur~env.var_cs*sd.anom*elev_cs*time*Sex + (1|Year/Sites), #(1|Year/Sites)
#                 REML = FALSE,
#                 na.action = 'na.omit', data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])
# mod.lmer <- lmer(Femur.anom~env.var+time +
#                    time:elev_cs +time:Sex +time:elev_cs:Sex+ 
#                    env.var:elev_cs +env.var:Sex +env.var:elev_cs:Sex+
#                    (1|Year/Sites),
#                  REML = FALSE, na.action = 'na.fail', 
#                  data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),]) 
# mod.lmer.f <- lmer(Mean_Femur~env.var_cs*elev_cs*time +(1|Year/Sites),
#                                  REML = FALSE,
#                                  na.action = 'na.omit',
#                    data = bs.scaled[which(bs.scaled$Species==specs[spec.k] & bs.scaled$Sex=="F"),])

for(spec.k in 1:length(specs)){
  for (env.k in 1:3){
    if(env.k==1) bs.scaled$env.var= bs.scaled$Tspr.anom_cs
    if(env.k==2) bs.scaled$env.var= bs.scaled$t28.anom_cs
    if(env.k==3) bs.scaled$env.var= bs.scaled$Tsum.anom.prev_cs
  
  mod.lmer <- lmer(Femur.anom~time*elev_cs+env.var+env.var:elev_cs+Sex +(1|Year/Sites),
                     REML = FALSE,
                     na.action = 'na.omit',
                     data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])
  
  stats.env[spec.k,env.k,,1:4]= as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")]
  coefs.env[spec.k,env.k,]= fixef(mod.lmer)[2:(length(vars)+1)]
  AICs.env[spec.k,env.k]= AIC(mod.lmer)
  BICs.env[spec.k,env.k]= BIC(mod.lmer)
  
  #plot output
  if(env.k==1){
  message(spec.k)
  modplots.env[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="pred",terms=c("elev_cs","time"), show.data=TRUE, title=specs[spec.k])
    print(p1)
  })
  
  modplots.env1[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="pred",terms=c("env.var","elev_cs"), show.data=TRUE, title=specs[spec.k])
    print(p1)
  })
  
  slopeplots.env[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="slope", title=specs[spec.k])
    print(p1)
  })
  } #check env
  
  
  } #end loop envi var
} #end loop specs 

#save figure
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_timeclim.pdf",height = 12, width = 12)
(modplots[[1]] | modplots.env[[1]] | modplots.env1[[1]])/
  (modplots[[2]] | modplots.env[[2]] | modplots.env1[[2]])/
  (modplots[[3]] | modplots.env[[3]] | modplots.env1[[3]])/
  (modplots[[4]] | modplots.env[[4]] | modplots.env1[[4]])/
  (modplots[[5]] | modplots.env[[5]] | modplots.env1[[5]])/
  (modplots[[6]] | modplots.env[[6]] | modplots.env1[[6]])
dev.off()

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim.pdf",height = 12, width = 12)
(modplots[[1]] | modplots[[4]]) / (modplots[[2]] | modplots[[5]]) / (modplots[[3]] | modplots[[6]])
dev.off()

pdf("SlopePlots_clim.pdf",height = 25, width = 6)
  (slopeplots[[1]] | slopeplots.env[[1]])/
  (slopeplots[[2]] | slopeplots.env[[2]])/
  (slopeplots[[3]] | slopeplots.env[[3]])/
  (slopeplots[[4]] | slopeplots.env[[4]])/
  (slopeplots[[5]] | slopeplots.env[[5]])/
  (slopeplots[[6]] | slopeplots.env[[6]])
dev.off()
#fix layout

#write stats
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")

#ANOVA output
anova.time= cbind(coefs[,1],stats[,1,3:4], coefs[,2],stats[,2,3:4], coefs[,3],stats[,3,3:4], coefs[,4],stats[,4,3:4])
anova.time= round(anova.time,2)
anova.time[which(anova.time[,3] < 0.05),3] <- "*"
anova.time[which(anova.time[,6] < 0.05),6] <- "*"
anova.time[which(anova.time[,9] < 0.05),9] <- "*"
anova.time[which(anova.time[,12] < 0.05),12] <- "*"
colnames(anova.time)[c(1,4,7,10)]=colnames(coefs)
write.csv(anova.time, "Anova_time.csv")

anova.tspr= cbind(coefs.env[,1,1],stats.env[,1,1,3:4], coefs.env[,1,2],stats.env[,1,2,3:4],
                  coefs.env[,1,3],stats.env[,1,3,3:4], coefs.env[,1,4],stats.env[,1,4,3:4],
                  coefs.env[,1,5],stats.env[,1,5,3:4], coefs.env[,1,6],stats.env[,1,6,3:4])
anova.tspr= round(anova.tspr,2)
anova.tspr[which(anova.tspr[,3] < 0.05),3] <- "*"
anova.tspr[which(anova.tspr[,6] < 0.05),6] <- "*"
anova.tspr[which(anova.tspr[,9] < 0.05),9] <- "*"
anova.tspr[which(anova.tspr[,12] < 0.05),12] <- "*"
anova.tspr[which(anova.tspr[,15] < 0.05),15] <- "*"
anova.tspr[which(anova.tspr[,18] < 0.05),18] <- "*"
colnames(anova.tspr)[c(1,4,7,10,13,16)]=dimnames(coefs.env)[[3]]
write.csv(anova.tspr, "Anova_Tspr.csv")

#significance
lmer.sig= cbind(stats[,,4], stats.env[,1,,4], stats.env[,2,,4], stats.env[,3,,4] )
lmer.sig= round(lmer.sig,3)
lmer.sig[lmer.sig < 0.05] <- "*"
write.csv(lmer.sig, "ModSig.csv")

#write coeficients
coefs.all= cbind(coefs, coefs.env[,1,],coefs.env[,2,],coefs.env[,3,] )
coefs.all= round(coefs.all,1)
row.names(coefs.all)= specs
write.csv(coefs.all, "ModCoef.csv")

#write AICS
AIC.all= cbind(AICs, AICs.env)
AIC.all= round(AIC.all,1)
row.names(AIC.all)= specs
colnames(AIC.all)= c("timeonly","spr","sprmonth","prevsum")
write.csv(AIC.all, "ModAIC.csv")

#write BICS
BIC.all= cbind(BICs, BICs.env)
BIC.all= round(BIC.all,1)
row.names(BIC.all)= specs
colnames(BIC.all)= c("timeonly","spr","sprmonth","prevsum")
write.csv(BIC.all, "ModBIC.csv")

#generally bigger through time (evolution) but larger with temperature (plasticity)?
#time effects: clavatus, pellucida
#temp*time*sex: simplex

#-------
#Model selection across climate variables
#https://doi.org/10.1002/ecy.3336
#try LASSO?

spec.k=6

#bs.scaled$envvar= bs.scaled$Tspr.anom_cs
#bs.scaled$envvar= bs.scaled$Tsum.anom.prev_cs
bs.scaled$envvar= bs.scaled$t28.anom_cs

ggplot(data=bs.scaled[which(bs.scaled$Species==specs[spec.k]),], 
       aes(x=envvar, y = Femur.anom, color=elev)) +
         geom_point()+geom_smooth(method="lm")
