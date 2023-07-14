#Analysis by species
#Run after BodysizeClimateAnalysis_NiwotAll

#============================
#By species time

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
stat= c("Sum Sq","NumDF","F value","Pr(>F)")

mod.lmer <- lmer(Femur.anom~time*elev_cs+Sex +
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled) 

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
  
  mod.lmer <- lmer(Femur.anom~time*elev_cs+Sex +
                     (1|Year/Sites),
                   REML = FALSE, na.action = 'na.fail', 
                   data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),]) 
  
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

mod.lmer <- lmer(Femur.anom~Tspr_cs*Tsum_cs*elev_cs*time*Sex + 
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
#mod.lmer <- lmer(Femur.anom~ time*elev_cs+env.var+env.var:elev_cs+Sex + 
#                   (1|Year/Sites),
#                 REML = FALSE,
#                 na.action = 'na.omit',
#                 data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])

env.k=1

for(spec.k in 1:length(specs)){
   
  mod.lmer <- lmer(Femur.anom~Tspr_cs*Tsum_cs*elev_cs*time*Sex + 
                   (1|Year/Sites),
                     REML = FALSE,
                     na.action = 'na.omit',
                     data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])
  
  stats.env[spec.k,env.k,,1:4]= as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")]
  coefs.env[spec.k,env.k,]= fixef(mod.lmer)[2:length(coef.names)]
  AICs.env[spec.k,env.k]= AIC(mod.lmer)
  BICs.env[spec.k,env.k]= BIC(mod.lmer)
  
  #plot output
  message(spec.k)
  modplots.env[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="pred",terms=c("elev_cs","time"), show.data=TRUE, title=specs[spec.k])
    print(p1)
  })
  
  modplots.env1[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="pred",terms=c("Tspr_cs","elev_cs"), show.data=TRUE, title=specs[spec.k])
    print(p1)
  })
  
  slopeplots.env[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="slope", title=specs[spec.k])
    print(p1)
  })
  
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

pdf("SlopePlots_clim.pdf",height = 12, width = 12)
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
