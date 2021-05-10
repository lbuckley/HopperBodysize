library(ggplot2)
library(sjPlot)
library(patchwork)

#load data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs= read.csv("Grasshopper_processed.csv")

#add time period
bs$time="historic"
bs$time[which(bs$Year>2000)]<-"current"

#subset data
sites= c("Chautauqua Mesa","A1","B1","C1","Niwot Ridge (D1)","Rollin's Pass") 
elevs= c(1752,2195,2591,3048,3745,3559)

#species by seasonal timing
specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
#specs= c("A. clavatus","C. pellucida","E. simplex","M. boulderensis","M. sanguinipes","X. corallipes") 

bs.sub= subset(bs, bs$Sites %in% sites)
bs.sub= subset(bs.sub, bs.sub$Species %in% specs)

#add elevation
bs.sub$elev= elevs[match(bs.sub$Sites, sites)]

#order factors
bs.sub$Sex= factor(bs.sub$Sex, order=TRUE, levels=c("F","M"))
bs.sub$Species= factor(bs.sub$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

#explore data
#full table
with(bs.sub, table(Species, Sites, Sex, time))
#counts by species and sites
with(bs.sub, table(Species, time))

#scatter plot
#ggplot(data=bs.sub, aes(x=Year, y = Mean_Femur, color=Mass))+ 
#  facet_grid(Species~Sites, scales="free")+geom_point()+
#  theme_bw()+ geom_smooth(method="lm")
 # theme(legend.position="bottom", legend.key.width=unit(3,"cm"), axis.title=element_text(size=16))+
#  scale_color_viridis_c()

#FIGURE 1- density plots
#density plots
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/")
pdf("DenPlots.pdf",height = 12, width = 12)
ggplot(data=bs.sub, aes(x=Mean_Femur, color=time,lty=Sex))+ 
  facet_grid(Species~elev, scales="free")+geom_density(aes(fill=time, alpha=0.3))+theme_bw()
dev.off()

#combined model
mod1= lm(Mean_Femur~time*elev*Sex*Species, data=bs.sub)
plot_model(mod1, type="pred",terms=c("elev","time","Species","Sex"), show.data=TRUE)

#By species
#ANOVA output
stat= c("df","sumsq","meansq","F","p")
vars= c("time","elev","sex","time:elev","time:sex","elev:sex","time:elev:sex","residuals")

stats= array(data=NA, dim=c(length(specs),8,5),
             dimnames=list(specs,vars,stat) ) 

#FIGURE 2- model output

modplots <- vector('list', length(specs))

for(spec.k in 1:length(specs)){

  mod1= lm(Mean_Femur~time*elev*Sex, data=bs.sub[which(bs.sub$Species==specs[spec.k]),])
  stats[spec.k,,]=as.matrix(anova(mod1))
    
    #plot output
   message(spec.k)
   modplots[[spec.k]] <- local({
     spec.k <- spec.k
     p1 <- plot_model(mod1, type="pred",terms=c("elev","time","Sex"), show.data=TRUE,
                      title=specs[spec.k], legend.title = "period",
                      axis.title=c("elevation (m)","femur length (mm)"))
     print(p1)
   })
   
} #end loop specs 

#save figure
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/")
pdf("ModPlots.pdf",height = 12, width = 12)
(modplots[[1]] | modplots[[4]]) / (modplots[[2]] | modplots[[5]]) / (modplots[[3]] | modplots[[6]])
dev.off()

#save output
#round
stats[,,5]= signif(stats[,,5],3)
#stats.all=rbind(stats[1,,],stats[2,,],stats[3,,],stats[4,,],stats[5,,],stats[6,,])
#write.csv(stats[,,5],"Table1.csv")

#save coefficients
#mo.p=summary(mod1)$coefficients
#signif(), round()

# perform a two-sample Kolmogorov-Smirnov test
bs.sub1= subset(bs.sub, bs.sub1$Species %in% "M. sanguinipes")
bs.sub2= subset(bs.sub1, bs.sub$Sites==c("Chautauqua Mesa") )
bs.sub2= subset(bs.sub2, bs.sub$Sex==c("M") )

hist= subset(bs.sub2, bs.sub2$time==c("historic") )
curr= subset(bs.sub2, bs.sub2$time==c("current") )

ks.test(hist$Mean_Femur,curr$Mean_Femur)

#=======================================
#relate to phenology

setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperPhenSynch/data/")
dat.all= read.csv("HopperData_Sept2019_forPhenOverlap.csv")

#find unique spsiteyr
dat= dat.all[duplicated(dat.all$spsiteyear)==FALSE, c("species","year","site","spsiteyear","doy_adult","gdd_adult")]

#match to body size data
gp= c("Eritettix simplex","Xanthipus corallipes","Aeropedellus clavatus","Melanoplus boulderensis","Camnula pellucida","Melanoplus sanguinipes")
bs.sub$gp= gp[match(bs.sub$Species, specs)]
bs.sub$spsiteyear= paste(bs.sub$Sites,bs.sub$Year,bs.sub$gp,sep="")
  
#subset
dat= subset(dat, dat$species %in% gp)

#match phenology to body size
match1= match(bs.sub$spsiteyear, dat$spsiteyear)
#check
#bs.sub$spsiteyear[is.na(match1)]

#estimate of doy_adult, gdd_adult
bs1= merge(bs.sub, dat,
      by.x = "spsiteyear", by.y = "spsiteyear", all.x="TRUE")

#plot relationship
plot.doy= ggplot(data=bs1, aes(x=doy_adult, y=Mean_Femur, shape=species, color=Year))+ 
  geom_point()+geom_smooth(method="lm")+theme_bw()+
facet_grid(Species~Sites, scales="free")+ theme(legend.position = "bottom")
#, group_by=spsiteyear

plot.gdd=ggplot(data=bs1, aes(x=gdd_adult, y=Mean_Femur, shape=species, color=Year))+ 
  geom_point()+geom_smooth(method="lm")+theme_bw()+
  facet_grid(Species~Sites, scales="free")+ theme(legend.position = "bottom")

#plot together
plot.doy+plot.gdd

#------
#stats
mod1= lm(Mean_Femur~doy_adult*species*site, data=bs1)


