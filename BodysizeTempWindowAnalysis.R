# TRY CLIMWIN ANALYSIS

library(car)
library(nlme)
library(lme4)
library(lmerTest)
library(patchwork)
library(ggplot2)
library(sjPlot)
library(plyr)
#library(MuMIn)
library(climwin)

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






#Format data
#across species and sites

dout.nona= dout[which(!is.na(dout$doy_adult)),]

match1= match(dout.nona$spsiteyear, dat$spsiteyear) 
dout.nona$species= dat[match1, "species"] 
dout.nona$site= dat[match1, "site"] 
dout.nona$year= dat[match1, "year"] 

#climate data
#cdat= hop[,c("date","ordinal","species","year","site","dd","dd_sum","cdd_sum","cdd_sumfall")]
cdat= cdat[,c("Site", "Julian", "Year", "Max", "Min", "dd","dd_sum","dd_sumfall")] 
cdat$date= as.Date(cdat$Julian, origin = paste(cdat$Year, "-01-01", sep="") )
cdat$date= format(cdat$date, format = "%d/%m/%Y")

#add date to dout
dout.nona$doysiteyear= paste(dout.nona$doy_adult,dout.nona$site,dout.nona$year, sep="")
dout.nona$date= as.Date(dout.nona$doy_adult, origin = paste(dout.nona$year, "-01-01", sep=""))
bdat= dout.nona
bdat$date= format(bdat$date, format = "%d/%m/%Y")

#restrict to sites
cdat1=cdat #[which(cdat$Site=="B1"),]
bdat1=bdat #[which(bdat$species=="Melanoplus boulderensis"),]
bdat1$phen=1

pwin <- slidingwin(xvar = list(Temp = cdat1$dd), exclude=c(21,14),
                   cdate = cdat1$date,
                   bdate = bdat1$date,
                   baseline = lm(doy_adult ~ 1, data = bdat1),
                   cinterval = "day",
                   range = c(80, 0),
                   type = "relative",
                   stat = "mean",
                   func = "lin", spatial = list(bdat1$site, cdat1$Site), cmissing="method1", cohort=bdat1$year) # 

prand <- randwin(repeats = 5, xvar = list(Temp = cdat1$dd), exclude=c(21,14),
                 cdate = cdat1$date,
                 bdate = bdat1$date,
                 baseline = lm(doy_adult ~ 1, data = bdat1),
                 cinterval = "day",
                 range = c(60, 0),
                 type = "relative",
                 stat = "mean",
                 func = "lin", spatial = list(bdat1$site, cdat1$Site), cmissing="method1", cohort=bdat1$year)

psingle <- singlewin(xvar = list(Temp = cdat1$dd), 
                     cdate = cdat1$date,
                     bdate = bdat1$date,
                     baseline = lm(doy_adult ~ 1, data = bdat1),
                     cinterval = "day",
                     range = c(60, 0),
                     type = "relative",
                     stat = "mean",
                     func = "lin", spatial = list(bdat1$site, cdat1$Site), cmissing="method1", cohort=bdat1$year) 

#p-value
pvalue(dataset = pwin[[1]]$Dataset, datasetrand = prand[[1]], metric = "C", sample.size = 52)

pOutput <- pwin[[1]]$Dataset
pRand <- prand[[1]]

plotdelta(dataset = pOutput)
plotbetas(dataset = pOutput)

plotall(dataset = pOutput,
        datasetrand = pRand,
        bestmodel = psingle$BestModel, 
        bestmodeldata = psingle$BestModelData)
