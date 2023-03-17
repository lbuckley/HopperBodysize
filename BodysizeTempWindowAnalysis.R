# analyze custom windows: 1 month before mean phenology
# try climwin analysis


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
library(zoo)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.all= read.csv("BodySize_wClim.csv")

#add mean and se
bs.all.sum= ddply(bs.all, c("Species", "elev", "Sex","Year","SexElev","clim.site"), summarise,
                  N    = length(Mean_Femur),
                  mean = mean(Mean_Femur),
                  std   = sd(Mean_Femur),
                  mean.anom = mean(Femur.anom),
                  std.anom   = sd(Femur.anom),
                  Tspr.anom = mean(Tspr.anom),
                  Tsum.anom = mean(Tsum.anom),
                  springdd.anom = mean(springdd.anom),
                  doy_spec = mean(doy_spec)
)
bs.all.sum$se= bs.all.sum$std / sqrt(bs.all.sum$N)
bs.all.sum$se.anom= bs.all.sum$std.anom / sqrt(bs.all.sum$N)

bs.all.sum$Species= factor(bs.all.sum$Species, order=TRUE, levels=c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes"))

bs.all.sum$time="historic"
bs.all.sum$time[which(as.numeric(bs.all.sum$Year)>2000)]<-"current"
bs.all.sum$SexTime= paste(bs.all.sum$Sex, bs.all.sum$time, sep="") 
bs.all.sum$SexTimeElev= paste(bs.all.sum$Sex, bs.all.sum$time, bs.all.sum$elev, sep="") 

#----
#load climate
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/ClimateData")
nc.hr.all <- readRDS("nchr.RDS")

nchr.month <- nc.hr.all %>%
  dplyr::arrange(doy) %>% 
  dplyr::group_by(site) %>% 
  dplyr::mutate(t_28d = zoo::rollmean(t2m, k = 28, fill = NA) )

#add temp 4 weeks before specimen date
bs.all$SitesYearDoy= paste(bs.all$SitesYear, bs.all$doy_spec, sep="")
nchr.month$SitesYearDoy= paste(nchr.month$site, nchr.month$year, bs.all$doy_spec, sep="")

match1= match(bs.all$SitesYearDoy, nchr.month$SitesYearDoy) 
bs.all$t_28d= nchr.month[match1, "t_28d"]-273.15 

#CLIMWIN ANALYSIS
#Format data
#across species and sites

# dout.nona= dout[which(!is.na(dout$doy_adult)),]
# 
# match1= match(dout.nona$spsiteyear, dat$spsiteyear) 
# dout.nona$species= dat[match1, "species"] 
# dout.nona$site= dat[match1, "site"] 
# dout.nona$year= dat[match1, "year"] 

#climate data
nc.hr.all$year_doy= paste(nc.hr.all$year, nc.hr.all$doy, sep="_")

nchr.day <- nc.hr.all %>%
  dplyr::arrange(doy) %>% 
  dplyr::group_by(site,year_doy, year, month, day, doy) %>% 
  dplyr::summarise(max= max(t2m)-273.15, min=min(t2m)-273.15, mean=mean(t2m)-273.15 )

nchr.day$date= as.Date(nchr.day$doy, origin = paste(nchr.day$year, "-01-01", sep="") )
nchr.day$date= format(nchr.day$date, format = "%d/%m/%Y")

#format body size date
#date <- strptime(as.character(bs.all$DateCollected), "%d/%m/%y")
bs.all.sum$date= as.Date(bs.all.sum$doy_spec, origin = paste(bs.all.sum$Year, "-01-01", sep="") )
bs.all.sum$date= format(bs.all.sum$date, format = "%d/%m/%Y")

#restrict to sites
cdat1=nchr.day
bdat1=bs.all.sum[,c("Species","Year","clim.site","mean.anom","date")]
bdat1$phen=1

pwin <- slidingwin(xvar = list(Temp = cdat1$mean), exclude=c(21,14),
                   cdate = cdat1$date,
                   bdate = bdat1$date,
                   baseline = lm(mean.anom ~ 1, data = bdat1),
                   cinterval = "day",
                   range = c(80, 0),
                   type = "relative",
                   stat = "mean",
                   func = "lin", spatial = list(bdat1$clim.site, cdat1$site), cmissing="method1", cohort=bdat1$Year) # 

prand <- randwin(repeats = 5, xvar = list(Temp = cdat1$mean), exclude=c(21,14),
                 cdate = cdat1$date,
                 bdate = bdat1$date,
                 baseline = lm(mean.anom ~ 1, data = bdat1),
                 cinterval = "day",
                 range = c(60, 0),
                 type = "relative",
                 stat = "mean",
                 func = "lin", spatial = list(bdat1$clim.site, cdat1$site), cmissing="method1", cohort=bdat1$Year)

psingle <- singlewin(xvar = list(Temp = cdat1$mean), 
                     cdate = cdat1$date,
                     bdate = bdat1$date,
                     baseline = lm(mean.anom ~ 1, data = bdat1),
                     cinterval = "day",
                     range = c(60, 0),
                     type = "relative",
                     stat = "mean",
                     func = "lin", spatial = list(bdat1$clim.site, cdat1$site), cmissing="method1", cohort=bdat1$year) 

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
