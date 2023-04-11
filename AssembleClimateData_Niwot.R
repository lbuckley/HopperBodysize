
library(NicheMapR)
library(ecmwfr)
library(mcera5)
library(sf)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(zoo)
library(plyr)
library(TrenchR)
library(reshape2)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
bs.all= read.csv("BodySize_sub.csv")
#bs.all= read.csv("BodySize_all.csv")

bs.all$Year= as.numeric(as.character(bs.all$Year))
bs.all$Year[which(bs.all$Year==1048)]<- 1948
bs.all$Year[which(bs.all$Year==1049)]<- 1949
bs.all$Year[which(bs.all$Year==1058)]<- 1958
bs.all$Year[which(bs.all$Year==1059)]<- 1959
bs.all$Year[which(bs.all$Year==1060)]<- 1960

#----------
#body size anomally
bs.all$SpecElevSex= paste(bs.all$Species, bs.all$elev, bs.all$Sex, sep="")
bs.size.m= aggregate(bs.all[,c("SpecElevSex","Mean_Femur")], list(bs.all$SpecElevSex), FUN=mean)
names(bs.size.m)[1]<-"SpecElevSex"
match1= match(bs.all$SpecElevSex, bs.size.m$SpecElevSex)
bs.all$Femur.anom= bs.all$Mean_Femur - bs.size.m$Mean_Femur[match1]

#----------
#Boulder data monthly
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/ClimateData/NOAA/")
clim.max= read.csv("NOAA_Boulder_monthly_max.csv")
clim.means= read.csv("NOAA_Boulder_monthly_means.csv")
clim.min= read.csv("NOAA_Boulder_monthly_min.csv")

clim.max$Tspr= rowMeans( clim.max[,c("MAR","APR","MAY")] )
clim.max$Tsum= rowMeans( clim.max[,c("JUN","JUL","AUG")] )
clim.means$Tspr= rowMeans( clim.means[,c("MAR","APR","MAY")] )
clim.means$Tsum= rowMeans( clim.means[,c("JUN","JUL","AUG")] )
clim.min$Tspr= rowMeans( clim.min[,c("MAR","APR","MAY")] )
clim.min$Tsum= rowMeans( clim.min[,c("JUN","JUL","AUG")] )

clim.max$metric="max"
clim.means$metric="means"
clim.min$metric="min"

clim.b= rbind(clim.max[,c("Year","Tspr","Tsum","metric")], clim.means[,c("Year","Tspr","Tsum","metric")], clim.min[,c("Year","Tspr","Tsum","metric")])

ggplot(data=clim.b[which(clim.b$Year>1930),], aes(x=Year, y = Tspr))+ 
  facet_wrap(~metric)+
  geom_line()+geom_point()+geom_smooth(method='lm')
ggplot(data=clim.b[which(clim.b$Year>1930),], aes(x=Year, y = Tsum))+ 
  facet_wrap(~metric)+
  geom_line()+geom_point()+geom_smooth(method='lm')

#----------------
#Niwot climate data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/ClimateData/Alexander/")

#Daily data from Nufio
#setwd("/Volumes/GoogleDrive/My Drive/AlexanderResurvey/DataForAnalysis/climate/")
clim= read.csv("AlexanderClimateAll.csv")

#data counts by sites, years
#counts by species and sites
with(clim, table(Site, Year))
#A1 2009, 2010, 2021, through 2014

#Just resurvey years with additional filling
clim.f= read.csv("AlexanderClimateAll_filled_May2022.csv")
#counts by species and sites
with(clim.f, table(Site, Year))

#Add A1 2009, 2010, 2012
a1.dat= clim.f[which(clim.f$Site=="A1" & clim.f$Year %in% c(2009,2010,2012)),c("Site","Year","Julian","Max","Min","Mean")]
a1.dat$Date= NA
a1.dat$Month= NA
a1.dat= a1.dat[,c("Site","Date","Year","Month","Julian","Max","Min","Mean")]
#add data
clim= rbind(clim, a1.dat)

#daily: https://psl.noaa.gov/boulder/data/boulderdaily.complete.txt
noaa.daily= read.csv("boulderdailycomplete.csv")
#add 2015-current
noaa.dat= noaa.daily[which(noaa.daily$year>2015),]
#convert to C
noaa.dat$Min= (noaa.dat$tmin_F -32)*5/9
noaa.dat$Max= (noaa.dat$tmax_F -32)*5/9
noaa.dat$Site="NOAA"
noaa.dat$Month= noaa.dat$mon
noaa.dat$Date= paste(noaa.dat$mon, noaa.dat$day, noaa.dat$year, sep="/")
noaa.dat$Julian= day_of_year(day= noaa.dat$Date, format = "%m/%d/%Y")
noaa.dat$Mean= NA
noaa.dat= noaa.dat[,c("Site","Date","year","mon","Julian","Max","Min","Mean")]
names(noaa.dat)[3:4]=c("Year","Month")
#add data
clim= rbind(clim, noaa.dat)

#recent A1 data hourly
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.3.6
clim.a1= read.csv("a-1hobo.hourly.jm.data.csv")
#drop before 2015
clim.a1= clim.a1[clim.a1$year>2014,]

#calculate daily Max, Min, Mean
clim.a1= ddply(clim.a1, c("date","jday", "year"), summarise,
                Mean = mean(airtemp, na.rm=TRUE), Min = min(airtemp, na.rm=TRUE), Max = max(airtemp, na.rm=TRUE) )
names(clim.a1)[1:3]= c("Date","Julian","Year")
clim.a1$Site= "A1"
clim.a1$Month= as.numeric(format(as.Date(clim.a1$Date, format="%Y-%m-%d"),"%m"))
  
clim.a1= clim.a1[,c("Site","Date","Year","Month","Julian","Max","Min","Mean")]
#add data
clim= rbind(clim, clim.a1)

#recent B1 data hourly
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.5.5
clim.b1= read.csv("b-1hobo.hourly.jm.data.csv")
#drop before 2015
clim.b1= clim.b1[clim.b1$year>2015,]

#calculate daily Max, Min, Mean
clim.b1= ddply(clim.b1, c("date","jday", "year"), summarise,
               Mean = mean(airtemp, na.rm=TRUE), Min = min(airtemp, na.rm=TRUE), Max = max(airtemp, na.rm=TRUE) )
names(clim.b1)[1:3]= c("Date","Julian","Year")
clim.b1$Site= "B1"
clim.b1$Month= as.numeric(format(as.Date(clim.b1$Date, format="%Y-%m-%d"),"%m"))

clim.b1= clim.b1[,c("Site","Date","Year","Month","Julian","Max","Min","Mean")]
#add data
clim= rbind(clim, clim.b1)

#recent C1
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.401.7
clim.c1= read.csv("c-1cr23x-cr1000.daily.ml.data.csv")
#through 2021
#drop before 2015
clim.c1= clim.c1[which(clim.c1$year>2014),c("date","year","jday","airtemp_hmp1_max","airtemp_hmp1_min","airtemp_hmp1_avg")]
names(clim.c1)= c("Date","Year","Julian","Max","Min","Mean")
clim.c1$Site= "C1"
clim.c1$Month= as.numeric(format(as.Date(clim.c1$Date, format="%Y-%m-%d"),"%m"))
clim.c1= clim.c1[,c("Site","Date","Year","Month","Julian","Max","Min","Mean")]
#add data
clim= rbind(clim, clim.c1)

#C1 chart recorder
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.411.15
clim.c1chart= read.csv("c-1tdayv.ml.data.csv")
#rough but more recent

#recent D1
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.402.5
clim.d1= read.csv("d-1cr23x-cr1000.daily.ml.data.csv")
#through 2021
#drop before 2015
clim.d1= clim.d1[which(clim.d1$year>2014),c("date","year","jday","airtemp_hmp1_max","airtemp_hmp1_min","airtemp_hmp1_avg")]
names(clim.d1)= c("Date","Year","Julian","Max","Min","Mean")
clim.d1$Site= "D1"
clim.d1$Month= as.numeric(format(as.Date(clim.d1$Date, format="%Y-%m-%d"),"%m"))
clim.d1= clim.d1[,c("Site","Date","Year","Month","Julian","Max","Min","Mean")]
#add data
clim= rbind(clim, clim.d1)

#infilled C1 temp data, 1952-2018
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.185.2

#infilled D1 temp data, 1952-2018
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.187.2

#real time met
#https://nwt.lternet.edu/real-time-met

#C1
c1.dat= read.table("c1_cr1000_tenminute_20221031.dat", sep=",", skip=4)
c1.dat= c1.dat[c(1,8)]
names(c1.dat)=c("TIMESTAMP","Mean") #AirTC_as_Avg

c1.dat$Year= as.numeric(format(as.Date(c1.dat$TIMESTAMP, format="%Y-%m-%d %H:%M:%S"),"%Y"))
c1.dat$Date= as.Date(c1.dat$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")

#drop before 2022
c1.dat= c1.dat[c1.dat$Year>2021,]

c1.dat$Julian= day_of_year(day= c1.dat$Date, format = "%Y-&M-%D")

#calculate daily Max, Min, Mean
c1.dat= ddply(c1.dat, c("Date","Julian", "Year"), summarise,
               Mean = mean(Mean, na.rm=TRUE), Min = min(Mean, na.rm=TRUE), Max = max(Mean, na.rm=TRUE) )
names(c1.dat)[1:3]= c("Date","Julian","Year")
c1.dat$Site= "C1"
c1.dat$Month= as.numeric(format(as.Date(c1.dat$Date, format="%Y-%m-%d"),"%m"))

c1.dat= c1.dat[,c("Site","Date","Year","Month","Julian","Max","Min","Mean")]
#add data
clim= rbind(clim, c1.dat)

#D1 data
#check header
#d1.dat= read.table("d1_cr1000_tenminute.dat", sep=",", skip=1, nrow=1)

d1.dat= read.table("d1_cr1000_tenminute.dat", sep=",", skip=4)
d1.dat= d1.dat[c(1,8)]
names(d1.dat)=c("TIMESTAMP","Mean") #AirTC_as_Avg

d1.dat$Year= as.numeric(format(as.Date(d1.dat$TIMESTAMP, format="%Y-%m-%d %H:%M:%S"),"%Y"))
d1.dat$Date= as.Date(d1.dat$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")

#drop before 2022
d1.dat= d1.dat[d1.dat$Year>2021,]

d1.dat$Julian= day_of_year(day= d1.dat$Date, format = "%Y-&M-%D")

#calculate daily Max, Min, Mean
d1.dat= ddply(d1.dat, c("Date","Julian", "Year"), summarise,
              Mean = mean(Mean, na.rm=TRUE), Min = min(Mean, na.rm=TRUE), Max = max(Mean, na.rm=TRUE) )
names(d1.dat)[1:3]= c("Date","Julian","Year")
d1.dat$Site= "D1"
d1.dat$Month= as.numeric(format(as.Date(d1.dat$Date, format="%Y-%m-%d"),"%m"))

d1.dat= d1.dat[,c("Site","Date","Year","Month","Julian","Max","Min","Mean")]
#add data
clim= rbind(clim, d1.dat)

#================
#Assess and fill data

#estimate mean from min and max
inds= which(!is.na(clim$Max)&!is.na(clim$Min)&is.na(clim$Mean))
clim$Mean[inds]= (clim$Min[inds] + clim$Max[inds])/2

#check NAs
with(clim, table(Site,Year))
clim.nas <- aggregate(clim[,c("Max","Min","Mean")], list(clim$Site, clim$Year),
                         FUN=function(x) { sum(is.na(x)) })

#PLOT RELATIONSHIPS
clim$YrDoy= clim$Year +clim$Julian/365
ggplot(data=clim[clim$Year %in% 1980:2022,], aes(x=YrDoy, y = Mean, color=Site))+ 
  geom_line()+geom_smooth(method='lm')+ylim(-5,30)+facet_wrap(~Site)
ggplot(data=clim, aes(x=YrDoy, y = Min, color=Site))+ 
  geom_line()+geom_smooth(method='lm')+ylim(-5,30)
ggplot(data=clim, aes(x=YrDoy, y = Max, color=Site))+ 
  geom_line()+geom_smooth(method='lm')+ylim(-5,30)

#reformat to wide format for Julian, Year by Site 
clim.min= dcast(clim, Julian +Year ~ Site, value.var="Min", fun.aggregate=mean, na.rm=TRUE)
clim.max= dcast(clim, Julian +Year ~ Site, value.var="Max", fun.aggregate=mean, na.rm=TRUE)
clim.mean= dcast(clim, Julian +Year ~ Site, value.var="Mean", fun.aggregate=mean, na.rm=TRUE)
#clim.min[is.nan(clim.min)] <- NA

#Fill A1 and B1 
#clim.nas[clim.nas$Group.1=="A1",]
clim.nas[clim.nas$Group.1=="B1",]
#A1 1970-1986, 2012
#B1 1970-1986, 2001

#models for A1

sites=c("A1","B1")
metrics=c("Min","Max")

for(clim.met in 1:2){

  if(clim.met==1) clim2= clim.min
  if(clim.met==2) clim2= clim.max
  
  for(site.ind in 1:2){
  
    msite=sites[site.ind]

m.noaa= summary(lm( get(msite) ~NOAA, data=clim2))
m.c1= summary(lm( get(msite) ~C1, data=clim2))
m.d1= summary(lm( get(msite) ~D1, data=clim2)) 

#predict
clim2$pnoaa= m.noaa$coefficients[1,1] +m.noaa$coefficients[2,1]*clim2$NOAA
clim2$pc1= m.c1$coefficients[1,1] +m.c1$coefficients[2,1]*clim2$C1
clim2$pd1= m.d1$coefficients[1,1] +m.d1$coefficients[2,1]*clim2$D1

#weight by r2
clim2$pclim=(m.noaa$adj.r.squared*clim2$pnoaa + m.c1$adj.r.squared*clim2$pd1 + m.d1$adj.r.squared*clim2$pd1)/(m.noaa$adj.r.squared+m.c1$adj.r.squared+m.d1$adj.r.squared)

#plot
plot(clim2[,match(msite, colnames(clim2))], clim2$pclim)

#subset to years
clim3= clim2[clim2$Year %in% 1970:1986,]
clim3$YrDoy= clim3$Year +clim3$Julian/365
clim3=clim3[order(clim3$YrDoy),]

#use model to fill in
nas= which(is.na(clim3[,match(msite, colnames(clim3))]))
clim3$clim.fill= clim3[,match(msite, colnames(clim3))]
clim3$clim.fill[nas]= clim3$pclim[nas]

} #end loop sites
  
  clim.colname= paste(sites)
  sites=c("A1","B1")
  metrics=c("Min","Max")
  
  if(clim.met==1) clim.min$sites[site.ind]= clim3$clim.fill
  if(clim.met==2) clim.max$sites[site.ind]= clim3$clim.fill
  
} #end loop climate metric

#plot
plot(clim3$YrDoy, clim3[,match(msite, colnames(clim3))], type="l")
#points(clim3$YrDoy, clim3$pclim, type="l", col="green")
points(clim3$YrDoy, clim3$clim.fill, type="l", col="blue")

#------------------

#Recombine data

#write out
#write.csv(clim,"AlexanderClimateModel.csv")

#=================================
#spring means, doy 60-151:
clim.spr= aggregate(clim[which(clim$Julian %in% 60:151),c("Max","Mean","Min")], list(clim$Site[which(clim$Julian %in% 60:151)], clim$Year[which(clim$Julian %in% 60:151)]), FUN=mean)
names(clim.spr)[1:2]=c("Site","Year")
clim.spr$Seas="spring"
clim.spr$SiteYr= paste(clim.spr$Site, clim.spr$Year, sep="_")

#summer means, doy 152-243:
clim.sum= aggregate(clim[which(clim$Julian %in% 152:243),c("Max","Mean","Min")], list(clim$Site[which(clim$Julian %in% 152:243)], clim$Year[which(clim$Julian %in% 152:243)]), FUN=mean)
names(clim.sum)[1:2]=c("Site","Year")
clim.sum$Seas="summer"
clim.sum$SiteYr= paste(clim.sum$Site, clim.sum$Year, sep="_")

clim.seas= rbind(clim.spr, clim.sum)

#plot
ggplot(data=clim.seas, aes(x=Year, y = Max, color=Site))+ 
  facet_wrap(~Seas)+
  geom_line()+geom_point()+geom_smooth(method='lm')
ggplot(data=clim.seas, aes(x=Year, y = Mean, color=Site))+ 
  facet_wrap(~Seas)+
  geom_line()+geom_point()+geom_smooth(method='lm')
ggplot(data=clim.seas, aes(x=Year, y = Min, color=Site))+ 
  facet_wrap(~Seas)+
  geom_line()+geom_point()+geom_smooth(method='lm')

#----------------
#Add climate data to body size

#add 2012, 2013 average
b.20125= aggregate(clim.seas[clim.seas$Year %in% 2012:2013,c("Max","Min","Mean")], list(clim.seas[clim.seas$Year %in% 2012:2013,]$Site, clim.seas[clim.seas$Year %in% 2012:2013,]$Seas), FUN=mean)
colnames(b.20125)[1:2]=c("Site","Seas")
b.20125$Year= "2012.5"
b.20125$SiteYr= paste(b.20125$Site, b.20125$Year, sep="_")
clim.seas= rbind(clim.seas, b.20125[,colnames(clim.seas)])

b.20125= aggregate(clim.seas[clim.seas$Year %in% 2011:2012,c("Max","Min","Mean")], list(clim.seas[clim.seas$Year %in% 2011:2012,]$Site, clim.seas[clim.seas$Year %in% 2011:2012,]$Seas), FUN=mean)
colnames(b.20125)[1:2]=c("Site","Seas")
b.20125$Year= "2011.5"
b.20125$SiteYr= paste(b.20125$Site, b.20125$Year, sep="_")
clim.seas= rbind(clim.seas, b.20125[,colnames(clim.seas)])

#estimate climate anomaly
clim.seas.ag= aggregate(clim.seas[clim.seas$Year %in% 1950:1980,c("Max","Min","Mean")], list(clim.seas[clim.seas$Year %in% 1950:1980,]$Site, clim.seas[clim.seas$Year %in% 1950:1980,]$Seas), FUN=mean, na.rm=TRUE)
names(clim.seas.ag)[1:2]=c("Site","Seas")

clim.seas.ag$SiteSeas= paste(clim.seas.ag$Site, clim.seas.ag$Seas, sep="_")
clim.seas$SiteSeas= paste(clim.seas$Site, clim.seas$Seas, sep="_")

match1= match(clim.seas$SiteSeas, clim.seas.ag$SiteSeas)
clim.seas$Max.anom= clim.seas$Max - clim.seas.ag$Max[match1]
clim.seas$Min.anom= clim.seas$Min - clim.seas.ag$Min[match1]
clim.seas$Mean.anom= clim.seas$Mean - clim.seas.ag$Mean[match1]

#spring and summer year
match1= match(bs.all$Year, clim.seas[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring"),"Year"])
bs.all$Tspr.min.C1= clim.seas$Min[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]
bs.all$Tspr.mean.C1= clim.seas$Mean[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]
bs.all$Tspr.max.C1= clim.seas$Max[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]

bs.all$Tspr.min.C1.anom= clim.seas$Min.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]
bs.all$Tspr.mean.C1.anom= clim.seas$Mean.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]
bs.all$Tspr.max.C1.anom= clim.seas$Max.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]

match1= match(bs.all$Year, clim.seas[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer"),"Year"])
bs.all$Tsum.min.C1= clim.seas$Min[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]
bs.all$Tsum.mean.C1= clim.seas$Mean[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]
bs.all$Tsum.max.C1= clim.seas$Max[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]

bs.all$Tsum.min.C1.anom= clim.seas$Min.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]
bs.all$Tsum.mean.C1.anom= clim.seas$Mean.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]
bs.all$Tsum.max.C1.anom= clim.seas$Max.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]

#spring and summer year before
match1= match(bs.all$Year-1, clim.seas[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring"),"Year"])
bs.all$Tspr.min.C1.prev= clim.seas$Min[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]
bs.all$Tspr.mean.C1.prev= clim.seas$Mean[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]
bs.all$Tspr.max.C1.prev= clim.seas$Max[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]

bs.all$Tspr.min.C1.anom.prev= clim.seas$Min.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]
bs.all$Tspr.mean.C1.anom.prev= clim.seas$Mean.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]
bs.all$Tspr.max.C1.anom.prev= clim.seas$Max.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="spring")][match1]

match1= match(bs.all$Year-1, clim.seas[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer"),"Year"])
bs.all$Tsum.min.C1.prev= clim.seas$Min[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]
bs.all$Tsum.mean.C1.prev= clim.seas$Mean[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]
bs.all$Tsum.max.C1.prev= clim.seas$Max[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]

bs.all$Tsum.min.C1.anom.prev= clim.seas$Min.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]
bs.all$Tsum.mean.C1.anom.prev= clim.seas$Mean.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]
bs.all$Tsum.max.C1.anom.prev= clim.seas$Max.anom[which(clim.seas$Site=="C1" & clim.seas$Seas=="summer")][match1]

#add temp 4 weeks before specimen date
clim.month <- clim %>%
  dplyr::arrange(Julian) %>% 
  dplyr::group_by(Site) %>% 
  dplyr::mutate(min_28d = zoo::rollmean(Min, k = 28, fill = NA), 
                mean_28d = zoo::rollmean(Mean, k = 28, fill = NA),  
                max_28d = zoo::rollmean(Max, k = 28, fill = NA)   )
clim.month= as.data.frame(clim.month)

#add temp 4 weeks before specimen date
clim.month$YearDoy= paste(clim.month$Year, clim.month$Julian, sep="")

match1= match(bs.all$yeardoy, clim.month[which(clim.month$Site=="C1"),"YearDoy"])
bs.all$tmin_28d.C1= NA
bs.all$tmean_28d.C1= NA
bs.all$tmax_28d.C1= NA

bs.all$tmin_28d.C1[which(!is.na(match1))]= clim.month[na.omit(match1), "min_28d"]
bs.all$tmean_28d.C1[which(!is.na(match1))]= clim.month[na.omit(match1), "mean_28d"]
bs.all$tmax_28d.C1[which(!is.na(match1))]= clim.month[na.omit(match1), "max_28d"]

#make into anomally
clim.sum= ddply(bs.all, c("Species", "elev"), summarise,
                tmin_28d.C1.anom = mean(tmin_28d.C1, na.rm=TRUE), 
                tmean_28d.C1.anom = mean(tmean_28d.C1, na.rm=TRUE),
                tmax_28d.C1.anom = mean(tmax_28d.C1, na.rm=TRUE) )
clim.sum$SpElev= paste(clim.sum$Species, clim.sum$elev, sep="")

match1= match(bs.all$SpElev, clim.sum$SpElev)
bs.all$tmin_28d.C1.anom= bs.all$tmin_28d.C1 - clim.sum$tmin_28d.C1.anom[match1]
bs.all$tmean_28d.C1.anom= bs.all$tmean_28d.C1 - clim.sum$tmean_28d.C1.anom[match1]
bs.all$tmax_28d.C1.anom= bs.all$tmax_28d.C1 - clim.sum$tmax_28d.C1.anom[match1]

#----

#save data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
write.csv(bs.all, "BodySize_wClim_plusWS.csv" )
