
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

setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
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
bs.size.m= aggregate(bs.all[bs.all$Year<1980,c("SpecElevSex","Mean_Femur")], list(bs.all$SpecElevSex[bs.all$Year<1980]), FUN=mean)
names(bs.size.m)[1]<-"SpecElevSex"
match1= match(bs.all$SpecElevSex, bs.size.m$SpecElevSex)
bs.all$Femur.anom= bs.all$Mean_Femur - bs.size.m$Mean_Femur[match1]

#estimate percent change
bs.all$f.per.change= (bs.all$Mean_Femur - bs.size.m$Mean_Femur[match1])/bs.size.m$Mean_Femur[match1]
mean(bs.all[which(bs.all$time=="current"),"f.per.change"], na.rm=TRUE)

#----------
#Boulder data monthly
setwd("/Users/laurenbuckley/Google Drive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/ClimateData/NOAA/")
#setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/ClimateData/NOAA/")
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

#add in missing doys
YrDoy= expand.grid(Site= unique(clim$Site),Year=1953:2023, Julian=60:243)
YrDoy$YrDoy= YrDoy$Year +YrDoy$Julian/365
YrDoy$SiteYrDoy= paste(YrDoy$Site, YrDoy$YrDoy, sep="_")

clim$YrDoy= clim$Year +clim$Julian/365
clim$SiteYrDoy= paste(clim$Site, clim$YrDoy, sep="_")
#find missing
match1= match(YrDoy$SiteYrDoy, clim$SiteYrDoy)
no.match= which(is.na(match1))

clim.add= clim[1:length(no.match),]
clim.add[]= NA
clim.add[,c("Site","Year","Julian", "YrDoy","SiteYrDoy")]= YrDoy[no.match,c("Site","Year","Julian", "YrDoy","SiteYrDoy")]

#combine
clim= rbind(clim, clim.add)

#----
#Just resurvey years with additional filling
clim.f= read.csv("AlexanderClimateAll_filled_May2022.csv")
#counts by species and sites
with(clim.f, table(Site, Year))

#find nas to fill
clim.f$YrDoy= clim.f$Year +clim.f$Julian/365
clim.f$SiteYrDoy= paste(clim.f$Site, clim.f$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], clim.f$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=clim.f[match1[matched],c("Max","Min","Mean")]

#----
#Add NOAA data
#daily: https://psl.noaa.gov/boulder/data/boulderdaily.complete.txt
noaa.dat= read.csv("boulderdailycomplete.csv")
#convert to C
noaa.dat$Min= (noaa.dat$tmin_F -32)*5/9
noaa.dat$Max= (noaa.dat$tmax_F -32)*5/9
noaa.dat$Site="NOAA"
noaa.dat$Month= noaa.dat$mon
noaa.dat$Date= paste(noaa.dat$mon, noaa.dat$day, noaa.dat$year, sep="/")
noaa.dat$Julian= day_of_year(day= noaa.dat$Date, format = "%m/%d/%Y")

#find nas to fill
noaa.dat$YrDoy= noaa.dat$year +noaa.dat$Julian/365
noaa.dat$SiteYrDoy= paste(noaa.dat$Site, noaa.dat$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], noaa.dat$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min")]=noaa.dat[match1[matched],c("Max","Min")]

#----------
#recent A1 data hourly
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.3.6
clim.a1= read.csv("a-1hobo.hourly.jm.data.csv")

#calculate daily Max, Min, Mean
clim.a1= ddply(clim.a1, c("date","jday", "year"), summarise,
                Mean = mean(airtemp, na.rm=TRUE), Min = min(airtemp, na.rm=TRUE), Max = max(airtemp, na.rm=TRUE) )
names(clim.a1)[1:3]= c("Date","Julian","Year")
clim.a1$Site= "A1"
clim.a1$Month= as.numeric(format(as.Date(clim.a1$Date, format="%Y-%m-%d"),"%m"))
  
#find nas to fill
clim.a1$YrDoy= clim.a1$Year +clim.a1$Julian/365
clim.a1$SiteYrDoy= paste(clim.a1$Site, clim.a1$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], clim.a1$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=clim.a1[match1[matched],c("Max","Min","Mean")]

#--------
#recent b1 data hourly
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.5.5
clim.b1= read.csv("b-1hobo.hourly.jm.data.csv")

#calculate daily Max, Min, Mean
clim.b1= ddply(clim.b1, c("date","jday", "year"), summarise,
               Mean = mean(airtemp, na.rm=TRUE), Min = min(airtemp, na.rm=TRUE), Max = max(airtemp, na.rm=TRUE) )
names(clim.b1)[1:3]= c("Date","Julian","Year")
clim.b1$Site= "B1"
clim.b1$Month= as.numeric(format(as.Date(clim.b1$Date, format="%Y-%m-%d"),"%m"))

#find nas to fill
clim.b1$YrDoy= clim.b1$Year +clim.b1$Julian/365
clim.b1$SiteYrDoy= paste(clim.b1$Site, clim.b1$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], clim.b1$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=clim.b1[match1[matched],c("Max","Min","Mean")]

#--------
#recent C1
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.401.7
clim.c1= read.csv("c-1cr23x-cr1000.daily.ml.data.csv")
#through 2021
clim.c1= clim.c1[,c("date","year","jday","airtemp_hmp1_max","airtemp_hmp1_min","airtemp_hmp1_avg")]
names(clim.c1)= c("Date","Year","Julian","Max","Min","Mean")
clim.c1$Site= "C1"
clim.c1$Month= as.numeric(format(as.Date(clim.c1$Date, format="%Y-%m-%d"),"%m"))

#find nas to fill
clim.c1$YrDoy= clim.c1$Year +clim.c1$Julian/365
clim.c1$SiteYrDoy= paste(clim.c1$Site, clim.c1$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], clim.c1$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=clim.c1[match1[matched],c("Max","Min","Mean")]

#--------

#C1 chart recorder
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.411.15
clim.c1chart= read.csv("c-1tdayv.ml.data.csv")
#rough but more recent

#recent D1
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.402.5
clim.d1= read.csv("d-1cr23x-cr1000.daily.ml.data.csv")
#through 2021
clim.d1= clim.d1[,c("date","year","jday","airtemp_hmp1_max","airtemp_hmp1_min","airtemp_hmp1_avg")]
names(clim.d1)= c("Date","Year","Julian","Max","Min","Mean")
clim.d1$Site= "D1"
clim.d1$Month= as.numeric(format(as.Date(clim.d1$Date, format="%Y-%m-%d"),"%m"))

#find nas to fill
clim.d1$YrDoy= clim.d1$Year +clim.d1$Julian/365
clim.d1$SiteYrDoy= paste(clim.d1$Site, clim.d1$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], clim.d1$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=clim.d1[match1[matched],c("Max","Min","Mean")]

#--------

#infilled C1 temp data, 1952-2018
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.185.2

clim.c1= read.csv("c1_infilled_temp_daily.tk.data.csv")
#through 2021
clim.c1$Site= "C1"
clim.c1$Julian= day_of_year(day= clim.c1$date, format = "%Y-%m-%d")

#find nas to fill
clim.c1$YrDoy= clim.c1$year +clim.c1$Julian/365
clim.c1$SiteYrDoy= paste(clim.c1$Site, clim.c1$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], clim.c1$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=clim.c1[match1[matched],c("max_temp","min_temp","mean_temp")]

#--------

#infilled D1 temp data, 1952-2018
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-nwt.187.2

clim.d1= read.csv("d1_infilled_temp_daily.tk.data.csv")
#through 2021
clim.d1$Site= "D1"
clim.d1$Julian= day_of_year(day= clim.d1$date, format = "%Y-%m-%d")

#find nas to fill
clim.d1$YrDoy= clim.d1$year +clim.d1$Julian/365
clim.d1$SiteYrDoy= paste(clim.d1$Site, clim.d1$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], clim.d1$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=clim.d1[match1[matched],c("max_temp","min_temp","mean_temp")]

#--------
#real time met
#https://nwt.lternet.edu/real-time-met

#C1
c1.dat= read.table("c1_cr1000_tenminute_20221031.dat", sep=",", skip=4)
c1.dat= c1.dat[c(1,8)]
names(c1.dat)=c("TIMESTAMP","Mean") #AirTC_as_Avg

c1.dat$Year= as.numeric(format(as.Date(c1.dat$TIMESTAMP, format="%Y-%m-%d %H:%M:%S"),"%Y"))
c1.dat$Date= as.Date(c1.dat$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")

c1.dat$Julian= day_of_year(day= c1.dat$Date, format = "%Y-&M-%D")

#calculate daily Max, Min, Mean
c1.dat= ddply(c1.dat, c("Date","Julian", "Year"), summarise,
               Mean = mean(Mean, na.rm=TRUE), Min = min(Mean, na.rm=TRUE), Max = max(Mean, na.rm=TRUE) )
names(c1.dat)[1:3]= c("Date","Julian","Year")
c1.dat$Site= "C1"
c1.dat$Month= as.numeric(format(as.Date(c1.dat$Date, format="%Y-%m-%d"),"%m"))

#find nas to fill
c1.dat$YrDoy= c1.dat$Year +c1.dat$Julian/365
c1.dat$SiteYrDoy= paste(c1.dat$Site, c1.dat$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], c1.dat$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=c1.dat[match1[matched],c("Max","Min","Mean")]

#----------------

#D1 data
#check header
#d1.dat= read.table("d1_cr1000_tenminute.dat", sep=",", skip=1, nrow=1)

d1.dat= read.table("d1_cr1000_tenminute.dat", sep=",", skip=4)
d1.dat= d1.dat[c(1,8)]
names(d1.dat)=c("TIMESTAMP","Mean") #AirTC_as_Avg

d1.dat$Year= as.numeric(format(as.Date(d1.dat$TIMESTAMP, format="%Y-%m-%d %H:%M:%S"),"%Y"))
d1.dat$Date= as.Date(d1.dat$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")

d1.dat$Julian= day_of_year(day= d1.dat$Date, format = "%Y-&M-%D")

#calculate daily Max, Min, Mean
d1.dat= ddply(d1.dat, c("Date","Julian", "Year"), summarise,
              Mean = mean(Mean, na.rm=TRUE), Min = min(Mean, na.rm=TRUE), Max = max(Mean, na.rm=TRUE) )
names(d1.dat)[1:3]= c("Date","Julian","Year")
d1.dat$Site= "D1"
d1.dat$Month= as.numeric(format(as.Date(d1.dat$Date, format="%Y-%m-%d"),"%m"))

#find nas to fill
d1.dat$YrDoy= d1.dat$Year +d1.dat$Julian/365
d1.dat$SiteYrDoy= paste(d1.dat$Site, d1.dat$YrDoy, sep="_")

nas= which(is.na(clim$Max))
match1= match(clim$SiteYrDoy[nas], d1.dat$SiteYrDoy)

matched=which(!is.na(match1))
clim[nas[matched],c("Max","Min","Mean")]=d1.dat[match1[matched],c("Max","Min","Mean")]

#drop very low D1 values in 2022
clim[which(clim$Site=="D1" & clim$Year>2021),"Max"]=NA
clim[which(clim$Site=="D1" & clim$Year>2021),"Min"]=NA
clim[which(clim$Site=="D1" & clim$Year>2021),"Mean"]=NA

#================
#Assess and fill data

#restrict days to spring and summer
clim= clim[clim$Julian %in% 60:243,]

#estimate mean from min and max
inds= which(!is.na(clim$Max)&!is.na(clim$Min)&is.na(clim$Mean))
clim$Mean[inds]= (clim$Min[inds] + clim$Max[inds])/2

#check NAs
with(clim, table(Site,Year))
clim.nas <- aggregate(clim[,c("Max","Min","Mean")], list(clim$Site, clim$Year),
                         FUN=function(x) { sum(is.na(x)) })

#PLOT RELATIONSHIPS
ggplot(data=clim[clim$Year %in% 1980:2022,], aes(x=YrDoy, y = Mean, color=Site))+ 
  geom_line()+geom_smooth(method='lm')+ylim(-5,30)+facet_wrap(~Site)
ggplot(data=clim, aes(x=YrDoy, y = Min, color=Site))+ 
  geom_line()+geom_smooth(method='lm')+ylim(-5,30)
ggplot(data=clim, aes(x=YrDoy, y = Max, color=Site))+ 
  geom_line()+geom_smooth(method='lm')+ylim(-5,30)

#make filled data
clim.fill= clim
#add fleg for filled data
clim.fill$filled=0
clim.fill$filled[which(is.na(clim.fill$Mean))]=1

#reformat to wide format for Julian, Year by Site 
clim.min= dcast(clim, Julian +Year ~ Site, value.var="Min", fun.aggregate=mean, na.rm=TRUE)
clim.max= dcast(clim, Julian +Year ~ Site, value.var="Max", fun.aggregate=mean, na.rm=TRUE)
clim.mean= dcast(clim, Julian +Year ~ Site, value.var="Mean", fun.aggregate=mean, na.rm=TRUE)
#clim.min[is.nan(clim.min)] <- NA

#make matching variables
clim.min$YrDoy= clim.min$Year +clim.min$Julian/365
clim.max$YrDoy= clim.max$Year +clim.max$Julian/365
clim.mean$YrDoy= clim.mean$Year +clim.mean$Julian/365

#Fill A1 and B1 
#clim.nas[clim.nas$Group.1=="A1",]
clim.nas[clim.nas$Group.1=="B1",]
#A1 1970-1986, 2012
#B1 1970-1986, 2001

sites=c("A1","B1","C1","D1","NOAA")
metrics=c("Min","Max")

for(clim.met in 1:2){

  if(clim.met==1) clim2= clim.min
  if(clim.met==2) clim2= clim.max
  
  for(site.ind in 1:2){
  
    msite=sites[site.ind]

    #for(time.k in 1:2){
    #  if(time.k==1) yrs= 1965:1990
    #  if(time.k==2) yrs= 2005:2015
    
    #subset to years
    #clim2= clim2[clim2$Year %in% yrs,]
    
m.noaa= summary(lm( get(msite) ~NOAA, data=clim2))
m.c1= summary(lm( get(msite) ~C1, data=clim2))
m.d1= summary(lm( get(msite) ~D1, data=clim2)) 

#predict
clim2$pnoaa= m.noaa$coefficients[1,1] +m.noaa$coefficients[2,1]*clim2$NOAA
clim2$pc1= m.c1$coefficients[1,1] +m.c1$coefficients[2,1]*clim2$C1
clim2$pd1= m.d1$coefficients[1,1] +m.d1$coefficients[2,1]*clim2$D1

#weight by r2
clim2$pclim=(m.noaa$adj.r.squared*clim2$pnoaa + m.c1$adj.r.squared*clim2$pc1 + m.d1$adj.r.squared*clim2$pd1)/(m.noaa$adj.r.squared+m.c1$adj.r.squared+m.d1$adj.r.squared)
#drop D1 for 2022
clim2$pclim[which(clim2$Year>2021)]=(m.noaa$adj.r.squared*clim2$pnoaa[which(clim2$Year>2021)] + m.c1$adj.r.squared*clim2$pc1[which(clim2$Year>2021)])/(m.noaa$adj.r.squared+m.c1$adj.r.squared)

#plot
plot(clim2[,match(msite, colnames(clim2))], clim2$pclim)

clim3=clim2
clim3$YrDoy= clim3$Year +clim3$Julian/365
clim3=clim3[order(clim3$YrDoy),]

#use model to fill in
nas= which(is.na(clim3[,match(msite, colnames(clim3))]))
clim3$clim.fill= clim3[,match(msite, colnames(clim3))]
clim3$clim.fill[nas]= clim3$pclim[nas]

clim3$SiteYrDoy= paste(sites[site.ind], clim3$YrDoy, sep="_")
match1= match(clim3$SiteYrDoy, clim.fill$SiteYrDoy)

clim3[is.na(match1),]

if(clim.met==1) clim.fill$Min[match1]= clim3$clim.fill
if(clim.met==2) clim.fill$Max[match1]= clim3$clim.fill
 
#} #end loop time periods
} #end loop sites
} #end loop climate metric

#sort clim.fill
clim.fill= clim.fill[order(clim.fill$YrDoy),]

#plot
site.ind=5
plot(clim.fill[clim.fill$Site==sites[site.ind],]$YrDoy, clim.fill[clim.fill$Site==sites[site.ind],]$Min, type="l")
points(clim[clim$Site==sites[site.ind],]$YrDoy, clim[clim$Site==sites[site.ind],]$Min, type="l", col="blue")

#estimate mean
clim.fill$Mean[which(is.na(clim.fill$Mean))]= (clim.fill$Min[which(is.na(clim.fill$Mean))] + clim.fill$Max[which(is.na(clim.fill$Mean))])/2

#assess NAs
clim.na= clim.fill[which(is.na(clim.fill$Max)),]

##=================================
#plot
ggplot(data=clim.fill[clim.fill$Year %in% 1980:2022,], aes(x=YrDoy, y = Mean, color=Site))+ 
  geom_line()+geom_smooth(method='lm')+ylim(-5,30)+facet_wrap(~Site)

clim= clim.fill

#spring means, doy 60-151:
clim.spr= aggregate(clim[which(clim$Julian %in% 60:151),c("Max","Mean","Min","filled")], list(clim$Site[which(clim$Julian %in% 60:151)], clim$Year[which(clim$Julian %in% 60:151)]), FUN=mean)
names(clim.spr)[1:2]=c("Site","Year")
clim.spr$Seas="spring"
clim.spr$SiteYr= paste(clim.spr$Site, clim.spr$Year, sep="_")

#summer means, doy 152-243:
clim.sum= aggregate(clim[which(clim$Julian %in% 152:243),c("Max","Mean","Min","filled")], list(clim$Site[which(clim$Julian %in% 152:243)], clim$Year[which(clim$Julian %in% 152:243)]), FUN=mean)
names(clim.sum)[1:2]=c("Site","Year")
clim.sum$Seas="summer"
clim.sum$SiteYr= paste(clim.sum$Site, clim.sum$Year, sep="_")

#growing season means, doy 60-243:
clim.gs= aggregate(clim[which(clim$Julian %in% 60:243),c("Max","Mean","Min","filled")], list(clim$Site[which(clim$Julian %in% 60:243)], clim$Year[which(clim$Julian %in% 60:243)]), FUN=mean)
names(clim.gs)[1:2]=c("Site","Year")
clim.gs$Seas="gs"
clim.gs$SiteYr= paste(clim.sum$Site, clim.sum$Year, sep="_")

clim.seas= rbind(clim.spr, clim.sum, clim.gs)

#plot
clim.seas$Year= as.numeric(clim.seas$Year)
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
#estimate climate anomaly
clim.seas.ag= aggregate(clim.seas[clim.seas$Year %in% 1950:1980,c("Max","Min","Mean")], list(clim.seas[clim.seas$Year %in% 1950:1980,]$Site, clim.seas[clim.seas$Year %in% 1950:1980,]$Seas), FUN=mean, na.rm=TRUE)
names(clim.seas.ag)[1:2]=c("Site","Seas")

clim.seas.ag$SiteSeas= paste(clim.seas.ag$Site, clim.seas.ag$Seas, sep="_")
clim.seas$SiteSeas= paste(clim.seas$Site, clim.seas$Seas, sep="_")
clim.seas$SiteYrSeas= paste(clim.seas$Site, clim.seas$Year, clim.seas$Seas, sep="_")

match1= match(clim.seas$SiteSeas, clim.seas.ag$SiteSeas)
clim.seas$Max.anom= clim.seas$Max - clim.seas.ag$Max[match1]
clim.seas$Min.anom= clim.seas$Min - clim.seas.ag$Min[match1]
clim.seas$Mean.anom= clim.seas$Mean - clim.seas.ag$Mean[match1]

#------------
#Add climate data to body size
bs.all$ClimSite= bs.all$Sites
bs.all$ClimSite[bs.all$ClimSite=="Niwot Ridge (D1)"]="D1"
bs.all$ClimSite[bs.all$ClimSite=="Chautauqua Mesa"]="NOAA"
bs.all$ClimSite[bs.all$ClimSite=="Summit lake"]="C1" ## Check choice of climate sites
bs.all$ClimSite[bs.all$ClimSite=="Sunshine Canyon"]="A1"
bs.all$ClimSite[bs.all$ClimSite=="Mt. Evans"]="C1"
bs.all$ClimSite[bs.all$ClimSite=="Chicken Ranch Gulch"]="NOAA"
bs.all$ClimSite[bs.all$ClimSite=="Rollin's Pass"]="C1"

#spring and summer year
bs.all$SiteYr= paste(bs.all$ClimSite, bs.all$Year, sep="_")
bs.all$SiteYrPrev= paste(bs.all$ClimSite, bs.all$Year-1, sep="_")

match1= match(bs.all$SiteYr, clim.seas[which(clim.seas$Seas=="spring"),"SiteYr"])
bs.all$Tspr.min= clim.seas$Min[which(clim.seas$Seas=="spring")][match1]
bs.all$Tspr.max= clim.seas$Max[which(clim.seas$Seas=="spring")][match1]
bs.all$Tspr.mean= clim.seas$Mean[which(clim.seas$Seas=="spring")][match1]

bs.all$Tspr.min.anom= clim.seas$Min.anom[which(clim.seas$Seas=="spring")][match1]
bs.all$Tspr.max.anom= clim.seas$Max.anom[which(clim.seas$Seas=="spring")][match1]
bs.all$Tspr.mean.anom= clim.seas$Mean.anom[which(clim.seas$Seas=="spring")][match1]

match1= match(bs.all$SiteYr, clim.seas[which(clim.seas$Seas=="summer"),"SiteYr"])
bs.all$Tsum.min= clim.seas$Min[which(clim.seas$Seas=="summer")][match1]
bs.all$Tsum.max= clim.seas$Max[which(clim.seas$Seas=="summer")][match1]
bs.all$Tsum.mean= clim.seas$Mean[which(clim.seas$Seas=="summer")][match1]

bs.all$Tsum.min.anom= clim.seas$Min.anom[which(clim.seas$Seas=="summer")][match1]
bs.all$Tsum.max.anom= clim.seas$Max.anom[which(clim.seas$Seas=="summer")][match1]
bs.all$Tsum.mean.anom= clim.seas$Mean.anom[which(clim.seas$Seas=="summer")][match1]

match1= match(bs.all$SiteYr, clim.seas[which(clim.seas$Seas=="gs"),"SiteYr"])
bs.all$Tgs.min= clim.seas$Min[which(clim.seas$Seas=="gs")][match1]
bs.all$Tgs.max= clim.seas$Max[which(clim.seas$Seas=="gs")][match1]
bs.all$Tgs.mean= clim.seas$Mean[which(clim.seas$Seas=="gs")][match1]

bs.all$Tgs.min.anom= clim.seas$Min.anom[which(clim.seas$Seas=="gs")][match1]
bs.all$Tgs.max.anom= clim.seas$Max.anom[which(clim.seas$Seas=="gs")][match1]
bs.all$Tgs.mean.anom= clim.seas$Mean.anom[which(clim.seas$Seas=="gs")][match1]

#spring and summer year before
match1= match(bs.all$SiteYrPrev, clim.seas[which(clim.seas$Seas=="spring"),"SiteYr"])
bs.all$Tspr.min.prev= clim.seas$Min[which(clim.seas$Seas=="spring")][match1]
bs.all$Tspr.max.prev= clim.seas$Max[which(clim.seas$Seas=="spring")][match1]
bs.all$Tspr.mean.prev= clim.seas$Mean[which(clim.seas$Seas=="spring")][match1]

bs.all$Tspr.min.anom.prev= clim.seas$Min.anom[which(clim.seas$Seas=="spring")][match1]
bs.all$Tspr.max.anom.prev= clim.seas$Max.anom[which(clim.seas$Seas=="spring")][match1]
bs.all$Tspr.mean.anom.prev= clim.seas$Mean.anom[which(clim.seas$Seas=="spring")][match1]

match1= match(bs.all$SiteYrPrev, clim.seas[which(clim.seas$Seas=="summer"),"SiteYr"])
bs.all$Tsum.min.prev= clim.seas$Min[which(clim.seas$Seas=="summer")][match1]
bs.all$Tsum.max.prev= clim.seas$Max[which(clim.seas$Seas=="summer")][match1]
bs.all$Tsum.mean.prev= clim.seas$Mean[which(clim.seas$Seas=="summer")][match1]

bs.all$Tsum.min.anom.prev= clim.seas$Min.anom[which(clim.seas$Seas=="summer")][match1]
bs.all$Tsum.max.anom.prev= clim.seas$Max.anom[which(clim.seas$Seas=="summer")][match1]
bs.all$Tsum.mean.anom.prev= clim.seas$Mean.anom[which(clim.seas$Seas=="summer")][match1]

match1= match(bs.all$SiteYrPrev, clim.seas[which(clim.seas$Seas=="gs"),"SiteYr"])
bs.all$Tgs.min.prev= clim.seas$Min[which(clim.seas$Seas=="gs")][match1]
bs.all$Tgs.max.prev= clim.seas$Max[which(clim.seas$Seas=="gs")][match1]
bs.all$Tgs.mean.prev= clim.seas$Mean[which(clim.seas$Seas=="gs")][match1]

bs.all$Tgs.min.anom.prev= clim.seas$Min.anom[which(clim.seas$Seas=="gs")][match1]
bs.all$Tgs.max.anom.prev= clim.seas$Max.anom[which(clim.seas$Seas=="gs")][match1]
bs.all$Tgs.mean.anom.prev= clim.seas$Mean.anom[which(clim.seas$Seas=="gs")][match1]

#add temp 4 weeks before specimen date
clim.month <- clim %>%
  dplyr::arrange(Julian) %>% 
  dplyr::group_by(Site) %>% 
  dplyr::mutate(min_28d = zoo::rollmean(Min, k = 28, fill = NA), 
                mean_28d = zoo::rollmean(Mean, k = 28, fill = NA),  
                max_28d = zoo::rollmean(Max, k = 28, fill = NA)   )
clim.month= as.data.frame(clim.month)

#-----------------

#add collection date
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/SpecimenData/")
mus1= read.csv("AlexanderSpecimens2021.csv")

#match bs to museum code
mus1$Barcode= mus1$SpecimenCode
mus1$Barcode= as.numeric(sub("UCMC ", "", mus1$Barcode))
bs.all$Barcode= as.numeric(bs.all$Barcode)

#match
match1= match(bs.all$Barcode, mus1$Barcode)
matched= which(!is.na(match1))
bs.all$DateCollected[matched]= mus1$DateCollected[na.omit(match1)]

dates= as.data.frame(str_split(bs.all$DateCollected, "/", simplify = TRUE))
colnames(dates)= c("month","day","year")
dates$month= as.numeric(dates$month)
dates$day= as.numeric(dates$day)
dates$year= as.numeric(dates$year)
dates$year[which(dates$year>20)]<- dates$year[which(dates$year>20)]+1900
dates$year[which(dates$year<20)]<- dates$year[which(dates$year<20)]+2000
bs.all$month= dates$month
bs.all$day= dates$day
bs.all$year= dates$year
bs.all$timeperiod="current"
bs.all$timeperiod[bs.all$Year<1990]="historic"

tmp <- as.Date(paste(bs.all$day, bs.all$month, bs.all$year, sep="/"), format = "%d/%m/%Y")
bs.all$doy_spec= as.numeric(format(tmp, "%j"))

#----------------
#add temp 4 weeks before specimen date
clim.month$YearDoySite= paste(clim.month$Year, clim.month$Julian, clim.month$Site, sep="")
bs.all$YearDoySite= paste(bs.all$Year, bs.all$doy_spec, bs.all$ClimSite, sep="")

match1= match(bs.all$YearDoySite, clim.month$YearDoySite)
bs.all$tmin_28d= NA
bs.all$tmean_28d= NA
bs.all$tmax_28d= NA

bs.all$tmin_28d[which(!is.na(match1))]= clim.month[na.omit(match1), "min_28d"]
bs.all$tmean_28d[which(!is.na(match1))]= clim.month[na.omit(match1), "mean_28d"]
bs.all$tmax_28d[which(!is.na(match1))]= clim.month[na.omit(match1), "max_28d"]

#make into anomally
clim.sum= ddply(bs.all, c("Species", "elev"), summarise,
                tmin_28d.anom = mean(tmin_28d, na.rm=TRUE), 
                tmean_28d.anom = mean(tmean_28d, na.rm=TRUE),
                tmax_28d.anom = mean(tmax_28d, na.rm=TRUE) )
clim.sum$SpElev= paste(clim.sum$Species, clim.sum$elev, sep="")
bs.all$SpElev= paste(bs.all$Species, bs.all$elev, sep="")

match1= match(bs.all$SpElev, clim.sum$SpElev)
bs.all$tmin_28d.anom= bs.all$tmin_28d - clim.sum$tmin_28d.anom[match1]
bs.all$tmean_28d.anom= bs.all$tmean_28d - clim.sum$tmean_28d.anom[match1]
bs.all$tmax_28d.anom= bs.all$tmax_28d - clim.sum$tmax_28d.anom[match1]

#--------------
#Add month data before phenology
#simplex: doy 141, apr-may
#corallipes: doy 163, may - june
#clavatus: 178, may-june
#boulderensis: 178, may-june
#pellucida, 211, june - july
#sanguinipes, 221, july - aug

specs= c("E. simplex","X. corallipes","A. clavatus","M. boulderensis","C. pellucida","M. sanguinipes")
months= c("4-5", "5-6", "5-6", "5-6", "6-7", "7-8")


#month means:
clim.simp= aggregate(clim[which(clim$Julian %in% 113:141),c("Max","Mean","Min")], list(clim$Site[which(clim$Julian %in% 113:141)], clim$Year[which(clim$Julian %in% 113:141)]), FUN=mean)
clim.cora= aggregate(clim[which(clim$Julian %in% 135:163),c("Max","Mean","Min")], list(clim$Site[which(clim$Julian %in% 135:163)], clim$Year[which(clim$Julian %in% 135:163)]), FUN=mean)
clim.clav= aggregate(clim[which(clim$Julian %in% 150:178),c("Max","Mean","Min")], list(clim$Site[which(clim$Julian %in% 150:178)], clim$Year[which(clim$Julian %in% 150:178)]), FUN=mean)
clim.boul= aggregate(clim[which(clim$Julian %in% 150:178),c("Max","Mean","Min")], list(clim$Site[which(clim$Julian %in% 150:178)], clim$Year[which(clim$Julian %in% 150:178)]), FUN=mean)
clim.pell= aggregate(clim[which(clim$Julian %in% 183:211),c("Max","Mean","Min")], list(clim$Site[which(clim$Julian %in% 183:211)], clim$Year[which(clim$Julian %in% 183:211)]), FUN=mean)
clim.sang= aggregate(clim[which(clim$Julian %in% 193:221),c("Max","Mean","Min")], list(clim$Site[which(clim$Julian %in% 193:221)], clim$Year[which(clim$Julian %in% 193:221)]), FUN=mean)

clim.mo= cbind(clim.simp[,c(1:2,4)],clim.cora[,4], clim.clav[,4], clim.boul[,4], clim.pell[,4], clim.sang[,4])
colnames(clim.mo)=c("Site","Year","simp.mean","cora.mean","clav.mean","boul.mean","pell.mean","sang.mean")

clim.mo$SiteYr= paste(clim.mo$Site, clim.mo$Year, sep="_")

#add data
bs.all$Mean.mo= NA

inds= which(bs.all$Species=="E. simplex")
match1=match( bs.all$SiteYr[inds],clim.mo$SiteYr)
matched= which(!is.na(match1))
bs.all$Mean.mo[inds[matched]]= clim.mo$simp.mean[match1[matched]]
inds= which(bs.all$Species=="X. corallipes")
match1=match( bs.all$SiteYr[inds],clim.mo$SiteYr)
matched= which(!is.na(match1))
bs.all$Mean.mo[inds[matched]]= clim.mo$cora.mean[match1[matched]]
inds= which(bs.all$Species=="A. clavatus")
match1=match( bs.all$SiteYr[inds],clim.mo$SiteYr)
matched= which(!is.na(match1))
bs.all$Mean.mo[inds[matched]]= clim.mo$clav.mean[match1[matched]]
inds= which(bs.all$Species=="M. boulderensis")
match1=match( bs.all$SiteYr[inds],clim.mo$SiteYr)
matched= which(!is.na(match1))
bs.all$Mean.mo[inds[matched]]= clim.mo$boul.mean[match1[matched]]
inds= which(bs.all$Species=="C. pellucida")
match1=match( bs.all$SiteYr[inds],clim.mo$SiteYr)
matched= which(!is.na(match1))
bs.all$Mean.mo[inds[matched]]= clim.mo$pell.mean[match1[matched]]
inds= which(bs.all$Species=="M. sanguinipes")
match1=match( bs.all$SiteYr[inds],clim.mo$SiteYr)
matched= which(!is.na(match1))
bs.all$Mean.mo[inds[matched]]= clim.mo$sang.mean[match1[matched]]

#make into anomally
clim.sum= ddply(bs.all, c("Species", "elev"), summarise,
                Tmo.anom = mean(Mean.mo, na.rm=TRUE) )
clim.sum$SpElev= paste(clim.sum$Species, clim.sum$elev, sep="")

match1= match(bs.all$SpElev, clim.sum$SpElev)
bs.all$Tmo.anom= bs.all$Mean.mo - clim.sum$Tmo.anom[match1]

#--------------
#add seasonal timing
SpTiming= c("nymph","nymph","early","early","late","late")
match1= match(bs.all$Species, specs)
bs.all$SpTiming= SpTiming[match1]
  
#--------------
#save data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
write.csv(bs.all, "BodySize_wClim_plusNiwot.csv" )
write.csv(clim.seas, "NiwotSeasClimFilled.csv" )






