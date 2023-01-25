#Niwot options:
#https://github.com/lbuckley/HopperPhenology/blob/master/ClimateExtremes_20May2022.R

library(NicheMapR)
library(ecmwfr)
library(mcera5)
library(sf)
library(ncdf4)
library(tidyverse)
library(lubridate)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/")
bs.all= read.csv("BodySize_sub_Sept2022.csv")
bs.all$Year[which(bs.all$Year==1058)]<- 1958
bs.all$Year[which(bs.all$Year==1059)]<- 1959
bs.all$Year[which(bs.all$Year==1060)]<- 1960
#----------
#get site lat, lon
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
sites= read.csv("HopperSites_keep2022.csv")

match1= match(bs.all$Sites, sites$Site)
site.matched=sites[unique(match1),]
inds= which(site.matched$Site %in% c("Summit lake","Mt. Evans","Rollin's Pass"))

sites.grid= site.matched[-inds,]
sites.ind= site.matched[inds,]

range(sites.grid$Latitude)
range(sites.grid$Longitude)

#https://github.com/bluegreen-labs/ecmwfr
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/ClimateData/")

# assign your credentials (register here: https://cds.climate.copernicus.eu/user/register)
uid <- "78176"
cds_api_key <- "062c3e77-bcc8-4c56-8e72-4872e7a92be6"

ecmwfr::wf_set_key(user = uid, key = cds_api_key, service = "cds")

#----
request.month <- list(
  "dataset_short_name" = "reanalysis-era5-single-levels-monthly-means",
  "product_type" = "monthly_averaged_reanalysis",
  "variable" = c("2m_temperature","skin_temperature","snow_depth","snowfall","total_precipitation"),
  "year" = c('1959', '1960', '1961', '1962', '1963', '1964', '1979', '1981', '2006',
             '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2022'),
  "month" = c("03","04","05","06","07","08"),
  "time" = "00:00",
  "area" = "40.06/-105.59/39.99/-105.28",
  "format" = "netcdf",
  "target" = "ERA5_month.nc"
)

request.month.early <- list(
  "dataset_short_name" = "reanalysis-era5-land-monthly-means",
  "product_type" = "monthly_averaged_reanalysis",
  "variable" = c("2m_temperature", "skin_temperature","total_precipitation","snow_depth"),
  "year" = c('1950','1951','1952','1953','1954','1955','1956','1957','1958', '1959', '1960', '1961', '1962', '1963', '1964','1965','1966','1967','1968','1969','1970','1971','1972','1973','1974','1975','1976','1977','1978','1979','1980', '1981', '2006','2007', '2008', '2009', '2010', '2011', '2012', '2013', '2022'),
  "month" = c("03","04","05","06","07","08"),
  "time" = "00:00",
  "area" = "40.06/-105.59/39.99/-105.28",
  "format" = "netcdf.zip",
  "target" = "ERA5_month_early.zip"
)
#,  "leaf_area_index_low_vegetation", "snow_cover","snowfall","soil_temperature_level_1"

request.hr <- list(
  "dataset_short_name" = "reanalysis-era5-single-levels",
  "product_type" = "reanalysis",
  "variable" = c("2m_temperature"), #,"skin_temperature","snow_depth","snowfall","total_precipitation"
  "year" = c('1959', '1960', '1961', '1962', '1963', '1964', '1979', '1981', '2006',
             '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2022'),
  "month" = c("03","04","05","06","07","08"),
  "day" = c('01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
            '13', '14', '15',
            '16', '17', '18',
            '19', '20', '21',
            '22', '23', '24',
            '25', '26', '27',
            '28', '29', '30',
            '31'),
  "time" = c('00:00', '01:00', '02:00',
             '03:00', '04:00', '05:00',
             '06:00', '07:00', '08:00',
             '09:00', '10:00', '11:00',
             '12:00', '13:00', '14:00',
             '15:00', '16:00', '17:00',
             '18:00', '19:00', '20:00',
             '21:00', '22:00', '23:00'),
  "area" = "40.06/-105.59/39.99/-105.28",
  "format" = "netcdf",
  "target" = "ERA5_hr.nc"
)

for(k in 1:3){

  if(k==1){
    request.month[[7]]<- "40.06/-105.59/39.99/-105.28"
    request.month.early[[7]]<- "40.06/-105.59/39.99/-105.28"
    request.hr[[8]]<- "40.06/-105.59/39.99/-105.28"
    request.month[[9]]<- "COgrid_ERA5_month.nc"
    request.month.early[[9]]<- "COgrid_ERA5_month.zip"
    request.hr[[10]]<- "COgrid_ERA5_hr.nc"
  }
  
  if(k==2){
    request.month[[7]]<- "39.94/-105.67/39.93/-105.66"
    request.month.early[[7]]<- "39.94/-105.67/39.93/-105.66"
    request.hr[[8]]<- "39.94/-105.67/39.93/-105.66"
    request.month[[9]]<- "Rollins_ERA5_month.nc"
    request.month.early[[9]]<- "Rollins_ERA5_month.zip"
    request.hr[[10]]<- "Rollins_ERA5_hr.nc"
  }
  
  if(k==3){
    request.month[[7]]<- "39.60/-105.64/39.58/-105.63"
    request.month.early[[7]]<- "39.60/-105.64/39.58/-105.63"
    request.hr[[8]]<- "39.60/-105.64/39.58/-105.63"
    request.month[[9]]<- "Evans_ERA5_month.nc"
    request.month.early[[9]]<- "Evans_ERA5_month.zip"
    request.hr[[10]]<- "Evans_ERA5_hr.nc"
  }
  
  #request
  # file <- wf_request(
  #   user     = "78176",   # user ID (for authentification)
  #   request  = request.month,  # the request
  #   transfer = TRUE,     # download the file
  #   path     = "."       # store data in current working directory
  # )
  
  file <- wf_request(
    user     = "78176",   # user ID (for authentification)
    request  = request.month.early,  # the request
    transfer = TRUE,     # download the file
    path     = "."       # store data in current working directory
  )
  
  # file <- wf_request(
  #   user     = "78176",   # user ID (for authentification)
  #   request  = request.hr,  # the request
  #   transfer = TRUE,     # download the file
  #   path     = "."       # store data in current working directory
  # )
}
 
#----
#process data

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/ClimateData/")

for(k in 1:3){

#open the connection with the ncdf file
if(k==1){ 
  nc.m <- nc_open("COgrid_ERA5_month.nc")
  nc.me <- nc_open("COgrid_ERA5_month_early.nc")
  nc.hr <- nc_open("COgrid_ERA5_hr.nc")
}
  if(k==2){ 
    nc.m <- nc_open("Rollins_ERA5_month.nc")
    nc.me <- nc_open("Rollins_ERA5_month_early.nc")
    nc.hr <- nc_open("Rollins_ERA5_hr.nc")
  }
  if(k==3){ 
    nc.m <- nc_open("Evans_ERA5_month.nc")
    nc.me <- nc_open("Evans_ERA5_month_early.nc")
    nc.hr <- nc_open("Evans_ERA5_hr.nc")
  }

#---  
#month data
#extract lon and lat
lat <- ncvar_get(nc.m,'latitude')
lon <- ncvar_get(nc.m,'longitude')
dim(lat);dim(lon)

#extract the time
t <- ncvar_get(nc.m, "time")
#convert the hours into date + hour
#as_datetime() function of the lubridate package needs seconds
timestamp <- as_datetime(c(t*60*60),origin="1900-01-01")

#import the data
t2m <- ncvar_get(nc.m,"t2m")
sd <- ncvar_get(nc.m,"sd") #snow depth
tp <- ncvar_get(nc.m,"tp") #total precipitation
#add soil temp, wind for microclimate?

#combine data
if(k==1) {
  nc.m.data= cbind(year(timestamp),month(timestamp), t2m[1,], sd[1,], tp[1,])
  nc.m.data= as.data.frame(nc.m.data)
  names(nc.m.data)=c("year","month","t2m","sd","tp")
  nc.m.data$site= "Boulder1"
  
  nc.m.data2= cbind(year(timestamp),month(timestamp), t2m[2,], sd[2,], tp[2,])
  nc.m.data2= as.data.frame(nc.m.data2)
  names(nc.m.data2)=c("year","month","t2m","sd","tp")
  nc.m.data2$site= "Boulder2"
  
  nc.m.all= rbind(nc.m.data,nc.m.data2)
  }

if(k>1){
  nc.m.data= cbind(year(timestamp),month(timestamp), t2m, sd, tp)
  nc.m.data= as.data.frame(nc.m.data)
  names(nc.m.data)=c("year","month","t2m","sd","tp")
  
  #add site info
  if(k==2) nc.m.data$site= "Rollins"
  if(k==3) nc.m.data$site= "Evans"

  nc.m.all= rbind(nc.m.all, nc.m.data)
}
#---
#monthly dataset with earlier data

#extract the time
t <- ncvar_get(nc.me, "time")
#convert the hours into date + hour
#as_datetime() function of the lubridate package needs seconds
timestamp <- as_datetime(c(t*60*60),origin="1900-01-01")

##import the data
t2m <- ncvar_get(nc.me,"t2m")
#lai_lv <- ncvar_get(nc.me,"lai_lv")
skt <- ncvar_get(nc.me,"skt")
#snowc <- ncvar_get(nc.me,"snowc")
sde <- ncvar_get(nc.me,"sde") #snow depth
#sf <- ncvar_get(nc.me,"sf") #snow fall
#stl1 <- ncvar_get(nc.me,"stl1") #soil temp
tp <- ncvar_get(nc.me,"tp") #total precipitation
#add soil temp, wind for microclimate?

#combine data
if(k==1) {
  #nc.me.data= cbind(year(timestamp),month(timestamp), t2m[1,], sde[1,], tp[1,], lai_lv[1,], skt[1,], stl1[1,])
  nc.me.data= cbind(year(timestamp),month(timestamp), t2m[1,], sde[1,], tp[1,], skt[1,])
  nc.me.data= as.data.frame(nc.me.data)
  names(nc.me.data)=c("year","month","t2m","sd","tp","skt")
  nc.me.data$site= "Boulder1"
  
  nc.me.data2= cbind(year(timestamp),month(timestamp), t2m[2,], sde[2,], tp[2,], skt[1,] )
  nc.me.data2= as.data.frame(nc.me.data2)
  names(nc.me.data2)=c("year","month","t2m","sd","tp","skt")
  nc.me.data2$site= "Boulder2"
  
  nc.me.all= rbind(nc.me.data,nc.me.data2)
}

if(k>1){
  nc.me.data= cbind(year(timestamp),month(timestamp), t2m, sde, tp, skt)
  nc.me.data= as.data.frame(nc.me.data)
  names(nc.me.data)=c("year","month","t2m","sd","tp","skt")
  
  #add site info
  if(k==2) nc.me.data$site= "Rollins"
  if(k==3) nc.me.data$site= "Evans"
  
  nc.me.all= rbind(nc.me.all, nc.me.data)
}

#---
#hr data

#extract the time
t <- ncvar_get(nc.hr, "time")
#convert the hours into date + hour
#as_datetime() function of the lubridate package needs seconds
timestamp <- as_datetime(c(t*60*60),origin="1900-01-01")

#import the data
t2m <- ncvar_get(nc.hr,"t2m")
#add soil temp, wind for microclimate?

#combine data
if(k==1) {
  nc.hr.data= cbind(year(timestamp),month(timestamp), day(timestamp), hour(timestamp), yday(timestamp), t2m[1,])
  nc.hr.data= as.data.frame(nc.hr.data)
  names(nc.hr.data)=c("year","month","day","hour","doy","t2m")
  nc.hr.data$site= "Boulder1"
  
  nc.hr.data2= cbind(year(timestamp),month(timestamp), day(timestamp), hour(timestamp), yday(timestamp), t2m[2,])
  nc.hr.data2= as.data.frame(nc.hr.data2)
  names(nc.hr.data2)=c("year","month","day","hour","doy","t2m")
  nc.hr.data2$site= "Boulder2"
  
  nc.hr.all= rbind(nc.hr.data,nc.hr.data2)
}

if(k>1){
  nc.hr.data= cbind(year(timestamp),month(timestamp), day(timestamp), hour(timestamp), yday(timestamp), t2m)
  nc.hr.data= as.data.frame(nc.hr.data)
  names(nc.hr.data)=c("year","month","day","hour","doy","t2m")
  
  #add site info
  if(k==2) nc.hr.data$site= "Rollins"
  if(k==3) nc.hr.data$site= "Evans"
  
  nc.hr.all= rbind(nc.hr.all, nc.hr.data)
}

} #end site loop

#close the connection with the ncdf file
nc_close(nc.m)
nc_close(nc.hr)

#------------------------
#add climate data to bodysize dataset

#aggregate data
#monthly data
#spring means March-May
#Summer means June-July

clim.m= nc.m.all[nc.m.all$month %in% c(3:5),]
clim.m= aggregate(clim.m, list(clim.m$year, clim.m$site), FUN=mean)
names(clim.m)[2]="site"
clim.m= clim.m[,c("site","year","t2m","tp","sd")]
#convert to C
clim.m$t2m= clim.m$t2m -273.15

clim.me= nc.me.all[nc.me.all$month %in% c(3:5),]
clim.me= aggregate(clim.me, list(clim.me$year, clim.me$site), FUN=mean)
names(clim.me)[2]="site"
clim.me= clim.me[,c("site","year","t2m","tp","sd","skt")]

#add 2012, 2013 average
clim.20125= aggregate(clim.me[clim.me$year %in% 2012:2013,], list(clim.me[clim.me$year %in% 2012:2013,]$site), FUN=mean)
clim.20125= clim.20125[,-2]
names(clim.20125)[1]<-"site"
clim.me= rbind(clim.me, clim.20125)

#convert to C
clim.me$t2m= clim.me$t2m -273.15

#estimate climate anomaly
#or use: https://cds.climate.copernicus.eu/cdsapp#!/dataset/ecv-for-climate-change?tab=overview
clim.site.me= aggregate(clim.me[clim.me$year %in% 1950:1980,], list(clim.me[clim.me$year %in% 1950:1980,]$site), FUN=mean)
names(clim.site.me)[1]<- "site"
match1= match(clim.me$site, clim.site.me$site)
clim.me$t2m.anom= clim.me$t2m - clim.site.me$t2m[match1]

#body size anomaly
bs.all$SpecElevSex= paste(bs.all$Species, bs.all$elev, bs.all$Sex, sep="")
bs.size.m= aggregate(bs.all[,c("SpecElevSex","Mean_Femur")], list(bs.all$SpecElevSex), FUN=mean)
names(bs.size.m)[1]<-"SpecElevSex"
match1= match(bs.all$SpecElevSex, bs.size.m$SpecElevSex)
bs.all$Femur.anom= bs.all$Mean_Femur - bs.size.m$Mean_Femur[match1]

#hr data
#spring degree days
nc.hr.data$t2m= nc.hr.data$t2m -273.15
diffs= cbind(nc.hr.data$t2m-12,0)
nc.hr.data$dd= apply(diffs, 1, FUN = max)
  
nc.hr.sum = nc.hr.data %>% group_by(year,site) %>% arrange(doy) %>% mutate(cdd_sum = cumsum(dd/24)) 
#cdd on June 1
nc.hr.sum= nc.hr.sum[nc.hr.sum$doy==152 & nc.hr.sum$hour==0,]
nc.hr.sum$siteyear= paste(nc.hr.sum$site, nc.hr.sum$year,sep="")
clim.m$siteyear= paste(clim.m$site, clim.m$year,sep="")

match1= match(clim.m$siteyear, nc.hr.sum$siteyear)
clim.m$springdd= nc.hr.sum$cdd_sum[match1]

#add closest CO grid cell 
diffs= cbind(abs(sites.grid$Longitude+105.59), abs(sites.grid$Longitude+105.34) )
sites.grid$clim.site= c("Boulder1", "Boulder2")[apply(diffs, 1, FUN = which.min)]
sites.ind$clim.site=c("Rollins", "Evans", "Evans")
sites.clim= rbind(sites.grid, sites.ind)

#match to grids
match1= match(bs.all$Sites, sites.clim$Site)
bs.all$clim.site= sites.clim$clim.site[match1]

bs.all$SitesYear= paste(bs.all$clim.site, bs.all$Year, sep="")
clim.m$SitesYear= paste(clim.m$site, clim.m$year, sep="")
clim.me$SitesYear= paste(clim.me$site, clim.me$year, sep="")

bs.all$SexElev= paste(bs.all$Sex, bs.all$elev, sep="")

# #add initial monthly data
# match1= match(bs.all$SitesYear, clim.m$SitesYear)
# bs.all$t2m= clim.m$t2m[match1]
# bs.all$sd= clim.m$sd[match1]
# bs.all$tp= clim.m$tp[match1]
# bs.all$springdd= clim.m$springdd[match1]

#add extended monthly data
match1= match(bs.all$SitesYear, clim.me$SitesYear)
bs.all$t2m= clim.me$t2m[match1]
bs.all$t2m.anom= clim.me$t2m.anom[match1]
bs.all$sd= clim.me$sd[match1]
bs.all$tp= clim.me$tp[match1]
#bs.all$lai= clim.me$lai[match1]
bs.all$skt= clim.me$skt[match1]
#bs.all$st= clim.me$st[match1]

match1= match(bs.all$SitesYear, clim.me$SitesYear)
bs.nomatch= bs.all[is.na(match1),]

#--------------------------
#plot

#add mean and se
bs.all.sum= ddply(bs.all, c("Species", "elev", "Sex","Year","SexElev"), summarise,
                  N    = length(Mean_Femur),
                  mean = mean(Mean_Femur),
                  std   = sd(Mean_Femur),
                  mean.anom = mean(Femur.anom),
                  std.anom   = sd(Femur.anom),
                  t2m= mean(t2m),
                  t2m.anom= mean(t2m.anom),
                  sd= mean(sd),
                  tp= mean(tp),
                  #springdd= mean(springdd),
                  #lai= mean(lai),
                  #st= mean(st),
                  skt= mean(skt)
                  )
bs.all.sum$se= bs.all.sum$std / sqrt(bs.all.sum$N)
bs.all.sum$se.anom= bs.all.sum$std.anom / sqrt(bs.all.sum$N)

plot.c2=ggplot(data=bs.all.sum, aes(x=t2m.anom, y = mean, group= SexElev, shape=Sex, color=factor(elev)) )+
  facet_wrap(Species~., scales="free")+
  geom_point(size=3)+
  theme_bw()+ geom_smooth(method="lm", se=FALSE)+
  geom_errorbar( aes(ymin=mean-se, ymax=mean+se), width=0, col="black")+
 # scale_shape_manual(values = c(21,24,25))+
  scale_color_brewer(palette = "Spectral") +
  xlab("Temperature anomaly (C)") +ylab("Femur size (mm)")
#+ scale_y_continuous(trans='log')

bs.all.sum$time="historic"
bs.all.sum$time[which(as.numeric(bs.all.sum$Year)>2000)]<-"current"
bs.all.sum$SexTime= paste(bs.all.sum$Sex, bs.all.sum$time, sep="") 
bs.all.sum$SexTimeElev= paste(bs.all.sum$Sex, bs.all.sum$time, bs.all.sum$elev, sep="") 

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim_time.pdf",height = 8, width = 9)
plot.c2
dev.off()

#---------------------------
#analysis

#combined model
bs.sub1= bs.all[,c("Mean_Femur","Femur.anom","Year","time","elev","Sex","Species","Sites","t2m","t2m.anom","sd","tp")] #,"springdd"
bs.sub1= na.omit(bs.sub1)
#check drops

bs.scaled <- transform(bs.sub1,
                       t2m_cs=scale(t2m),
                       elev_cs=scale(elev),
                       t2m.anom_cs=scale(t2m.anom)
)

mod.lmer <- lmer(Femur.anom~t2m.anom*elev_cs*time*Sex*Species +
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled) 

#Include sex
mod.lmer1 <- lmer(Femur.anom~t2m.anom+time +
                   time:elev_cs +time:Sex +time:elev_cs:Sex+ 
                   t2m.anom:elev_cs +t2m.anom:Sex +t2m.anom:elev_cs:Sex+
                   t2m.anom:Species+time:Species +
                   time:elev_cs:Species +time:Sex:Species +time:elev_cs:Sex:Species+ 
                   t2m.anom:elev_cs:Species +t2m.anom:Sex:Species +t2m.anom:elev_cs:Sex:Species+
                   (1|Year/Sites),
                 REML = FALSE, na.action = 'na.fail', 
                 data = bs.scaled) 

#split by Sex?
#size in a year: determined by time period (vary by elevation, Sex, species), climate, 

anova(mod.lmer1)
summary(mod.lmer)$coefficients
summary(mod.lmer)$AICtab
coef(mod.lmer)

plot_model(mod.lmer, type = "pred", terms = c("t2m","time"), show.data=TRUE)

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim_combined.pdf",height = 12, width = 12)
plot_model(mod.lmer, type = "pred", terms = c("t2m.anom","time","elev_cs","Species"), show.data=TRUE)
dev.off()

#-------
#By species

#ANOVA output
stat= c("Sum Sq","NumDF","F value","Pr(>F)")
vars= c("t2m.anom","time","time:elev_cs","time:Sex","t2m.anom:elev_cs",    
        "t2m.anom:Sex","time:elev_cs:Sex","t2m.anom:elev_cs:Sex")
#vars= c("t2m","time","sex","t2m*time","t2m*sex","time*sex","t2m*time*sex")
#vars= c("t2m.anom","elev","time","Sex","t2m.anom:elev","t2m.anom:time","elev:time","t2m.anom:Sex","elev:Sex","time:Sex","t2m.anom:elev:time","t2m.anom:elev:Sex","t2m.anom:time:Sex","elev:time:Sex","t2m.anom:elev:time:Sex")

stats= array(data=NA, dim=c(length(specs),8,4),
             dimnames=list(specs,vars,stat) ) 

modplots <- vector('list', length(specs))

for(spec.k in 1:length(specs)){
  
  #mod.lmer <- lmer(Mean_Femur~t2m.anom_cs*elev_cs*time*Sex + (1|Year/Sites), #(1|Year/Sites)
  #                 REML = FALSE,
  #                 na.action = 'na.omit', data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),])
  mod.lmer <- lmer(Femur.anom~t2m.anom+time +
                     time:elev_cs +time:Sex +time:elev_cs:Sex+ 
                     t2m.anom:elev_cs +t2m.anom:Sex +t2m.anom:elev_cs:Sex+
                     (1|Year/Sites),
                   REML = FALSE, na.action = 'na.fail', 
                   data = bs.scaled[which(bs.scaled$Species==specs[spec.k]),]) 
  
  stats[spec.k,,1:4]=as.matrix(anova(mod.lmer))[,c("Sum Sq","NumDF","F value","Pr(>F)")]
  
  #plot output
  message(spec.k)
  modplots[[spec.k]] <- local({
    spec.k <- spec.k
    p1 <- plot_model(mod.lmer, type="eff",terms=c("t2m.anom","elev_cs","time"), show.data=TRUE, title=specs[spec.k])
    print(p1)
  })
  
} #end loop specs 

#save figure
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/figures/Sept2022/")
pdf("ModPlots_clim.pdf",height = 12, width = 12)
(modplots[[1]] | modplots[[4]]) / (modplots[[2]] | modplots[[5]]) / (modplots[[3]] | modplots[[6]])
dev.off()

lmer.sig= stats[,,4]
lmer.sig[lmer.sig < 0.05] <- "*"

#generally smaller through time (evolution) but larger with temperature (plasticity)?
#time effects: clavatus, pellucida
#temp*time*sex: simplex



