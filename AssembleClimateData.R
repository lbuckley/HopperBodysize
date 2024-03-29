#Niwot options:
#https://github.com/lbuckley/HopperPhenology/blob/master/ClimateExtremes_20May2022.R

library(NicheMapR)
library(ecmwfr)
library(mcera5)
library(sf)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(zoo)
library(plyr)

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

#write out climate data
saveRDS(nc.hr.all, "nchr.RDS") 
saveRDS(nc.hr.all, "nchr.RDS") 

#------------------------
#add climate data to bodysize dataset

#aggregate data
#monthly data
#spring means March-May
#Summer means June-July

nc.m.all$seas= NA
nc.m.all$seas[nc.m.all$month %in% c(3:5)]<- "spring"
nc.m.all$seas[nc.m.all$month %in% c(7:8)]<- "summer"

clim.m= aggregate(nc.m.all[,c("t2m","sd","tp")], list(nc.m.all$year, nc.m.all$site, nc.m.all$seas), FUN=mean)
names(clim.m)[1:3]=c("year","site","seas")
#convert to C
clim.m$t2m= clim.m$t2m -273.15

nc.hr.all$seas= NA
nc.hr.all$seas[nc.hr.all$month %in% c(3:5)]<- "spring"
nc.hr.all$seas[nc.hr.all$month %in% c(7:8)]<- "summer"

clim.me= aggregate(nc.hr.all[,c("t2m")], list(nc.hr.all$year, nc.hr.all$site, nc.hr.all$seas), FUN=mean)
names(clim.me)[1:4]=c("year","site","seas","t2m")
#convert to C
clim.me$t2m= clim.me$t2m -273.15

#add 2012, 2013 average
clim.20125= aggregate(clim.me[clim.me$year %in% 2012:2013,c("t2m")], list(clim.me[clim.me$year %in% 2012:2013,]$site, clim.me[clim.me$year %in% 2012:2013,]$seas), FUN=mean)
clim.20125= cbind("2012.5",clim.20125)
names(clim.20125)[1:4]=c("year","site","seas","t2m")
clim.me= rbind(clim.me, clim.20125)

#body size anomaly
bs.all$SpecElevSex= paste(bs.all$Species, bs.all$elev, bs.all$Sex, sep="")
bs.size.m= aggregate(bs.all[,c("SpecElevSex","Mean_Femur")], list(bs.all$SpecElevSex), FUN=mean)
names(bs.size.m)[1]<-"SpecElevSex"
match1= match(bs.all$SpecElevSex, bs.size.m$SpecElevSex)
bs.all$Femur.anom= bs.all$Mean_Femur - bs.size.m$Mean_Femur[match1]

#spread temp data
clim.me <- spread(clim.me, seas, t2m)
colnames(clim.me)[3:4]<- c("Tspr","Tsum")

#hr data
#spring degree days
nc.hr.all$t2m= nc.hr.all$t2m -273.15
diffs= cbind(nc.hr.all$t2m-12,0)
nc.hr.all$dd= apply(diffs, 1, FUN = max)
nc.hr.all = nc.hr.all %>% arrange(doy) %>% mutate(cdd_sum = cumsum(dd/24)) 
#write data to add to museum specimens
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/out/")
write.csv(nc.hr.all, "climhr.csv")

nc.hr.sum = nc.hr.all %>% group_by(year,site) %>% arrange(doy) %>% mutate(cdd_sum = cumsum(dd/24)) 
#cdd on June 1
nc.hr.sum= nc.hr.sum[nc.hr.sum$doy==152 & nc.hr.sum$hour==0,]
#add 2012, 2013 average
cdd.20125= aggregate(nc.hr.sum[nc.hr.sum$year %in% 2012:2013,c("cdd_sum")], list(nc.hr.sum[nc.hr.sum$year %in% 2012:2013,]$site, nc.hr.sum[nc.hr.sum$year %in% 2012:2013,]$seas), FUN=mean)
cdd.20125= cbind("2012.5",cdd.20125)
colnames(cdd.20125)[1:4]=c("year","site","seas","cdd_sum")
clim.cdd= as.data.frame(nc.hr.sum[,c("year","site","seas","cdd_sum")])
clim.cdd= rbind(clim.cdd, cdd.20125)

clim.cdd$siteyear= paste(clim.cdd$site, clim.cdd$year,sep="")
clim.me$siteyear= paste(clim.me$site, clim.me$year,sep="")

match1= match(clim.me$siteyear, clim.cdd$siteyear)
clim.me$springdd= nc.hr.sum$cdd_sum[match1]

#add closest CO grid cell 
diffs= cbind(abs(sites.grid$Longitude+105.59), abs(sites.grid$Longitude+105.34) )
sites.grid$clim.site= c("Boulder1", "Boulder2")[apply(diffs, 1, FUN = which.max)]
sites.ind$clim.site=c("Rollins", "Evans", "Evans")
sites.clim= rbind(sites.grid, sites.ind)

#match to grids
match1= match(bs.all$Sites, sites.clim$Site)
bs.all$clim.site= sites.clim$clim.site[match1]

bs.all$SitesYear= paste(bs.all$clim.site, bs.all$Year, sep="")
clim.m$SitesYear= paste(clim.m$site, clim.m$year, sep="")
clim.me$SitesYear= paste(clim.me$site, clim.me$year, sep="")

bs.all$SexElev= paste(bs.all$Sex, bs.all$elev, sep="")

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

#dd collection
nc.hr.all$yeardoy= paste(nc.hr.all$year,nc.hr.all$doy, sep="")
bs.all$yeardoy= paste(bs.all$year,bs.all$doy_spec, sep="")

match1= match(bs.all$yeardoy, nc.hr.all$yeardoy)
matched= which(!is.na(match1))
bs.all$dd_collect[matched]= nc.hr.all$cdd_sum[na.omit(match1)]

#estimate climate anomaly
#or use: https://cds.climate.copernicus.eu/cdsapp#!/dataset/ecv-for-climate-change?tab=overview
clim.site.me= aggregate(clim.me[clim.me$year %in% 1950:1980,c("Tspr","Tsum","springdd")], list(clim.me[clim.me$year %in% 1950:1980,]$site), FUN=mean)
names(clim.site.me)[1]=c("site")

match1= match(clim.me$site, clim.site.me$site)
clim.me$Tspr.anom= clim.me$Tspr - clim.site.me$Tspr[match1]
clim.me$Tsum.anom= clim.me$Tsum - clim.site.me$Tsum[match1]
clim.me$springdd.anom= clim.me$springdd - clim.site.me$springdd[match1]

#-----
#Add climate data given time before collection date
# #add initial monthly data
# match1= match(bs.all$SitesYear, clim.m$SitesYear)
# bs.all$t2m= clim.m$t2m[match1]
# bs.all$sd= clim.m$sd[match1]
# bs.all$tp= clim.m$tp[match1]
# bs.all$springdd= clim.m$springdd[match1]

#add extended monthly data
match1= match(bs.all$SitesYear, clim.me$SitesYear)
bs.all$Tspr= clim.me$Tspr[match1]
bs.all$Tsum= clim.me$Tsum[match1]
bs.all$springdd= clim.me$springdd[match1]
bs.all$Tspr.anom= clim.me$Tspr.anom[match1]
bs.all$Tsum.anom= clim.me$Tsum.anom[match1]
bs.all$springdd.anom= clim.me$springdd.anom[match1]

#add previous year summer (relevant for nymphal diapausers)
clim.me$year_Plus1= as.numeric(clim.me$year) +1
clim.me$SitesYear_Plus1= paste(clim.me$site,clim.me$year_Plus1, sep="")

match1= match(bs.all$SitesYear, clim.me$SitesYear_Plus1)
bs.all$Tsum.prev= clim.me$Tsum[match1]
bs.all$Tsum.anom.prev= clim.me$Tsum.anom[match1]

#add temp 4 weeks before specimen date
nchr.month <- nc.hr.all %>%
  dplyr::arrange(doy) %>% 
  dplyr::group_by(site) %>% 
  dplyr::mutate(t_28d = zoo::rollmean(t2m, k = 28, fill = NA),  t_28d.anom = zoo::rollmean(t2m, k = 28, fill = NA))
nchr.month= as.data.frame(nchr.month)

#add temp 4 weeks before specimen date
bs.all$SitesYearDoy= paste(bs.all$SitesYear, bs.all$doy_spec, sep="")
nchr.month$SitesYearDoy= paste(nchr.month$site, nchr.month$year, bs.all$doy_spec, sep="")

match1= match(bs.all$SitesYearDoy, nchr.month$SitesYearDoy) 
bs.all$t_28d= nchr.month[match1, "t_28d"]

#bs.all$t2m= clim.me$t2m[match1]
#bs.all$t2m.anom= clim.me$t2m.anom[match1]

match1= match(bs.all$SitesYear, clim.me$SitesYear)
bs.nomatch= bs.all[is.na(match1),]

#make into anomally
clim.sum= ddply(bs.all, c("Species", "elev"), summarise,
                  t_28d.anom = mean(t_28d, na.rm=TRUE) )
clim.sum$SpElev= paste(clim.sum$Species, clim.sum$elev, sep="")
bs.all$SpElev= paste(bs.all$Species, bs.all$elev, sep="")

match1= match(bs.all$SpElev, clim.sum$SpElev)
bs.all$t28.anom= bs.all$t_28d - clim.sum$t_28d.anom[match1]

#--------
#save data
setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/")
write.csv(bs.all, "BodySize_wClim.csv" )

#--------
#climate plots

#seasonal
clim.me$year= as.numeric(clim.me$year)
ggplot(data=clim.me, aes(x=year, y = Tspr, group= site, color=site))+
  geom_line()+geom_point()+geom_smooth(method='lm')
ggplot(data=clim.me, aes(x=year, y = Tsum, group= site, color=site))+
  geom_line()+geom_point()+geom_smooth(method='lm')
ggplot(data=clim.me, aes(x=year, y = springdd, group= site, color=site))+
  geom_line()+geom_point()+geom_smooth(method='lm')

ggplot(data=clim.m, aes(x=year, y = t2m, group= site, color=site))+ 
  facet_wrap(~seas)+
  geom_line()+geom_point()+geom_smooth(method='lm')

ggplot(data=nc.m.all, aes(x=year, y = t2m, group= site, color=site))+ 
  facet_wrap(~month)+
  geom_line()+geom_point()+geom_smooth(method='lm')

#----------
#Boulder data
#daily: https://psl.noaa.gov/boulder/data/boulderdaily.complete.txt

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
#climate data

#means
setwd("/Volumes/GoogleDrive/My Drive/Buckley/Work/GrasshopperPhenSynch/data/")
clim1= read.csv("Clim1Data.csv")

setwd("/Volumes/GoogleDrive/My Drive/AlexanderResurvey/DataForAnalysis/climate/")
clim= read.csv("AlexanderClimateAll.csv")
#Just resurvey years
#clim= read.csv("AlexanderClimateAll_filled_May2022.csv")

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
#Add Boulder and C1 climate data to body size

#Add Boulder spring, summer
#add 2012, 2013 average
b.20125= aggregate(clim.b[clim.b$Year %in% 2012:2013,c("Tspr","Tsum")], list(clim.b[clim.b$Year %in% 2012:2013,]$metric), FUN=mean)
b.20125= cbind("2012.5",b.20125[,c(2,3,1)])
colnames(b.20125)[1:4]=c("Year","Tspr","Tsum","metric")
clim.b= rbind(clim.b, b.20125)

#estimate climate anomaly
clim.b.ag= aggregate(clim.b[clim.b$Year %in% 1950:1980,c("Tspr","Tsum")], list(clim.b[clim.b$Year %in% 1950:1980,]$metric), FUN=mean, na.rm=TRUE)
names(clim.b.ag)[1]=c("metric")

match1= match(clim.b$metric, clim.b.ag$metric)
clim.b$Tspr.anom= clim.b$Tspr - clim.b.ag$Tspr[match1]
clim.b$Tsum.anom= clim.b$Tsum - clim.b.ag$Tsum[match1]

#add data
match1= match(bs.all$Year, clim.b[which(clim.b$metric=="min"),"Year"])
bs.all$Tspr.min.bould= clim.b$Tspr[which(clim.b$metric=="min")][match1]
bs.all$Tsum.min.bould= clim.b$Tsum[which(clim.b$metric=="min")][match1]
bs.all$Tspr.min.anom.bould= clim.b$Tspr.anom[which(clim.b$metric=="min")][match1]
bs.all$Tsum.min.anom.bould= clim.b$Tsum.anom[which(clim.b$metric=="min")][match1]

match1= match(bs.all$Year, clim.b[which(clim.b$metric=="means"),"Year"])
bs.all$Tspr.mean.bould= clim.b$Tspr[which(clim.b$metric=="means")][match1]
bs.all$Tsum.mean.bould= clim.b$Tsum[which(clim.b$metric=="means")][match1]
bs.all$Tspr.mean.anom.bould= clim.b$Tspr.anom[which(clim.b$metric=="means")][match1]
bs.all$Tsum.mean.anom.bould= clim.b$Tsum.anom[which(clim.b$metric=="means")][match1]

match1= match(bs.all$Year, clim.b[which(clim.b$metric=="max"),"Year"])
bs.all$Tspr.max.bould= clim.b$Tspr[which(clim.b$metric=="max")][match1]
bs.all$Tsum.max.bould= clim.b$Tsum[which(clim.b$metric=="max")][match1]
bs.all$Tspr.max.anom.bould= clim.b$Tspr.anom[which(clim.b$metric=="max")][match1]
bs.all$Tsum.max.anom.bould= clim.b$Tsum.anom[which(clim.b$metric=="max")][match1]

#---
#add spring, summer year before
#add 2011, 2012 average
b.20125= aggregate(clim.b[clim.b$Year %in% 2011:2012,c("Tspr","Tsum")], list(clim.b[clim.b$Year %in% 2011:2012,]$metric), FUN=mean)
b.20125= cbind("2011.5",b.20125[,c(2,3,1)])
colnames(b.20125)[1:4]=c("Year","Tspr","Tsum","metric")
clim.b= rbind(clim.b, b.20125)

match1= match(bs.all$Year-1, clim.b[which(clim.b$metric=="min"),"Year"])
bs.all$Tspr.min.bould.prev= clim.b$Tspr[which(clim.b$metric=="min")][match1]
bs.all$Tsum.min.bould.prev= clim.b$Tsum[which(clim.b$metric=="min")][match1]
bs.all$Tspr.min.anom.bould.prev= clim.b$Tspr.anom[which(clim.b$metric=="min")][match1]
bs.all$Tsum.min.anom.bould.prev= clim.b$Tsum.anom[which(clim.b$metric=="min")][match1]

match1= match(bs.all$Year-1, clim.b[which(clim.b$metric=="means"),"Year"])
bs.all$Tspr.mean.bould.prev= clim.b$Tspr[which(clim.b$metric=="means")][match1]
bs.all$Tsum.mean.bould.prev= clim.b$Tsum[which(clim.b$metric=="means")][match1]
bs.all$Tspr.mean.anom.bould.prev= clim.b$Tspr.anom[which(clim.b$metric=="means")][match1]
bs.all$Tsum.mean.anom.bould.prev= clim.b$Tsum.anom[which(clim.b$metric=="means")][match1]

match1= match(bs.all$Year-1, clim.b[which(clim.b$metric=="max"),"Year"])
bs.all$Tspr.max.bould.prev= clim.b$Tspr[which(clim.b$metric=="max")][match1]
bs.all$Tsum.max.bould.prev= clim.b$Tsum[which(clim.b$metric=="max")][match1]
bs.all$Tspr.max.anom.bould.prev= clim.b$Tspr.anom[which(clim.b$metric=="max")][match1]
bs.all$Tsum.max.anom.bould.prev= clim.b$Tsum.anom[which(clim.b$metric=="max")][match1]

#------
#Add C1

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
