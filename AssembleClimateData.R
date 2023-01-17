#Niwot options:
#https://github.com/lbuckley/HopperPhenology/blob/master/ClimateExtremes_20May2022.R

library(NicheMapR)
library(ecmwfr)
library(mcera5)
library(sf)
library(ncdf4)
library(tidyverse)
library(lubridate)

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

for(k in 2:3){

  if(k==1){
    request.month[[7]]<- "40.06/-105.59/39.99/-105.28"
    request.hr[[8]]<- "40.06/-105.59/39.99/-105.28"
    request.month[[9]]<- "COgrid_ERA5_month.nc"
    request.hr[[10]]<- "COgrid_ERA5_hr.nc"
  }
  
  if(k==2){
    request.month[[7]]<- "39.94/-105.67/39.93/-105.66"
    request.hr[[8]]<- "39.94/-105.67/39.93/-105.66"
    request.month[[9]]<- "Rollins_ERA5_month.nc"
    request.hr[[10]]<- "Rollins_ERA5_hr.nc"
  }
  
  if(k==3){
    request.month[[7]]<- "39.60/-105.64/39.58/-105.63"
    request.hr[[8]]<- "39.60/-105.64/39.58/-105.63"
    request.month[[9]]<- "Evans_ERA5_month.nc"
    request.hr[[10]]<- "Evans_ERA5_hr.nc"
  }
  
  #request
  file <- wf_request(
    user     = "78176",   # user ID (for authentification)
    request  = request.month,  # the request
    transfer = TRUE,     # download the file
    path     = "."       # store data in current working directory
  )
  
  file <- wf_request(
    user     = "78176",   # user ID (for authentification)
    request  = request.hr,  # the request
    transfer = TRUE,     # download the file
    path     = "."       # store data in current working directory
  )
}
 
#----
#process data

setwd("/Volumes/GoogleDrive/Shared drives/RoL_FitnessConstraints/projects/BodySize/data/ClimateData/")

for(k in 1:3){

#open the connection with the ncdf file
if(k==1){ 
  nc.m <- nc_open("COgrid_ERA5_month.nc")
  nc.hr <- nc_open("COgrid_ERA5_hr.nc")
}
  if(k==2){ 
    nc.m <- nc_open("Rollins_ERA5_month.nc")
    nc.hr <- nc_open("Rollins_ERA5_hr.nc")
  }
  if(k==3){ 
    nc.m <- nc_open("Evans_ERA5_month.nc")
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

#DEAL WITH GRIDS
nc.m.data= cbind(timestamp, t2m[1,], sd[1,], tp[1,])
if(k==1) nc.m.all= nc.m.data
if(k>1) nc.m.all= rbind(nc.m.all, nc.m.data)

#---
#hr data

#extract the time
t <- ncvar_get(nc.hr, "time")
#convert the hours into date + hour
#as_datetime() function of the lubridate package needs seconds
timestamp <- as_datetime(c(t*60*60),origin="1900-01-01")

#import the data
t2m <- ncvar_get(nc.m,"t2m")
#add soil temp, wind for microclimate?

#combine data
#DEAL WITH GRIDS
nc.m.data= cbind(timestamp, t2m[1,])
if(k==1) nc.m.all= nc.m.data
if(k>1) nc.m.all= rbind(nc.m.all, nc.m.data)

}

#close the conection with the ncdf file
nc_close(nc.m)
nc_close(nc.hr)

