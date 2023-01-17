#Niwot options:
#https://github.com/lbuckley/HopperPhenology/blob/master/ClimateExtremes_20May2022.R

library(NicheMapR)
library(ecmwfr)
library(mcera5)

# get ERA5 data with package mcera5 (just do once for region and time of interest)
# assign your credentials (register here: https://cds.climate.copernicus.eu/user/register)
uid <- "78176"
cds_api_key <- "062c3e77-bcc8-4c56-8e72-4872e7a92be6"

ecmwfr::wf_set_key(user = uid, key = cds_api_key, service = "cds")

if(loc.k==1){xmn <- -119.6; xmx <- -119.45; ymn <- 46.75; ymx <- 47}
if(loc.k==1){loc <- c(-119.535331192, 46.850663264)}  #Corfu

#build_era5_request
#request_era5

# Designate your desired bounding coordinates (in WGS84 / EPSG:4326)


xmn <- -4

xmx <- -2

ymn <- 49

ymx <- 51


# Designate your desired temporal extent


st_time <- lubridate::ymd("2010:02:26")

en_time <- lubridate::ymd("2010:03:01")


# Set a unique prefix for the filename (here based on spatial



# coordinates), and the file path for downloaded .nc files (here,



# the user's working directory)

#https://doi.org/10.1111/2041-210X.13877

file_prefix <- "era5_-4_-2_49_51"

file_path <- getwd()


# Build a request


req <- build_era5_request(xmin = xmn, xmax = xmx,
                          
                          ymin = ymn, ymax = ymx,
                          
                          start_time = st_time,
                          
                          end_time = en_time,
                          
                          outfile_name = file_prefix)


# Submit your request


request_era5(request = req, uid = uid, out_path = file_path)
