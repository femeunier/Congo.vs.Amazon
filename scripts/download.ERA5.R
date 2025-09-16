library(reticulate)
library(future)
library(purrr)
library(furrr)
library(lubridate)

# setwd("/home/femeunier/Documents/projects/YGB/outputs/") # change this to your own working directory
setwd("/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/")

plan(multicore)
# Manaus
# Latitude: -3.117034
# Longitude: -60.025780

# Paracou
# Latitude: 5.3
# Longitude: -52.9

files.already.dow <- tools::file_path_sans_ext(list.files(
  getwd()))
yrs <- as.numeric(sub(".*\\_","",files.already.dow))
years <- 2023:2024
cmonths <- as.list(as.character(1:month(today())))
# years <- years[!(years %in% yrs)]

c(years) %>%
  future_map(function(year) {

    # you need to have an account for downloaing the files
    # Read the documantion for how to setup your account and settings before trying this
    # https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5#HowtodownloadERA5-3-DownloadERA5datathroughtheCDSAPI
    cdsapi <-import("cdsapi")
    c <- cdsapi$Client()

    c$retrieve(
      'reanalysis-era5-single-levels',
      list(
        'product_type' = 'reanalysis',
        'format' = 'netcdf',
        'day' = list('01','02','03',
                     '04','05','06',
                     '07','08','09',
                     '10','11','12',
                     '13','14','15',
                     '16','17','18',
                     '19','20','21',
                     '22','23','24',
                     '25','26','27',
                     '28','29','30',
                     '31'),
        'time' = list('00:00','01:00','02:00','03:00','04:00','05:00','06:00',
                      '07:00','08:00','09:00','10:00','11:00','12:00',
                      '13:00','14:00','15:00','16:00','17:00','18:00',
                      '19:00','20:00','21:00','22:00','23:00'),
        'month' = cmonths,
        'year' = as.character(year),
        'area' = "25/-100/-25/180", #"5/-53/5.5/-52.5",
        'grid' = "0.25/0.25",
        'variable' = list( "2m_temperature",
                           "2m_dewpoint_temperature",
                           "total_precipitation")
      ),
      paste0('ERA5_Tropics_fine_',year,'.nc')
    )
  })

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/download.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R



