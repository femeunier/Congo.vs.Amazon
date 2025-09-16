rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(lubridate)
library(matlab)

init.years <- seq(1980,2010,1)

WD <- "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/Tropics/"

df.CRU <- data.frame()
for (iyear in seq(1,length(init.years))){

  print(iyear/length(init.years))

  cfile <- file.path(WD,paste0("CRUNCEP.",init.years[iyear],".nc"))

  if (file.exists(cfile)){

    nc <- nc_open(cfile)

    lats <- fliplr(as.vector(ncvar_get(nc,"latitude")))
    lons <- ncvar_get(nc,"longitude")
    times <- ncvar_get(nc,"time")

    tair <- ncvar_get(nc,"air_temperature")
    prate <- ncvar_get(nc,"precipitation_flux")

    nc_close(nc)

    prate.df <- melt(prate) %>%
      mutate(lon = (lons)[Var1],
             lat = (lats)[Var2],
             time = times[Var3]) %>%
      dplyr::select(lat,lon,time,value) %>%
      rename(prate = value) %>%
      filter(abs(lat) <= 30) %>%
      filter(!is.na(prate))

    tair.df <- melt(tair) %>%
      mutate(lon = (lons)[Var1],
             lat = (lats)[Var2],
             time = times[Var3]) %>%
      dplyr::select(lat,lon,time,value) %>%
      rename(tair = value) %>%
      filter(abs(lat) <= 30) %>%
      filter(!is.na(tair))

    prate.df.time <- prate.df %>%
      mutate(date = as.Date(time,
                            origin = paste0(init.years[iyear],"-01-01"))) %>%
      mutate(day = day(date),
             month = month(date),
             year = year(date)) %>%
      group_by(year,month,lat,lon) %>%
      summarise(MAP = sum(prate)*21600,
                .groups = "keep")

    tair.df.time <- tair.df %>%
      mutate(date = as.Date(time,
                            origin = paste0(init.years[iyear],"-01-01"))) %>%
      mutate(day = day(date),
             month = month(date),
             year = year(date)) %>%
      group_by(year,month,day,lat,lon) %>%
      summarise(tmin = min(tair,na.rm = TRUE),
                tmax = max(tair,na.rm = TRUE),
                tmp = mean(tair,na.rm = TRUE),
                .groups = "keep") %>%
      group_by(year,month,lat,lon) %>%
      summarise(tmin = mean(tmin,na.rm = TRUE),
                tmax = mean(tmax,na.rm = TRUE),
                tmp = mean(tmp,na.rm = TRUE),
                .groups = "keep")

    cdf <- prate.df.time %>%
      left_join(tair.df.time,
                by = c("lon","lat","year","month"))

  }

  df.CRU <- bind_rows(list(df.CRU,
                           cdf))

}

saveRDS(df.CRU,"./outputs/df.CRU.pantropical.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/summarise.CRU.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

