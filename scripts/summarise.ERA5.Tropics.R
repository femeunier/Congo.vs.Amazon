rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(lubridate)

init.years <- seq(1940,2023,1)

WD <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/"

coord <- bind_rows(
  readRDS("./outputs/Amazon.coord.ILF.RDS"),
  readRDS("./outputs/Congo.coord.ILF.RDS")) %>%
  filter(model == "CABLE-POP") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
  filter(is.undisturbed.factor == 1)

df.ERA5 <- data.frame()
for (iyear in seq(1,length(init.years))){

  print(iyear/length(init.years))

  cfile <- file.path(WD,paste0("ERA5_Tropics_",init.years[iyear],".nc"))
  if (file.exists(cfile)){

    nc <- nc_open(cfile)
    tmp <- ncvar_get(nc,"t2m")
    prate <- ncvar_get(nc,"tp")
    lats <- ncvar_get(nc,"latitude")
    lons <- ncvar_get(nc,"longitude")
    times <- ncvar_get(nc,"time")

    nc_close(nc)

    if (init.years[iyear] == 2023){

      # First merge expver
      pos.na <- which(is.na(prate[1,1,1,]))
      prate[,,1,pos.na] <-
        prate[,,2,pos.na]

      pos.na2 <- which(is.na(tmp[1,1,1,]))
      tmp[,,1,pos.na2] <-
        tmp[,,2,pos.na2]

      prate.df <- melt(prate[,,1,]) %>%
        mutate(lon = (lons)[Var1],
               lat = (lats)[Var2],
               time = times[Var3]) %>%
        dplyr::select(lat,lon,time,value) %>%
        rename(prate = value)

      tmp.df <- melt(tmp[,,1,]) %>%
        mutate(lon = (lons)[Var1],
               lat = (lats)[Var2],
               time = times[Var3]) %>%
        dplyr::select(lat,lon,time,value) %>%
        rename(tmp = value)

    } else {
      prate.df <- melt(prate) %>%
        mutate(lon = (lons)[Var1],
               lat = (lats)[Var2],
               time = times[Var3]) %>%
        dplyr::select(lat,lon,time,value) %>%
        rename(prate = value)

      tmp.df <- melt(tmp) %>%
        mutate(lon = (lons)[Var1],
               lat = (lats)[Var2],
               time = times[Var3]) %>%
        dplyr::select(lat,lon,time,value) %>%
        rename(tmp = value)

    }


    prate.df.time <- prate.df %>%
      left_join(tmp.df,
                by = c("lon","lat","time")) %>%
      mutate(date = as.Date(time/24,origin = "1900-01-01")) %>%
      mutate(month = month(date),
             year = year(date))

    cdf <- prate.df.time %>%
      group_by(year,month,lat,lon) %>%
      summarise(MAP = sum(prate)*1000*3,
                tmp = mean(tmp),
                .groups = "keep")

    df.ERA5 <- bind_rows(list(df.ERA5,
                              cdf %>%
                                mutate(lon.lat = paste0(lon,".",lat)) %>%
                                dplyr::filter(lon.lat %in% coord[["lon.lat"]]) %>%
                                dplyr::select(-lon.lat)))
  }
}

saveRDS(df.ERA5,
        "./outputs/df.ERA5.Tropics.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/summarise.ERA5.Tropics.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

