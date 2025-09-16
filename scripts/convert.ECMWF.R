rm(list = ls())

library(ncdf4)
library(ggplot2)
library(reshape2)
library(dplyr)
library(rnaturalearth)
library(tidyr)
library(gribr)
library(lubridate)
library(raster)

ncfile2 <- "/data/gent/vo/000/gvo00074/felicien/ECMWF/req2_1.nc"
nc2 <- nc_open(ncfile2)

vars <- c("var130",
          "var133")
var.names <- c("tmp","spfh")

first <- TRUE

for (ivar in seq(1,length(vars))){

  cvar <- vars[ivar] ; cvar.name <- var.names[ivar]

  print(cvar.name)

  if (ivar == 1){

    lats <- round(ncvar_get(nc2,
                      "lat"),digits = 2)
    lons <- round(ncvar_get(nc2,
                      "lon"),digits = 2)

    times <- ncvar_get(nc2,
                       "time")

    dates <- as.Date(times/24, origin = "2024-05-01")
    months <- unique(month(dates))
    years <- unique(year(dates))

  }


  # for (cyear in years){
  #   for (cmonth in months){
  #
  #     print(paste0(cmonth," - ",cyear))
  #      pos <- which(month(dates) == cmonth & year(dates) == cyear)
  #      ctimes <- times[pos]
  #      cdates <- dates[pos]
  #
       # if (length(pos) == 0){
       #   next()
       # }

       A <- ncvar_get(nc2,cvar)
       # cvalues <- f[,pos.lats,seq(min(1),max(pos),51)]
       # stop()
        # A <- rowMeans(cvar.df, dims = 3)

        cdf <- melt(A) %>%
          dplyr::mutate(lon = lons[Var1],
                        lat = lats[Var2],
                        time = times[Var3]) %>%
          dplyr::select(-c(starts_with("Var"))) %>%
          mutate(lon = case_when(lon > 180 ~ lon - 360,
                                 TRUE ~ lon)) %>%
          rename(!!cvar.name := value) %>%
          filter(abs(lat) <= 23)

        if (first){
          part2 <- cdf
          first = FALSE
        } else {
          part2 <- part2 %>%
            left_join(cdf,
                      by = c("lat","lon","time"))
        }

  #   }
  # }
}

nc_close(nc2)

part2.final <- part2 %>%
  mutate(rh = PEcAn.data.atmosphere::qair2rh(spfh,
                                             tmp - 273.15,
                                             press = 1000)) %>%
  mutate(VPD = PEcAn.data.atmosphere::get.vpd(rh*100,
                                              tmp - 273.15)) %>%
  mutate(day=time/24) %>%
  mutate(date = as.Date(day, origin = "2024-05-01")) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  group_by(year,month,lat,lon) %>%
  summarise(tmp1 = mean(tmp),
            spfh = mean(spfh),
            VPD = mean(VPD),
            .groups = "keep")

# Finally read and plot
ncfile <- "/data/gent/vo/000/gvo00074/felicien/ECMWF/req1.nc"
nc <- nc_open(ncfile)

vars <- c("MX2T24_GDS4_SFC",
          "MN2T24_GDS4_SFC",
          "2T_GDS4_SFC",
          "VAR_169_GDS4_SFC",
          "VAR_175_GDS4_SFC",
          "VAR_228_GDS4_SFC")
var.names <- c("tmax","tmin",
              "tmp","dswrf","dlwrf","pre")

for (ivar in seq(1,length(vars))){
  cvar <- vars[ivar] ; cvar.name <- var.names[ivar]

  print(cvar.name)

  if (ivar == 1){

    lats <- round(ncvar_get(nc,
                      "g4_lat_1"),digits = 2)
    # lons <- round(ncvar_get(nc,
    #                   "g4_lon_2"),digits = 2)

    time.dimension <- any(grepl("time",nc$dim))

    if (time.dimension){
      times <- ncvar_get(nc,
                         "forecast_time3")
    }

    # time0 <- ncvar_get(nc,
    #                    "forecast_time0")

  }

  if (time.dimension){
    cdf <- melt(ncvar_get(nc,
                          cvar)) %>%
      dplyr::mutate(lon = lons[Var1],
                    lat = lats[Var2],
                    time = times[Var3]) %>%
      dplyr::select(-c(starts_with("Var"))) %>%
      mutate(lon = case_when(lon > 180 ~ lon - 360,
                             TRUE ~ lon)) %>%
      rename(!!cvar.name := value) %>%
      filter(abs(lat) <= 23)

  } else{
    cdf <- melt(ncvar_get(nc,
                          cvar)) %>%
      dplyr::mutate(lon = lons[Var1],
                    lat = lats[Var2],
                    time = 1) %>%
      dplyr::select(-c(starts_with("Var"))) %>%
      mutate(lon = case_when(lon > 180 ~ lon - 360,
                             TRUE ~ lon)) %>%
      rename(!!cvar.name := value) %>%
      filter(abs(lat) <= 23)
  }

  if (ivar == 1){
    part1 <- cdf
  } else {
    part1 <- part1 %>%
      left_join(cdf,
                by = c("lat","lon","time"))
  }

}

nc_close(nc)

part1.final <- part1 %>%
  # dplyr::select(lon,lat,pre,tmp,tmin,tmax,dlwrf,dswrf) %>%
  mutate(month = 4 + round(time/744),
         pre = pre*86400*1000/8,
         dswrf = dswrf*3600*6,
         year = 2024)


merged.ECMWF <- part1.final %>%
  left_join(part2.final,
            by = c("lon","lat","year","month")) %>%
  dplyr::select(c(year,month,lon,lat,tmp,tmin,tmax,dswrf,dlwrf,pre,spfh,VPD))

saveRDS(merged.ECMWF,
        "./data/monthly.climate.pantropical.Tropics.ECMWF.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/convert.ECMWF.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
