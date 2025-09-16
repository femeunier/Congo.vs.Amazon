rm(list = ls())

library(dplyr)
library(tidyr)
library(ncdf4)
library(tidyr)
library(reshape2)
library(lubridate)

latmin = -30 ; latmax = 30
lonmin = -180 ; lonmax = 180

vars <- c("tas","pr",
          "tasmin","tasmax")

years <- seq(1981,2001,10)
df.GSWP.all <- data.frame()

for (ivar in seq(1,length(vars))){

  cvar <- vars[ivar]
  path <- paste0("/data/gent/vo/000/gvo00074/ED_common_data/met/CB/GSWP/",cvar,"_gswp3_")

  for (iyear in seq(1,length(years))){
    cyear <- years[iyear]

    file <- paste0(path,cyear,"_",cyear+9,".nc4")
    print(file)

    nc <- nc_open(file)

    lats <- ncvar_get(nc,"lat")
    poslat <- which(lats >= latmin & lats <= latmax)
    select.lat <- lats[poslat]

    lons <- ncvar_get(nc,"lon")
    poslon <- which(lons >= lonmin & lons <= lonmax)
    select.lon <- lons[poslon]

    times <- ncvar_get(nc,"time")
    df.all <- data.frame()

    cdata <- ncvar_get(nc,cvar,
                    start = c(min(poslon),min(poslat),1),
                    count = c(length(poslon),length(poslat),length(times)))   # mm/month

    df.GSWP <- as.data.frame(melt(cdata))
    df.GSWP.na <- df.GSWP %>%
      dplyr::filter(!is.na(value)) %>%
      rename(lon = Var1,
             lat = Var2,
             time = Var3)

    df.GSWP.var <- df.GSWP.na %>%
      mutate(lon = select.lon[lon],
             lat = select.lat[lat]) %>%
      mutate(date = as.Date(times[time],origin = "1860-01-01")) %>%
      mutate(days = lubridate::days_in_month(date))

    if (cvar == "pr"){
      df.GSWP.month <- df.GSWP.var %>%
        mutate(year = as.numeric(year(date)),
               month = as.numeric(month(date))) %>%
        group_by(year,month,lon,lat) %>%
        summarise(value.m = mean(value)*unique(days)*86400,
                  variable = cvar,
                  .groups = "keep") %>%
        distinct()
    } else {
      df.GSWP.month <- df.GSWP.var %>%
        mutate(year = as.numeric(year(date)),
               month = as.numeric(month(date))) %>%
        group_by(year,month,lon,lat) %>%
        summarise(value.m = mean(value,
                                 na.rm = TRUE),
                  variable = cvar,
                  .groups = "keep") %>%
        distinct()
    }

    nc_close(nc)

    df.GSWP.all <- bind_rows(list(df.GSWP.all,
                                  df.GSWP.month %>%
                                    ungroup() %>%
                                    na.omit()))

  }
}

saveRDS(df.GSWP.all %>%
          pivot_wider(names_from = "variable",
                      values_from = "value.m"),
        "./outputs/df.GSWP.Tropics.climate.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/summarise.GSWP.climate.R hpc:/data/gent/vo/000/gvo00074/felicien/R

