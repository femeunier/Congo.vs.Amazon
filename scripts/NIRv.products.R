rm(list = ls())

library(ncdf4)
library(dplyr)
library(reshape2)
library(raster)
library(stringr)
library(ggplot2)
library(zoo)

Amazon.coord <- readRDS("./data/Amazon.coord.ILF.RDS") %>%
  filter(model == "DLEM") %>%
  mutate(lon.lat = paste0(lon,".",lat))

Congo.coord <- readRDS("./data/Congo.coord.ILF.RDS") %>%
  filter(model == "DLEM") %>%
  mutate(lon.lat = paste0(lon,".",lat))

all.files <- list.files("/home/femeunier/Downloads/TROP/",
                        full.names = TRUE)
all.df <- data.frame()
for (ncfile in all.files){

  print(ncfile)

  cdate <- str_split_i(basename(ncfile), "\\_", 7)
  cyear <- substr(cdate,1,4)
  cmonth <- substr(cdate,5,6)
  cday <- substr(cdate,7,8)
  nc <- nc_open(ncfile)
  lats <- ncvar_get(nc,"lat")
  lons <- ncvar_get(nc,"lon")
  NIRv <- melt(ncvar_get(nc,"NIRv")) %>%
    mutate(lon = round(lons[Var1],digits = 2),
           lat = round(lats[Var2],digits = 2)) %>%
    dplyr::select(lon,lat,value)

  craster <- rasterFromXYZ(NIRv[,c("lon","lat","value")])
  bigraster <- raster::aggregate(craster,
                                 5)

  cdf <- as.data.frame(bigraster,
                       xy = TRUE) %>%
    rename(lon = x,
           lat = y) %>%
    mutate(year = as.numeric(cyear),
           month = as.numeric(cmonth),
           day = as.numeric(cday)) %>%
    filter(!is.na(value))

  all.df <- bind_rows(all.df,
                      cdf %>%
                        mutate(lon.lat = paste0(round(lon,
                                                      digits = 2),".",
                                                round(lat,
                                                      digits = 2))) %>%
                        filter(lon.lat %in% Amazon.coord[["lon.lat"]]) %>%
                        mutate(basin = "Amazon"),
                      cdf %>%
                        mutate(lon.lat = paste0(round(lon,
                                                      digits = 2),".",
                                                round(lat,
                                                      digits = 2))) %>%
                        filter(lon.lat %in% Congo.coord[["lon.lat"]]) %>%
                        mutate(basin = "Congo"))
  nc_close(nc)

}


all.df.sum <- all.df %>%
  group_by(basin,year,month,lon,lat) %>%
  summarise(value = mean(value,
                         na.rm = TRUE),
            N = n(),
            .groups = "keep")


anomalies <- all.df.sum %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(basin,lon,lat) %>%
  mutate(slope = coef(lm(value ~ time))[2],
         intercept = coef(lm(value ~ time))[1]) %>%
  mutate(mean.obs =  intercept + slope *time) %>%
  mutate(mean.obs =  mean(value)) %>%
  mutate(detrended = value - mean.obs) %>%
  group_by(basin,lon,lat,month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin,lon,lat) %>%
  mutate(anomaly.m = anomaly/sd(anomaly)) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         value.m.rm = rollapply(value, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"))

ggplot(data = anomalies %>%
         filter(year ==2023,
                month == 11,
                lon <= -30)) +
  geom_tile(aes(x = lon,
                y = lat,
                fill = anomaly.m)) +
  scale_fill_gradient2() +
  theme_bw()

all.df.sum <- all.df %>%
  group_by(basin,year,month,lon,lat) %>%
  summarise(value = mean(value,
                           na.rm = TRUE),
            N = n(),
            .groups = "keep") %>%
  group_by(basin,year,month) %>%
  summarise(value.m = mean(value,
                         na.rm = TRUE),
            N = n(),
            .groups = "keep")

ggplot(all.df.sum) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m)) +
  facet_wrap(~ basin) +
  theme_bw()

Window = 6

all.df.sum.anomaly <- all.df.sum %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(basin) %>%
  mutate(slope = coef(lm(value.m ~ time))[2],
         intercept = coef(lm(value.m ~ time))[1]) %>%
  mutate(mean.obs =  intercept + slope *time) %>%
  mutate(mean.obs =  mean(value.m)) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(basin,month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin) %>%
  mutate(anomaly.m = anomaly/sd(anomaly)) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         value.m.rm = rollapply(value.m, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"))

ggplot(all.df.sum.anomaly) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = mean.obs),
            color = "lightgrey") +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = reconstructed),
            color = "lightgrey",linetype = 2) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m)) +
  facet_wrap(~ basin) +
  theme_bw()

ggplot(all.df.sum.anomaly) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.m)) +
  facet_wrap(~ basin) +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw()
