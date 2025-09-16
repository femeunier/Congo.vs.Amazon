rm(list = ls())

library(gdalUtils)
library(raster)
library(dplyr)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(zoo)
library(tidyr)
library(sf)

files <- list.files("/home/femeunier/Downloads/MOD",pattern = "MOD13C2.A*",full.names = TRUE)




coord.list <- readRDS("./outputs/ILF2020.small.df")
rast.coord <- rasterFromXYZ(coord.list[c("lon","lat","is.undisturbed.factor")])

all.df <- data.frame()

for (ifile in seq(1,length(files))){

  cfile <- files[ifile]
  print(paste(cfile,"-",which(cfile == files)/length(files)))
  cformat <- substr(sub("MOD13C2.A","",basename(cfile)),1,7)
  cyear <- as.numeric(substr(cformat,1,4))
  cday <- as.numeric(substr(cformat,5,7))
  cdate <- as.Date(cday-1, origin = paste0(cyear,"-01-01"))

  sds <- tryCatch(get_subdatasets(cfile),
                  error = function(err){NULL})

  if (is.null(sds)){
    next
  }

  gdal_translate(sds[1], dst_dataset = "/home/femeunier/Downloads/NDVI.tif")
  gdal_translate(sds[2], dst_dataset = "/home/femeunier/Downloads/EVI.tif")
  gdal_translate(sds[13], dst_dataset = "/home/femeunier/Downloads/reliability.tif")
  # Load and plot the new .tif

  rast <- raster("~/Downloads/EVI.tif")
  rast2 <- raster("~/Downloads/NDVI.tif")
  rast3 <- raster("~/Downloads/reliability.tif")
  rast3[rast3>3] <- NA

  rast[rast3 != 0] <- NA
  rast2[rast3 != 0] <- NA

  rast.mod <- rast
  rast.mod[rast.coord == 0] <- NA

  rast2.mod <- rast2
  rast2.mod[rast.coord == 0] <- NA


  cdf <- as.data.frame(rast.mod,xy = TRUE) %>%
    rename(lon = x,
           lat = y,
           EVI = CMG.0.05.Deg.Monthly.EVI) %>%
    filter(!is.na(EVI)) %>%
    filter(abs(lat) < 25,
           ((lon >= -80 & lon <= -35) | (lon >= -15 & lon <= 45))) %>%
    mutate(basin = case_when(lon <= -35 ~ "Amazon",
                             TRUE ~ "Congo"))

  cdf2 <- as.data.frame(rast2.mod,xy = TRUE) %>%
    rename(lon = x,
           lat = y,
           NDVI = CMG.0.05.Deg.Monthly.NDVI) %>%
    filter(!is.na(NDVI)) %>%
    filter(abs(lat) < 25,
           ((lon >= -80 & lon <= -35) | (lon >= -15 & lon <= 45))) %>%
    mutate(basin = case_when(lon <= -35 ~ "Amazon",
                             TRUE ~ "Congo"))

  all.df <- bind_rows(all.df,
                      cdf %>%
                        left_join(cdf2,
                                  by = c("lat","lon","basin")) %>%
                        mutate(year = cyear,
                               month = month(cdate)) %>%
                        group_by(year,month,basin) %>%
                        summarise(EVI.m = mean(EVI),
                                  NDVI.m = mean(NDVI),
                                  .groups = "keep"))


  # rast.big <- raster::aggregate(rast,10)
  # rast2.big <- raster::aggregate(rast2,10)
  # rast3.big <- round(raster::aggregate(rast3,10))
  #
  #
  # cdf <- as.data.frame(rast.big,xy = TRUE) %>%
  #   rename(lon = x,
  #          lat = y,
  #          EVI = CMG.0.05.Deg.Monthly.EVI) %>%
  #   filter(!is.na(EVI)) %>%
  #   filter(abs(lat) < 25)
  #
  # cdf2 <- as.data.frame(rast2.big,xy = TRUE) %>%
  #   rename(lon = x,
  #          lat = y,
  #          NDVI = CMG.0.05.Deg.Monthly.NDVI) %>%
  #   filter(!is.na(NDVI)) %>%
  #   filter(abs(lat) < 25)
  #
  # all.df <- bind_rows(all.df,
  #                     cdf %>%
  #                       left_join(cdf2,
  #                                 by = c("lat","lon")) %>%
  #                       mutate(year = cyear,
  #                              month = month(cdate)))
}

# all.df.filt <- all.df %>%
#   left_join(coord.list %>%
#               dplyr::select(lat,lon,is.undisturbed.factor,basin),
#             by = c("lat","lon")) %>%
#   filter(is.undisturbed.factor == 1)


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")


# all.df.filt.keep <- all.df.filt %>%
#   dplyr::select(-any_of(c("day","is.undisturbed.factor"))) %>%
#   ungroup() %>%
#   complete(lat = unique(all.df.filt$lat),
#            lon = unique(all.df.filt$lon),
#            year = 2000:2024,
#            month = 1:12) %>%
#   group_by(lon,lat) %>%
#   mutate(Nobs = sum(!is.na(EVI))) %>%
#   group_by(basin) %>%
#   filter(Nobs >= 0.5*max(Nobs)) %>%
#   mutate(basin = case_when(lon <= -30 ~ "Amazon",
#                            TRUE ~ "Congo")) %>%
#   mutate(time = year + (month - 1/2)/12) %>%
#   filter(time <= 2024 + (4 -1/2)/12)
#
#
# ggplot(data = all.df.filt.keep %>%
#          filter(year == 2019,
#                 month == 4)) +
#   geom_tile(aes(x=lon,y = lat,
#                 fill = EVI),alpha = 1) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   geom_sf(data = Amazon.shp,fill = NA, color = "black") +
#   geom_sf(data = Congo.shp,fill = NA, color = "black") +
#   coord_sf(xlim = c(-85, 45), ylim = c(-25, 10), expand = FALSE) +
#   labs(x = "",y = "") +
#   theme_map() +
#   # guides(fill = "none") +
#   theme(text = element_text(size = 20),
#         strip.background = element_blank(),
#         strip.text = element_blank())

# all.df.filt.sum <- all.df.filt.keep %>%
#   group_by(basin,year,month) %>%
#   summarise(EVI.m = mean(EVI,na.rm = TRUE)*0.0001,
#             NDVI.m = mean(NDVI,na.rm = TRUE)*0.0001,
#             N = length(EVI),
#             .groups = "keep") %>%
#   pivot_longer(cols = c(EVI.m,NDVI.m,N),
#                names_to = "var",
#                values_to = "value.m") %>%
#   ungroup()
all.df.filt.sum <- all.df %>%
    pivot_longer(cols = c(EVI.m,NDVI.m),
                 names_to = "var",
                 values_to = "value.m") %>%
  mutate(value.m = value.m/10000)

ggplot(data = all.df.filt.sum) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = basin)) +
  facet_wrap(~var, scales = "free_y") +
  theme_bw()


Window = 6
year.min = 2000
year.max = 2023

MEM.reference <- all.df.filt.sum %>%
  mutate(time = year + (month - 1/2)/12) %>%
  filter(year >= year.min,
         year < year.max) %>%
  group_by(basin,var) %>%
  mutate(slope = coef(lm(value.m ~ time))[2],
         intercept = coef(lm(value.m ~ time))[1]) %>%
  ungroup() %>%
  dplyr::select(basin,var,slope,intercept) %>%
  distinct()

SC <- all.df.filt.sum %>%
  group_by(basin,var,month) %>%
  summarise(value.m = mean(value.m,
                           na.rm = TRUE),
            .groups = "keep")


ggplot(data = SC) +
  geom_line(aes(x = month,
                y = value.m,
                color = basin)) +
  # scale_x_continuous(limits = c(2020,2025)) +
  facet_wrap(~ var,scales = "free_y") +
  theme_bw()

MEM.anomaly <- all.df.filt.sum %>%
  mutate(time = year + (month - 1/2)/12) %>%
  left_join(MEM.reference,
            by = c("basin","var")) %>%
  group_by(basin,var) %>%
  mutate(mean.obs =  intercept + slope *time) %>%
  # mutate(mean.obs = mean(value.m[time >= year.min & time < year.max],
  #                        na.rm = TRUE)) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(basin,var,month) %>%
  mutate(mean.month = mean(detrended[time >= year.min & time < year.max],
                           na.rm = TRUE)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin,var) %>%
  mutate(anomaly.sd = sd(anomaly[time >= year.min & time < year.max],
                         na.rm = TRUE)) %>%
  mutate(anomaly.m = anomaly/anomaly.sd) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         value.m.rm = rollapply(value.m, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"))


ggplot(data = MEM.anomaly %>%
         na.omit(),
       aes(x = time,
           y = value.m,
           color = basin)) +
  geom_line(aes(x = time,
                y = value.m.rm,
                color = basin),
            linetype = 1, linewidth = 1) +
  geom_line(size = 0.25) +
  # scale_x_continuous(limits = c(2020,2025)) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE) +
  facet_wrap(~ var,scales = "free_y") +
  theme_bw()


ggplot(data = MEM.anomaly %>%
         na.omit(),
       aes(x = time,
           y = value.m,
           color = basin)) +
  geom_line(aes(x = time,
                y = value.m.rm,
                color = basin),
            linetype = 1, linewidth = 1) +
  geom_line(size = 0.25) +
  scale_x_continuous(limits = c(2020,2025)) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE) +
  facet_wrap(~ var,scales = "free_y") +
  theme_bw()


ggplot(data = MEM.anomaly) +
  geom_line(aes(x = time,
                y = anomaly.m.rm,
                color = basin),
            linetype = 1, linewidth = 1) +
  geom_line(aes(x = time,
                 y = anomaly.m,
                 color = basin),
             size = 0.25) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  scale_x_continuous(limits = c(2000,2025)) +
  facet_wrap(~ var,scales = "free_y") +
  theme_bw()

