rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(stringr)
library(ggplot2)
library(tidyr)
library(raster)
library(zoo)

Amazon.coord <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Amazon.coord.TROPOSIF.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat))


ncfiles <- list.files("~/Downloads/TROP/",full.names = TRUE)
vars <- c("CF","NDVI","NIRvP_743","NIRv","SIF_743","SIF_Corr_743",
          "sza","vza")

all.df.ts <- data.frame()

for (ifile in seq(1,length(ncfiles))){

  ncfile <- ncfiles[ifile]
  print(paste(ifile/length(ncfiles)," - ",ncfile))

  cdate <- (str_split(basename(ncfile),"_")[[1]][6])
  cyear <- as.numeric(substr(cdate,1,4))
  cmonth <- as.numeric(substr(cdate,5,6))

  nc <- nc_open(ncfile)

  lats <- ncvar_get(nc,"lat")
  lons <- ncvar_get(nc,"lon")

  all.df <- data.frame()
  for (ivar in seq(1,length(vars))){
    cvar <- ncvar_get(nc,vars[ivar])

    cdf <- melt(cvar) %>%
      dplyr::rename(lon = Var1,
                    lat = Var2) %>%
      filter(!is.na(value)) %>%
      dplyr::rename(!!vars[ivar] := value) %>%
      mutate(lon = round(lons[lon],digits = 2),
             lat = round(lats[lat],digits = 2)) %>%
      filter(abs(lat) <= 25) %>%
      mutate(lon.lat = paste0(round(lon,
                                    digits = 2),".",
                              round(lat,
                                    digits = 2))) %>%
      filter(lon.lat %in% Amazon.coord[["lon.lat"]]) %>%
      dplyr::select(-lon.lat)

    # cgrid <- rasterFromXYZ((cdf %>%
    #                          ungroup() %>%
    #                          dplyr::select(c("lat","lon",vars[ivar])))[,c("lon","lat",vars[ivar])])
    #
    # if (ivar == 1){
    #   pos <-(cgrid > 0.2)
    # } else{
    #   cgrid[pos] <- NA
    # }
    #
    # cgrid.coarse <- aggregate(cgrid,
    #                           5)
    # cdf.coarse <- as.data.frame(cgrid.coarse,
    #                             xy = TRUE) %>%
    #   rename(lon = x,
    #          lat = y) %>%
    #   mutate(lon = round(lon,digits = 2),
    #          lat = round(lat,digits = 2)) %>%
    #   mutate(lon.lat = paste0(lon,".",lat)) %>%
    #   filter(lon.lat %in% Amazon.coord[["lon.lat"]]) %>%
    #   dplyr::select(-lon.lat)


    if (ivar == 1){
      all.df <- cdf
    } else {
      all.df <- all.df %>%
        left_join(cdf,
                  by = c("lat","lon"))
    }
  }

  all.df.ts <- bind_rows(all.df.ts,
                         all.df %>%
                           mutate(year = cyear,
                                  month = cmonth))

  nc_close(nc)
}

hist(all.df.ts$CF)

all.df.ts.long <- all.df.ts %>%
  dplyr::select(lon,lat,year,month,any_of(vars)) %>%
  filter(CF < 0.2) %>%
  pivot_longer(cols = -c(year,month,lon,lat),
               names_to = "variable",
               values_to = "value")

all.df.ts.long.month <- all.df.ts.long %>%
  group_by(variable,year,month,lon,lat) %>%
  summarise(value.m = mean(value,
                           na.rm = TRUE),
            .groups = "keep")

all.df.ts.sum <- all.df.ts.long.month %>%
  group_by(variable,year,month) %>%
  summarise(value.m.month = mean(value.m,
                                 na.rm = TRUE),
            .groups = "keep")

ggplot(data = all.df.ts.sum) +
  geom_rect(data = data.frame(xmin = 2023 + (6.5)/12, xmax = 2024 + (5.5)/12,
                              ymin = -Inf, ymax = Inf,
                              variable = unique(all.df.ts.sum$variable)),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = NA, fill = "lightgrey",alpha = 0.5) +
  geom_line(aes(x = year + (month -1/2)/12,
                y = value.m.month)) +
  facet_wrap(~ variable,scales = "free") +
  theme_bw()

Window <- 6 ; year.min <- 2000 ; year.max <- 2023

anomalies <- all.df.ts.sum %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(variable) %>%
  mutate(slope = coef(lm(value.m.month ~ time))[2],
         intercept = coef(lm(value.m.month ~ time))[1]) %>%
  mutate(mean.obs =  intercept + slope *time) %>%
  # mutate(mean.obs = mean(value.m.month,
  #                        na.rm = TRUE)) %>%
  mutate(detrended = value.m.month - mean.obs) %>%
  group_by(variable,month) %>%
  mutate(mean.month = mean(detrended,
                           na.rm = TRUE)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(variable,month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly)) %>%
  group_by(variable) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         pred.m.rm = rollapply(value.m.month, width=Window,
                               FUN=function(x) mean(x, na.rm=TRUE),
                               partial=TRUE, fill=NA, align="right"))


ggplot() +
  geom_rect(data = data.frame(xmin = 2023 + (6.5)/12, xmax = 2024 + (5.5)/12,
                              ymin = -Inf, ymax = Inf,
                              variable = unique(anomalies$variable)),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = NA, fill = "lightgrey",alpha = 0.5) +
  geom_line(data = anomalies,
            aes(x = year + (month -1/2)/12,
                y = anomaly),
            linewidth = 0.2) +
  geom_line(data = anomalies,
            aes(x = year + (month -1/2)/12,
                y = anomaly.rm)) +
  geom_point(data = anomalies %>%
               filter(year == 2023,month == 11),
            aes(x = year + (month -1/2)/12,
                y = anomaly), color = "red") +
  geom_hline(yintercept = 0,linetype = 2) +
  facet_wrap(~ variable,scales = "free") +
  theme_bw()


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")

all.df.ts.long.anomaly <- all.df.ts.long.month %>%
  ungroup() %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(variable,lat,lon) %>%
  # mutate(slope = coef(lm(value.m ~ time))[2],
  #        intercept = coef(lm(value.m ~ time))[1]) %>%
  # mutate(mean.obs =  as.numeric(intercept + slope *time)) %>%
  mutate(mean.obs = mean(value.m,
                         na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(variable,month,lat,lon) %>%
  mutate(mean.month = mean(detrended,
                           na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(variable,month,lat,lon) %>%
  mutate(anomaly.m = anomaly/sd(anomaly,
                                na.rm = TRUE)) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         value.rm = rollapply(value.m, width=Window,
                               FUN=function(x) mean(x, na.rm=TRUE),
                               partial=TRUE, fill=NA, align="right"))

ggplot(data = all.df.ts.long.anomaly %>%
         filter(year == 2023, month == 10,
                variable == "SIF_743") ) +
  geom_tile(aes(x = lon, y = lat,
                fill = value.m),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkgreen") +

  coord_sf(xlim = c(-85, -35), ylim = c(-25, 10), expand = FALSE) +
  labs(x = "",y = "") +
  theme_map() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

df.test <- all.df.ts.long.anomaly %>%
  ungroup() %>%
  dplyr::select(variable,year,month,lat,lon,anomaly) %>%
  pivot_wider(names_from = variable,
              values_from = anomaly)

ggplot(data = df.test ,
       aes(x = vza,y = NIRv)) +
  geom_bin2d() +
  stat_smooth(method = "lm") +
  theme_bw()


saveRDS(all.df.ts.long.anomaly,
        "./outputs/TROPOSIF.anomalies.RDS")

saveRDS(all.df.ts.long,
        "./outputs/TROPOSIF.compiled.RDS")
