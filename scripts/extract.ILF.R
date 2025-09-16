rm(list = ls())

library(sf)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(rgeos)

IFL <- read_sf(dsn = "/home/femeunier/Documents/projects/Congo.ED2/data/IFL/",
                      layer = "ifl_2020")
IFL$type = 1
N = 1
r <- raster(ncol=7200*N, nrow=3600*N)
extent(r) <- extent(-180,
                    180,
                    -90,90)

rp <- rasterize(IFL, r,
                'type',
                fun = mean)
plot(rp)

rp.df <- as.data.frame(rp,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         is.undisturbed = layer) %>%
  mutate(is.undisturbed.factor = case_when(is.na(is.undisturbed) ~ 0,
                                           is.undisturbed < 0.5 ~ 0,
                                           is.undisturbed >= 0.5 ~1))

saveRDS(rp.df,
        "./outputs/ILF2020.small.df")

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")
Congo <- as_Spatial(Congo.shp)


ILF.df <- rp.df %>%
  filter(is.undisturbed.factor == 1)

sp <- SpatialPoints((ILF.df %>%
                       filter(lon >= -95,lon <= -30,
                              abs(lat) <= 23.15))[,c("lon","lat")])
sp2 <- SpatialPoints((ILF.df %>%
                        filter(lon >= -30,lon <= 60,
                               abs(lat) <= 23.15))[,c("lon","lat")])

out <- as.data.frame(gIntersection(sp,Amazon))
out2 <- as.data.frame(gIntersection(sp2,Congo))

Amazon.coord <- data.frame(basin = "Amazon",
                           lon = round(out$x,digits = 3),
                           lat = round(out$y,digits = 3)) %>%
  mutate(lon.lat =
           paste0(lon,".",lat))

Congo.coord <- data.frame(basin = "Congo",
                          lon = round(out2$x,digits = 3),
                          lat = round(out2$y,digits = 3)) %>%
  mutate(lon.lat =
           paste0(lon,".",lat))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = bind_rows(Amazon.coord,
                        Congo.coord)) +
  geom_point(aes(x = lon, y = lat,
                 color = basin),
             size = 0.1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "red") +
  geom_sf(data = Congo.shp,fill = NA, color = "blue") +
  scale_x_continuous(limits = c(-90,-35)) +
  scale_y_continuous(limits = c(-10,0)) +
  theme_bw()

saveRDS(Amazon.coord,
        "./outputs/Amazon.IFL.coord.small.RDS")
saveRDS(Congo.coord,
        "./outputs/Congo.IFL.coord.small.RDS")

system2("rsync",
        c("-avz",
          "./outputs/Amazon.IFL.coord.small.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
system2("rsync",
        c("-avz",
          "./outputs/Congo.IFL.coord.small.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

