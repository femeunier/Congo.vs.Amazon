rm(list = ls())

library(sf)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(rgeos)
library(terra)

IFL <- read_sf(dsn = "/home/femeunier/Documents/projects/Congo.ED2/data/IFL/",
               layer = "ifl_2020")
IFL$type = 1

coord <- expand.grid(lon = seq(-179.75,179.75,0.5),
                     lat = seq(-30.25,30.25,0.5)) %>%
  mutate(value = 1)

r <- rasterFromXYZ(coord)

rp <- rasterize(IFL, r,
                'type',
                fun = mean)

rp.df <- as.data.frame(rp,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         is.undisturbed = layer) %>%
  mutate(is.undisturbed.factor = case_when(is.na(is.undisturbed) ~ 0,
                                           is.undisturbed < 0.5 ~ 0,
                                           is.undisturbed >= 0.5 ~1))

saveRDS(rp.df,
        "./outputs/Intact.Forests.RDS")


s <- sprc(max(rast("/home/femeunier/Documents/projects/YGB/data/Fluxcom/FLUXCOM_SEA_GPP_month.grd")),
          max(rast("/home/femeunier/Documents/projects/YGB/data/Fluxcom/FLUXCOM_AFR_GPP_month.grd")),
          max(rast("/home/femeunier/Documents/projects/YGB/data/Fluxcom/FLUXCOM_SAM_GPP_month.grd")))
m <- merge(s)
m[m < 4] <- NA
plot(m)
hist(m)

ILF <- rp.df %>%
  filter(is.undisturbed.factor == 1) %>%
  dplyr::select(lon,lat,is.undisturbed.factor) %>%
  rename(IFL = is.undisturbed.factor)

Tropical.Forests <- as.data.frame(m,
                                  xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  left_join(ILF,
            by = c("lon","lat")) %>%
  mutate(IFL = case_when(IFL == 1 ~ IFL,
                         TRUE ~ 0)) %>%
  filter(abs(lat) <= 23.25)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = Tropical.Forests ,
              aes(x = lon, y = lat,
                  fill = as.factor(IFL))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

saveRDS(Tropical.Forests %>%
          dplyr::select(lon,lat,IFL),
        "./outputs/Tropical.Forests.mask.RDS")
