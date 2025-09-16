rm(list = ls())

library(terra)

# https://www.wur.nl/en/research-results/chair-groups/environmental-sciences/laboratory-of-geo-information-science-and-remote-sensing/research/integrated-land-monitoring/forest_biomass.htm?utm_source=chatgpt.com

r <- rast("/home/femeunier/Downloads/Avitabile_AGB_Map/Avitabile_AGB_Map.tif")
plot(r)

coord <- expand.grid(lon = seq(-179.75,179.75,0.5),
                     lat = seq(-30.25,30.25,0.5)) %>%
  mutate(value = 1)

raster <- rast(rasterFromXYZ(coord))

r.rspld <- resample(r,raster,method = "bilinear")

df.r.rspld <- as.data.frame(r.rspld,
                            xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         AGB = Avitabile_AGB_Map) %>%
  mutate(AGB = AGB/10*0.47)

saveRDS(df.r.rspld,
        "./outputs/AGB.maps.RDS")
