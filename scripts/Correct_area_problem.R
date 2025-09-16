rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(raster)
library(ggplot2)

nc <- nc_open("/home/femeunier/Downloads/raw_data_land_area_land_area_0.50x0.50.nc")
land.frac <- ncvar_get(nc,"data")
lats <- ncvar_get(nc,"latitude")
lons <- ncvar_get(nc,"longitude")
nc_close(nc)

df <- melt(land.frac) %>%
  mutate(lon = lons[Var1],
         lat = lats[Var2]) %>%
  dplyr::select(lon,lat,value) %>%
  filter(value != -99)

hist(df$value)

df %>%
  filter(lat <= 10, lat >= -15,
         lon >= -15, lon <= 60) %>%
  summarise(tot = sum(value,na.rm = TRUE)/1000/1000)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df,
              aes(x = lon,
                  y = lat,
                  fill = value)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  theme_bw()

summary(as.vector(land.frac))

e <- extent(-15,60,-15,10)
str_name<-'./data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tif'

imported_raster=terra::rast(crop(raster(str_name),e))
imported_raster[imported_raster == 210] <- NA
ter <- terra::cellSize(imported_raster)
area_masked <- mask(ter, imported_raster)
plot(area_masked)

total_area <- terra::global(area_masked, fun = "sum", na.rm = TRUE)
mean_area <- terra::global(area_masked, fun = "mean", na.rm = TRUE)
total_area$sum/1e12
df.raster <- as.data.frame(imported_raster,
                           xy = TRUE) %>%
  rename(lon = x,
         lat = y)
