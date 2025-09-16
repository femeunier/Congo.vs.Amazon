rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(raster)
library(TrENDY.analyses)

biomes <- readRDS("./outputs/biome.ERA5.1940.2023_global.RDS") %>%
  filter(model == "ORCHIDEE")


ncfile <- "~/Downloads/GMTED2010_15n120_0500deg.nc"
nc <- nc_open(ncfile)
lats <- ncvar_get(nc,"latitude")
lons <- ncvar_get(nc,"longitude")
altitude <- ncvar_get(nc,"elevation")
df <- melt(altitude) %>%
  mutate(lon = lons[Var1],
         lat = lats[Var2],
         z = value) %>%
  dplyr::select(lon,lat,z)


grid <- rasterFromXYZ((biomes %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","MAT")))[,c("lon","lat","MAT")])

df.rspld <- resample.df.all.col(bigdf = df %>%
                                  mutate(model = "alt"),
                                raster2resample = grid,
                                var.names = c("z"),
                                res = 0.00001,
                                verbose = FALSE) %>%
  dplyr::select(lon,lat,z)

df.rspld$z[is.na(df.rspld$z)] <- 0

ggplot(data = df.rspld) +
  geom_raster(aes(x = lon,
                  y = lat,
                  fill = z)) +
  theme_bw()

saveRDS(df.rspld,
        "./data/Altitude.map.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/data/Altitude.map.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/data
