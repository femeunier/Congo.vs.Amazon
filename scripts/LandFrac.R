rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(raster)

ncfile <- "/home/femeunier/Downloads/TMPA_mask.nc"
nc <- nc_open(ncfile)
lons <- ncvar_get(nc,"lon")
lats <- ncvar_get(nc,"lat")
frac <- melt(ncvar_get(nc,"landseamask")) %>%
  rename(lon = Var1,
         lat = Var2) %>%
  mutate(lon = lons[lon],
         lat = lats[lat]) %>%
  filter(!is.na(value))
nc_close(nc)

r <- rasterFromXYZ(frac)
r.agg <- aggregate(r,2)
as.data.frame(r.agg,xy = TRUE)

coord <- expand.grid(lon = seq(-179.25,179.25,0.1),
                     lat = seq(-89.25,89.25,0.1)) %>%
  mutate(lon.lat = 1)
r.coord <- rasterFromXYZ(coord)
r.agg.rspld <- resample(r.agg,r.coord)

df <- as.data.frame(r.agg.rspld,xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  mutate(value = 1-(pmax(0,pmin(value,100))/100))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df,
              aes(x = lon, y = lat,
                  fill = value)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw()

saveRDS(df,
        "./outputs/landFrac.small.RDS")
