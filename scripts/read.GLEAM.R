rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(raster)

years <- c(1980:2009)

all.df <- data.frame()
for (cyear in years){

  print(cyear)

  ncfile <- paste0("/home/femeunier/Documents/data/GLEAM/Ep_",cyear,"_GLEAM_v4.1a_YR.nc")
  nc <- nc_open(ncfile)
  E <- ncvar_get(nc,"Ep")
  lats <- ncvar_get(nc,"lat")
  lons <- ncvar_get(nc,"lon")

  df.E <- melt(E) %>%
    mutate(lon = lons[Var1],
           lat = lats[Var2]) %>%
    rename(E = value) %>%
    dplyr::select(lon,lat,E)

  cr <- rasterFromXYZ(df.E)
  cr.coarse <- aggregate(cr,5)
  df.E.coarse <- as.data.frame(cr.coarse,
                xy = TRUE) %>%
    rename(lon = x,
           lat = y)

  nc_close(nc)

  all.df <- bind_rows(all.df,
                      df.E.coarse %>%
                        filter(!is.na(E)) %>%
                        mutate(year = cyear) %>%
                        filter(abs(lat) <= 23.25))
}

all.df.sum <- all.df %>%
  group_by(lat,lon) %>%
  summarise(E.m = mean(E),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = all.df.sum,
              aes(x = lon, y = lat,
                  fill = E.m)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(limits = c(1000,2000),
                       low = 'darkblue',high = "darkred",midpoint = 1500,
                       oob = scales::squish) +
  theme_bw() +
  theme(legend.position = "bottom")

hist(all.df.sum$E.m)

saveRDS(all.df.sum,
        "/home/femeunier/Documents/data/GLEAM/GLEAM.RDS")

all.df.sum <- readRDS( "/home/femeunier/Documents/data/GLEAM/GLEAM.RDS")
