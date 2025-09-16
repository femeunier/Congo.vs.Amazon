rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(raster)

years <- c(2010)

# all.df <- data.frame()
all.df <- readRDS("/home/femeunier/Documents/data/GLEAM/GLEAM.pantropical.months.RDS")
for (cyear in years){

  print(cyear)

  ncfile <- paste0("/home/femeunier/Documents/data/GLEAM/Ep_",cyear,"_GLEAM_v4.2a_MO.nc")

  if(!file.exists(ncfile)){
    next()
  }

  nc <- nc_open(ncfile)
  E <- ncvar_get(nc,"Ep")
  lats <- ncvar_get(nc,"lat")
  lons <- ncvar_get(nc,"lon")

  df.E <- melt(E) %>%
    mutate(lon = lons[Var1],
           lat = lats[Var2]) %>%
    rename(E = value,
           month = Var3) %>%
    dplyr::select(lon,lat,month,E)

  df.E.coarse <- data.frame()
  for (imonth in seq(1,12)){

    print(paste("-",imonth))

    cr <- rasterFromXYZ(df.E %>%
                          filter(month == imonth))
    cr.coarse <- aggregate(cr,5)
    df.E.coarse <- bind_rows(df.E.coarse,
                             as.data.frame(cr.coarse,
                                 xy = TRUE) %>%
      rename(lon = x,
             lat = y) %>%
      mutate(month = imonth))
  }


  nc_close(nc)

  all.df <- bind_rows(all.df,
                      df.E.coarse %>%
                        filter(!is.na(E)) %>%
                        mutate(year = cyear) %>%
                        filter(abs(lat) <= 23.25))
}

all.df.sum <- all.df %>%
  mutate(lon = round(lon,digits = 2),
         lat = round(lat,digits = 2)) %>%
  group_by(lat,lon,year) %>%
  summarise(E = sum(E),
            .groups = "keep") %>%
  group_by(lon,lat) %>%
  summarise(E.m = mean(E,na.rm = TRUE),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

all.df.sum %>%
  ungroup() %>%
  filter(lon == 25.25)

ggplot() +
  geom_raster(data = all.df.sum %>%
                ungroup() ,
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

saveRDS(all.df %>%
          mutate(lon = round(lon,digits = 2),
                 lat = round(lat,digits = 2),
                 month = as.integer(month)) %>%
          filter(year %in% 1981:2010),
        "/home/femeunier/Documents/data/GLEAM/GLEAM.pantropical.months.RDS")
