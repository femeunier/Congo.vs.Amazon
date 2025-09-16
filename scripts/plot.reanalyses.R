rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/ED_common_data/met/Precip.Tropics/Pantropical.climate.recent.seasonal.rspld.RDS",
          "./outputs/"))

Clim.Mask <- readRDS("./outputs/Pantropical.climate.recent.seasonal.rspld.RDS")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

products <- unique(Clim.Mask$model)
product.na.tas <- Clim.Mask %>% ungroup() %>%
  filter(is.na(tas)) %>% pull(model) %>% unique()
product.w.everything <- products[!(products %in% product.na.tas)]

Clim.Mask.filled <- data.frame(Clim.Mask %>%
                                 filter(model %in%
                                          product.w.everything))
Clim.Mask.filled.m <- Clim.Mask.filled %>%
  group_by(lon,lat,month) %>%
  summarise(tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            .groups = "keep")

for (cproduct in product.na.tas){

  print(cproduct)

  cdf <- Clim.Mask %>%
    filter(model == cproduct)

  Clim.Mask.filled <- bind_rows(Clim.Mask.filled,
                                cdf %>%
                                  dplyr::select(-c(tas,tasmin,tasmax)) %>%
                                  left_join(Clim.Mask.filled.m,
                                            by = c("lon","lat","month")))
}


Clim.Mask.sum <-
  Clim.Mask.filled %>%
  group_by(lon,lat,model) %>%
  summarise(MAP = sum(pre,na.rm = TRUE),
            tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_raster(data = Clim.Mask.sum,
              aes(x = lon, y = lat,
                  fill = MAP)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ model) +
  scale_fill_gradient(low = "white", high = "darkblue",
                      limits = c(0,2000),oob = scales::squish) +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggplot() +
  geom_raster(data = Clim.Mask.sum,
              aes(x = lon, y = lat,
                  fill = tas)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ model) +
  scale_fill_gradient2(low = "darkblue",mid = "white", high = "darkred",
                       midpoint = 25+273.15,
                       limits = c(20,30)+273.15,
                       oob = scales::squish) +
  theme(legend.position = "bottom",
        text = element_text(size = 20))


Clim.Mask.sum.region <- Clim.Mask.filled %>%
  group_by(month,model) %>%
  summarise(pre.m = mean(pre,na.rm = TRUE),
            tas.m = mean(tas,na.rm = TRUE),
            tasmin.m = mean(tasmin,na.rm = TRUE),
            tasmax.m = mean(tasmax,na.rm = TRUE),
            .groups = "keep")


ggplot(data = Clim.Mask.sum.region) +
  geom_line(aes(x = month, y =  pre.m,
                color = model)) +
  theme_bw()

ggplot(data = Clim.Mask.sum.region %>%
         filter(!is.na(tas.m))) +
  geom_line(aes(x = month, y = tas.m - 273.15, color = model)) +
  geom_line(aes(x = month, y = tasmin.m - 273.15, color = model),
            linetype = 2) +
  geom_line(aes(x = month, y = tasmax.m - 273.15, color = model),
            linetype = 3) +
  # scale_y_continuous(limits = c(0,40)) +
  facet_wrap(~model) +
  theme_bw()


