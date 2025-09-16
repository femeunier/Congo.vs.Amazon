rm(list = ls())

library(rnaturalearth)

# system2("scp",
#         paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/data/monthly.climate.pantropical.Tropics.ECMWF.RDS",
#               "./outputs/"))

era5 <- readRDS("/home/femeunier/Documents/data/monthly.climate.global.ERA5.2022_2024.RDS") %>%
  dplyr::select(-any_of("lon.lat")) %>%
  filter(abs(lat) <= 25)

merged.ECMWF <- readRDS("./outputs/monthly.climate.pantropical.Tropics.ECMWF.RDS")

all.long <- bind_rows(era5 %>%
                        filter(year >= 2022) %>%
                        pivot_longer(cols = -c(lon,lat,year,month),
                                     names_to = "var") %>%
                        mutate(source = "ERA5"),

                      merged.ECMWF %>%
                        pivot_longer(cols = -c(lon,lat,year,month),
                                     names_to = "var") %>%
                        mutate(source = "ECMWF"))

ggplot(data = all.long %>%
         filter(year == 2024,
                month == 5)) +
  geom_density(aes(x = value, fill = source), alpha = 0.5, color = NA) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, color = "darkgrey") +
  geom_raster(data = all.long %>%
                group_by(source) %>%
                filter(year == 2024,
                       month == 5,
                       var == "tmin"),
              aes(x=lon, y = lat, fill = value),
              alpha = 0.3) +
  labs(x = "",y = "") +
  theme_bw() +
  facet_wrap(~ source) +
  # scale_x_continuous(limits = c(-60,-50)) +
  # scale_y_continuous(limits = c(-15,-5)) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 3,
                                        linewidth = 0.3),
        panel.background = element_rect(fill = "white"))



all.long.filt <- all.long

all.long.sum <- all.long.filt %>%
  group_by(source,var,year,month) %>%
  summarise(value.m = mean(value),
            .groups = "keep")


ggplot() +
  geom_sf(data = world, color = "darkgrey") +
  geom_raster(data = all.long.filt %>%
                filter(year == 2024, month == 5, var == "pre"),
              aes(x=lon, y = lat, fill = value),
              alpha = 1) +
  labs(x = "",y = "") +
  theme_bw() +
  facet_wrap(~ source) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 3,
                                        linewidth = 0.3),
        panel.background = element_rect(fill = "white"))

ggplot() +
  geom_line(data = all.long.sum,
            aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = source)) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
