rm(list = ls())

library(rnaturalearth)

# system2("scp",
#         paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/data/monthly.climate.pantropical.Tropics.ECMWF.RDS",
#               "./outputs/"))

era5 <- readRDS("/home/femeunier/Documents/data/monthly.climate.global.ERA5.2022_2024.RDS") %>%
  dplyr::select(-any_of("lon.lat")) %>%
  filter(abs(lat) <= 25)


grid <- rasterFromXYZ((era5 %>%
                         filter(year == year[1],
                                month == month[1]) %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","tmp")))[,c("lon","lat","tmp")])

# merged.ECMWF <- readRDS("./outputs/monthly.climate.pantropical.Tropics.ECMWF.RDS")
# new.ECMWF <- resample.df.all.col(bigdf = merged.ECMWF,
#
#                             raster2resample = grid,
#                             var.names = c("tmp","tmin","tmax","dswrf","dlwrf","pre","spfh",'VPD'),
#                             res = NULL)
# saveRDS(new.ECMWF,"./outputs/monthly.climate.pantropical.Tropics.ECMWF.ERA5.RDS")
new.ECMWF <- readRDS("./outputs/monthly.climate.pantropical.Tropics.ECMWF.ERA5.RDS")

all.long <- bind_rows(era5 %>%
                        filter(year >= 2022) %>%
                        pivot_longer(cols = -c(lon,lat,year,month),
                                     names_to = "var") %>%
                        mutate(source = "ERA5"),

                      new.ECMWF %>%
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

coords.ERA5 <- bind_rows(readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Amazon.coord.ILF.ERA5.RDS") %>%
                           dplyr::select(lon,lat) %>%
                           mutate(basin = "Amazon",
                                  is.undisturbed.factor = 1),
                         readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Congo.coord.ILF.ERA5.RDS") %>%
                           dplyr::select(lon,lat) %>%
                           mutate(basin = "Congo",
                                  is.undisturbed.factor = 1))

coords.ERA5 %>%
  group_by(basin) %>%
  summarise(N = n())


ggplot() +
  geom_sf(data = world, color = "darkgrey") +
  geom_raster(data = coords.ERA5,
              aes(x=lon, y = lat, fill = is.undisturbed.factor),
              alpha = 1) +
  labs(x = "",y = "") +
  theme_bw() +
  scale_x_continuous(limits = c(-90,40)) +
  scale_y_continuous(limits = c(-15,10)) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 3,
                                        linewidth = 0.3),
        panel.background = element_rect(fill = "white"))

all.long.filt <- all.long %>%
  left_join(coords.ERA5,
            by = c("lon","lat")) %>%
  filter(is.undisturbed.factor == 1)

all.long.sum <- all.long.filt %>%
  group_by(source,basin,var,year,month) %>%
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
  scale_x_continuous(limits = c(-90,30)) +
  scale_y_continuous(limits = c(-15,10)) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 3,
                                        linewidth = 0.3),
        panel.background = element_rect(fill = "white"))

ggplot() +
  geom_line(data = all.long.sum %>%
              mutate(value.m = case_when(var %in% c("tmp","tmin","tmax") ~ value.m - 273.15,
                                         TRUE ~ value.m)),
            aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = source)) +
  facet_grid(var ~ basin, scales = "free") +
  theme_bw()
