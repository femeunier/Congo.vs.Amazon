rm(list = ls())

A <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/data/GPP/monthly/NIR.GPP.RDS") %>%
  mutate(product = "NIR") %>%
  ungroup()  # Wang et al. 2022

B <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/data/GPP/monthly/SIF.GPP2.RDS") %>%
  mutate(product = "TwoLeaf") %>%
  ungroup()  # Bi et al. 2022

C <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/data/GPP/monthly/SIF.GPP.2023.RDS") %>%
  mutate(product = "SIF") %>%
  mutate(lon = case_when(abs(lon - floor(lon)-0.275) < 1e-6  ~ round(floor(lon) + 0.25,digits = 2),
                         abs(lon - floor(lon) - 0.775) < 1e-6  ~ round(floor(lon) + 0.75,digits = 2),
                         abs(lon - floor(lon)- 0.775) < 1e-6  ~ round(floor(lon) + 0.25,digits = 2),
                         abs(lon - floor(lon)- 0.725) < 1e-6  ~ round(floor(lon) + 0.75,digits = 2),
                         TRUE ~ lon)) %>%
  ungroup()  # Li and Xiao 2019

D <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/data/GPP/monthly/VOD.GPP.RDS") %>%
  mutate(product = "VOD") %>%
  ungroup()  # Wild et al. 2022

mask <- D %>%
  filter(year == year[1],
         month == month[1]) %>%
  dplyr::select(lat,lon) %>%
  mutate(lon.lat = paste0(lon,".",lat))

all <- bind_rows(A,
                 B,
                 C,
                 D) %>%
  filter(abs(lat) <= 25) %>%
  ungroup() %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(lon.lat %in% mask[["lon.lat"]]) %>%
  dplyr::select(-lon.lat)

# Global
ggplot(data = all %>%
         filter(year == 2010,
                value > 0)) +
  geom_density(aes(x = value,
                   fill = product),
               alpha = 0.5) +
  theme_bw()

# Continent
ggplot(data = all %>%
         filter(year == 2010,
                value > 0) %>%
         mutate(continent = case_when(lon <= -20 ~ "America",
                                      lon <= 30 ~ "Africa",
                                      TRUE ~ "Australasia"))) +
  geom_density(aes(x = value,
                   fill = product),
               alpha = 0.5) +
  facet_wrap(~ continent) +
  theme_bw()


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = all %>%
         filter(year == 2010,
                month == 1)) +
  geom_raster(aes(x=lon,y = lat,
                  fill = value),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  # geom_sf(data = Amazon.shp,fill = NA, color = "black") +

  coord_sf(ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient(limits = c(0,10),
                      oob = scales::squish,
                      low = "grey",high = "darkgreen") +
  labs(x = "",y = "") +
  facet_wrap(~ product) +
  theme_map() +
  guides(fill = "none")



ggplot(data = all %>%
         mutate(lat = round(lat)) %>%
         group_by(product,lat) %>%
         summarise(value.m = mean(value,
                                  na.rm = TRUE),
                   .groups = "keep")) +
  geom_line(aes(x = lat,
                y = value.m,
                   color = product)) +
  coord_flip() +
  theme_bw()


ggplot(data = all %>%
         mutate(lon = round(lon)) %>%
         group_by(product,lon) %>%
         summarise(value.m = mean(value,
                                  na.rm = TRUE),
                   .groups = "keep")) +
  geom_line(aes(x = lon,
                y = value.m,
                color = product)) +
  theme_bw()

saveRDS(all,
        "./data/GPP.products.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/data/GPP.products.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/data/
