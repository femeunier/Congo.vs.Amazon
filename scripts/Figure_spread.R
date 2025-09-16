rm(list = ls())

df.Biomass.map <- readRDS("./outputs/df.Biomass.map.RDS")

df.sum <- df.Biomass.map %>%
  ungroup() %>%
  filter(weighting == "m") %>%
    mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
    mutate(biome = case_when(MAP > 1700 & AI < -3.8 ~ "Humid_large",
                             AI < -3.8 ~ "Humid_low",
                             AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
                             AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
                             AI < -0.25 & AI >= -1 ~ "Semiarid",
                             AI < -0.05 & AI >= -0.25 ~ "Arid",
                             AI < 0 & AI >= -0.05 ~ "Hyperarid",
                             TRUE ~ NA_character_)) %>%
  mutate(biome.num = as.numeric(as.factor(biome))) %>%
  group_by(weighting,period,scenario,lon,lat) %>%
  summarise(biome = max(biome.num,na.rm = TRUE) - min(biome.num,na.rm = TRUE),
            MAP = max(MAP,na.rm = TRUE) - min(MAP,na.rm = TRUE),
            MCWD = max(MCWD,na.rm = TRUE) - min(MCWD,na.rm = TRUE),
            AGB = max(AGB,na.rm = TRUE) - min(AGB,na.rm = TRUE),
            Etot = max(Etot,na.rm = TRUE) - min(Etot,na.rm = TRUE),
            .groups = "keep")

# saveRDS(df.sum,
#         "./outputs/df.Biomass.map.spread.RDS")
#

# df.sum <- readRDS("./outputs/df.Biomass.map.spread.RDS")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df.sum %>%
                filter(scenario == "historical"),
              aes(x = lon, y = lat,
                  fill = MAP)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  scale_fill_gradient2(limits = c(0,2000),low = "darkred",high = "darkblue",
                       midpoint = 1000,
                       oob = scales::squish) +
  labs(x = "", y = "", fill = "")


ggplot() +
  geom_tile(data = df.sum %>%
              filter(scenario == "historical"),
            aes(x = lon, y = lat,
                fill = MCWD)) +

  geom_sf(data = world,fill = NA, color = "grey") +

  scale_fill_gradient2(limits = c(0,2000),low = "darkred",high = "darkblue",
                       midpoint = 1000,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(legend.position = "right",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "", fill = "")

ggplot() +
  geom_tile(data = df.sum %>%
              filter(scenario == "historical"),
            aes(x = lon, y = lat,
                fill = biome)) +

  geom_sf(data = world,fill = NA, color = "grey") +

  scale_fill_gradient2(limits = c(0,6),low = "darkred",high = "darkblue",
                       midpoint = 3,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(legend.position = "right",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "", fill = "")
