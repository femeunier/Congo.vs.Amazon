rm(list = ls())

# df.Biomass.map <- readRDS("./outputs/df.Biomass.map.RDS")
#
# df.sum <- df.Biomass.map %>%
#   group_by(weighting,period,scenario,lon,lat) %>%
#   summarise(MAP = mean(MAP,na.rm = TRUE),
#             MCWD = mean(MCWD,na.rm = TRUE),
#             AGB = mean(AGB,na.rm = TRUE),
#             Etot = mean(Etot,na.rm = TRUE),
#             .groups = "keep") %>%
#   mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
#   mutate(biome = case_when(MAP > 1700 & AI < -3.8 ~ "Humid_large",
#                            AI < -3.8 ~ "Humid_low",
#                            AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
#                            AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
#                            AI < -0.25 & AI >= -1 ~ "Semiarid",
#                            AI < -0.05 & AI >= -0.25 ~ "Arid",
#                            AI < 0 & AI >= -0.05 ~ "Hyperarid",
#                            TRUE ~ NA_character_)) %>%
#   filter(!is.na(biome)) %>%
#   mutate(biome = as.numeric(factor(biome,
#                                    levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
#                                               "Semiarid","Arid","Hyperarid"))))
#
# saveRDS(df.sum,
#         "./outputs/df.Biomass.map.sum.RDS")

df.sum <- readRDS("./outputs/df.Biomass.map.sum.RDS")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


ggplot() +
  geom_raster(data = df.sum %>%
                filter(scenario != "historical"),
              aes(x = lon, y = lat,
                  fill = as.factor(biome))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_manual(values = c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb")) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(interaction(period,scenario) ~ weighting) +
  theme(legend.position = "none",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "")


ggplot() +
  geom_tile(data = df.sum %>%
              filter(scenario != "historical"),
            aes(x = lon, y = lat,
                fill = MAP)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient(limits = c(1000,2500),low = "white",high = "darkblue",
                      oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(interaction(period,scenario) ~ weighting) +
  theme(legend.position = "right",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "")



ggplot() +
  geom_tile(data = df.sum %>%
              filter(scenario != "historical"),
            aes(x = lon, y = lat,
                fill = MCWD)) +

  geom_sf(data = world,fill = NA, color = "grey") +

  scale_fill_gradient2(limits = c(-800,0),low = "darkred",high = "darkblue",
                       midpoint = -400,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(interaction(period,scenario) ~ weighting) +
  theme(legend.position = "right",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "")


ggplot() +
  geom_tile(data = df.sum,
            aes(x = lon, y = lat,
                fill = Etot)) +

  geom_sf(data = world,fill = NA, color = "grey") +

  scale_fill_gradient(limits = c(0,2000),
                      low = "white",high = "darkred",
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(interaction(period,scenario) ~ weighting) +
  theme(legend.position = "bottom")

gleam <-
  readRDS("/home/femeunier/Documents/data/GLEAM/GLEAM.RDS")


ggplot() +
  geom_tile(data = gleam,
            aes(x = lon, y = lat,
                fill = E.m)) +

  geom_sf(data = world,fill = NA, color = "grey") +

  scale_fill_gradient(limits = c(0,2000),
                      low = "white",high = "darkred",
                      oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom")

test <- df.sum %>%
  ungroup() %>%
  mutate(lat = round(lat,2),
         lon = round(lon,2)) %>%
  filter(scenario == "historical") %>%
  left_join(gleam %>%
              mutate(lat = round(lat,2),
                     lon = round(lon,2)),
            by = c("lon","lat"))

ggplot(data = test,
       aes(x = E.m,y = Etot)) +
  geom_hex() +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, color = "black") +
  theme_bw() +
  facet_wrap(~ weighting)

test %>%
  group_by(weighting) %>%
  summarise(r2 = summary(lm(formula = Etot ~ E.m))[["r.squared"]],
            slope = summary(lm(formula = Etot ~ E.m))[["coefficients"]][2,1],
            pval = summary(lm(formula = Etot ~ E.m))[["coefficients"]][2,4])


################################################################################
difference

df.sum.diff <- df.sum %>%
  pivot_longer(cols = -c(weighting,period,scenario,lon,lat),
               names_to = "variable",
               values_to = "value") %>%
  pivot_wider(names_from = "weighting",
              values_from = "value") %>%
  mutate(diff = m - w)


ggplot() +
  geom_tile(data = df.sum.diff %>%
              filter(variable == "biome"),
            aes(x = lon, y = lat,
                fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",high = "darkblue",mid = "white",
                       limits = c(-3,3),
                      oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~interaction(period,scenario),ncol = 1) +
  theme(legend.position = "right",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "")


ggplot() +
  geom_tile(data = df.sum.diff %>%
              filter(variable == "MAP"),
            aes(x = lon, y = lat,
                fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",high = "darkblue",mid = "white",
                       limits = c(-1,1)*500,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~interaction(period,scenario),ncol = 1) +
  theme(legend.position = "right",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "")


ggplot() +
  geom_tile(data = df.sum.diff %>%
              filter(variable == "MCWD"),
            aes(x = lon, y = lat,
                fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",high = "darkblue",mid = "white",
                       limits = c(-1,1)*1000,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~interaction(period,scenario),ncol = 1) +
  theme(legend.position = "right",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "")


ggplot() +
  geom_tile(data = df.sum.diff %>%
              filter(variable %in% c("MCWD","MAP"),
                     scenario != "historical"),
            aes(x = lon, y = lat,
                fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",high = "darkblue",mid = "white",
                       limits = c(-1,1)*1000,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(interaction(period,scenario) ~ variable) +
  theme(legend.position = "right",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  labs(x = "", y = "")

