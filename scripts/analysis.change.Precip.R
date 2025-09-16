rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)

A <- readRDS("./outputs/df.Biomass.map.RDS")
models <- unique(A$model)

CMIP6.files <- list.files("./outputs","*CMIP6.classifications*",
                          full.names = TRUE)
CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]
CMIP6.files <- CMIP6.files[grepl(paste(paste0(models,".RDS"), collapse = "|"),
                                 CMIP6.files)]

df.all <- data.frame()
for (file in CMIP6.files){

  print(file)
  temp <- readRDS(file)

  CMIP6 <- bind_rows(temp %>%
                       filter(scenario == "historical",
                              period == 2000),
                     temp %>%
                       filter(scenario != "historical",
                              period == 2090)) %>%
    ungroup()


  df.all <- bind_rows(df.all,
                      CMIP6)
}

df.all.selected <- df.all %>%
  dplyr::select(period,scenario,model,lon,lat,MAP,MCWD)

Biomass.vs.climate <- A %>%
  left_join(df.all.selected,
            by = c("period","model","scenario","lon","lat"))


# Biomass.vs.climate %>%
#   ungroup() %>%
#   filter(period == 2000) %>%
#   filter(weighting == "w") %>%
#   pull(model) %>% table()


Biomass.vs.climate.m <- Biomass.vs.climate %>%
  group_by(lon,lat,scenario,weighting,period) %>%
  summarise(MAP.m = mean(MAP),
            MCWD.m = mean(MCWD),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ref <- Biomass.vs.climate.m %>%
  filter(period == 2000)
Biomass.vs.climate.m.wide <- ref %>%
  pivot_wider(names_from = "weighting",
              values_from = c("MAP.m","MCWD.m"))


ggplot() +
  geom_raster(data = Biomass.vs.climate.m.wide,
              aes(x = lon, y = lat,
                  fill = MAP.m_w - MAP.m_m)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkblue",
                       limits = c(-500,500),
                       breaks = c(-500,0,500),
                       oob = scales::squish) +
  theme_bw() +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))

hist(Biomass.vs.climate.m.wide$MAP.m_w - Biomass.vs.climate.m.wide$MAP.m_m)

ggplot() +
  geom_raster(data = Biomass.vs.climate.m.wide,
              aes(x = lon, y = lat,
                  fill = MCWD.m_w - MCWD.m_m)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkblue",
                       limits = c(-200,200),
                       breaks = c(-200,0,200),
                      oob = scales::squish) +
  theme_bw() +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))

hist(Biomass.vs.climate.m.wide$MCWD.m_w - Biomass.vs.climate.m.wide$MCWD.m_m)

# future.vs.ref <-
#   Biomass.vs.climate %>%
#   ungroup() %>%
#   filter(period != 2000) %>%
#   dplyr::select(period,scenario,iter,weighting,lon,lat,MAP,MCWD) %>%
#   left_join(Biomass.vs.climate %>%
#               ungroup() %>%
#               rename(MAP.ref = MAP,
#                      MCWD.ref = MCWD) %>%
#               dplyr::select(weighting,iter,lon,lat,MAP.ref,MCWD.ref),
#             by = c("weighting","iter","lon","lat")
#             )
#
# future.vs.ref.m <- future.vs.ref %>%
#   group_by(period,scenario,weighting,lon,lat) %>%
#   summarise(MAP.m = mean(MAP,na.rm = TRUE),
#             MCWD.m = mean(MCWD,na.rm = TRUE),
#             MAP.ref.m = mean(MAP.ref,na.rm = TRUE),
#             MCWD.ref.m = mean(MCWD.ref,na.rm = TRUE),
#             .groups = "keep") %>%
#   mutate(diff.MAP.m = MAP.ref.m - MAP.m,
#          diff.MCWD.m = MCWD.ref.m - MCWD.m)

future.vs.ref.m <- Biomass.vs.climate.m %>%
  filter(period != 2000) %>%
  left_join(Biomass.vs.climate.m %>%

              mutate(AI = MAP.m/pmin(-1e-6,MCWD.m)) %>%
              mutate(biome = case_when(MAP.m > 1700 & AI < -3.8 ~ "Humid_large",
                                       AI < -3.8 ~ "Humid_low",
                                       AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
                                       AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
                                       AI < -0.25 & AI >= -1 ~ "Semiarid",
                                       AI < -0.05 & AI >= -0.25 ~ "Arid",
                                       AI < 0 & AI >= -0.05 ~ "Hyperarid",
                                       TRUE ~ NA_character_)) %>%
              filter(!is.na(biome)) %>%
              mutate(biome = (factor(biome,
                                     levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                                          "Semiarid","Arid","Hyperarid")))) %>%
              ungroup() %>%
              filter(period == 2000) %>%
              dplyr::select(-c(period,scenario)) %>%
              rename(MAP.ref.m = MAP.m,
                     MCWD.ref.m = MCWD.m),
            by = c("lon","lat","weighting"))

future.vs.ref.m.diff <- future.vs.ref.m %>%
  mutate(diff.MAP = MAP.m - MAP.ref.m,
         diff.MCWD = MCWD.m - MCWD.ref.m)

ggplot() +
  geom_raster(data = future.vs.ref.m.diff,
              aes(x = lon, y = lat,
                  fill = diff.MAP)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkblue",
                       limits = c(-1,1)*500,
                       breaks = c(-1,0,1)*500,
                       oob = scales::squish) +
  facet_grid(scenario ~ weighting) +
  theme_bw() +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))

hist(future.vs.ref.m.diff$diff.MAP)

ggplot() +
  geom_raster(data = future.vs.ref.m.diff,
              aes(x = lon, y = lat,
                  fill = diff.MCWD)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkblue",
                       limits = c(-1,1)*500,
                       breaks = c(-1,0,1)*500,
                       oob = scales::squish) +
  facet_grid(scenario ~ weighting) +
  theme_bw() +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))

hist(future.vs.ref.m.diff$diff.MCWD)

future.vs.ref.m.diff %>%
  filter(lon <= -30) %>%
  group_by(biome,scenario,weighting) %>%
  summarise(diff.MAP.m = mean(diff.MAP),
            diff.MCWD.m = mean(diff.MCWD),
            .groups = "keep") %>%
  filter(biome == "Humid_large")
