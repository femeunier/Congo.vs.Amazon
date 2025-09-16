rm(list = ls())

library(caret)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)


coord <- expand.grid(lat = seq(-23.25,23.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))


# data <- readRDS("./outputs/CRU")
data <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.CRU_coord.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  dplyr::select(lon,lat,MAP,MAT,MCWD) %>%
  mutate(MAT = MAT - 273.15) %>%
  mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
  mutate(biome = case_when(MAP > 1700 & AI < -3.8 ~ "Humid_large",
                           AI < -3.8 ~ "Humid_low",
                           AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
                           AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
                           AI < -0.25 & AI >= -1 ~ "Semiarid",
                           AI < -0.05 & AI >= -0.25 ~ "Arid",
                           AI < 0 & AI >= -0.05 ~ "Hyperarid",
                           TRUE ~ NA_character_)) %>%
  filter(!is.na(biome)) %>%
  mutate(biome = as.numeric(factor(biome,
                                   levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                              "Semiarid","Arid","Hyperarid"))))


data.ERA5 <- readRDS("./outputs/Data.used.RDS")

differences <- readRDS("./outputs/Diff.averages.RDS") %>%
  filter(variable == "biome.num")

differences.changes <- differences %>%
  left_join(data %>%
              dplyr::select(lon,lat,biome),
            by = c("lon","lat")) %>%
  mutate(diff = biome - mod)

LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                  lat = as.vector(unique(coord$lat))) %>%
  melt() %>%
  mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
         Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
  rename(lon = Var1,
         lat = Var2,
         area = value) %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100) %>%
  left_join(LandFrac,
            by = c("lat","lon")) %>%
  mutate(area = area*value)


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = data,
              aes(x = lon, y = lat,
                  fill = as.factor(biome))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_manual(values = c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb"),
                    labels = c("Humid (MAP > 1700)",
                               "Humid (MAP < 1700)",
                               "Humid seasonal",
                               "Dry subhumid",
                               "Semiarid",
                               "Arid",
                               "Hyperarid")) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  labs(x = "",y = "",
       fill = "Aridity index") +
  theme(legend.position = "none",
        text = element_text(size = 20))


ggplot() +
  geom_raster(data = differences ,
              aes(x = lon, y = lat,
                  fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue",
                       breaks = c(-4,-2,0,2,4)) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  labs(x = "",y = "") +
  facet_wrap(~ source, nrow = 2) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20))


ERA5.vs.CRUJRA <- data %>%
  dplyr::select(lon,lat,biome) %>%
  left_join(data.ERA5 %>%
              dplyr::select(lon,lat,biome) %>%
              rename(biome.ERA5 = biome),
            by = c("lon","lat")) %>%
  mutate(diff = biome - biome.ERA5)

ggplot() +
  geom_raster(data = ERA5.vs.CRUJRA ,
              aes(x = lon, y = lat,
                  fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue",
                       breaks = c(-4,-2,0,2,4)) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  labs(x = "",y = "") +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20))

CM <- confusionMatrix(factor(ERA5.vs.CRUJRA %>%
                               pull(biome),
                             levels = 1:7),
                      factor(ERA5.vs.CRUJRA %>%
                               pull(biome.ERA5),
                             levels = 1:7),
                      mode = "everything")
precision <- mean(as.matrix(CM$byClass)[,"Precision"])


ERA5.vs.CRUJRA %>%
  mutate(biome = factor(biome, levels = 1:7),
         biome.ERA5 = factor(biome.ERA5, levels = 1:7)) %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  group_by(continent) %>%
  summarise(Acc = (confusionMatrix(biome,biome.ERA5))[["overall"]][1],
            precision = mean(as.matrix(confusionMatrix(biome,biome.ERA5)[["byClass"]])[,"Precision"],
                             na.rm = TRUE))


differences %>%
  filter(variable == "biome.num") %>%
  mutate(mod = factor(mod, levels = 1:7),
         obs = factor(obs, levels = 1:7)) %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  group_by(continent,source) %>%
  summarise(Acc = (confusionMatrix(mod,obs))[["overall"]][1],
            precision = mean(as.matrix(confusionMatrix(mod,obs)[["byClass"]])[,"Precision"],
                             na.rm = TRUE))


CM <- confusionMatrix(factor(differences %>%
                               filter(variable == "biome.num",
                                      source == "Weighted.mean") %>%
                               pull(mod),
                             levels = 1:7),
                      factor(differences %>%
                               filter(variable == "biome.num",
                                      source == "Weighted.mean") %>%
                               pull(obs),
                             levels = 1:7),
                      mode = "everything")

precision <- mean(as.matrix(CM$byClass)[,"Precision"])


CM0 <- confusionMatrix(factor(differences %>%
                               filter(variable == "biome.num",
                                      source == "Mean") %>%
                               pull(mod),
                             levels = 1:7),
                      factor(differences %>%
                               filter(variable == "biome.num",
                                      source == "Mean") %>%
                               pull(obs),
                             levels = 1:7),
                      mode = "everything")

precision0 <- mean(as.matrix(CM0$byClass)[,"Precision"])
