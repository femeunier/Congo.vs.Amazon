rm(list = ls())

library(dplyr)
library(caret)

model <- readRDS("./outputs/df.Biomass.map.RDS") %>%
  filter(period == 2000,
         scenario == "historical")

model.biome <- model %>%
  mutate(biome.num = as.numeric(factor(biome,
                                       levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                                  "Semiarid","Arid","Hyperarid"))))

data <- readRDS("./outputs/Data.used.RDS") %>%
  dplyr::select(lon,lat,biome)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")



data.vs.model <- model.biome %>%
  dplyr::select(weighting,iter,lon,lat,biome.num) %>%
  left_join(data,
            by = c("lon","lat")) %>%
  ungroup() %>%
  mutate(biome = factor(biome,
                        level = 1:7),
         biome.num = factor(biome.num,
                            level = 1:7))

df.CM <- data.vs.model %>%
  group_by(weighting,iter) %>%
  summarise(acc = (confusionMatrix(biome.num,
                                  biome))[["overall"]][1],
            rS = rowSums(as.matrix(confusionMatrix(biome.num,
                                                   biome)))[1],
            rC = colSums(as.matrix(confusionMatrix(biome.num,
                                                   biome)))[1],
            precision = mean(as.matrix((confusionMatrix(biome.num,
                                                        biome))[["byClass"]])[,"Precision"]),
            .groups = "keep")

df.CM.sum <- df.CM %>%
  group_by(weighting) %>%
  summarise(acc.m = mean(acc),
            rS.m = mean(rS),
            precision.m = mean(precision),
            .groups = "keep")

data.vs.model.m <- data.vs.model %>%
  group_by(weighting,lon,lat) %>%
  mutate(diff = as.numeric(biome)- as.numeric(biome.num)) %>%
  summarise(diff.m = mean(diff,na.rm = TRUE),
            .groups = "keep")


ggplot() +
  geom_raster(data = data.vs.model.m,
              aes(x = lon, y = lat,
                  fill = diff.m)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue",
                       breaks = c(-4,-2,0,2,4)) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  labs(x = "",y = "") +
  facet_wrap(~ weighting, nrow = 2) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20))


