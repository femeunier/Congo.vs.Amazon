rm(list = ls())

library(ggthemes)
library(dplyr)
library(ggplot2)
library(multcompView)

mask <- readRDS("./outputs/mask.vegetation.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(layer == 1)

df.biomass <- readRDS("./outputs/Map.biomass.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(lon.lat %in% mask[["lon.lat"]]) %>%
  mutate(AGB = case_when(source == "Xu" ~ AGB*2,
                         TRUE ~ AGB))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df.biomass,
              aes(x = lon, y = lat,
                  fill = AGB)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient(low = "white",
                      high = "darkgreen",na.value = "lightgrey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_map() +
  facet_wrap(~ source, ncol = 1) +
  theme(legend.position = "bottom")

climate <-
  data <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.ERA5_coord.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  ungroup()

biomass.vs.climate <- df.biomass %>%
  left_join(climate %>%
              dplyr::select(lon.lat,MAT,MAP,MCWD),
            by = c("lon.lat")) %>%
  dplyr::select(-lon.lat) %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
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
  mutate(biome = (factor(biome,
                         levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                    "Semiarid","Arid","Hyperarid"))))
ggplot(data = biomass.vs.climate %>%
         filter(!is.na(AGB))) +
  geom_point(aes(x = MCWD, y = MAP,
                 color = AGB), size = 0.1) +
  scale_color_gradient(low = "white",high = "darkgreen") +
  geom_segment(aes(x = -1700/3.8, xend = 0,
                   y = 1700,yend = 1700), linetype = 2, linewidth = 0.5,
               color = "black") +
  geom_abline(slope = c(-3.8,-1.8,-1,-0.25,-0.05), linetype = 2, linewidth = 0.5,
              color = "black") +
  geom_vline(xintercept = 0,linetype = 2,
             linewidth = 0.5, color = "black") +
  facet_wrap(~ source) +
  theme_bw()

ggplot(data = biomass.vs.climate %>%
         filter(!is.na(AGB))) +
  geom_point(aes(x = MCWD, y = MAP,
                 color = AGB), size = 0.1) +
  scale_color_gradient(low = "white",high = "darkgreen") +
  geom_segment(aes(x = -1700/3.8, xend = 0,
                   y = 1700,yend = 1700), linetype = 2, linewidth = 0.5,
               color = "black") +
  geom_abline(slope = c(-3.8,-1.8,-1,-0.25,-0.05),
              linetype = 2, linewidth = 0.5,
              color = "black") +
  geom_vline(xintercept = 0,linetype = 2,
             linewidth = 0.5, color = "black") +
  facet_grid(continent ~ source) +
  theme_bw()

ggplot(data = biomass.vs.climate) +
  geom_boxplot(aes(x = source, y = AGB/20, fill = biome)) +
  facet_wrap(~ continent) +
  theme_bw()

ggplot(data = biomass.vs.climate) +
  geom_boxplot(aes(x = source, y = AGB/20, fill = biome)) +
  theme_bw()

df2plot <- biomass.vs.climate %>%
  group_by(lon,lat,biome) %>%
  summarise(AGB = mean(AGB,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df2plot) +
  geom_boxplot(aes(x = biome, y = AGB/20, fill = biome),
               alpha = 0.7) +
  scale_fill_manual(values = c("#253b10","#005401","#448704","#86a540",
                                "#c49402","#d0ce9a","#e5e4cb")) +
  theme_bw() +
  # scale_y_log10() +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 20)) +
  guides(fill = "none")

LM <- (lm(data = df2plot,
    formula = (AGB) ~ biome))
summary(LM)

agb_aov <- aov(AGB ~ biome, data = df2plot)
summary(agb_aov)

Tukey.agb <- TukeyHSD(agb_aov)
letters_agb <- multcompLetters4(agb_aov, Tukey.agb)
print(letters_agb)



biomass.vs.climate.sum <- biomass.vs.climate %>%
  group_by(biome,continent,source) %>%
  summarise(AGB = mean(AGB/20,na.rm = TRUE),
            .groups = "keep")

biomass.vs.climate.sum.tropics <- biomass.vs.climate %>%
  group_by(biome,source) %>%
  summarise(AGB = mean(AGB/20,na.rm = TRUE),
            .groups = "keep")

saveRDS(biomass.vs.climate.sum,
        "./outputs/Biomass.per.biome.RDS")

saveRDS(biomass.vs.climate.sum.tropics,
        "./outputs/Biomass.per.biome.Tropics.RDS")

saveRDS(biomass.vs.climate %>%
          mutate(AGB = AGB/20) %>%
          dplyr::select(source,continent,biome,AGB) %>%
          arrange(source,continent,biome,desc(AGB)),
        "./outputs/Biomass.per.biome.all.RDS")

saveRDS(biomass.vs.climate,
        "./outputs/Biomass.per.biome.all.coord.RDS")


saveRDS(biomass.vs.climate.sum.tropics %>%
          group_by(biome) %>%
          summarise(AGB.m = mean(AGB, na.rm = TRUE),
                    .groups = "keep"),
        "./outputs/Biomass.per.biome.Tropics.avg.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Biomass.per.biome.Tropics.avg.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
