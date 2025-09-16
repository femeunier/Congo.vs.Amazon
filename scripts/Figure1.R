rm(list = ls())

library(caret)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

data <- readRDS("./outputs/Data.used.RDS")
differences <- readRDS("./outputs/Diff.averages.RDS")

coord <- expand.grid(lat = seq(-30.25,30.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

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


######################################################################
# Areas
data %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  ungroup() %>%
  summarise(tot.area = sum(area)/1e6/1e6)

data %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  group_by(biome) %>%
  summarise(tot.area = sum(area)/1e6/1e6)


frac.per.biome.continent <- data %>%
  ungroup() %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  group_by(continent) %>%
  mutate(tot = sum(area)/1e6/1e6) %>%
  group_by(biome,continent) %>%
  summarise(tot.area = sum(area)/1e6/1e6,
            frac = tot.area/mean(tot)*100,
            .groups = "keep") %>%
  group_by(continent) %>%
  arrange(desc(biome)) %>%
  mutate(ypos = cumsum(frac)- 0.5*frac) %>%
  mutate(lab = round(frac,digits = 2))

frac.per.biome.continent

ggplot(frac.per.biome.continent ,
       aes(x="", y=frac,
           fill=interaction(biome))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  facet_wrap(~ continent) +
  scale_fill_manual(values = c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb"),
                    labels = c("Humid (MAP > 1700)",
                               "Humid (MAP < 1700)",
                               "Humid seasonal",
                               "Dry subhumid",
                               "Semiarid",
                               "Arid",
                               "Hyperarid")) +

  # geom_text(aes(y = ypos, label = lab), color = "white", size=6) +
  theme_void() +
  facet_wrap(~ continent) +
  guides(fill = "none")

frac.per.biome.continent %>%
  group_by(continent) %>%
  summarise(tot = sum(tot.area),
            .groups = "keep")

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
  geom_raster(data = differences %>%
                filter(variable == "biome.num"),
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
sort(as.matrix(CM0$byClass)[,"Precision"])

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

as.matrix(CM$byClass)[,"Precision"] - as.matrix(CM0$byClass)[,"Precision"]
mean(as.matrix(CM$byClass)[1:6,"Precision"] - as.matrix(CM0$byClass)[1:6,"Precision"])



as.matrix(CM) - as.matrix(CM0)

differences %>%
  filter(obs == 1) %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  filter(variable == "biome.num") %>%
  filter(diff == 0) %>%
  group_by(source) %>%
  summarise(tot = sum(area)/1e12) %>%
  pivot_wider(names_from = source,
              values_from = tot) %>%
  mutate(diff = Weighted.mean - Mean)



differences %>%
  left_join(data %>%
              dplyr::select(lon,lat,biome),
            by = c("lon","lat")) %>%
  filter(variable == "MAP") %>%
  filter(biome == 1) %>%
  filter(lon <= -30) %>%
  group_by(source) %>%
  summarise(Min = summary(diff)[1],
            first.Q = summary(diff)[2],
            Med = summary(diff)[3],
            Mean = summary(diff)[4],
            third.Q = summary(diff)[5])


differences %>%
  filter(mod == 1) %>%
  filter(lon <= -30) %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  filter(variable == "biome.num") %>%
  filter(diff == 0) %>%
  group_by(source) %>%
  summarise(tot = sum(area)/1e12) %>%
  pivot_wider(names_from = source,
              values_from = tot) %>%
  mutate(diff = Weighted.mean - Mean) %>%
  pull(Weighted.mean)

differences %>%
  filter(source == "Mean") %>%
  filter(obs == 1) %>%
  filter(lon <= -30) %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  pull(area) %>% sum() /1e12


differences %>%
  filter(variable %in% c("MAP","MCWD","MAT")) %>%
  group_by(source,variable) %>%
  summarise(r2 = summary(lm(formula = obs~mod))[["r.squared"]],
            RMSE = sqrt(1/sum(!is.na(obs))*sum((obs-mod)**2)),
            .groups = "keep") %>%
  pivot_wider(names_from = source,
              values_from = c(r2,RMSE))

# Per continent

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



##################################################################


Biomes <- c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
            "Semiarid","Arid","Hyperarid")

data.ERA5 <- readRDS("./outputs/ERA5.bootstrap.RDS") %>%
  mutate(biome = Biomes[biome]) %>%
  mutate(biome = factor(biome,
                        levels = Biomes))

data.ERA5 %>%
  group_by(iter) %>%
  summarise(frac = AGB[biome == "Humid_low"]/sum(AGB),
            .groups = "keep") %>%
  ungroup() %>%
  summarise(frac.low = quantile(frac,0.025),
            frac.m = mean(frac),
            frac.high = quantile(frac,0.975),
            .groups = "keep")


data.ERA5 %>%
  group_by(iter) %>%
  summarise(AGB.tot = sum(AGB),
            .groups = "keep") %>%
  ungroup() %>%
  summarise(AGB.tot.low = quantile(AGB.tot,0.025),
            AGB.tot.m = mean(AGB.tot),
            AGB.tot.high = quantile(AGB.tot,0.975),
            .groups = "keep")

CF <- differences %>%
  filter(variable == "biome.num") %>%
  group_by(source,mod,obs) %>%
  summarise(N = n(),
            .groups = "keep")

ggplot(data = CF) +
  geom_tile(aes(x = obs,
                y = 8 - mod,
                fill = N),
            color = "black", alpha = 0.8) +
  geom_label(aes(x = obs,
                 y = 8 - mod,
                 label = as.character(N)),
             fill = NA, label.size = 0) +
  scale_fill_gradient(low = "white",high = "darkblue",na.value = "white") +
  facet_wrap(~ source) +
  scale_x_continuous(limits = c(0.5,7.5),
                     expand = c(0,0),
                     breaks = c(),
                     labels =  c()) +
  scale_y_continuous(limits = c(0.5,7.5),expand = c(0,0),
                     breaks = c(),
                     labels =  c()) +
  geom_hline(yintercept = seq(0.5,7.5,1)) +
  geom_vline(xintercept = seq(0.5,7.5,1)) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust=1)) +
  theme(text = element_text(20),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = "none")

CF %>%
  filter(source == "Mean") %>%
  filter(obs == 1,mod != 1) %>%
  arrange(desc(N))
