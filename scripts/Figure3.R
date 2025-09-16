rm(list = ls())

library(ggthemes)
library(dplyr)
library(ggplot2)

# df.biomass <- readRDS("./outputs/Biomass.per.biome.Tropics.RDS") %>%
#   # filter(source != "Xu") %>%
#   group_by(biome) %>%
#   summarise(AGB = mean(AGB,na.rm = TRUE),
#             .groups = "keep") %>%
#   mutate(biome = as.numeric(as.factor(biome)))
#
# AGB.losses <- readRDS("./outputs/AGB.losses.RDS") %>%
#   mutate(continent = case_when(lon <= -30 ~ "America",
#                                lon <= 55 ~ "Africa",
#                                TRUE ~ "Australasia"))
# AGB.losses$AGB.change.rel[is.na(AGB.losses$AGB.change.rel)] <- 0
#
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot() +
#   geom_raster(data = AGB.losses %>%
#                 dplyr::filter(scenario %in% c("ssp245","ssp585"),
#                               period %in% c(2090)),
#               aes(x = lon, y = lat,
#                   fill = AGB.change.rel)) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
#   scale_fill_gradient2(low = "darkred",
#                        mid = "white",
#                        high = "darkgreen",
#                        limits = c(-100,100),
#                        breaks = c(-100,100),
#                        oob = scales::squish) +
#   theme_bw() +
#   facet_grid(scenario ~ weighting) +
#   labs(x = "", y = "", fill = "") +
#   theme(legend.position = "bottom",
#         strip.background = element_blank(),
#         strip.text = element_blank(),
#         text = element_text(size = 20),
#         panel.spacing = unit(2, "lines"))
#
#
# AGB.losses %>%
#   dplyr::filter(scenario %in% c("ssp245","ssp585"),
#                 period %in% c(2090)) %>%
#   group_by(scenario,weighting) %>%
#   summarise(S = sum(AGB.change,na.rm = TRUE))
#
# AGB.losses %>%
#   # dplyr::filter(scenario %in% c("ssp245","ssp585"),
#   #               period %in% c(2100)) %>%
#   filter(period %in% c(2000,2090)) %>%
#   group_by(scenario,weighting,period) %>%
#   summarise(S = sum(AGB*area,na.rm = TRUE)/1e12) %>%
#   arrange(period)
#
# BS.AGB <- readRDS("./outputs/df.Biomass.cat.RDS") %>%
#   group_by(period,scenario,weighting,biome,iter) %>%
#   summarise(AGB = sum(AGB),
#             .groups = "keep")
#
# BS.AGB.m <- BS.AGB %>%
#   group_by(period,scenario,weighting) %>%
#   summarise(AGB.m = sum(AGB),
#             .groups = "keep") %>%
#   filter(period %in% c(2000,2090))

################################################################

rm(list = ls())

library(ggthemes)
library(reshape2)

data.map <- readRDS("./outputs/ERA5.map.RDS")
AI.ERA5.map <- readRDS("./outputs/Data.used.RDS")

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

biomes <- c("Humid (MAP > 1700)",
            "Humid (MAP < 1700)",
            "Humid seasonal",
            "Dry subhumid",
            "Semiarid",
            "Arid",
            "Hyperarid")

data2pie <- data.map %>%
  dplyr::select(lon,lat,AGB.m) %>%
  left_join(AI.ERA5.map %>%
              dplyr::select(lon,lat,biome),
            by = c("lon","lat")) %>%
  left_join(Gridarea %>%
              dplyr::select(lon,lat,area),
            by = c("lon","lat")) %>%
  ungroup() %>%
  mutate(biome = biomes[biome]) %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  group_by(biome,continent) %>%
  summarise(AGB = sum(AGB.m*area,na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  group_by(continent) %>%
  mutate(frac = AGB/sum(AGB,na.rm = TRUE)*100) %>%
  mutate(biome = factor(biome,
                        levels = biomes))

ggplot(data2pie,
       aes(x="", y=frac,
           fill=biome)) +
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
  theme_void() +
  facet_grid( ~ continent) +
  guides(fill = "none")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

df <- readRDS("./outputs/df.Biomass.map.RDS") %>%
  group_by(period,scenario,weighting,lon,lat) %>%
  summarise(AGB = mean(AGB,na.rm = TRUE),
            area = mean(area),
            .groups = "keep") %>%
  mutate(ref = case_when(period == 2000 ~ TRUE,
                         TRUE ~ FALSE))

df.continent <- readRDS("./outputs/df.Biomass.map.RDS") %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  group_by(iter,period,scenario,weighting,biome,continent) %>%
  summarise(AGB = sum(AGB*area,na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  mutate(ref = case_when(period == 2000 ~ TRUE,
                         TRUE ~ FALSE))


df.continent.vs.ref <-
  df.continent %>%
  filter(!ref) %>%
  dplyr::select(-ref) %>%
  left_join(df.continent %>%
              filter(ref) %>%
              ungroup() %>%
              rename(AGB.ref = AGB) %>%
              dplyr::select(-c(period,ref,scenario)),
            by = c("continent","biome","weighting","iter")) %>%
  mutate(diff = AGB.ref - AGB)

df.continent.vs.ref.m <- df.continent.vs.ref %>%
  group_by(weighting,scenario,biome,continent) %>%
  summarise(diff.low = quantile(diff,0.025,na.rm = TRUE),
            diff.m = mean(diff,na.rm = TRUE),
            diff.high = quantile(diff,0.975,na.rm = TRUE),
            .groups = "keep")

df.continent.vs.ref.m %>%
  group_by(scenario,weighting) %>%
  summarise(sum(diff.m))

df.continent.vs.ref %>%
  group_by(scenario,weighting,iter) %>%
  summarise(AGB.ref = sum(AGB.ref),
            AGB = sum(AGB)) %>%
  group_by(scenario,weighting) %>%
  summarise(AGB.ref = mean(AGB.ref),
            AGB = mean(AGB))

df2barplot <- df.continent.vs.ref.m %>%
  mutate(continent = factor(as.character(continent),
                            levels = c("America","Africa","Australasia"))) %>%
  mutate(biome = factor(as.character(biome),
                        rev(levels(df.continent.vs.ref$biome)))) %>%
  mutate(weighting.scenario = paste0(weighting,".",scenario))


ggplot(data = df2barplot) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_bar(aes(x = biome, y = diff.m,
               fill = biome),
           stat = "identity") +
  facet_grid(weighting.scenario ~ continent) +
  scale_fill_manual(values = (c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb"))) +
  theme_bw() +
  scale_x_discrete(breaks = levels(df.continent.vs.ref$biome)) +
  scale_y_continuous(breaks = c(-10,0,10,20),
                     limits = c(-10,25)) +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(fill = "none") +
  labs(x = "", y = "")

df2barplot %>%
  filter(continent == "America",
         biome == "Humid_large")

df2barplot.tot <- df.continent.vs.ref %>%
  group_by(weighting,continent,scenario,iter) %>%
  summarise(AGB.ref = sum(AGB.ref,na.rm = TRUE),
            AGB = sum(AGB),
            .groups = "keep") %>%
  group_by(weighting,continent,scenario) %>%
  summarise(AGB.ref.low = quantile(AGB.ref,0.025,na.rm = TRUE),
            AGB.ref.m = mean(AGB.ref,na.rm = TRUE),
            AGB.ref.high = quantile(AGB.ref,0.975,na.rm = TRUE),

            AGB.low = quantile(AGB,0.025,na.rm = TRUE),
            AGB.m = mean(AGB,na.rm = TRUE),
            AGB.high = quantile(AGB,0.975,na.rm = TRUE),

            .groups = "keep") %>%
  mutate(continent = factor(as.character(continent),
                            levels = c("America","Africa","Australasia")))

df.continent.vs.ref %>%
  group_by(scenario,weighting,continent,iter) %>%
  mutate(diff.tot = sum(diff,na.rm = TRUE),
         .groups = "keep") %>%
  group_by(scenario,weighting,continent) %>%
  summarise(diff.tot.low = quantile(diff.tot,0.025,na.rm = TRUE),
            diff.tot.m = mean(diff.tot,na.rm = TRUE),
            diff.tot.high = quantile(diff.tot,0.975,
                                     na.rm = TRUE),
            .groups = "keep") %>%
  arrange(weighting,scenario,continent) %>%
  filter(continent == "America")

df2barplot.tot %>%
  filter(continent == "Australasia")

ggplot(data = df2barplot.tot %>%
         mutate(continent.weighting =
                  factor(paste0(continent,".",weighting),
                         levels = c("America.m","Africa.m","Australasia.m",
                                    "America.w","Africa.w","Australasia.w")))) +
  geom_bar(aes(x = as.factor(1), y = AGB.m),
           fill = "darkgrey", color = "darkgrey", stat = "identity") +
  geom_bar(aes(x = as.factor(1), y = AGB.ref.m),
           fill = NA, color = "black", stat = "identity") +
  facet_grid(scenario ~ continent.weighting) +
  theme_void() +
  scale_x_discrete(labels = c(),breaks = c()) +
  scale_y_continuous(limits = c(0,120)) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x = "", y = "")


data.vs.model <- df %>%
  filter(ref) %>%
  left_join(data.map %>%
              dplyr::select(-c(AGB.low,AGB.high)),
            by = c("lon","lat")) %>%
  left_join(AI.ERA5.map %>%
              dplyr::select(lon,lat,biome),
            by = c("lon","lat"))

data.vs.model %>%
  group_by(weighting) %>%
  summarise(tot = sum((AGB)*area/1e12))

data.vs.model %>%
  group_by(biome,weighting) %>%
  summarise(tot = sum((AGB)*area/1e12),
            tot.obs = sum(AGB.m*area)/1e12)

data.vs.model %>%
  group_by(biome,weighting) %>%
  summarise(diff.m = sum((AGB - AGB.m)*area/1e12))


ggplot() +
  geom_raster(data = data.vs.model,
              aes(x = lon, y = lat,
                  fill = AGB.m - AGB)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                      high = "darkgreen") +
  #                      limits = c(-100,100),
  #                      breaks = c(-100,100),
  #                      oob = scales::squish) +
  facet_wrap(scenario ~ weighting) +
  theme_bw() +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))

data.vs.model %>%
  group_by(weighting) %>%
  mutate(diff = AGB.m - AGB) %>%
  summarise(m = median(diff),)


df2plot <- df %>%
  filter(!ref) %>%
  left_join(df %>%
              filter(ref) %>%
              ungroup() %>%
              dplyr::select(-c(period,scenario,area)) %>%
              rename(AGB.ref = AGB),
            by = c("weighting","lon","lat")) %>%
  mutate(diff = (AGB - AGB.ref),
         diff.rel = 100*(AGB - AGB.ref)/AGB.ref)

ggplot() +
  geom_raster(data = df2plot %>%
                dplyr::filter(scenario %in% c("ssp245","ssp585"),
                              period %in% c(2090)),
              aes(x = lon, y = lat,
                  fill = diff.rel)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkgreen",
                       limits = c(-100,100),
                       breaks = c(-100,100),
                       oob = scales::squish) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))


ggplot() +
  geom_raster(data = df2plot %>%
                dplyr::filter(scenario %in% c("ssp245","ssp585"),
                              period %in% c(2090)),
              aes(x = lon, y = lat,
                  fill = diff.rel)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkgreen",
                       limits = c(-100,100),
                       breaks = c(-100,100),
                       oob = scales::squish) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  labs(x = "", y = "", fill = "") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))


df2plot %>%
  group_by(weighting, scenario) %>%
  summarise(tot = sum(diff*area,
                      na.rm = TRUE)/1e12,
            .groups = "keep")

