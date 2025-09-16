rm(list = ls())

library(dplyr)
library(reshape2)
library(zoo)

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

df.scenarios <- readRDS("./outputs/df.climate.change.Tropics.RDS") %>%
  left_join(Gridarea,
            by = c("lon","lat")) %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia"))

df.scenarios %>%
  group_by(lon,lat) %>%
  slice_head(n = 1) %>%
  pull(area) %>% sum()/1e6/1e6

df.all <- readRDS("./outputs/df.all.climate.change.Tropics.RDS") %>%
  left_join(Gridarea,
            by = c("lon","lat")) %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia"))

df.biomass <- readRDS("./outputs/Biomass.per.biome.RDS") %>%
  group_by(biome,continent) %>%
  summarise(AGB = mean(AGB,na.rm = TRUE),
            .groups = "keep")

df.weights <- readRDS("./outputs/CMIP6.weights.Tropics.RDS") %>%
  group_by(model) %>%
  summarise(w = mean(w))

df.GW <- readRDS("./outputs/df.Global.warming.RDS") %>%
  mutate(period = year) %>%
  filter(model %in% df.weights$model)

df.GW.w <- df.GW %>%
  filter(!is.na(tas)) %>%
  left_join(df.weights,
            by = "model")

df.GW.sum <- df.GW.w %>%
  group_by(scenario, period) %>%
  summarise(tas.m = sum(tas.rm*w,na.rm = TRUE)/
              sum(w,na.rm = TRUE),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(Delta_T = tas.m - mean(tas.m[period %in% c(1900:1929)],
                               na.rm = TRUE))

df.AGB <- df.scenarios %>%
  mutate(biome = factor(as.character(biome),
                        levels = rev(levels(df.scenarios$biome)))) %>%
  left_join(df.biomass %>%
              dplyr::select(-any_of("source")),
            by = c("biome","continent"))

df.AGB.model <- df.all %>%
  mutate(biome = factor(as.character(biome),
                        levels = rev(levels(df.scenarios$biome)))) %>%
  left_join(df.biomass %>%
              dplyr::select(-any_of("source")),
            by = c("biome","continent"))

df.AGB.sum <- df.AGB %>%
  group_by(period,scenario,weighting) %>%
  summarise(AGB = sum(AGB*area,
                      na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  group_by(scenario,weighting) %>%
  arrange(period) %>%
  mutate(AGB.change = AGB - AGB[1])

df.delta.AGB <- df.AGB %>%
  filter(period > 1990) %>%
  group_by(scenario,weighting,lat,lon) %>%
  arrange(period) %>%
  mutate(AGB.change = (AGB - AGB[1])*area/1e12,
         AGB.change.rel = 100*AGB.change/(AGB[1]*area/1e12))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df.delta.AGB %>%
                dplyr::filter(period %in% c(2100)),
              aes(x = lon, y = lat,
                  fill = AGB.change)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkgreen",
                       # limits = c(-100,100),
                       oob = scales::squish) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")

saveRDS(df.delta.AGB,
        "./outputs/AGB.losses.RDS")

df.AGB.model.sum <- df.AGB.model %>%
  group_by(model,period,scenario) %>%
  summarise(AGB = sum(AGB*area,
                      na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  group_by(scenario,model) %>%
  arrange(period) %>%
  mutate(AGB.change = AGB - AGB[1])


df.AGB.biome.sum <- df.AGB %>%
  group_by(biome,period,scenario,weighting) %>%
  summarise(AGB = sum(AGB*area,
                      na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  group_by(biome,scenario,weighting) %>%
  arrange(period) %>%
  mutate(AGB.change = AGB - AGB[1])


df.AGB.continent.biome.sum <- df.AGB %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  group_by(continent,biome,period,scenario,weighting) %>%
  summarise(AGB = sum(AGB*area,
                      na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  group_by(continent,biome,scenario,weighting) %>%
  arrange(period) %>%
  mutate(AGB.change = AGB - AGB[1])


ggplot(data = df.AGB.biome.sum %>%
         mutate(weighting.period = paste0(weighting,".",period))) +
  geom_bar(aes(x = period,
               y = AGB,
               fill = biome),
           stat = "identity") +
  facet_grid(scenario ~ weighting) +
  scale_fill_manual(values = rev(c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb"))) +
  theme_bw() +
  guides(fill = "none")

saveRDS(df.AGB.biome.sum,
        "./outputs/biomass.per.biome.RDS")

saveRDS(df.AGB.continent.biome.sum,
        "./outputs/biomass.per.continent.biome.RDS")

extreme.models <- df.AGB.model.sum %>%
  ungroup() %>%
  filter(scenario == "ssp585",
         period == max(period)) %>%
  filter(AGB.change %in% c(min(AGB.change),
                           max(AGB.change))) %>%
  pull(model)

df.delta.AGB.model <- df.AGB.model %>%
  filter(model %in% extreme.models) %>%
  group_by(scenario,model,lat,lon) %>%
  filter(period > 1990) %>%
  arrange(period) %>%
  mutate(AGB.change = (AGB - AGB[1])*area/1e12,
         AGB.change.rel = 100*AGB.change/(AGB[1]*area/1e12))

ggplot() +
  geom_raster(data = df.delta.AGB.model %>%
                dplyr::filter(period %in% c(2100)),
              aes(x = lon, y = lat,
                  fill = AGB.change)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",
                       mid = "white",
                       high = "darkgreen",
                       # limits = c(-100,100),
                       oob = scales::squish) +
  theme_bw() +
  facet_grid(scenario ~ model) +
  theme(legend.position = "bottom")

ggplot(data = df.AGB.sum %>%
         filter(period >= 2000)) +
  geom_line(data = df.AGB.model.sum %>%
              filter(period >= 2000),
            aes(x = period,
                y = AGB,
                color = scenario,
                group = interaction(scenario,model)),
            size = 0.1) +
  geom_label(data = df.AGB.model.sum %>%
               filter(model %in% extreme.models) %>%
               filter(period == max(period)),
             aes(x = 2110,
                 y = AGB + 5,
                 color = scenario,
                 label = model)) +
  geom_line(aes(x = period,
                y = AGB,
                color = scenario,
                linetype = weighting)) +
  facet_wrap(~ scenario, scales = "fixed",
             nrow = 1) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw()

ggplot(data = df.AGB.sum %>%
         filter(period >= 2000)) +
  geom_line(data = df.AGB.model.sum %>%
              filter(period >= 2000),
            aes(x = period,
                y = AGB.change,
                color = scenario,
                group = interaction(scenario,model)),
            size = 0.1) +
  geom_line(aes(x = period,
                y = AGB.change,
                color = scenario,
                linetype = weighting)) +
  facet_wrap(~ scenario, scales = "fixed",
             nrow = 1) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw()

AGB.vs.GW <-
  df.AGB.sum %>%
  mutate(scenario = case_when(period <= 2014 ~"historical",
                              TRUE ~ scenario)) %>%
  left_join(df.GW.sum,
            by = c("period","scenario"))

saveRDS(AGB.vs.GW,
        "./outputs/AGB.vs.GW.RDS")

ggplot(data = AGB.vs.GW,
       aes(x = Delta_T,
           y = AGB.change,
           shape = weighting,
           linetype = weighting)) +
  stat_smooth(method = lm,
              se = FALSE, color = "black",
              size = 0.5,
              formula = y ~ poly(x,2)) +
  geom_point(aes(color = scenario)) +
  geom_hline(yintercept = 0,
             linetype = 2) +
  scale_color_manual(values = c("darkgrey","#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw()

AGB.vs.GW %>%
  group_by(weighting) %>%
  summarise(r2 = summary(lm(AGB.change ~ poly(Delta_T,2)))[["r.squared"]])

df.delta.AGB.model2plot <-
  df.delta.AGB.model %>%
  ungroup()

ggplot() +
  geom_raster(data = df.delta.AGB.model2plot %>%
                dplyr::filter(period %in% c(2000,2100)),
              aes(x = lon, y = lat,
                  fill = as.factor(biome))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_manual(values = rev(c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb"))) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ model) +
  theme(legend.position = "bottom")

df.weights %>%
  filter(model %in% extreme.models)
