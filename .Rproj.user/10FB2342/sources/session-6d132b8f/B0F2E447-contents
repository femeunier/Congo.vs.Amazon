rm(list = ls())

library(dplyr)
library(reshape2)

coord <- expand.grid(lat = seq(-30.25,30.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                  lat = as.vector(unique(coord$lat))) %>%
  melt() %>%
  mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
         Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
  rename(lon = Var1,
         lat = Var2,
         area = value) %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

df.scenarios <- readRDS("./outputs/df.climate.change.RDS") %>%
  left_join(Gridarea,
            by = c("lon","lat"))

df.biomass <- readRDS("./outputs/Biomass.per.biome.RDS") %>%
  ungroup() %>%
  filter(source == "ESA")

# df.weights <- readRDS("./outputs/CMIP6.weights.RDS") %>%
#   group_by(model) %>%
#   summarise(w = mean(w))
#
# df.GW <- readRDS("./outputs/df.Global.warming.RDS") %>%
#   mutate(period = case_when(year %in% 1901:1930 ~ "historical1",
#                             year %in% 1950:1979 ~ "historical2",
#                             year %in% 1980:2009 ~ "current",
#                             year %in% 2015:2040 ~ "near_future",
#                             year %in% 2041:2070 ~ "mid_future",
#                             year %in% 2071:2100 ~ "long_future",
#                             TRUE ~ NA_character_))
#
# df.GW.sum <- df.GW %>%
#   left_join(df.weights,
#             by = "model") %>%
#   group_by(scenario, period) %>%
#   summarise(tas.m = weighted.mean(tas,w, na.rm = TRUE),
#             .groups = "keep")


df.AGB <- df.scenarios %>%
  left_join(df.biomass %>%
              dplyr::select(-source),
            by = c("biome","basin"))

df.AGB.sum <- df.AGB %>%
  group_by(period,basin,scenario,weighting) %>%
  summarise(AGB = sum(AGB*area,
                      na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  group_by(basin,scenario,weighting) %>%
  arrange(period) %>%
  mutate(AGB.change = AGB - AGB[1])


df.AGB.biome.sum <- df.AGB %>%
  group_by(biome,basin,period,scenario,weighting) %>%
  summarise(AGB = sum(AGB*area,
                      na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  group_by(basin,scenario,weighting) %>%
  arrange(period) %>%
  mutate(AGB.change = AGB - AGB[1])

ggplot(data = df.AGB.biome.sum %>%
         filter(scenario == "ssp245")) +
  geom_bar(aes(x = period,
                y = AGB,
                fill = biome),
           stat = "identity") +
  facet_grid(weighting ~ basin) +
  theme_bw()


ggplot(data = df.AGB.biome.sum) +
  geom_bar(aes(x = period,
               y = AGB,
               fill = biome),
           stat = "identity") +
  facet_grid(weighting ~ scenario) +
  theme_bw()

ggplot(data = df.AGB.sum) +
  geom_line(aes(x = period,
                y = AGB,
                color = scenario,
                linetype = weighting)) +
  facet_wrap(~ basin) +
  theme_bw()

ggplot(data = df.AGB.sum) +
  geom_line(aes(x = period,
                y = AGB.change,
                color = scenario,
                linetype = weighting)) +
  facet_wrap(~ basin) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw()

df.AGB.Tropics.sum <- df.AGB %>%
  group_by(period,scenario,weighting) %>%
  summarise(AGB = sum(AGB*area,na.rm = TRUE)/1e12,
            .groups = "keep") %>%
  group_by(scenario,weighting) %>%
  arrange(period) %>%
  mutate(AGB.change = AGB - AGB[1])

ggplot(data = df.AGB.Tropics.sum) +
  geom_line(aes(x = period,
                y = AGB,
                color = scenario,
                linetype = weighting)) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw()
