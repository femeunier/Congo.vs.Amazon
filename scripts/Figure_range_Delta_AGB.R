rm(list = ls())

library(dplyr)
library(reshape2)
library(ggplot2)

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

df.biomass <- readRDS("./outputs/Biomass.per.biome.all.RDS")

df.weights <- readRDS("./outputs/CMIP6.weights.Tropics.RDS") %>%
  group_by(model) %>%
  summarise(w = mean(w))

# ggplot(data = df.biomass) +
#   geom_boxplot(aes(x = biome, y = AGB, fill = biome)) +
#   facet_wrap(~ source) +
#   theme_bw()

df.biomass.sum <- df.biomass %>%
  group_by(biome) %>%
  summarise(AGB.m = mean(AGB,na.rm = TRUE),
            .groups = "keep")


df.temp <- readRDS("./outputs/df.all.climate.change.Tropics.RDS") %>%
  ungroup() %>%
  left_join(Gridarea,
            by = c("lon","lat")) %>%
  # filter((period > 2014) | (period <= 2014 & scenario == "ssp126")) %>%
  # mutate(scenario = case_when(period <= 2014 ~ "historical",
  #                             TRUE ~ scenario)) %>%
  left_join(df.weights,
            by = 'model')

df.temp.AGB <- df.temp %>%
  left_join(df.biomass.sum,
            by = "biome")

df.ts <- df.temp.AGB %>%
  group_by(scenario,period,model) %>%
  summarise(AGB.tot = sum(AGB.m*area,
                          na.rm = TRUE)/1e12,
            w = unique(w),
            .groups = "keep") %>%
  filter(!is.na(w))

df.ts.sum <- df.ts %>%
  group_by(period,scenario) %>%
  summarise(AGB.tot.min = min(AGB.tot),
            AGB.tot.mean = mean(AGB.tot,na.rm = TRUE),
            AGB.tot.w.mean = weighted.mean(AGB.tot,w,na.rm = TRUE),
            AGB.tot.max = max(AGB.tot),
            .groups = "keep")

ggplot(data = df.ts.sum) +
  geom_ribbon(aes(x = period, ymin = AGB.tot.min, ymax = AGB.tot.max,
                  fill = scenario),
              alpha = 0.5) +
  geom_line(aes(x = period, y = AGB.tot.mean, color = scenario)) +
  geom_line(aes(x = period, y = AGB.tot.w.mean, color = scenario),
            linetype = 2) +
  facet_wrap(~ scenario) +
  theme_bw()


df.ts %>%
  filter(period == 2000,
         scenario == scenario[1]) %>%
  ungroup() %>%
  summarise(m = mean(AGB.tot),
            w = weighted.mean(AGB.tot,w),
            Min = min(AGB.tot),
            Max = max(AGB.tot),
            .groups = "keep") %>%
  mutate(spread = Max - Min)

ggplot(data = df.ts %>%
         ungroup() %>%
         filter(period == 2000,
                scenario == scenario[1])) +
  geom_rect(aes(xmin = -Inf, xmax = Inf,x = 0, y = 234,
                ymin = 231, ymax = 236),
            alpha = 0.5,fill = "lightgrey") +
  geom_jitter(aes(x = 0,y = AGB.tot),width = 0.1) +
  geom_boxplot(aes(x = 0,y = AGB.tot),width = 0.3,
               outlier.shape = NA, fill = NA) +

  geom_hline(yintercept = 234) +
  labs(x = "", y = "",) +
  theme_bw() + theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks = c(),labels = c(),
                     limits = c(-1,1)*0.5)

df.ts.sum.diff <- df.ts %>%
  filter(period >= 2010) %>%
  group_by(scenario,model) %>%
  mutate(AGB.tot.diff = AGB.tot - AGB.tot[period == 2010],
         AGB.tot.diff.rel = (AGB.tot - AGB.tot[period == 2010])/
                               AGB.tot[period == 2010])

df.ts.sum.diff.sum <- df.ts.sum.diff %>%
  group_by(period,scenario) %>%
  summarise(AGB.tot.diff.min = min(AGB.tot.diff),
            AGB.tot.diff.mean = mean(AGB.tot.diff,na.rm = TRUE),
            AGB.tot.diff.w.mean = weighted.mean(AGB.tot.diff,w,na.rm = TRUE),
            AGB.tot.diff.max = max(AGB.tot.diff),

            AGB.tot.diff.rel.min = min(AGB.tot.diff.rel),
            AGB.tot.diff.rel.mean = mean(AGB.tot.diff.rel,na.rm = TRUE),
            AGB.tot.diff.rel.w.mean = weighted.mean(AGB.tot.diff.rel,w,na.rm = TRUE),
            AGB.tot.diff.rel.max = max(AGB.tot.diff.rel),

            .groups = "keep")

ggplot(data = df.ts.sum.diff.sum) +
  geom_ribbon(aes(x = period, ymin = AGB.tot.diff.min, ymax = AGB.tot.diff.max,
                  fill = scenario),
              alpha = 0.2) +
  geom_line(aes(x = period, y = AGB.tot.diff.mean, color = scenario)) +
  geom_line(aes(x = period, y = AGB.tot.diff.w.mean, color = scenario),
            linetype = 2) +
  scale_fill_manual(values = c(rgb(0,52/255,102/255),
                               rgb(112/255,160/255,205/255),
                               rgb(196/255,121/255,0),
                               rgb(153/255,0,2/255))) +
  scale_color_manual(values = c(rgb(0,52/255,102/255),
                               rgb(112/255,160/255,205/255),
                               rgb(196/255,121/255,0),
                               rgb(153/255,0,2/255))) +
  labs(x = "", y = "", color = "", fill = "") +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.15,0.3))


ggplot(data = df.ts.sum.diff.sum) +
  geom_ribbon(aes(x = period, ymin = AGB.tot.diff.rel.min, ymax = AGB.tot.diff.rel.max,
                  fill = scenario),
              alpha = 0.2) +
  geom_line(aes(x = period, y = AGB.tot.diff.rel.mean, color = scenario)) +
  geom_line(aes(x = period, y = AGB.tot.diff.rel.w.mean, color = scenario),
            linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  scale_fill_manual(values = c(rgb(0,52/255,102/255),
                               rgb(112/255,160/255,205/255),
                               rgb(196/255,121/255,0),
                               rgb(153/255,0,2/255))) +
  scale_color_manual(values = c(rgb(0,52/255,102/255),
                                rgb(112/255,160/255,205/255),
                                rgb(196/255,121/255,0),
                                rgb(153/255,0,2/255))) +
  labs(x = "", y = "", color = "", fill = "") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.15,0.3))


df.ts.sum.diff.sum %>%
  filter(period == 2090) %>%
  mutate(spread = AGB.tot.diff.max - AGB.tot.diff.min) %>%
  dplyr::select(scenario,spread)

