rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpattern)

# CMIP5 <- data.frame(period = c(2000,2090,2090),
#                     scenario = c("historical","ssp245","ssp585"),
#                     AGB.m = c(209.14,197.86,184.71),
#                     AGB.low = c(207.37,196.2,183.18),
#                     AGB.high = c(210.89,199.54,186.32)) %>%
#   mutate(period.scenar = paste0(period,".",scenario))


Biomes <- c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
            "Semiarid","Arid","Hyperarid")

data <- readRDS("./outputs/ERA5.bootstrap.RDS") %>%
  mutate(biome = Biomes[biome]) %>%
  mutate(biome = factor(biome,
                        levels = Biomes))
data.biome <- data %>%
  group_by(biome) %>%
  summarise(AGB.low = quantile(AGB,0.025,na.rm = TRUE),
            AGB.m = mean(AGB,na.rm = TRUE),
            AGB.high = quantile(AGB,0.975,na.rm = TRUE),
            .groups = "keep")

temp.data <- data %>%
  group_by(iter) %>%
  summarise(AGB = sum(AGB,na.rm = TRUE),
            .groups = "keep")

data.biome.eb <- temp.data %>%
  ungroup() %>%
  summarise(AGB.m = mean(AGB,na.rm = TRUE),
            AGB.low = quantile(AGB,0.025,na.rm = TRUE),
            AGB.high = quantile(AGB,0.975,na.rm = TRUE),)

BS.AGB <- readRDS("./outputs/df.Biomass.cat.RDS") %>%
  group_by(period,scenario,weighting,biome,iter) %>%
  summarise(AGB = sum(AGB),
            .groups = "keep")

readRDS("./outputs/df.Biomass.cat.RDS") %>%
  filter(iter == 23) %>%
  filter(period == 2000,
         biome == "Humid_large") %>%
  group_by(weighting,iter) %>%
  summarise(AGB = sum(AGB)) %>%
  group_by(weighting) %>%
  summarise(AGB.m = mean(AGB),
            .groups = "keep")

reference.AGB.biome <- BS.AGB %>%
  filter(period == 2000) %>%
  ungroup() %>%
  dplyr::select(-c(period,scenario))

data.and.model <- bind_rows(reference.AGB.biome %>%
  group_by(weighting,biome) %>%
  summarise(AGB.low = quantile(AGB,0.025),
            AGB.m = mean(AGB),
            AGB.high = quantile(AGB,0.975),
            .groups = "keep"),
  data.biome %>% mutate(weighting = 'data')) %>%
  # filter(biome %in% c("Humi d_large","Humid_low","Humid_seasonal")) %>%
  arrange(biome,weighting) %>%
  mutate(ref = case_when(weighting == "data" ~ TRUE,
                         TRUE ~ FALSE))

data.vs.model <- data.and.model %>%
  filter(!ref) %>%
  dplyr::select(-c(ref)) %>%
  left_join(data.and.model %>%
              filter(ref) %>%
              ungroup() %>%
              dplyr::select(-c(ref,weighting)),
            by = c("biome"))

ggplot(data = data.vs.model,
       aes(x = AGB.m.y,
           xmin = AGB.low.y,xmax = AGB.high.y,
           y = AGB.m.x,
           ymin = AGB.low.x,ymax = AGB.high.x,
           shape = weighting,
           group = weighting,
           color = biome)) +
  geom_errorbar(width = 0) +
  geom_errorbarh(height = 0) +
  geom_point() +
  # stat_smooth(aes(linetype = weighting),
  #             method = "lm",
  #             color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope = 1, linetype = 1) +
  theme_bw() +
  scale_color_manual(values = rev(c("#253b10","#005401","#448704","#86a540",
                                   "#c49402","#d0ce9a","#e5e4cb"))) +
  guides(shape = "none",
         linetype = "none")

BS.AGB %>%
  filter(period == 2090) %>%
  left_join(reference.AGB.biome,
            by = c("weighting","biome","iter")) %>%
  mutate(diff = (AGB.x - AGB.y)) %>%
  group_by(scenario,biome,weighting) %>%
  summarise(diff.low = quantile(diff,0.025,
                                na.rm = TRUE),
            diff.m = mean(diff,
                          na.rm = TRUE),
            diff.high = quantile(diff,0.975,
                                 na.rm = TRUE),
            .groups = "keep") %>%
  filter(scenario %in% c("ssp585"),
         weighting == "w")


BS.AGB.tot <- readRDS("./outputs/df.Biomass.cat.RDS") %>%
  group_by(period,scenario,weighting,iter) %>%
  summarise(AGB = sum(AGB),
            .groups = "keep")

reference.BS <- BS.AGB.tot %>%
  filter(period == 2000) %>%
  ungroup() %>%
  dplyr::select(-c(period,scenario))

BS.AGB.tot %>%
  filter(period == 2090) %>%
  left_join(reference.BS,
            by = c("weighting","iter")) %>%
  mutate(diff = AGB.x - AGB.y) %>%
  group_by(scenario,weighting) %>%
  summarise(diff.low = quantile(diff,0.025,
                                na.rm = TRUE),
            diff.m = mean(diff,
                          na.rm = TRUE),
            diff.high = quantile(diff,0.975,
                                 na.rm = TRUE),
            .groups = "keep")

BS.AGB.m <- bind_rows(BS.AGB %>%
  group_by(period,scenario,weighting,biome) %>%
  summarise(AGB.m = mean(AGB),
            AGB.low = quantile(AGB,0.025),
            AGB.high = quantile(AGB,0.975),
            .groups = "keep") %>%
  filter(period %in% c(2000,2090)) %>%
  mutate(period.scenar = paste0(period,".",scenario)),
  data.biome %>%
    mutate(period.scenar = "1990",
           weighting = "m"))

BS.AGB.errorbar <- bind_rows(BS.AGB.tot %>%
  group_by(period,scenario,weighting) %>%
  summarise(AGB.m = mean(AGB),
            AGB.low = quantile(AGB,0.025),
            AGB.high = quantile(AGB,0.975),
            .groups = "keep") %>%
  filter(period %in% c(2000,2090)) %>%
  mutate(period.scenar = paste0(period,".",scenario)),

  data.biome.eb %>%
    mutate(period.scenar = "1990",
           weighting = "m"))


ggplot(data = BS.AGB.m) +

  geom_bar(aes(x = period.scenar,
               y = AGB.m,
               fill = biome),
           stat = "identity") +
  geom_errorbar(data = BS.AGB.errorbar,
                aes(x = period.scenar,
                    y = AGB.m,
                    ymin = AGB.low,
                    ymax = AGB.high),
                width = 0.25) +

  # geom_errorbar(data = CMIP5,
  #               aes(x = period.scenar,
  #                y = AGB.m,
  #                ymin = AGB.low,
  #                ymax = AGB.high), width = 0.) +
  # geom_point(data = CMIP5,
  #            aes(x = period.scenar,
  #                y = AGB.m)) +

  facet_grid( ~ weighting) +
  scale_fill_manual(values = rev(c("#253b10","#005401","#448704","#86a540",
                                   "#c49402","#d0ce9a","#e5e4cb"))) +
  theme_minimal() +
  labs(x = "",y = "") +
  scale_x_discrete(labels = c()) +
  guides(fill = "none") +
  theme(text = element_text(size = 20))

BS.AGB.tot %>%
  filter(period %in% c(2000,2090)) %>%
  group_by(period,scenario,weighting) %>%
  summarise(AGB.low = quantile(AGB,0.025,na.rm = TRUE),
            AGB.m = mean(AGB,na.rm = TRUE),
            AGB.high = quantile(AGB,0.975,na.rm = TRUE),
            .groups = "keep") %>% arrange(weighting)

A <-BS.AGB.tot %>%
  filter(period %in% c(2000,2090)) %>%
  group_by(period,scenario,weighting) %>%
  summarise(AGB.low = quantile(AGB,0.025,na.rm = TRUE),
            AGB.m = mean(AGB,na.rm = TRUE),
            AGB.high = quantile(AGB,0.975,na.rm = TRUE),
            .groups = "keep") %>% arrange(weighting) %>%
  ungroup() %>%
  dplyr::select(-c(period,AGB.low,AGB.high))

A %>%
  filter(scenario != "historical") %>%
  left_join(A %>%
              filter(scenario == "historical") %>%
              dplyr::select(-scenario) %>%
              rename(ref = AGB.m),
            by = c("weighting")) %>%
  mutate(diff = ref - AGB.m)

BS.AGB.tot %>%
  filter(period == 2000) %>%
  left_join(temp.data,
            by = "iter") %>%
  ungroup() %>%
  mutate(diff = AGB.y - AGB.x) %>%
  group_by(weighting) %>%
  summarise(m = mean(diff),
         low = quantile(diff,0.025),
         high = quantile(diff,0.975))

############################################################


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
  mutate(diff = AGB - AGB.ref)

df.continent.vs.ref.m <- df.continent.vs.ref %>%
  group_by(weighting,scenario,biome,continent) %>%
  summarise(diff.low = quantile(diff,0.025,na.rm = TRUE),
            diff.m = mean(diff,na.rm = TRUE),
            diff.high = quantile(diff,0.975,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.continent.vs.ref.m %>%
         filter(scenario == "ssp245") %>%
         mutate(continent = factor(continent,
                                   levels = c("America","Africa","Australasia"))) %>%
         mutate(biome.weighting = paste0(biome,".",weighting)) %>%
         mutate(biome.weighting = factor(biome.weighting,
                                         levels = paste0(
                                           rep(rev(levels(df.continent.vs.ref$biome)),each = 2),
                                           ".",
                                           rep(c("m","w"),7))))) +
  geom_errorbar(aes(x = biome.weighting,
                    y = diff.m,ymin = diff.low, ymax = diff.high,
               fill = biome), width = 0.1,
           position = "dodge", stat = "identity") +
  geom_bar(aes(x = biome.weighting,
                   y = diff.m,
                   fill = biome),
           position = "dodge", stat = "identity") +
  theme_bw() +
  facet_wrap(~ continent,
             scales = "free",
             ncol = 1) +
  scale_fill_manual(values = rev(c("#253b10","#005401","#448704","#86a540",
                                   "#c49402","#d0ce9a","#e5e4cb"))) +
  scale_x_discrete(labels = c(),
                   breaks = c()) +
  theme() +
  guides(fill = "none") +
  labs(x = "", y = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(2, "lines"),
    text = element_text(size = 20))


ggplot(data = df.continent.vs.ref.m %>%
         filter(scenario == "ssp245") %>%
         mutate(biome = factor(biome,
                               levels = rev(levels(df.continent.vs.ref.m$biome)))) %>%
         mutate(continent = factor(continent,
                                   levels = c("America","Africa","Australasia"))) %>%
         mutate(biome.weighting = paste0(biome,".",weighting)) %>%
         mutate(biome.weighting = factor(biome.weighting,
                                         levels = paste0(
                                           rep(rev(levels(df.continent.vs.ref$biome)),each = 2),
                                           ".",
                                           rep(c("m","w"),7))))) +
  geom_errorbar(aes(x = biome,
                    y = diff.m,ymin = diff.low,
                    ymax = diff.high,
                    fill = biome.weighting), width = 0.1,
                position = position_dodge(width = 0.9), stat = "identity") +
  geom_bar(aes(x = biome,
               y = diff.m,
               fill = biome.weighting,
               alpha = weighting), color = "black",
           position = "dodge", stat = "identity") +
  theme_bw() +
  facet_wrap(~ continent,
             scales = "free",
             ncol = 1) +
  scale_alpha_manual(values = c(1,1)) +
  scale_fill_manual(values = c("#253b10","#253b10",
                               "#005401","#005401",
                               "#448704","#448704",
                               "#86a540","#86a540",
                               "#c49402","#c49402",
                               "#d0ce9a","#d0ce9a",
                               "#e5e4cb","#e5e4cb")) +
  scale_x_discrete(labels = c(),
                   breaks = c()) +
  theme() +
  guides(fill = "none",alpha = "none") +
  labs(x = "", y = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(2, "lines"),
    text = element_text(size = 20))


df2plot <- df.continent.vs.ref.m %>%
  filter(scenario == "ssp245") %>%
  mutate(biome = factor(biome,
                        levels = rev(levels(df.continent.vs.ref.m$biome)))) %>%
  mutate(continent = factor(continent,
                            levels = c("America","Africa","Australasia"))) %>%
  mutate(biome.weighting = paste0(biome,".",weighting)) %>%
  mutate(biome.weighting = factor(biome.weighting,
                                  levels = paste0(
                                    rep(rev(levels(df.continent.vs.ref$biome)),each = 2),
                                    ".",
                                    rep(c("m","w"),7))))


df2plot.bnd <- df2plot

# %>%
#   mutate(diff.low = case_when(TRUE ~ diff.m),
#          diff.high = case_when(diff.m > 0 ~ diff.high,
#                               TRUE ~ diff.m))

ggplot(data = df2plot.bnd,
       aes(x = biome, fill = biome, pattern = weighting,
           y = diff.m)) +
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.01,
                   pattern_key_scale_factor = 0.6,
                   alpha = 0.7) +
  geom_errorbar(aes(x = biome,
                    y = diff.m,ymin = diff.low,
                    ymax = diff.high), width = 0.1,
                position = position_dodge(width = 0.9), stat = "identity") +
  # scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)) +
  scale_pattern_manual(values = c(w = "stripe",
                                  m = "none")) +
  labs(x = "", y = "", pattern = "") +
  guides(pattern = "none",
         fill = "none") +
  theme_bw() +

  facet_wrap(~ continent,
             scales = "free",
             ncol = 1) +
  scale_fill_manual(values = c("#253b10",
                               "#005401",
                               "#448704",
                               "#86a540",
                               "#c49402",
                               "#d0ce9a",
                               "#e5e4cb")) +

  scale_x_discrete(labels = c(),
                   breaks = c()) +
  labs(x = "", y = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(2, "lines"),
    text = element_text(size = 20))
