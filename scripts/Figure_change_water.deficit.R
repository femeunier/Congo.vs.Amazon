rm(list = ls())

library(dplyr)
library(ggplot2)
library(YGB)
library(raster)
library(terra)
library(tidyr)
library(sf)
library(ggdist)

reanalyses <- readRDS("./outputs/SC.water.deficit.RDS") %>%
  rename(hemisph = hemisp)

LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))
rainforests <- read_sf("./data/Rainforests.shp")

all.CMIP6.MCWD <- readRDS("./outputs/All.CMIP6.states.timing.MEM.RDS") %>%
  ungroup() %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  dplyr::select(-LC) %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(!(model %in% c("NESM3","CIESM")))

df2plot <- all.CMIP6.MCWD %>%
  ungroup() %>%
  filter(model == "MEM",
         scenario != "ssp534-over",
         period %in% c("Long_future","historical"),
         LC %in% c(1,2,3)) %>%
  filter((period == "historical" & scenario == "ssp245") |
           (period != "historical"))

df2plot.LC <- df2plot %>%
  mutate(hemisph = case_when(lat < 0 ~ "S",
                             TRUE ~ "N")) %>%
  group_by(scenario,period,hemisph,
           LC,month) %>%
  summarise(Etot.m = mean(Etot, na.rm = TRUE),
            water.deficit = mean(pre,na.rm = TRUE) - mean(Etot.m,na.rm = TRUE),
            pre = mean(pre, na.rm = TRUE),
            tas = mean(tas, na.rm = TRUE),
            tasmin = mean(tasmin, na.rm = TRUE),
            tasmax = mean(tasmax, na.rm = TRUE),
            .groups = "keep") %>%
  mutate(LC = factor(LC,
                     levels = c(2,3,1)),
         hemisph = factor(hemisph,
                          levels = c("N","S")))

dry_season_rects <- data.frame(
  LC = factor(c(1, 1, 2, 2, 2, 3, 3, 3),
              levels = c(2,3,1)),
  hemisph = c("N", "S", "N", "N", "S", "N", "N", "S"),
  xmin = c(1, 4, 12, 1, 6, 11, 1, 4) -0.1,
  xmax = c(12, 10, 12, 2, 8, 12, 4, 10) + 0.1)

ggplot(data = df2plot.LC) +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +

  geom_line(aes(x = month,
                y = Etot.m,
                color = interaction(scenario,period))) +
  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("grey17","#263b5d","#8b9bac","#b48a40","#6a2d31")) +

  theme_bw() +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12, expand = c(0,0),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


ggplot(data = df2plot.LC) +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +
  geom_line(aes(x = month,
                y = pre,
                color = interaction(scenario,period))) +
  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("grey17","#263b5d","#8b9bac","#b48a40","#6a2d31")) +

  theme_bw() +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12, expand = c(0,0),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())



ggplot(data = df2plot.LC %>%
         mutate(deficit.m = pmin(0,water.deficit))) +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +

  geom_line(data = reanalyses,
            aes(x = month, y = deficit.m),
            linetype = 2) +
  geom_line(aes(x = month,
                y = deficit.m,
                color = interaction(scenario,period))) +
  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("grey17","#263b5d","#8b9bac","#b48a40","#6a2d31")) +

  theme_bw() +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12, expand = c(0,0),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


ggplot(data = df2plot.LC) +

  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +

  geom_line(data = df2plot.LC %>%
              filter(period == "historical"),
            aes(x = month,
                y = tas,
                color = interaction(scenario,period)),
            linetype = 3) +

  geom_line(aes(x = month,
                y = tasmin,
                color = interaction(scenario,period)),
            linetype = 1) +

  geom_line(aes(x = month,
                y = tasmax,
                color = interaction(scenario,period)),
            linetype = 2) +

  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("grey17","#263b5d","#8b9bac","#b48a40","#6a2d31")) +

  theme_bw() +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12,expand = c(0,0),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  # guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


################################################################################
# Changes

df2plot.LC.changes <- df2plot.LC %>%
  ungroup() %>%
  filter(period != "historical") %>%
  left_join(df2plot.LC %>%
              ungroup() %>%
              filter(period == "historical") %>%
              rename(Etot.m.ref = Etot.m,
                     water.deficit.ref = water.deficit,
                     pre.ref = pre,
                     tas.ref = tas,
                     tasmin.ref = tasmin,
                     tasmax.ref = tasmax) %>%
              dplyr::select(-c(scenario,period)),
            by = c("hemisph","LC","month")) %>%
  mutate(diff_tas = tas - tas.ref,
         diff_tasmin = tasmin - tasmin.ref,
         diff_tasmax = tasmax - tasmax.ref,
         diff_tas.extr = sqrt(tasmax - tasmin) - sqrt(tasmax.ref - tasmin.ref),
         diff_Etot = Etot.m - Etot.m.ref,
         diff_pre = pre - pre.ref,
         diff_water.deficit = water.deficit - water.deficit.ref)


ggplot(data = df2plot.LC.changes) +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +
  geom_hline(data = data.frame(LC = rep(1:3,2),
                               hemisph = c(rep("N",3),rep("S",3)),
                               y = 0),
             aes(yintercept = y), linetype = 2, color = "grey17") +
  geom_line(aes(x = month,
                y = diff_Etot,
                color = interaction(scenario,period))) +
  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +

  theme_bw() +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12,expand = c(0,0),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


ggplot(data = df2plot.LC.changes) +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +
  geom_hline(data = data.frame(LC = rep(1:3,2),
                               hemisph = c(rep("N",3),rep("S",3)),
                               y = 0),
             aes(yintercept = y), linetype = 2, color = "grey17") +
  geom_line(aes(x = month,
                y = diff_water.deficit,
                color = interaction(scenario,period))) +
  geom_line(aes(x = month,
                y = diff_pre,
                color = interaction(scenario,period)),
            linetype = 2) +
  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12,expand = c(0,0),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


ggplot(data = df2plot.LC.changes) +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +
  geom_hline(data = data.frame(LC = rep(1:3,2),
                               hemisph = c(rep("N",3),rep("S",3)),
                               y = 0),
             aes(yintercept = y), linetype = 2, color = "grey17") +
  geom_line(aes(x = month,
                y = diff_pre,
                color = interaction(scenario,period))) +
  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12,expand = c(0,0),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

ggplot(data = df2plot.LC.changes) +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +
  geom_line(aes(x = month,
                y = diff_tas,
                color = interaction(scenario,period))) +
  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  geom_hline(data = data.frame(LC = rep(1:3,2),
                               hemisph = c(rep("N",3),rep("S",3)),
                               y = 0),
             aes(yintercept = y), linetype = 2, color = "grey17") +
  theme_bw() +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_y_continuous(limits = c(0,5)) +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


ggplot(data = df2plot.LC.changes) +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +
  geom_line(aes(x = month,
                y = diff_tas.extr,
                color = interaction(scenario,period))) +
  facet_grid(hemisph ~ as.factor(LC)) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +

  theme_bw() +
  geom_hline(data = data.frame(LC = rep(1:3,2),
                               hemisph = c(rep("N",3),rep("S",3)),
                               y = 0),
             aes(yintercept = y), linetype = 2, color = "grey17") +
  labs(x = "", y = "", color = "") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

