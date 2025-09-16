rm(list = ls())

library(dplyr)
library(ggplot2)
library(YGB)
library(raster)
library(tidyr)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/All.CMIP6.states.timing.MEM.RDS",
          "./outputs/"))

GridArea <- readRDS("./outputs/GridArea.RDS")

threshold.sum <- readRDS(
  "./outputs/Sensitivity.thresholds.sum.RDS")
threshold.sum.MAP <- threshold.sum %>%
  filter(variable == "MAP")
threshold.sum.MCWD <- threshold.sum %>%
  filter(variable == "MCWD")

MAP.threshold <- threshold.sum %>%
  filter(variable == "MAP") %>%
  pull(mean)
MCWD.threshold <- threshold.sum %>%
  filter(variable == "MCWD") %>%
  pull(mean)
MAP.threshold <- threshold.sum %>%
  filter(variable == "MAP") %>%
  pull(mean)
MCWD.threshold <- threshold.sum %>%
  filter(variable == "MCWD") %>%
  pull(mean)

LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

all.CMIP6.MCWD <- readRDS("./outputs/All.CMIP6.states.timing.MEM.RDS") %>%
  ungroup() %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  dplyr::select(-LC) %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(!(model %in% c("NESM3","CIESM")))


A <- readRDS("./outputs/All.CMIP6.states.timing.MEM.RDS") %>%
  ungroup() %>%
  filter(!(model %in% c("NESM3","CIESM"))) %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  group_by(lon,lat,model,scenario,period) %>%
  summarise(MCWD = unique(MCWD),
            MAP = sum(pre),
            .groups = "keep") %>%
  ungroup() %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  mutate(LC.pred = as.factor(case_when(MAP < MAP.threshold ~ 1,
                                       MCWD > MCWD.threshold ~ 2,
                                       TRUE ~ 3)))
df2plot <- A %>%
  ungroup() %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  filter(model == "MEM",
         period == "historical",
         scenario == "ssp245",
         !is.na(LC), LC %in% c(1,2,3),
         land.frac > 0.25)


all.CMIP6.MCWD.selected <- all.CMIP6.MCWD %>%
  group_by(scenario,period,model,lon,lat,LC) %>%
  summarise(MAP = sum(pre,na.rm = TRUE),
            MCWD = unique(MCWD),
            .groups = "keep") %>%
  ungroup() %>%
  dplyr::select(scenario,period,model,lon,lat,MCWD,MAP,LC) %>%
  mutate(LC.pred = as.factor(case_when(MCWD > MCWD.threshold ~ 2,
                                       MAP < MAP.threshold ~ 1,
                                       TRUE ~ 3)))

saveRDS(all.CMIP6.MCWD.selected,
        "./outputs/All.Transitions.timing_long.RDS")

transitions <- all.CMIP6.MCWD.selected %>%
  pivot_wider(values_from = c(MCWD,MAP,LC.pred),
              names_from = period)


model2analyze <- "MEM"
scenario2analyze <- "ssp245"
cdf.transitions <- transitions %>%
  mutate(change = case_when(LC.pred_historical != LC.pred_Long_future ~ TRUE,
                            TRUE ~ FALSE)) %>%
  mutate(Delta_MCWD = MCWD_Long_future - MCWD_historical) %>%
  mutate(seasonality = case_when(Delta_MCWD < 0 ~ "More",
                                 TRUE ~ "Less"))
cdf.transitions %>%
  filter(LC %in% c(1,2,3)) %>%
  filter(model == model2analyze,
         scenario == scenario2analyze) %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  dplyr::select(LC.pred_historical,LC.pred_Long_future,area,land.frac) %>%
  group_by(LC.pred_historical,LC.pred_Long_future) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep")

cdf.transitions %>%
  filter(lon >= -15, lon <= 60,
         lat >= -15, lat <= 10) %>%
  # filter(LC %in% c(1,2,3)) %>%
  filter(model == model2analyze,
         scenario == scenario2analyze) %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  group_by(LC) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep")

saveRDS(cdf.transitions,
        "./outputs/All.Transitions.timing.RDS")

cdf.transitions %>%
  filter(model == model2analyze,
         scenario == scenario2analyze) %>%
  filter(LC == 2) %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  group_by(seasonality) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep")
0.8/(0.8 + 1.4)
1.4/(0.8 + 1.4)

cdf <- all.CMIP6.MCWD %>%
  ungroup() %>%
  filter(LC %in% c(1,2,3)) %>%
  left_join(cdf.transitions %>%
              ungroup() %>%
              dplyr::select(scenario,model,lon,lat,seasonality,change),
            by = c("scenario","model","lon","lat")) %>%
  filter(period %in% c("Long_future","historical"))

all.CMIP6.change <- cdf %>%
  group_by(period,model,lon,lat,scenario) %>%
  mutate(MAP = sum(pre,na.rm = TRUE),
         .groups = "keep") %>%
  ungroup() %>%
  group_by(LC,model,scenario,period) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),
            .groups = "keep")

all.CMIP6.change.delta <- all.CMIP6.change %>%
  ungroup() %>%
  filter(period != "historical") %>%
  left_join(all.CMIP6.change %>%
              filter(period == "historical") %>%
              ungroup() %>%
              dplyr::select(-c(period)),
            by = c("LC","model","scenario"))

all.CMIP6.change.seas <- cdf %>%
  group_by(period,model,lon,lat,scenario,seasonality,change) %>%
  mutate(MAP = sum(pre,na.rm = TRUE),
         .groups = "keep") %>%
  ungroup() %>%
  group_by(LC,model,scenario,period,seasonality) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),
            .groups = "keep")

all.CMIP6.change.delta.seas <- all.CMIP6.change.seas %>%
  ungroup() %>%
  filter(period != "historical") %>%
  left_join(all.CMIP6.change.seas %>%
              filter(period == "historical") %>%
              ungroup() %>%
              dplyr::select(-c(period)),
            by = c("LC","model","scenario","seasonality"))

Clim.Mask.MCWD.sum <- readRDS("./outputs/Summary.climate.RDS") %>%
  filter(model == "MEM") %>%
  filter(LC %in% c(1,2,3))

MEM.climate <- Clim.Mask.MCWD.sum %>%
  group_by(LC) %>%
  summarise(MAP.m = median(MAP,na.rm = TRUE),
            MCWD.m = median(MCWD,na.rm = TRUE),
            .groups = "keep")


all.CMIP6.change.delta.DB <- all.CMIP6.change.delta %>%
  left_join(MEM.climate %>%
              mutate(LC = as.numeric(LC)),
            by = 'LC') %>%
  mutate(bias.MCWD =  (MCWD.m.y - MCWD.m),
         bias.MAP = (MAP.m.y - MAP.m),
         Delta.MCWD = (MCWD.m.x - MCWD.m.y) ,
         Delta.MAP = (MAP.m.x - MAP.m.y))

all.CMIP6.change.delta.DB.sum <-
  all.CMIP6.change.delta.DB %>%
  group_by(LC,scenario) %>%
  summarise(MAP.m.x = mean(MAP.m.x,na.rm = TRUE),
            MAP.m.y = mean(MAP.m.y,na.rm = TRUE),

            MCWD.m.x = mean(MCWD.m.x,na.rm = TRUE),
            MCWD.m.y = mean(MCWD.m.y,na.rm = TRUE),

            .groups = "keep") %>%
  ungroup() %>%

  left_join(MEM.climate %>%
              mutate(LC = as.numeric(LC)),
            by = 'LC') %>%
  mutate(bias.MCWD =  (MCWD.m.y - MCWD.m),
         bias.MAP = (MAP.m.y - MAP.m),

         Delta.MCWD = (MCWD.m.x - MCWD.m.y) ,
         Delta.MAP = (MAP.m.x - MAP.m.y))

all.CMIP6.change.delta.seas.DB <- all.CMIP6.change.delta.seas %>%
  left_join(MEM.climate %>%
              mutate(LC = as.numeric(LC)),
            by = 'LC') %>%
  mutate(bias.MCWD =  (MCWD.m.y - MCWD.m),
         bias.MAP = (MAP.m.y - MAP.m),
         Delta.MCWD = (MCWD.m.x - MCWD.m.y) ,
         Delta.MAP = (MAP.m.x - MAP.m.y))


all.CMIP6.change.delta.seas.DB.sum <-
  all.CMIP6.change.delta.seas.DB %>%
  group_by(LC,scenario) %>%
  summarise(MAP.m.x = mean(MAP.m.x,na.rm = TRUE),
            MAP.m.y = mean(MAP.m.y,na.rm = TRUE),

            MCWD.m.x = mean(MCWD.m.x,na.rm = TRUE),
            MCWD.m.y = mean(MCWD.m.y,na.rm = TRUE),

            .groups = "keep") %>%
  ungroup() %>%

  left_join(MEM.climate %>%
              mutate(LC = as.numeric(LC)),
            by = 'LC') %>%
  mutate(bias.MCWD =  (MCWD.m.y - MCWD.m),
         bias.MAP = (MAP.m.y - MAP.m),

         Delta.MCWD = (MCWD.m.x - MCWD.m.y) ,
         Delta.MAP = (MAP.m.x - MAP.m.y))


df.numbers <- all.CMIP6.change.delta %>%
  left_join(MEM.climate,
            by = c("LC"))

df.numbers %>%
  filter(scenario == "ssp245",
         model == "MEM") %>%
  mutate(bias.MAP = MAP.m.x - MAP.m,
         bias.MCWD = MCWD.m.x - MCWD.m) %>%
  dplyr::select(LC,model,scenario,bias.MAP,bias.MCWD)

df.numbers %>%
  filter(scenario == "ssp245",
         model != "MEM") %>%
  mutate(delta_MAP = MAP.m.x - MAP.m.y,
         delta_MCWD = MCWD.m.x - MCWD.m.y) %>%
  group_by(LC) %>%
  summarise(Nneg = length(which(delta_MCWD < 0)),
            Npos = length(which(delta_MCWD >= 0)),

            Nneg.MAP = length(which(delta_MAP < 0)),
            Npos.MAP = length(which(delta_MAP >= 0)),

            .groups = "keep")

df.numbers %>%
  filter(scenario == "ssp245",
         model != "MEM") %>%
  mutate(delta_MAP = MAP.m.x - MAP.m.y,
         delta_MCWD = MCWD.m.x - MCWD.m.y) %>%
  filter(LC == 3,
         delta_MCWD < 0) %>%
  pull(delta_MCWD) %>%
  summary()

df.numbers %>%
  filter(scenario == "ssp245",
         model == "MEM") %>%
  mutate(delta_MAP = MAP.m.x - MAP.m.y,
         delta_MCWD = MCWD.m.x - MCWD.m.y) %>%
  dplyr::select(LC,model,scenario,delta_MAP,delta_MCWD)

df.numbers %>%
  filter(scenario == "ssp245") %>%
  mutate(delta_MAP = MAP.m.x - MAP.m.y,
         delta_MCWD = MCWD.m.x - MCWD.m.y) %>%
  group_by(LC) %>%
  summarise(
    av.MAP = mean(delta_MAP,na.rm = TRUE),
    min.MAP = min(delta_MAP,na.rm = TRUE),
    max.MAP = max(delta_MAP,na.rm = TRUE),

    # model.max = model[delta_MAP == max(delta_MAP)],

    av.MCWD = mean(delta_MCWD,na.rm = TRUE),
    min.MCWD = min(delta_MCWD,na.rm = TRUE),
    max.MCWD = max(delta_MCWD,na.rm = TRUE),

    .groups = "keep")

df.numbers %>%
  filter(scenario == "ssp585") %>%
  mutate(delta_MAP = MAP.m.x - MAP.m.y,
         delta_MCWD = MCWD.m.x - MCWD.m.y) %>%
  group_by(LC) %>%
  summarise(
    av.MAP = mean(delta_MAP,na.rm = TRUE),
    min.MAP = min(delta_MAP,na.rm = TRUE),
    max.MAP = max(delta_MAP,na.rm = TRUE),

    # model.max = model[delta_MAP == max(delta_MAP)],

    av.MCWD = mean(delta_MCWD,na.rm = TRUE),
    min.MCWD = min(delta_MCWD,na.rm = TRUE),
    max.MCWD = max(delta_MCWD,na.rm = TRUE),

    .groups = "keep")


ggplot() +

  # geom_rect(data = threshold.sum.MCWD,
  #           aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf),
  #           fill = "grey", alpha = 0.25) +
  # geom_rect(data = threshold.sum.MAP,
  #           aes(ymin = min, ymax = max, xmin = -Inf, xmax = Inf),
  #           fill = "grey", alpha = 0.25) +
  geom_vline(xintercept = MCWD.threshold, linetype = 1,
             color = "grey17") +
  geom_hline(yintercept = MAP.threshold, linetype = 1,
             color = "grey17") +

  stat_density_2d(data = df2plot,
                  aes(x = MCWD, y = MAP,
                      fill = as.factor(LC.pred),
                      alpha = ..level..),
                  geom = "polygon", contour = TRUE, color = NA, bins = 6) +
  scale_alpha(range = c(0.1, 0.4)) +
  geom_segment(data = all.CMIP6.change.delta %>%
                 filter(scenario == scenario2analyze),
               aes(x = MCWD.m.y, y = MAP.m.y, xend = MCWD.m.x, yend = MAP.m.x,
                   color = as.factor(LC)),
               arrow = arrow(length = unit(0.1, "cm")),
               size = 0.2) +

  geom_segment(data = all.CMIP6.change.delta %>%
                 filter(model == "MEM",
                        LC != 2,
                        scenario == scenario2analyze),
               aes(x = MCWD.m.y, y = MAP.m.y, xend = MCWD.m.x, yend = MAP.m.x,
                   color = as.factor(LC)),
               arrow = arrow(length = unit(0.1, "cm")),
               size = 1) +

  geom_segment(data = all.CMIP6.change.delta %>%
                 filter(model == "MEM",
                        scenario == scenario2analyze,
                        LC == 2),
               aes(x = MCWD.m.y, y = MAP.m.y, xend = MCWD.m.x, yend = MAP.m.x,
                   color = as.factor(LC)),
               arrow = arrow(length = unit(0.1, "cm")),
               size = 0.8) +

  geom_point(data = MEM.climate,
             aes(x = MCWD.m, y = MAP.m, color = as.factor(LC))) +

  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  labs(x = "",y ="") +
  scale_x_continuous(limits = c(-1800,0)) +
  scale_y_continuous(limits = c(0,3000)) +
  theme_bw() +
  guides(color = "none",fill = "none", alpha = "none") +
  theme(text = element_text(size = 20))



ggplot() +
  geom_segment(data = all.CMIP6.change.delta.DB,
               aes(x = MCWD.m, y = MAP.m,
                   xend = MCWD.m + Delta.MCWD, yend = MAP.m +Delta.MAP,
                   color = as.factor(LC)),
               arrow = arrow(length = unit(0.1, "cm")),
               size = 0.2) +

  geom_segment(data = all.CMIP6.change.delta.DB %>%
                 filter(model == "MEM"),
               aes(x = MCWD.m, y = MAP.m,
                   xend = MCWD.m + Delta.MCWD, yend = MAP.m +Delta.MAP,
                   color = as.factor(LC)),
               arrow = arrow(length = unit(0.1, "cm")),
               size = 1) +

  geom_point(data = MEM.climate,
             aes(x = MCWD.m, y = MAP.m, color = as.factor(LC))) +

  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  geom_vline(xintercept = -450,linetype = 2) +
  geom_hline(yintercept = 1000,linetype = 2) +
  facet_grid(~ scenario) +
  labs(x = "",y ="") +
  theme_bw() +
  guides(color = "none") +
  theme(text = element_text(20))

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/GridArea.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Sensitivity.thresholds.sum.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/data/LC_Congo.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/data
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Summary.climate.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Figure_transitions.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

