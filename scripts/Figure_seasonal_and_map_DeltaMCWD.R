rm(list = ls())

library(dplyr)
library(ggplot2)
library(YGB)
library(raster)
library(terra)
library(tidyr)
library(sf)
library(ggdist)

LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))
rainforests <- read_sf("./data/Rainforests_CMIP6.shp")


all.CMIP6.MCWD <- readRDS("./outputs/All.CMIP6.states.MEM.RDS") %>%
  ungroup() %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  dplyr::select(-LC) %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(!(model %in% c("NESM3","CIESM")))


seasonality <- all.CMIP6.MCWD %>%
  group_by(scenario,period,model,lon,lat) %>%
  summarise(MAP = sum(pre),
            MCWD = unique(MCWD),
            LC = unique(LC),
            .groups = "keep") %>%
  ungroup() %>%
  dplyr::select(model,period,LC,scenario,lon,lat,MCWD,MAP) %>%
  pivot_wider(names_from = period,
              values_from = c(MCWD,MAP))

lats.lons <- seasonality %>%
  ungroup() %>%
  mutate(lat.lon = paste0(lat,".",lon)) %>%
  pull(lat.lon) %>%
  unique()

seasonality.change <- seasonality %>%
  mutate(Delta_MCWD = MCWD_scenario - MCWD_historical,
         Delta_MAP = MAP_scenario - MAP_historical) %>%
  mutate(seasonality = case_when(Delta_MCWD < 0 ~ "More",
                                 TRUE ~ "Less")) %>%
  na.omit()

seasonality.change.select <- seasonality.change %>%
  dplyr::select(model,LC,scenario,lon,lat,seasonality,Delta_MCWD)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

segments <- expand.grid(
  scenario = unique(seasonality.change.select$scenario),
  LC = c(1, 2, 3)) %>%
  mutate(x = 5, xend = 47,
         y = 0, yend = 0)


threshold <- 0.7

all.lats <- sort(unique(seasonality.change.select$lat))
all.lons <- sort(unique(seasonality.change.select$lon))
direction <- seasonality.change.select %>%
  filter(lon %in% all.lons[seq(1,length(all.lons),1)],
         lat %in% all.lats[seq(1,length(all.lats),1)]) %>%
  mutate(sign.Delta = case_when(Delta_MCWD == 0 ~ 1,
                                TRUE ~ sign(Delta_MCWD))) %>%
  group_by(scenario,lon,lat) %>%
  summarise(mode = modal(sign.Delta),
            N = length(sign.Delta),
            agreement = sum(sign.Delta == mode) >=
              threshold*N,
            .groups = "keep")

model2plot <- "MEM"
# model2plot <- unique(seasonality.change.select$model)[38]
# model2plot <- "E3SM-1-1"



map2plot <- seasonality.change.select %>%
  filter(LC %in% c(1:3),
         scenario %in% c("ssp245","ssp585"),
         scenario != "ssp534-over",
         model == model2plot) %>%
  complete(model = model2plot,
           scenario = c("ssp245","ssp585"),
           lon = all.lons,
           lat = all.lats,

           fill = list(Delta_MCWD = NA,
                       LC = NA,
                       seasonality = NA_character_)) %>%
  mutate(lat.lon = paste0(lat,".",lon)) %>%
  filter(lat.lon %in% lats.lons)

ggplot() +
  geom_raster(data = map2plot,
              aes(x = lon, y = lat,
                  fill = Delta_MCWD)) +
  geom_point(data = direction %>%
               left_join(LC,
                         by = c("lon","lat")) %>%
               filter(agreement,
                      LC %in% c(1,2,3),
                      scenario %in% c("ssp245","ssp585"),
                      scenario != "ssp534-over"),
             aes(x = lon, y = lat),
             shape = 20,
             size = 0.01) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1)*300,
                       oob = scales::squish,na.value = "grey",
                       breaks = c(-1,0,1)*300) +
  geom_segment(data = segments %>%
                 filter(scenario != "ssp534-over",
                        scenario %in% c("ssp245","ssp585")),
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               inherit.aes = FALSE, linetype = 2,
               color = "grey12") +
  facet_wrap(~ scenario, nrow = 2) +
  theme_map() +
  labs(x = "",y = "", fill = "") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20))

models <- sort(unique(seasonality.change.select$model))
models <- models[models != "MEM"]

df2plot <- seasonality.change.select %>%
  filter(scenario != "ssp534-over") %>%
  filter(model %in% c("MEM",models)) %>%
  filter(LC %in% c(2)) %>%
  mutate(hemisph = case_when(lat<0 ~ "S",
                             TRUE ~ "N")) %>%
  mutate(scenario = factor(scenario,
                           levels = rev(paste0("ssp",
                                           c("126","245","370","585")))),
         hemisph = factor(hemisph,
                           levels = rev(c("N","S")))) %>%
  mutate(model.type = case_when(model == "MEM" ~ "MEM",
                                TRUE ~ "Other"))
df2plot.sum <- df2plot %>%
  group_by(LC,hemisph,scenario,model) %>%
  summarise(Delta_MCWD.m = mean(Delta_MCWD,na.rm = TRUE),
            .groups = "keep")

ggplot(df2plot,
       aes(
         x = Delta_MCWD,
         y = scenario,
         color = hemisph,
         fill = hemisph
       )) +
  stat_pointinterval(
    .width = c(0.7, 0.95),
    # 50%, 80%, 95% intervals
    point_interval = median_qi,
    position = position_dodge(width = 0.5)
  ) +
  # facet_wrap(~ scenario, nrow = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  facet_wrap( ~ model.type) +
  theme(
  axis.title.y = element_blank(),
  strip.background = element_blank(),
  strip.text = element_blank(),
  # legend.position = "none",
  text = element_text(size = 20)
) +
  labs(x = "", y = "")

ggplot() +
  geom_raster(data = seasonality.change.select %>%
                mutate(model = factor(model,
                                       levels = c(models,"MEM"))) %>%
                filter(LC %in% c(1:3),
                       scenario == "ssp245"),
              aes(x = lon, y = lat,
                  fill = Delta_MCWD)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1)*300,
                       oob = scales::squish,
                       breaks = c(-1,0,1)*300) +
  geom_segment(data = segments,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               inherit.aes = FALSE, linetype = 2,
               color = "grey12") +
  geom_segment(y = 0, yend = 0, x = -5, xend = 50,
               color = "black") +
  facet_wrap(~ model) +
  theme_map() +
  theme(legend.position = "bottom",
        text = element_text(size = 12))

ggplot() +
  geom_raster(data = seasonality.change.select %>%
                filter(LC %in% c(1:3),
                       scenario == "ssp585"),
              aes(x = lon, y = lat,
                  fill = Delta_MCWD)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1)*300,
                       oob = scales::squish) +
  geom_segment(data = segments,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               inherit.aes = FALSE, linetype = 2,
               color = "grey12") +
  geom_segment(y = 0, yend = 0, x = -5, xend = 50,
               color = "black") +
  facet_wrap(~ model) +
  theme_map() +
  theme(legend.position = "bottom",
        text = element_text(size = 12))

seasonality.change %>%
  filter(LC %in% c(1,2,3)) %>%
  group_by(LC,seasonality,scenario) %>%
  summarise(N = n(),
            m = median(Delta_MCWD,na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(names_from = seasonality,
              values_from = c(N,m)) %>%
  mutate(frac = N_More/(N_Less + N_More))

################################################################################
# Plot seasonality

all.CMIP6.MCWD.seasonality <- all.CMIP6.MCWD %>%
  ungroup() %>%
  left_join(seasonality.change.select %>%
              dplyr::select(model,scenario,lon,lat,seasonality,Delta_MCWD) %>%
              distinct(),
            by = c("model","scenario","lon","lat"))

all.CMIP6.MCWD.seasonality.sum <- all.CMIP6.MCWD.seasonality %>%
  group_by(model,scenario,seasonality,period,LC,month) %>%
  summarise(pre.m = mean(pre,na.rm = TRUE),
            Etot.m = mean(Etot,na.rm = TRUE),
            .groups = "keep")

ggplot(data = all.CMIP6.MCWD.seasonality.sum %>%
         filter(model == "MEM",
                LC %in% c(1,2,3))) +
  geom_line(aes(x = month,
                y = pre.m,
                color = scenario,
                linetype = period)) +
  facet_grid(LC ~ seasonality,scales = "free") +
  theme_bw()

all.CMIP6.MCWD.seasonality.sum.diff <-
  all.CMIP6.MCWD.seasonality.sum %>%
  mutate(water_deficit = Etot.m - pre.m) %>%
  pivot_wider(values_from = c("pre.m","Etot.m","water_deficit"),
              names_from = period) %>%

  mutate(diff_pre = pre.m_scenario - pre.m_historical,
         diff_Etot = Etot.m_scenario - Etot.m_historical,
         diff_WD = water_deficit_scenario - water_deficit_historical)


ggplot(data = all.CMIP6.MCWD.seasonality.sum.diff %>%
         filter(model == "MEM",
                LC %in% c(1,2,3))) +
  geom_line(aes(x = month,
                y = diff_pre,
                color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey12") +
  facet_grid(LC ~ seasonality,scales = "free") +
  theme_bw()

ggplot(data = all.CMIP6.MCWD.seasonality.sum.diff %>%
         filter(model == "MEM",
                LC %in% c(1,2,3))) +
  geom_line(aes(x = month,
                y = diff_Etot,
                color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey12") +
  facet_grid(LC ~ seasonality,scales = "free") +
  theme_bw()

ggplot(data = all.CMIP6.MCWD.seasonality.sum.diff %>%
         filter(model == "MEM",
                LC %in% c(1,2,3))) +
  geom_line(aes(x = month,
                y = 100*diff_WD/Etot.m_historical,
                color = scenario)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey12") +
  facet_grid(LC ~ seasonality) +
  theme_bw()
