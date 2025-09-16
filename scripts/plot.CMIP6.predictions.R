rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)
library(sf)
library(SPEI)

system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.pr.RDS",
              "./outputs/"))

system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.tas.RDS",
              "./outputs/"))

################################################################################
thresholds <-
  data.frame(basin = c("Amazon","Congo"),
             MCWD = c(-300,-250),
             MAP = c(1500,1000))

biomes <- readRDS("~/Documents/projects/Congo.ED2/outputs/biome.ERA5.1940.2023_global.RDS") %>%
  filter(model == "ORCHIDEE") %>%
  filter(abs(lat) <= 30) %>%
  mutate(basin = case_when(lon > -85 & lon < -30 ~ "Amazon",
                           lon > -15 & lon <= 45 ~ "Congo",
                           TRUE ~ "Else")) %>%
  filter(basin != "Else") %>%
  ungroup() %>%
  mutate(MCWD.threshold = thresholds$MCWD[match(basin,thresholds$basin)],
         MAP.threshold = thresholds$MAP[match(basin,thresholds$basin)]) %>%
  mutate(type = case_when(MCWD >= MCWD.threshold ~ 2,
                          MAP <= MAP.threshold ~ 1,
                          TRUE ~ 3)) %>%
  mutate(MAT = MAT - 273.15)


biomes.sum <- biomes %>%
  group_by(basin,type) %>%
  summarise(MCWD = mean(MCWD,na.rm = TRUE),
            MAT = mean(MAT,na.rm = TRUE),
            MAP = mean(MAP,na.rm = TRUE),
            lat = mean(lat),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")

ggplot() +
  geom_raster(data = biomes,
              aes(x = lon, y = lat,
                  fill = MAP)) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +
  scale_fill_continuous(limits = c(0,1500),
                        oob = scales::squish) +
  coord_sf(xlim = c(-90, 50), ylim = c(-30, 20), expand = FALSE) +
  theme_bw()

ggplot() +

  geom_raster(data = biomes,
              aes(x = lon, y = lat,
                  fill = as.factor(type))) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  coord_sf(xlim = c(-90, 50), ylim = c(-30, 20), expand = FALSE) +
  theme_bw()


################################################################################

CMIP6.pr.raw <- readRDS("./outputs/CMIP6.pr.RDS") %>%
  filter(!is.na(basin))
CMIP6.tas.raw <- readRDS("./outputs/CMIP6.tas.RDS") %>%
  mutate(value.m = value.m - 273.15) %>%
  filter(!is.na(basin))


CMIP6.pr <- CMIP6.pr.raw %>%
  mutate(Pmm = value.m*86400*365/12) %>%
  dplyr::select(-c(value.m,var)) %>%
  left_join(CMIP6.tas.raw %>%
              dplyr::select(-var),
            by = c("year","month","basin",
                   "type","scenario","model")) %>%
  filter(year %in% c(1900:2014,
                     2070:2100)) %>%
  left_join(biomes.sum,
            by = c("basin","type"))

CMIP6.MCWD <- CMIP6.pr %>%
  group_by(basin,type,scenario,model,month) %>%
  summarise(Pmm = mean(Pmm),
            lat = mean(lat),
            value.m = mean(value.m),
            .groups = "keep") %>%
  group_by(basin,type,scenario,model) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = pmin(6,
                  SPEI::thornthwaite(value.m,
                                     unique(lat),
                                     na.rm = TRUE)/Ndays),
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD),
         MAP = sum(Pmm))

CMIP6.MCWD.wide <- CMIP6.MCWD %>%
  ungroup() %>%
  filter(month == 1) %>%
  dplyr::select(basin,type,model,scenario,MAP,MCWD) %>%
  pivot_wider(names_from = scenario,
              values_from = c(MAP,MCWD))


ggplot() +

  geom_point(data = CMIP6.MCWD %>%
               filter(scenario == "historical",
                      basin %in% c("Amazon","Congo")),
             aes(x = MCWD,y = MAP,
                 color = as.factor(type)),
             size = 0.5) +

  geom_segment(data = CMIP6.MCWD.wide %>%
                 filter(basin %in% c("Amazon","Congo")),
               aes(x = MCWD_historical,y = MAP_historical,
                   xend = MCWD_ssp585, yend = MAP_ssp585,
                 color = as.factor(type)),
               arrow = arrow(length = unit(0.1, "cm"))) +

  geom_point(data = biomes.sum %>%
               filter(basin %in% c("Amazon","Congo")),
             aes(x = MCWD,y = MAP,
                 color = as.factor(type)),
             size = 2) +

  geom_hline(data = thresholds,
             aes(yintercept = MAP), linetype = 2) +
  geom_vline(data = thresholds,
             aes(xintercept = MCWD), linetype = 2) +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  scale_color_manual(values = c("#c49402","#005401","#448704")) +
  facet_wrap(~ basin) +
  theme_bw()

################################################################################

CMIP6.all <- bind_rows(CMIP6.pr %>%
                         dplyr::select(-Pmm),
                        CMIP6.tas) %>%
  pivot_wider(names_from = var,
              values_from = value.m) %>%
  mutate(pr = pr*86400*365/12) %>%
  group_by(basin,type,scenario,model,year) %>%
  summarise(MAP = sum(pr,na.rm = TRUE),
            MAT = mean(tas,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(basin,type,scenario,model) %>%
  summarise(MAP = mean(MAP,na.rm = TRUE),
            MAT = mean(MAT,na.rm = TRUE),
            .groups = "keep")

CMIP6.all.wide <- CMIP6.all %>%
  ungroup() %>%
  dplyr::select(basin,type,model,scenario,MAP,MAT) %>%
  pivot_wider(names_from = scenario,
              values_from = c(MAP,MAT))

CMIP6.all.wide.sum <- CMIP6.all %>%
  group_by(basin,type,scenario) %>%
  summarise(MAP = mean(MAP,na.rm = TRUE),
            MAT = mean(MAT,na.rm = TRUE),
            .groups = "keep") %>%
  dplyr::select(basin,type,scenario,MAP,MAT) %>%
  pivot_wider(names_from = scenario,
              values_from = c(MAP,MAT))

ggplot() +

  # geom_polygon(data = plotbiomes::Whittaker_biomes,
  #              aes(x    = temp_c,
  #                  y    = precp_cm*10,
  #                  group = biome),
  #              fill = NA,
  #              colour = "grey", # colour of polygon border
  #              size   = 0.5,
  #              alpha = 0.7) +    # thickness of polygon border

  geom_point(data = CMIP6.all %>%
               filter(scenario == "historical",
                      basin %in% c("Amazon","Congo")),
             aes(x = MAT,y = MAP,
                 color = as.factor(type)),
             size = 0.5) +

  geom_segment(data = CMIP6.all.wide.sum %>%
                 filter(basin %in% c("Amazon","Congo")),
               aes(x = MAT_historical,y = MAP_historical,
                   xend = MAT_ssp245, yend = MAP_ssp245,
                   color = as.factor(type)),
               arrow = arrow(length = unit(0.2, "cm"))) +

  geom_point(data = biomes.sum %>%
               filter(basin %in% c("Amazon","Congo")),
             aes(x = MAT,y = MAP,
                 color = as.factor(type)),
             size = 2) +
  scale_fill_manual(values = c("#005401","#c49402","#448704")) +
  scale_color_manual(values = c("#005401","#c49402","#448704")) +
  facet_wrap(~ basin) +
  theme_bw()
