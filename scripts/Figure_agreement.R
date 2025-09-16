rm(list = ls())

library(cowplot)
library(dplyr)
library(sf)
library(ggplot2)
library(caret)

GridArea <- readRDS("./outputs/GridArea.RDS")

LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(LC = case_when(LC %in% c(1,2,3) ~ LC,
                        TRUE ~ NA)) %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

as.data.frame(LC) %>%
  ungroup() %>%
  filter(LC == 2) %>%
  mutate(N = n()) %>%
  filter(abs(lat) < 5) %>%
  distinct() %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")


LC %>%
  ungroup() %>%
  filter(LC == 3) %>%
  mutate(N = n()) %>%
  filter((lat > 5 & lat < 10)) %>%
  distinct() %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")


as.data.frame(LC) %>%
  ungroup() %>%
  filter(LC == 3) %>%
  mutate(N = n()) %>%
  filter((lat < -5)) %>%
  distinct() %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")

LC %>%
  ungroup() %>%
  filter(LC == 1) %>%
  mutate(N = n()) %>%
  filter(lon > 25, lon <= 55) %>%
  distinct() %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")

LC.pred.MEM.reanal <- readRDS("./outputs/Summary.climate.RDS") %>%
  filter(model == "MEM") %>%
  mutate(LC.pred.RA = as.factor(case_when(MAP < MAP.threshold ~ 1,
                                          MCWD > MCWD.threshold ~ 2,
                                          TRUE ~ 3))) %>%
  mutate(LC.pred.RA = factor(LC.pred.RA,
                             levels = c(1,2,3))) %>%
  ungroup() %>%
  dplyr::select(lon,lat,LC.pred.RA) %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

LC.pred.MEM.CMIP6 <- readRDS("./outputs/LC.pred.MEM.CMIP6.RDS") %>%
  filter(model == "MEM",
         scenario == "ssp245") %>%
  dplyr::select(lon,lat,LC.pred) %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  mutate(LC.pred = factor(LC.pred,
                             levels = c(1,2,3)))

all <- LC %>%
  left_join(LC.pred.MEM.CMIP6,
            by = c("lon","lat")) %>%
  left_join(LC.pred.MEM.reanal,
            by = c("lon","lat")) %>%
  mutate(LC.pred = case_when(is.na(LC) ~ NA,
                             TRUE ~ LC.pred),
         LC.pred.RA = case_when(is.na(LC) ~ NA,
                                TRUE ~ LC.pred.RA)) %>%
  left_join(GridArea,
            by = c("lon","lat"))

rainforests <- read_sf("./data/Rainforests.shp")
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

obs <- ggplot() +
  geom_raster(data = all,
              aes(x = lon, y = lat,
                  fill = as.factor(LC))) +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  guides(fill = "none")

reanal <- ggplot() +
  geom_raster(data = all,
              aes(x = lon, y = lat,
                  fill = as.factor(LC.pred.RA))) +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  guides(fill = "none")


CMIP6 <- ggplot() +
  geom_raster(data = all,
              aes(x = lon, y = lat,
                  fill = as.factor(LC.pred))) +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  guides(fill = "none")

obs.vs.reanal <- ggplot() +
  geom_raster(data = all,
              aes(x = lon, y = lat,
                  fill = LC == LC.pred.RA),
              alpha = 0.8) +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values =rev(c("darkblue","darkred")),
                    na.value = "grey") +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  guides(fill = "none")

all %>%
  mutate(agreement = (LC == LC.pred.RA)) %>%
  group_by(agreement) %>%
  summarise(area = sum(area*land.frac)/1e12,
            .groups = "keep")

obs.vs.CMIP6 <- ggplot() +
  geom_raster(data = all,
              aes(x = lon, y = lat,
                  fill = LC == LC.pred),
              alpha = 0.8) +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values =rev(c("darkblue","darkred")),
                    na.value = "grey") +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  guides(fill = "none")

all %>%
  mutate(agreement = (LC == LC.pred)) %>%
  group_by(agreement) %>%
  summarise(area = sum(area*land.frac)/1e12,
            .groups = "keep")

CMIP6.vs.reanal <- ggplot() +
  geom_raster(data = all,
              aes(x = lon, y = lat,
                  fill = LC.pred.RA == LC.pred),
              alpha = 0.8) +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values =rev(c("darkblue","darkred")),
                    na.value = "grey") +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  guides(fill = "none")

all %>%
  mutate(agreement = (LC.pred.RA == LC.pred)) %>%
  group_by(agreement) %>%
  summarise(area = sum(area*land.frac)/1e12,
            .groups = "keep")


plot_grid(obs, NULL, NULL,
          obs.vs.reanal,reanal,NULL,
          obs.vs.CMIP6,CMIP6.vs.reanal,CMIP6,
          ncol = 3, align = "hv",
          rel_widths = c(1,1,1), rel_heights = c(1,1,1))

all <- all %>%
  filter(LC %in% c(1,2,3)) %>%
  mutate(LC = factor(LC,
                     levels = c(1,2,3)),
         LC.pred.RA = factor(LC.pred.RA,
                     levels = c(1,2,3)),
         LC.pred = factor(LC.pred,
                     levels = c(1,2,3)))

# Observed vs Reanalysis
Acc = confusionMatrix(all$LC.pred.RA,all$LC)[["overall"]][1]
as.matrix(confusionMatrix(all$LC.pred.RA,all$LC)[["byClass"]])[,"Precision"]

df2test.Mat <- all %>%
  filter(LC %in% c(1,2,3),
         LC.pred.RA %in% c(1,2,3)) %>%

  mutate(LC = factor(LC,levels = c(2,3,1)),
         LC.pred.RA = factor(LC.pred.RA,levels = c(2,3,1))) %>%

  # filter(lat < 0) %>%
  group_by(LC,LC.pred.RA) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep") %>%
  mutate(lab = paste("N =",
                     N),
         lab2 = paste(signif(area,2)))

ggplot(data = df2test.Mat,
       aes(x = as.factor(LC.pred.RA),
           y = factor(1 + 3 - as.numeric(LC)))) +
  geom_tile(aes(fill = area),
            color = "black",
            size = 0.5) +

  scale_fill_gradient(low = "white",
                      high = "grey32") +
  geom_label(aes(label = lab2), fill = NA,
             label.size = NA) +
  theme_minimal() +
  scale_x_discrete(breaks = c()) +
  scale_y_discrete(breaks = c()) +
  guides(fill = "none") +
  coord_equal() +
  labs(x="",y = "") +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(angle = 360-45, vjust = 0, hjust=1),
        panel.grid = element_blank())


# Observed vs CMIP6
Acc = confusionMatrix(all$LC.pred,all$LC)[["overall"]][1]
as.matrix(confusionMatrix(all$LC.pred,all$LC)[["byClass"]])[,"Precision"]

df2test.Mat <- all %>%
  filter(LC %in% c(1,2,3),
         LC.pred %in% c(1,2,3)) %>%

  mutate(LC = factor(LC,levels = c(2,3,1)),
         LC.pred = factor(LC.pred,levels = c(2,3,1))) %>%

  # filter(lat < 0) %>%
  group_by(LC,LC.pred) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep") %>%
  mutate(lab = paste("N =",
                     N),
         lab2 = paste(signif(area,2)))

ggplot(data = df2test.Mat,
       aes(x = as.factor(LC.pred),
           y = factor(1 + 3 - as.numeric(LC)))) +
  geom_tile(aes(fill = area),
            color = "black",
            size = 0.5) +

  scale_fill_gradient(low = "white",
                      high = "grey32") +
  geom_label(aes(label = lab2), fill = NA,
             label.size = NA) +
  theme_minimal() +
  scale_x_discrete(breaks = c()) +
  scale_y_discrete(breaks = c()) +
  guides(fill = "none") +
  coord_equal() +
  labs(x="",y = "") +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(angle = 360-45, vjust = 0, hjust=1),
        panel.grid = element_blank())



# CMIP6 vs Reanalysis
Acc = confusionMatrix(all$LC.pred,all$LC.pred.RA)[["overall"]][1]
as.matrix(confusionMatrix(all$LC.pred,all$LC.pred.RA)[["byClass"]])[,"Precision"]

df2test.Mat <- all %>%
  filter(LC.pred.RA %in% c(1,2,3),
         LC.pred %in% c(1,2,3)) %>%

  mutate(LC.pred.RA = factor(LC.pred.RA,levels = c(2,3,1)),
         LC.pred = factor(LC.pred,levels = c(2,3,1))) %>%

  # filter(lat < 0) %>%
  group_by(LC.pred.RA,LC.pred) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep") %>%
  mutate(lab = paste("N =",
                     N),
         lab2 = paste(signif(area,2)))

ggplot(data = df2test.Mat,
       aes(x = as.factor(LC.pred),
           y = factor(1 + 3 - as.numeric(LC.pred.RA)))) +
  geom_tile(aes(fill = area),
            color = "black",
            size = 0.5) +

  scale_fill_gradient(low = "white",
                      high = "grey32") +
  geom_label(aes(label = lab2), fill = NA,
             label.size = NA) +
  theme_minimal() +
  scale_x_discrete(breaks = c()) +
  scale_y_discrete(breaks = c()) +
  guides(fill = "none") +
  coord_equal() +
  labs(x="",y = "") +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(angle = 360-45, vjust = 0, hjust=1),
        panel.grid = element_blank())



