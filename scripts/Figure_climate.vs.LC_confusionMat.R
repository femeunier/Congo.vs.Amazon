rm(list = ls())

library(dplyr)
library(ggplot2)
library(caret)
library(raster)
library(cowplot)
library(sf)
library(reshape2)

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

Sensitivity.thresholds <- readRDS(
  "./outputs/Sensitivity.thresholds.RDS")

Clim.Mask.MCWD <- readRDS("./outputs/Climate.CA.summ.RDS") %>%
  group_by(model,lon,lat) %>%
  summarise(MAP = sum(pre,na.rm = TRUE),
            MCWD = unique(MCWD),
            LC = unique(LC),
            .groups = "keep")

Clim.Mask.MCWD.LC.sum.class <- Clim.Mask.MCWD %>%
  ungroup() %>%
  filter(LC %in% c(1:3)) %>%
  left_join(Sensitivity.thresholds %>%
              dplyr::select(-Acc),
            by = "model") %>%
  mutate(LC = as.factor(LC),
         LC.pred = as.factor(case_when(MCWD > MCWD.threshold ~ 2,
                                       MAP < MAP.threshold ~ 1,
                                       TRUE ~ 3)))

main <- ggplot(data = Clim.Mask.MCWD.LC.sum.class %>%
         filter(model == "MEM")) +
  geom_rect(data = threshold.sum.MCWD,
            aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.25) +
  geom_rect(data = threshold.sum.MAP,
            aes(ymin = min, ymax = max, xmin = -Inf, xmax = Inf),
            fill = "grey", alpha = 0.25) +
  geom_vline(xintercept = MCWD.threshold, linetype = 1,
             color = "grey17") +
  geom_hline(yintercept = MAP.threshold, linetype = 1,
             color = "grey17") +

  stat_density_2d(aes(x = MCWD, y = MAP,
                      fill = as.factor(LC), alpha = ..level..),
                  geom = "polygon", contour = TRUE, color = NA, bins = 6) +

  # geom_point(aes(x = MCWD,y = MAP,
  #                color = as.factor(LC)),
  #            size = 0.1) +
  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme_bw() +
  labs(x = "", y = "", fill = "", color = "") +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(limits = c(0,3000)) +
  scale_x_continuous(limits = c(-1800,0)) +
  guides(color = "none", alpha = "none", fill = "none")

main

side <- ggplot(data = Clim.Mask.MCWD.LC.sum.class %>%
                 filter(model == "MEM") %>%
         mutate(LC = factor(LC,levels = c(2,3,1)))) +
  geom_rect(data = threshold.sum.MAP,
            aes(ymin = min, ymax = max,xmin = -Inf, xmax = Inf),
            fill = "grey",
            alpha = 0.25) +
  scale_fill_manual(values = c("#005401","#448704","#c49402","grey")) +
  geom_boxplot(aes(x = as.factor(LC),
                   y = MAP,
                 fill = as.factor(LC)),
             size = 0.1) +
  geom_hline(yintercept = MAP.threshold, linetype = 1,
             color = "grey17") +
  theme_void() +
  labs(x = "", y = "", fill = "", color = "") +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(limits = c(0,3000)) +
  guides(color = "none", fill = "none")


top <- ggplot(data = Clim.Mask.MCWD.LC.sum.class %>%
                 filter(model == "MEM") %>%
                 mutate(LC = factor(LC,levels = c(2,3,1)))) +
  geom_rect(data = threshold.sum.MCWD,
            aes(ymin = min, ymax = max,xmin = -Inf, xmax = Inf),
            fill = "grey",
            alpha = 0.25) +
  geom_boxplot(aes(x = as.factor(LC),
                   y = MCWD,
                   fill = as.factor(LC)),
               size = 0.1) +
  scale_fill_manual(values = c("#005401","#448704","#c49402","grey")) +
  geom_hline(yintercept = MCWD.threshold, linetype = 1,
             color = "grey17") +
  theme_void() +
  labs(x = "", y = "", fill = "", color = "") +
  scale_y_continuous(limits = c(-1800,0)) +
  coord_flip() +

  theme(text = element_text(size = 20)) +
  guides(color = "none", fill = "none")

plot_grid(top, NULL, main, side, ncol = 2, align = "hv",
          rel_widths = c(2, 1), rel_heights = c(1, 2))

df2test <- Clim.Mask.MCWD.LC.sum.class %>%
  filter(model == "MEM") %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  mutate(LC = factor(LC,levels = c(2,3,1)),
         LC.pred = factor(LC.pred,levels = c(2,3,1)))

df2test.Mat <- df2test %>%
  # filter(lat < 0) %>%
  group_by(LC,LC.pred) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep") %>%
  mutate(lab = paste("N =",
                     N),
         lab2 = paste(signif(area,2)))

Tot.area <- sum(df2test.Mat$area)
correct.area <- df2test.Mat %>%
  filter(LC == LC.pred) %>%
  pull(area) %>%
  sum()

Tot.area.VT <- df2test.Mat %>%
  group_by(LC) %>%
  summarise(area = sum(area),
            .groups = "keep")

correct.area.VT <- df2test.Mat %>%
  filter(LC == LC.pred) %>%
  group_by(LC) %>%
  summarise(area = sum(area),
            .groups = "keep") %>%
  left_join(Tot.area.VT,
            by = "LC") %>%
  mutate(frac = area.x/area.y,
         missclassified = area.y - area.x)

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
  # scale_x_discrete(breaks = c(1,2,3),position = "top",
  #                  labels = c("Savannas","Rainforests","Seasonal forests")) +
  # scale_y_discrete(breaks = c(3,2,1),
  #                  labels = c("Rainforests","Seasonal forests","Savannas")) +
  scale_x_discrete(breaks = c()) +
  scale_y_discrete(breaks = c()) +
  guides(fill = "none") +
  coord_equal() +
  labs(x="",y = "") +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(angle = 360-45, vjust = 0, hjust=1),
        panel.grid = element_blank())

rainforests <- read_sf("./data/Rainforests.shp")
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

map2plot <- Clim.Mask.MCWD.LC.sum.class %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  filter(model == "MEM",
         !is.na(LC),
         land.frac > 0.25)

map2plot %>%
  group_by(LC.pred) %>%
  summarise(area = sum(land.frac*area)/1e12,
            .groups = "keep")

CM <- confusionMatrix(map2plot$LC.pred,
                      reference = map2plot$LC)

map2plot %>%
  group_by(LC,LC.pred) %>%
  summarise(N = n())

ggplot() +
  geom_raster(data = map2plot,
              aes(x = lon, y = lat,
                  fill = as.factor(LC.pred))) +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  guides(fill = "none")



ggplot() +
  geom_raster(data = map2plot %>%
                filter(LC != LC.pred),
              aes(x = lon, y = lat,
                  fill = as.factor(LC))) +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  guides(fill = "none")

################################################################################
#
# Clim.Mask.MCWD.LC.sum.class %>%
#   group_by(model) %>%
#   summarise(Acc = (confusionMatrix(LC.pred,LC))[["overall"]][1],
#             precision = mean(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
#                              na.rm = TRUE),
#             precision.min = min(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
#                                 na.rm = TRUE))
#
# Clim.Mask.MCWD.LC.sum.class.sum <- Clim.Mask.MCWD.LC.sum.class %>%
#   group_by(lon,lat) %>%
#   summarise(product.agreement = (length(unique(LC.pred)) == 1),
#             model.agreement = modal(LC.pred == unique(LC)),
#             LC = unique(LC),
#             Modal = as.numeric(names(sort(table(LC.pred),decreasing = TRUE)[1])),
#             Nmodal = sort(table(LC.pred),decreasing = TRUE)[1],
#             N = length(LC.pred),
#             .groups = "keep") %>%
#   mutate(frac = Nmodal/N)
#
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot() +
#   geom_raster(data = Clim.Mask.MCWD.LC.sum.class.sum,
#               aes(x = lon, y = lat,
#                   fill = as.factor(product.agreement))) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-15, 55),
#            ylim = c(-1, 1)*23.25,
#            expand = FALSE) +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 20))
#
# ggplot() +
#   geom_raster(data = Clim.Mask.MCWD.LC.sum.class.sum,
#               aes(x = lon, y = lat,
#                   fill = frac)) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-15, 55),
#            ylim = c(-1, 1)*23.25,
#            expand = FALSE) +
#   scale_fill_gradient(high = "white", low = "black",
#                       limits = c(0,1)) +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 20))
#
#
# ggplot() +
#   geom_raster(data = Clim.Mask.MCWD.LC.sum.class.sum,
#               aes(x = lon, y = lat,
#                   fill = as.factor(Modal))) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-15, 55),
#            ylim = c(-1, 1)*23.25,
#            expand = FALSE) +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 20))
#
#
# ggplot(data = Clim.Mask.MCWD.LC.sum %>%
#          filter(LC %in% c(1:3))) +
#   geom_density(aes(x = MAT - 273.15,fill = as.factor(LC)),
#                alpha = 0.5, color = NA) +
#   # facet_wrap(~ as.factor(LC)) +
#   theme_bw()
#
# ggplot(data = Clim.Mask.MCWD.LC.sum %>%
#          filter(LC %in% c(1:3))) +
#   geom_density(aes(x = MAP,fill = as.factor(LC)),
#                alpha = 0.5, color = NA) +
#   # facet_wrap(~ as.factor(LC)) +
#   theme_bw()
#
# ggplot(data = Clim.Mask.MCWD.LC.sum %>%
#          filter(LC %in% c(1:3))) +
#   geom_density(aes(x = MCWD,fill = as.factor(LC)),
#                alpha = 0.5, color = NA) +
#   # facet_wrap(~ as.factor(LC)) +
#   theme_bw()
#
#
# ggplot(data = Clim.Mask.MCWD.LC %>%
#          filter(LC %in% c(1:3))) +
#   geom_line(aes(x = month, y = tas - 273.15,
#                 color = model)) +
#   geom_line(aes(x = month, y = tasmin - 273.15,
#                 color = model),
#             linetype = 2) +
#   geom_line(aes(x = month, y = tasmax - 273.15,
#                 color = model),
#             linetype = 3) +
#   facet_wrap(~ as.factor(LC)) +
#   theme_bw()
#
#
# Clim.Mask.MCWD.LC.map.sum <- Clim.Mask.MCWD.LC.sum %>%
#   group_by(lon,lat,LC) %>%
#   summarise(MAP.m = mean(MAP,na.rm = TRUE),
#             MAP.sd = sd(MAP,na.rm = TRUE),
#             MCWD.m = mean(MCWD,na.rm = TRUE),
#             MCWD.sd = sd(MCWD,na.rm = TRUE),
#             .groups = "keep") %>%
#   mutate(MAP.CV = MAP.sd/MAP.m*100,
#          MCWD.CV = MCWD.sd/abs(MCWD.m)*100)
#
# ggplot() +
#   geom_raster(data = Clim.Mask.MCWD.LC.map.sum,
#               aes(x = lon, y = lat,
#                   fill = MAP.m)) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-15, 55),
#            ylim = c(-1, 1)*23.25,
#            expand = FALSE) +
#   scale_fill_gradient(low = "white") +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 20))
#
# ggplot() +
#   geom_raster(data = Clim.Mask.MCWD.LC.map.sum,
#               aes(x = lon, y = lat,
#                   fill = MAP.sd)) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-15, 55),
#            ylim = c(-1, 1)*23.25,
#            expand = FALSE) +
#   scale_fill_gradient(low = "white") +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 20))
#
# ggplot() +
#   geom_raster(data = Clim.Mask.MCWD.LC.map.sum,
#               aes(x = lon, y = lat,
#                   fill = MCWD.m)) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-15, 55),
#            ylim = c(-1, 1)*23.25,
#            expand = FALSE) +
#   scale_fill_gradient(low = "darkred",high = "white") +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 20))
#
#
# ggplot() +
#   geom_raster(data = Clim.Mask.MCWD.LC.map.sum,
#               aes(x = lon, y = lat,
#                   fill = MCWD.sd)) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-15, 55),
#            ylim = c(-1, 1)*23.25,
#            expand = FALSE) +
#   scale_fill_gradient(low = "white",high = "darkred") +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         text = element_text(size = 20))
#
#
#
