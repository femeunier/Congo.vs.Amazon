rm(list = ls())

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


LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

all.CMIP6.MCWD <- readRDS("./outputs/All.CMIP6.states.timing.MEM.RDS") %>%
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

rainforests <- read_sf("./data/Rainforests.shp")
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

df2plot <- all.CMIP6.MCWD %>%
  ungroup() %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  filter(model == "MEM",
         period == "historical",
         scenario == "ssp245",
         !is.na(LC),
         land.frac > 0.25)

df2plot %>%
  filter(LC %in% c(1,2,3)) %>%
  group_by(LC.pred) %>%
  summarise(area = sum(area*land.frac)/1e12,
            .groups = "keep")

ggplot() +
  geom_raster(data = df2plot %>%
                filter(LC %in% c(1,2,3)),
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

saveRDS(df2plot,
        "./outputs/LC.pred.MEM.CMIP6.RDS")
