rm(list = ls())

library(fields)
library(sf)

MCWD.threshold <- -475 ; MAP.threshold <- 975

LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))
lons.lats <- LC %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  pull(lon.lat)

GridArea <- readRDS("./outputs/GridArea.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))
Biomass <- readRDS("./outputs/Biomass.vs.climate.RDS") %>%
  na.omit() %>%
  filter(source == "MEM",
         MCWD <= 0)

tps_model <- Tps(x = Biomass[, c("MCWD", "MAP")],
                 Y = Biomass$AGB.m)

Biomass$AGB.m_pred <-
  predict(tps_model, x = Biomass[, c("MCWD", "MAP")])[,1]

df2map <- data.frame()
for (cMCWD in sort(unique(Biomass$MCWD))){

  cdf <- Biomass %>%
    filter(MCWD == cMCWD)

  df2map <- bind_rows(df2map,
                      data.frame(MCWD = cMCWD,
                                 MAP = seq(min(cdf$MAP,na.rm = TRUE),
                                           max(cdf$MAP,na.rm = TRUE),
                                           50)))
}

df2map$AGB.m_pred <-
  predict(tps_model, x = df2map[, c("MCWD", "MAP")])[,1]

MCWD.threshold <- -475 ; MAP.threshold <- 975

df_plot <- Biomass %>%
  mutate(LC_pred = case_when(MAP < MAP.threshold ~ 1,
                             MCWD > MCWD.threshold ~ 2,
                             TRUE ~ 3)) %>%
  mutate(LC_pred = factor(LC_pred,
                          levels = c(2,3,1)))

ggplot(df_plot,
       aes(x = MCWD, y = MAP, fill = AGB.m/20)) +
  geom_tile() +
  geom_hline(yintercept = MAP.threshold,color = "grey17") +
  geom_vline(xintercept = MCWD.threshold,color = "grey17") +
  scale_fill_viridis_c(limits = c(0,20),
                       oob = scales::squish) +
  coord_fixed() +
  theme_bw() +
  labs(x = "",y= "", fill = "")+
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(size = 14),  # Ensure tick labels show
        axis.text.y = element_text(size = 14))


ggplot(df2map,
       aes(x = MCWD, y = MAP, fill = AGB.m_pred/20)) +

  geom_tile() +
  geom_hline(yintercept = MAP.threshold,color = "grey17") +
  geom_vline(xintercept = MCWD.threshold,color = "grey17") +
  scale_fill_viridis_c(limits = c(0,20),
                       oob = scales::squish) +
  coord_fixed() +
  theme_bw() +
  labs(x = "",y= "", fill = "")+
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_text(size = 14),  # Ensure tick labels show
        axis.text.y = element_text(size = 14))

df2plot2 <- df_plot %>%
  dplyr::select(-c(AGB.max,source)) %>%
  pivot_longer(cols = c(AGB.m, AGB.m_pred),
               names_to = "source",
               values_to = "AGB.m")
ggplot(df2plot2,
       aes(x = as.factor(LC_pred), y = AGB.m/20,
           fill = as.factor(LC_pred))) +
  geom_boxplot(alpha = 1) +

  scale_fill_manual(values = c("#005401","#448704","#c49402","grey")) +
  theme_bw() +
  # scale_y_log10() +
  facet_wrap(~ source) +
  scale_x_discrete(labels = c()) +
  labs(x = "", y = "",fill = "") +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

df2plot2 %>%
  group_by(source) %>%
  summarise(rsq = summary(lm(AGB.m ~ as.factor(LC_pred)))[["r.squared"]])

summary(lm(data = df_plot,
   formula = AGB.m ~ AGB.m_pred))

ggplot(data = df_plot,
       aes(x = AGB.m_pred/20, y = AGB.m/20)) +
  geom_point(aes(color = as.factor(LC_pred)),) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  stat_smooth(method = "lm",   color = "black",
              se = FALSE) +
  scale_color_manual(values = c("#005401","#448704","#c49402","grey")) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))


ggplot(data = df_plot,
       aes(x = AGB.m_pred/20 - AGB.m/20)) +
  geom_density(aes(fill = as.factor(LC_pred)),
               alpha = 0.5) +
  scale_fill_manual(values = c("#005401","#448704","#c49402","grey")) +

  labs(x = "",y = "") +
  guides(fill = "none") +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  theme(text = element_text(size = 20))


################################################################http://127.0.0.1:30281/graphics/plot_zoom_png?width=326&height=167
# Spatial distributions

# Based on observations
df.biomass <- readRDS("./outputs/All.biomass.maps.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  filter(source == "MEM") %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(lon.lat %in% lons.lats) %>%
  dplyr::select(-lon.lat) %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(LC %in% c(1,2,3))


ggplot(data = df.biomass) +
  geom_boxplot(aes(x = as.factor(LC),
                   y = AGB/20)) +
  theme_bw()

ggplot(data = df.biomass) +
  geom_raster(aes(x = lon, y = lat,
                  fill = AGB/20)) +
  theme_bw()

df.biomass %>%
  group_by(LC) %>%
  summarise(AGB.tot = sum(AGB/20* area*land.frac,na.rm = TRUE)/1e12)

df.biomass %>%
  ungroup() %>%
  summarise(AGB.tot = sum(AGB/20* area*land.frac, na.rm = TRUE)/1e12)

# # Based on observations, LC4
# df.biomass <- readRDS("./outputs/All.biomass.maps.RDS") %>%
#   mutate(lon = round(lon,2),
#          lat = round(lat,2)) %>%
#   mutate(lon.lat = paste0(lon,".",lat)) %>%
#   filter(lon.lat %in% lons.lats) %>%
#   dplyr::select(-lon.lat) %>%
#   filter(source == "MEM") %>%
#   left_join(GridArea,
#             by = c("lon","lat")) %>%
#   left_join(LC,
#             by = c("lon","lat"))
#


# Based on climate only
Clim.Mask.MCWD.sum <- readRDS("./outputs/Summary.climate.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  filter(model == "MEM") %>%
  left_join(GridArea,
            by = c("lon","lat"))

Clim.Mask.MCWD.sum$AGB.m_pred <-
  predict(tps_model, x = Clim.Mask.MCWD.sum[, c("MCWD", "MAP")])[,1]

ggplot(data = Clim.Mask.MCWD.sum) +
  geom_boxplot(aes(x = as.factor(LC),
                   y = AGB.m_pred/20)) +
  theme_bw()


df.diff <- df.biomass %>%
  rename(model = "source") %>%
  left_join(Clim.Mask.MCWD.sum %>%
              dplyr::select(lon,lat,model,LC,AGB.m_pred),
            by = c("lon","lat","model","LC")) %>%
  mutate(diff = AGB.m_pred - AGB)


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
rainforests <- read_sf("./data/Rainforests.shp")

A <- ggplot(data = df.diff) +
  geom_raster(aes(x = lon, y = lat,
                  fill = AGB/20)) +
  scale_fill_gradient2(limits = c(0,20),oob = scales::squish,
                       low = "#c49402",
                       high = "#005401",
                       mid = "#448704",midpoint = 9) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  theme_map() +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  labs(x = "",y = "", fill = "") +
  guides(fill = "none")

A

ggplot(data = df.diff) +
  geom_density(aes(x = AGB/20)) +
  theme_bw() +
  scale_x_continuous(limits = c(0,20)) +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "", fill = "")


B <- ggplot(data = df.diff %>% na.omit()) +
  geom_raster(aes(x = lon, y = lat,
                  fill = AGB.m_pred/20)) +
  scale_fill_gradient2(limits = c(0,20),oob = scales::squish,
                       low = "#c49402",
                       high = "#005401",
                       mid = "#448704",midpoint = 9) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  theme_map() +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  labs(x = "",y = "", fill = "")

B

ggplot(data = df.diff) +
  geom_density(aes(x = AGB.m_pred/20)) +
  theme_bw() +
  scale_x_continuous(limits = c(0,20)) +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "", fill = "")

C <- ggplot(data = df.diff) +
  geom_raster(aes(x = lon, y = lat,
                  fill = diff/20)) +
  scale_fill_gradient2() +
  geom_sf(data = world,fill = NA, color = "grey17") +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  theme_map() +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  labs(x = "",y = "", fill = "") +
  theme(legend.position = "bottom")


ggplot(data = df.diff) +
  geom_density(aes(x = diff/20)) +
  theme_bw() +
  geom_vline(xintercept = 0,linetype = 2) +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "", fill = "")

cowplot::plot_grid(A,B,C, nrow = 3,
                   align = "hv")



# # Based on climate only + LC4
#
# Clim.Mask.MCWD <- readRDS("./outputs/Climate.CA.summ.RDS") %>%
#   filter(model == "MEM") %>%
#   mutate(lon = round(lon,2),
#          lat = round(lat,2)) %>%
#   left_join(GridArea,
#             by = c("lon","lat")) %>%
#   group_by(lon,lat,model) %>%
#   summarise(MAP = sum(pre,na.rm = TRUE),
#             MCWD = unique(MCWD),
#             area = unique(area),
#             land.frac = unique(land.frac),
#             LC = unique(LC),
#
#             .groups = "keep") %>%
#   mutate(LC_pred = case_when(MAP < MAP.threshold ~ 1,
#                              MCWD > MCWD.threshold ~ 2,
#                              TRUE ~ 3)) %>%
#   filter(lat <= 10,lat >= -15)
#
# ggplot(data = Clim.Mask.MCWD) +
#   geom_raster(aes(x = lon, y = lat,
#                   fill = as.factor(LC_pred))) +
#   theme_bw()
#
# Clim.Mask.MCWD$AGB.m_pred <-
#   predict(tps_model, x = Clim.Mask.MCWD[, c("MCWD", "MAP")])[,1]
#
# ggplot(data = Clim.Mask.MCWD) +
#   geom_boxplot(aes(x = as.factor(LC),
#                    y = AGB.m_pred/20)) +
#   theme_bw()
#
# ggplot(data = Clim.Mask.MCWD) +
#   geom_boxplot(aes(x = as.factor(LC_pred),
#                    y = AGB.m_pred/20)) +
#   theme_bw()
#
# ggplot(data = Clim.Mask.MCWD) +
#   geom_raster(aes(x = lon, y = lat,
#                   fill = AGB.m_pred/20)) +
#   theme_bw()
#
# Clim.Mask.MCWD %>%
#   group_by(LC) %>%
#   summarise(AGB.tot = sum(AGB.m_pred/20* area*land.frac)/1e12)
#
# Clim.Mask.MCWD %>%
#   ungroup() %>%
#   summarise(AGB.tot = sum(AGB.m_pred/20* area*land.frac)/1e12)

# CMIP6 scenarios
CMIP6 <- readRDS("./outputs/All.Transitions.timing_long.RDS") %>%
  filter(scenario != "ssp534-over") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  filter(LC %in% c(1,2,3)) %>%
  left_join(GridArea,
            by = c("lon","lat"))

CMIP6$AGB.m_pred <-
  predict(tps_model, x = CMIP6[, c("MCWD", "MAP")])[,1]

df.temp <- CMIP6 %>%
  filter(model %in% c("MEM"),
         scenario %in% c("ssp245","ssp585"),
         period == "Long_future") %>%
  left_join(CMIP6 %>%
              filter(model == "MEM",
                     scenario == "ssp245",
                     period == "historical") %>%
              dplyr::select(lon,lat,AGB.m_pred) %>%
              rename(AGB_ref = AGB.m_pred),
            by = c("lon","lat")) %>%
  mutate(diff = AGB.m_pred - AGB_ref)

ggplot(data = df.temp) +
  geom_raster(aes(x = lon, y = lat,
                  fill = diff/20)) +
  scale_fill_gradient2(na.value = "grey") +
  geom_sf(data = world,fill = NA, color = "grey17") +
  geom_sf(data = rainforests,
          fill = NA, color = "black",
          linewidth = 0.5) +
  theme_map() +
  facet_wrap(~ scenario, ncol = 1) +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  labs(x = "",y = "", fill = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        strip.text = element_blank())

ggplot(data = df.temp) +
  geom_density(aes(x = diff/20)) +
  theme_bw() +
  geom_vline(xintercept = 0,linetype = 2) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  facet_wrap(~ scenario,ncol = 1) +
  labs(x = "",y = "", fill = "")

df.temp %>%
  group_by(scenario) %>%
  summarise(m = mean(diff/20),
            med = median(diff/20),
            min = min(diff/20),
            max = max(diff/20),
            .groups = "keep")


ggplot(data = CMIP6 %>%
         filter(model == "MEM")) +
  geom_boxplot(aes(x = as.factor(LC.pred),
                   y = AGB.m_pred/20)) +
  facet_wrap(~ interaction(scenario,period)) +
  theme_bw()

ggplot(data = CMIP6 %>%
         filter(model == "MEM",
                period == "historical", scenario == "ssp245")) +
  geom_raster(aes(x = lon, y = lat,
                  fill = AGB.m_pred/20)) +
  theme_bw()

df.CMIP6.LC <- CMIP6 %>%
  group_by(LC,model,scenario,period) %>%
  summarise(AGB.tot = sum(AGB.m_pred/20* area*land.frac)/1e12,
            .groups = "keep")



df.barplot <- bind_rows(df.biomass %>%
                          group_by(LC) %>%
                          summarise(AGB.tot = sum(AGB/20* area*land.frac,na.rm = TRUE)/1e12) %>%
                          mutate(source = "observed"),
                        Clim.Mask.MCWD.sum %>%
                          group_by(LC) %>%
                          summarise(AGB.tot = sum(AGB.m_pred/20* area*land.frac)/1e12) %>%
                          mutate(source = "Reanalysis"),
                        df.CMIP6.LC %>%
                          filter(model == "MEM",
                                 scenario == "ssp245",
                                 period == "historical") %>%
                          mutate(source = "CMIP6")) %>%
  mutate(LC = factor(LC,
                     levels =rev(c(2,3,1))),
         source = factor(source,
                         levels = c("observed","Reanalysis","CMIP6")))

ggplot(data = df.barplot) +
  geom_bar(aes(x = source,
               y = AGB.tot,
               fill = as.factor(LC)),
           stat = "identity") +
  scale_fill_manual(values = c("#c49402","#448704","#005401")) +
  theme_bw() +
  guides(fill = "none") +
  scale_y_continuous(limits = c(0,60)) +
  labs(x = "", y = "") +
  theme(text = element_text(size = 20))


df.biomass %>%
  ungroup() %>%
  summarise(AGB.tot = sum(AGB/20* area*land.frac, na.rm = TRUE)/1e12)
Clim.Mask.MCWD.sum %>%
  ungroup() %>%
  summarise(AGB.tot = sum(AGB.m_pred/20* area*land.frac)/1e12)
df.CMIP6.LC %>%
  filter(model == "MEM",
         scenario == "ssp245",
         period == "historical") %>%
  pull(AGB.tot) %>% sum()

df.GW <- readRDS("./outputs/df.Global.warming_periods.RDS")

df.CMIP6.LC.change <- df.CMIP6.LC %>%
  left_join(df.CMIP6.LC %>%
              ungroup() %>%
              filter(period == "historical") %>%
              dplyr::select(-c(period)) %>%
              rename(ref = AGB.tot),
            by = c("LC","model","scenario")) %>%

  mutate(diff = AGB.tot - ref) %>%
  left_join(df.GW %>%
              dplyr::select(-c(tas.m,tas.m.ref)),
            by = c("model","scenario","period"))

df.CMIP6.LC.change.sum <- bind_rows(df.CMIP6.LC.change %>%
                                      group_by(model,scenario,period) %>%
                                      summarise(gw = unique(gw),
                                                diff = sum(diff,na.rm = TRUE),
                                                LC = 0,
                                                .groups = "keep"))


df2plot.changes.shitter <- df.CMIP6.LC.change %>%
  rowwise()  %>%
  mutate(gw = gw*(runif(1,0.95,1.05)))

df2plot.changes.CI <- df2plot.changes.shitter %>%
  filter(model == "MEM") %>%
  left_join(df2plot.changes.shitter %>%
              filter(model != "MEM") %>%
              group_by(LC,scenario, period) %>%
              summarise(gw.low = quantile(gw,0.15,na.rm = TRUE),
                        gw.high = quantile(gw,0.85,na.rm = TRUE),
                        diff.high = quantile(diff,0.15,na.rm = TRUE),
                        diff.low = quantile( diff,0.85,na.rm = TRUE),
                        .groups = "keep"),
            by = c("scenario","period","LC"))


df2plot.changes.CI %>%
  group_by(scenario,period) %>%
  summarise(net = sum(diff)) %>%
  filter(period == "Long_future")

df2plot.changes.CI %>%
  filter(period == "Long_future",
         LC == 3)


ggplot(data = df2plot.changes.CI,
       mapping = aes(x = gw, y = diff, color = as.factor(LC),
                     fill = as.factor(LC))) +
  geom_point(aes(shape = scenario)) +
  geom_errorbar(aes(ymin = diff.low, ymax = diff.high)) +
  geom_errorbar(aes(xmin = gw.low, xmax = gw.high)) +
  geom_hline(yintercept = 0, linetype = 2) +
  stat_smooth(fullrange = TRUE,
              method = "lm", formula = y ~ poly(x,1)) +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  guides(color = "none", shape = "none",
         fill = "none") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(text = element_text(size = 20))


df.CMIP6.LC.change %>%
  filter(model == "MEM") %>%
  group_by(LC) %>%
  summarise(slope = summary(lm(diff ~ gw))[["coefficients"]][2,1],
            p.val = summary(lm(diff ~ gw))[["coefficients"]][2,4],
            r2 = summary(lm(diff ~ gw))[["r.squared"]],
            .groups = "keep")

CMIP6 %>%
  group_by(model,scenario,period) %>%
  summarise(AGB.tot = sum(AGB.m_pred/20* area*land.frac)/1e12)

