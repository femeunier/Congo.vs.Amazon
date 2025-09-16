rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)
library(sf)
library(SPEI)
library(ggalluvial)
library(matlab)
library(ggsankey)

system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.classifications*",
              "./outputs/"))

# coord <- bind_rows(readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Amazon.coord.RDS") %>%
#                      mutate(lon.lat = paste0(lon,".",lat)),
#                    readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Congo.coord.RDS") %>%
#                      mutate(lon.lat = paste0(lon,".",lat)))

coord <- expand.grid(lat = seq(-23.25,23.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

# coord <- readRDS("/home/femeunier/Documents/projects/Santiago/outputs/Amazon.All.coord.v12.RDS") %>%
#   mutate(lon.lat = paste0(lon,".",lat)) %>%
#   filter(model == "ORCHIDEE")

models.selection <- readRDS("./outputs/models.selected.RDS")
# models.selection <- readRDS("./outputs/models.all.RDS")
# models.selection <- readRDS("./outputs/models.penman.RDS")

scenarios <- c("historical","ssp126","ssp245","ssp370","ssp585")

data <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.ERA5_coord.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat))

data.selected <- data %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  dplyr::select(lon,lat,MAP,MAT,MCWD) %>%
  mutate(MAT = MAT - 273.15) %>%
  mutate(basin = case_when(lon >= -120 & lon <= -30 ~ "Amazon",
                           lon <= 55 ~ "Congo",
                           lon <= 160 ~ "Australasia",
                           TRUE ~ NA_character_)) %>%
  filter(!is.na(basin)) %>%
  mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
  mutate(biome = case_when(MAP > 1700 & AI < -3.8 ~ "Humid_large",
                           AI < -3.8 ~ "Humid_low",
                           AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
                           AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
                           AI < -0.25 & AI >= -1 ~ "Semiarid",
                           AI < -0.05 & AI >= -0.25 ~ "Arid",
                           AI < 0 & AI >= -0.05 ~ "Hyperarid",
                           TRUE ~ NA_character_)) %>%
  filter(!is.na(biome)) %>%
  mutate(biome = as.numeric(factor(biome,
                                   levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                              "Semiarid","Arid","Hyperarid"))))



world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")

CMIP6.files <- list.files("./outputs","*CMIP6.classifications*",
                          full.names = TRUE)
CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]

df.all <- data.frame()
for (file in CMIP6.files){

  CMIP6 <- readRDS(file)

  df.all <- bind_rows(df.all,
                      CMIP6)
}

df.all.selected <- df.all %>%
  filter(scenario %in% scenarios) %>%
  filter(model %in% models.selection) %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
  mutate(biome = case_when(MAP > 1700 & AI < -3.8 ~ "Humid_large",
                           AI < -3.8 ~ "Humid_low",
                           AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
                           AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
                           AI < -0.25 & AI >= -1 ~ "Semiarid",
                           AI < -0.05 & AI >= -0.25 ~ "Arid",
                           AI < 0 & AI >= -0.05 ~ "Hyperarid",
                           TRUE ~ NA_character_)) %>%
  mutate(biome = factor(biome,
                        levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                   "Semiarid","Arid","Hyperarid"))) %>%
  mutate(basin = case_when(lon >= -120 &lon <= -30 ~ "Amazon",
                           lon <= 55 ~ "Congo",
                           lon <= 160 ~ "Australasia",
                           TRUE ~ NA_character_)) %>%
  filter(!is.na(basin))

################################################################################
# Weighting difference

df.weights <- readRDS("./outputs/CMIP6.weights.RDS")

ggplot(data = df.weights %>%
         filter(model != "CRUJRA")) +
  geom_density(aes(x = w,
                   fill = basin),
               alpha = 0.5, color = NA) +
  theme_bw()

df.all.selected.weights <- df.all.selected %>%
  left_join(df.weights %>%
              dplyr::select(model,w,basin),
            by = c("model","basin"))

df.all.selected.weights %>%
  ungroup() %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  group_by(basin,scenario) %>%
  filter(period == period[1]) %>%
  summarise(w.tot = sum(w)/length(unique(lon.lat)),
            Nmod = length(unique(model[w > 0.0])))

df.all.selected.sum <- df.all.selected.weights %>%
  group_by(period,basin,scenario,lat,lon) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            MAT.m = mean(MAT,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),
            Etot.m = mean(Etot,na.rm = TRUE),

            MAP.w = weighted.mean(MAP,w,na.rm = TRUE),
            MAT.w = weighted.mean(MAT,w,na.rm = TRUE),
            MCWD.w = weighted.mean(MCWD,w,na.rm = TRUE),
            Etot.w = weighted.mean(Etot,na.rm = TRUE),

            .groups = "keep") %>%
  ungroup()

df.all.selected.sum.long <-
  df.all.selected.sum %>%
  pivot_longer(cols = c(MAP.m,MAT.m,MCWD.m,Etot.m,
                        MAP.w,MAT.w,MCWD.w,Etot.w),
               names_to = "var",
               values_to = "value") %>%
  mutate(variable = sub("\\..*", "", var),
         weighting = sub(".*\\.", "", var))


df.all.selected.sum.wide <- df.all.selected.sum.long %>%
  dplyr::select(-var) %>%
  pivot_wider(names_from = variable,
              values_from = value)

df.all.sum <- df.all.selected.sum.wide %>%
  mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
  mutate(biome = case_when(MAP > 1700 & AI < -3.8 ~ "Humid_large",
                           AI < -3.8 ~ "Humid_low",
                           AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
                           AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
                           AI < -0.25 & AI >= -1 ~ "Semiarid",
                           AI < -0.05 & AI >= -0.25 ~ "Arid",
                           AI < 0 & AI >= -0.05 ~ "Hyperarid",
                           TRUE ~ NA_character_)) %>%
  mutate(biome = factor(biome,
                        levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                   "Semiarid","Arid","Hyperarid")))

ggplot() +
  geom_raster(data = df.all.sum %>%
                dplyr::filter(period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = as.factor(biome))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_manual(values = c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb")) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting)

ggplot() +
  geom_raster(data = df.all.sum,
              aes(x = lon, y = lat,
                  fill = Etot/12)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(limits = c(100,200),
                       low = "darkblue",high = "darkred",
                       midpoint = 150,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")

ggplot(data = df.all.sum %>%
         filter(period %in% c("current","long_future"))) +
  geom_boxplot(aes(y = MAP -Etot,
                   x = basin,
                   fill = scenario), alpha = 0.5) +
  facet_wrap(~ weighting) +
  theme_bw()


ggplot() +
  geom_raster(data = df.all.sum %>%
                filter(period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = MCWD)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(limits = c(-1000,0),
                       low = "darkred",high = "darkblue",
                       midpoint = -500,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")


ggplot() +
  geom_raster(data = df.all.sum %>%
                filter(period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = MAP)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(limits = c(0,3000),low = "darkred",high = "darkblue",
                       midpoint = 1500,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160),
           ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")


ggplot() +
  geom_raster(data = df.all.sum %>%
                filter(period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = MAT)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(limits = c(20,35),low = "darkblue",high = "darkred",
                       midpoint = 27.5,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160),
           ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")

################################################################################


df.diff.temp <- df.all.sum %>%
  mutate(reference = case_when(period == "current" ~ "yes",
                               TRUE ~ "no")) %>%
  mutate(biome.num = as.numeric(biome)) %>%
  dplyr::select(-biome) %>%
  pivot_longer(cols = c(MAP,MAT,MCWD,biome.num,Etot,AI),
               names_to = "variable",
               values_to = "value")

df.diff <- df.diff.temp %>%
  filter(reference == "no") %>%
  dplyr::select(-reference) %>%
  rename(mod = value) %>%
  left_join(df.diff.temp %>%
              filter(reference == "yes",
                     period == "current") %>%
              rename(obs = value) %>%
              dplyr::select(-c("reference","scenario","period")),
            by = c("basin","lon","lat","variable","weighting")) %>%
  filter(!is.na(obs)) %>%
  group_by(period,scenario,weighting,variable,basin) %>%
  mutate(diff = obs - mod)

ggplot() +
  geom_raster(data = df.diff %>%
                filter(variable == "biome.num",
                       period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")

ggplot() +
  geom_raster(data = df.diff %>%
                filter(variable == "MAP",
                       period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = -diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")

ggplot() +
  geom_raster(data = df.diff %>%
                filter(variable == "Etot",
                       period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = -diff/12)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkblue",
                       limits = c(0,50),
                       midpoint = 25,
                       oob = scales::squish,
                       high = "darkred") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")


ggplot() +
  geom_raster(data = df.diff %>%
                filter(variable == "MCWD",
                       period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = -diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       limits = c(-500,0),
                       midpoint = -250,
                       oob = scales::squish,
                       high = "darkblue") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_grid(scenario ~ weighting) +
  theme(legend.position = "bottom")



################################################################################


df2plot <- df.all.sum %>%
  dplyr::select(lon,lat,period,basin,scenario,weighting,biome)  %>%
  group_by(basin,scenario,weighting,biome,period) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  group_by(basin,scenario,weighting,period) %>%
  mutate(Nsum = sum(N),
         frac = N/sum(N)) %>%
  mutate(gridcell = 1:length(N)) %>%
  mutate(biome = factor(biome, levels=rev(levels(biome))))


ggplot(data = df2plot %>%
         filter(period %in% c("current","long_future")),
       aes(x = scenario, stratum = biome, alluvium = gridcell,
           y = frac,
           fill = biome)) +
  # geom_flow() +
  geom_stratum(alpha = 1) +
  # geom_text(stat = "stratum", size = 3) +
  scale_fill_manual(values = fliplr(c("#253b10","#005401","#448704","#86a540",
                                      "#c49402","#d0ce9a","#e5e4cb")
                                    [1:length(unique(df2plot$biome))])) +
  facet_grid(basin ~ weighting) +
  theme_bw()

df.transitions <- df.all.sum %>%
  group_by(weighting,period,scenario,basin) %>%
  summarise(MAT = mean(MAT,
                       na.rm = TRUE),
            MAP = mean(MAP,
                       na.rm = TRUE),
            MCWD = mean(MCWD,
                        na.rm = TRUE),
            trop = sum(biome == "Humid_large"),
            .groups = "keep") %>%
  group_by(basin) %>%
  mutate(trop.rel = trop/max(trop))

ggplot(data = df.transitions) +
  geom_line(aes(x = MAT, y = trop.rel,
                 color = basin,
                linetype = weighting)) +
  theme_bw()

df2plot.tropics <- df.all.sum %>%
  dplyr::select(lon,lat,period,scenario,weighting,biome)  %>%
  group_by(scenario,weighting,biome,period) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  group_by(scenario,weighting,period) %>%
  mutate(Nsum = sum(N),
         frac = N/sum(N)) %>%
  mutate(gridcell = 1:length(N)) %>%
  mutate(biome = factor(biome, levels=rev(levels(biome))))


ggplot(data = df2plot.tropics %>%
         filter(period %in% c("current","long_future")),
       aes(x = scenario, stratum = biome, alluvium = gridcell,
           y = frac,
           fill = biome)) +
  # scale_x_discrete(expand = c(.1, .1)) +
  # geom_flow() +
  geom_stratum(alpha = 1) +
  # geom_text(stat = "stratum", size = 3) +
  scale_fill_manual(values = fliplr(c("#253b10","#005401","#448704","#86a540",
                                      "#c49402","#d0ce9a","#e5e4cb")[1:length(unique(df2plot.tropics$biome))])) +

  facet_grid(~ weighting) +
  theme_bw()


################################################################################
# Transitions
cscenarios <- scenarios[scenarios != "historical"]
df2augment <- data.frame()
for (cscenario in cscenarios){

  df2augment <- bind_rows(df2augment,
                          df.all.sum %>%
                            filter(scenario == "historical") %>%
                            mutate(scenario = cscenario),
                          df.all.sum %>%
                            filter(scenario == cscenario))

}

saveRDS(df2augment,
        "./outputs/df.climate.change.RDS")

df2augment.sum <- df2augment %>%
  mutate(period.num = as.numeric(factor(period,
                                        levels = c("historical1","historical2","current",
                                                   "near_future","mid_future","long_future")))) %>%
  filter(period.num > 2) %>%
  group_by(weighting,scenario,period,period.num,basin,biome) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  group_by(weighting,scenario,period,basin) %>%
  mutate(r = N/sum(N))


ggplot(data = df2augment.sum) +
  geom_line(aes(x = period.num,
                y = r,
                color = biome,
                linetype = weighting)) +
  facet_grid(scenario ~ basin) +
  scale_color_manual(values = c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb")) +
  theme_bw()



df.transitions <- df2augment %>%
  mutate(biome.num = as.numeric(factor(biome,
                                   levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                              "Semiarid","Arid","Hyperarid")))) %>%
  mutate(period.num = as.numeric(factor(period,
                                       levels = c("historical1","historical2","current",
                                                  "near_future","mid_future","long_future")))) %>%
  filter(period.num > 2) %>%
  dplyr::select(period,basin,scenario,lat,lon,weighting,biome,
                period.num,biome.num) %>%
  group_by(basin,scenario,lat,lon,weighting) %>%
  arrange(period.num) %>%
  mutate(biome.last.change = c(NA,diff(biome.num)),
         biome.change = biome.num - biome.num[1])

df.transitions.sum <- df.transitions %>%
  group_by(basin,scenario,period,period.num,weighting) %>%
  summarise(Npos = length(which(biome.change > 0)),
            Nneg = length(which(biome.change < 0)),
            Nchange = length(which(abs(biome.change) > 0)),
            Ntot = length(biome.change),
            .groups = "keep") %>%
  mutate(r.pos = Npos/Ntot,
         r.neg = Nneg/Ntot,
         r.change = Nchange/Ntot)

ggplot(data = df.transitions.sum) +
  geom_line(aes(x = period.num,
                y = r.change,
                color = scenario,
                linetype = weighting)) +
  facet_wrap(~ basin) +
  theme_bw()

