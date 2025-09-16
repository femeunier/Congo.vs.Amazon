rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)
library(sf)
library(SPEI)
library(caret)

models.selection <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon//outputs/models.selected.RDS")
# models.selection <- readRDS("./outputs/models.all.RDS")
# models.selection <- readRDS("./outputs/models.penman.RDS")

system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.classifications.*",
              "/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/"))

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

data <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.ERA5_coord.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat))

data.selected <- data %>%
  ungroup() %>%
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

CMIP6.files <- list.files("/home/femeunier/Documents/projects/Congo.vs.Amazon//outputs","*CMIP6.classifications*",
                          full.names = TRUE)
CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]

df.all <- data.frame()
for (file in CMIP6.files){

  CMIP6 <- readRDS(file) %>%
    filter(scenario == "historical",
           period == 2000)

  df.all <- bind_rows(df.all,
                      CMIP6)
}

model.selected <- df.all %>%
  filter(model %in% models.selection) %>%
  dplyr::select(lon,lat,model,MAP,MAT,MCWD) %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  mutate(basin = case_when(lon >= -120 &lon <= -30 ~ "Amazon",
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

model.vs.data <- model.selected %>%
  dplyr::select(lon,lat,model,MAT,MAP,MCWD,biome) %>%
  pivot_longer(cols = c(MAT,MAP,MCWD,biome),
               values_to = "Model",
               names_to = "variable") %>%
  left_join(data.selected %>%
    dplyr::select(basin,lon,lat,MAT,MAP,MCWD,biome) %>%
    pivot_longer(cols = c(MAT,MAP,MCWD,biome),
                 values_to = "Obs",
                 names_to = "variable") %>%
    mutate(source = "obs"),
    by = c("lon","lat","variable"))


model.and.data <- bind_rows(model.vs.data,
                            data.selected %>%
                              dplyr::select(basin,lon,lat,MAT,MAP,MCWD,biome) %>%
                              pivot_longer(cols = c(MAT,MAP,MCWD,biome),
                                           values_to = "Obs",
                                           names_to = "variable") %>%
                              mutate(source = "obs") %>%
                              mutate(Model = Obs,
                                     model = "CRUJRA"))

models <- unique(model.and.data$model)

D <- data.frame() ; S <- data.frame()

vars2test <- c("MCWD")
# vars2test <- c("biome")

for (imodel in seq(1,length(models))){
  cimodel = models[imodel]

  print(paste0(cimodel))
  # D <- bind_rows(D,
  #                model.and.data %>%
  #   filter(model == cimodel) %>%
  #   group_by(basin, variable) %>%
  #   summarise(RMSE = sqrt(1/length(Obs[which(!is.na(Obs))])*sum((Model - Obs)**2,
  #                                                               na.rm = TRUE)),
  #             .groups = "keep") %>%
  #     mutate(model = cimodel))

  for (jmodel in seq(1,length(models))){
    cjmodel = models[jmodel]

    if (cjmodel != cimodel){

      S <- bind_rows(S,
                     model.and.data %>%
        filter(model %in% c(cimodel,cjmodel)) %>%
        filter(variable %in% vars2test) %>%
        ungroup() %>%
        dplyr::select(-Obs) %>%
        mutate(type = case_when(model == cimodel ~ "Obs",
                                model == cjmodel ~ "Model")) %>%
        dplyr::select(-model) %>%
        pivot_wider(names_from = type,
                    values_from = Model) %>%
        group_by(basin,variable) %>%
        summarise(RMSE = sqrt(1/length(Obs[which(!is.na(Obs))])*sum((Model - Obs)**2,
                                                                    na.rm = TRUE)),
                  .groups = "keep") %>%
          mutate(cimodel,
                 cjmodel))

    }
  }
}

model.vs.data %>%
  filter(variable == "biome") %>%
  group_by(model) %>%
  summarise(Acc = confusionMatrix(factor(Model,
                                         levels = 1:7),
                                  factor(Obs,
                                         levels = 1:7))[["overall"]][1],
            .groups = "keep") %>%
  arrange(desc(Acc))

sigma_D_main = 0.4  # model performance
sigma_S_main = 0.48 # model similarities

Snorm <- S %>%
  filter(!is.na(RMSE)) %>%
  group_by(variable,basin) %>%
  mutate(delta = RMSE/mean(RMSE,na.rm = TRUE))

# ggplot(data = Snorm) +
#   geom_density(aes(x = delta,
#                    fill = variable),
#                alpha = 0.5) +
#   facet_wrap(~ basin, scales = "free") +
#   theme_bw()

Snorm.sum <- Snorm %>%
  group_by(basin,cimodel,cjmodel) %>%
  summarise(delta = sum(delta,
                        na.rm = TRUE),
            .groups = "keep") %>%
  mutate(sigma_S = sigma_S_main) %>%
  mutate(S = exp(-(delta/sigma_S)**2))

# ggplot(data = Snorm.sum) +
#   geom_raster(aes(x = cimodel,
#                   y = cjmodel,
#                   fill = delta),
#               alpha = 0.5) +
#   facet_wrap(~ basin, scales = "free") +
#   # scale_bre(palette = "OrRd") +
#   theme_bw()

w_u <- Snorm.sum %>%
  group_by(basin,cimodel) %>%
  summarise(w_u = 1/(1 + sum(S,na.rm = TRUE)),
            .groups = "keep") %>%
  rename(model = cimodel)

# ggplot(data = w_u) +
#   geom_density(aes(x = w_u,
#                    fill = basin),
#                alpha = 0.5) +
#   facet_wrap(~ basin, scales = "free") +
#   theme_bw()

w_q <- Snorm.sum %>%
  filter(cimodel == "CRUJRA") %>%
  group_by(cjmodel,basin) %>%
  summarise(w_q = exp(-(delta/sigma_D_main)**2),
            .groups = "keep") %>%
  rename(model = cjmodel)

df.weights <- bind_rows(w_q %>%
  left_join(w_u,
            by = c("model","basin")) %>%
  group_by(model,basin) %>%
  summarise(w = w_q*w_u,
            .groups = "keep") %>%
  group_by(basin) %>%
  mutate(w = w/(sum(w,na.rm = TRUE))),
  data.frame(model = c("CRUJRA"),
             basin = c("Amazon","Congo","Australasia"),
             w = 1))

# ggplot(data = df.weights %>%
#          filter(model != "CRUJRA") %>%
#          filter(w > 0.001) %>%
#          arrange(desc(w))) +
#   geom_density(aes(x = w,
#                    fill = basin),
#            alpha = 0.5, color = NA) +
#   theme_bw()

df.weights %>%
  filter(model != "CRUJRA") %>%
  filter(w > 0.02) %>%
  pull(model) %>% unique()

df.weights %>%
  filter(model != "CRUJRA") %>%
  filter(w > 0.02) %>%
  group_by(basin) %>%
  summarise(N = n())


df.weights %>%
  filter(basin == "Amazon",
         model != "CRUJRA") %>%
  arrange(desc(w)) %>%
  slice_head(n = 5)

df.weights %>%
  filter(basin == "Congo",
         model != "CRUJRA") %>%
  arrange(desc(w)) %>%
  slice_head(n = 5)

df.weights %>%
  filter(basin == "Australasia",
         model != "CRUJRA") %>%
  arrange(desc(w)) %>%
  slice_head(n = 5)

saveRDS(df.weights,
        "/home/femeunier/Documents/projects/Congo.vs.Amazon//outputs/CMIP6.weights.RDS")

df.all.selected <- bind_rows(
  model.selected %>%
    mutate(source = "CMIP6"),
  data.selected %>%
    mutate(source = "obs",
           model = "CRUJRA")) %>%
  left_join(df.weights %>%
              dplyr::select(model,basin,w),
            by = c("model","basin")) %>%
  ungroup()

df.all.selected.sum <- bind_rows(df.all.selected %>%
  group_by(basin,source,lat,lon) %>%
  summarise(MAP = weighted.mean(MAP,w,na.rm = TRUE),
            MAT = weighted.mean(MAT,w,na.rm = TRUE),
            MCWD = weighted.mean(MCWD,w,na.rm = TRUE),
            .groups = "keep"),

  df.all.selected %>%
    filter(source == "CMIP6") %>%
    group_by(basin,source,lat,lon) %>%
    summarise(MAP = mean(MAP,na.rm = TRUE),
              MAT = mean(MAT,na.rm = TRUE),
              MCWD = mean(MCWD,na.rm = TRUE),
              .groups = "keep") %>%
    mutate(source = "Mean CMIP6")) %>%
  ungroup() %>%
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
  mutate(biome = factor(biome,
                        levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                   "Semiarid","Arid","Hyperarid")))

################################################################################

ggplot() +
  geom_raster(data = df.all.selected.sum,
              aes(x = lon, y = lat,
                  fill = as.factor(biome))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_manual(values = c("#253b10","#005401","#448704","#86a540",
                               "#c49402","#d0ce9a","#e5e4cb")) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*30, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ source, ncol = 1) +
  theme(legend.position = "bottom")

df.diff.temp <- df.all.selected.sum %>%
  mutate(biome.num = as.numeric(biome)) %>%
  dplyr::select(basin,source,lon,lat,source,biome.num,MAP,MCWD,MAT) %>%
  pivot_longer(cols = c(biome.num,MAP,MCWD,MAT),
               names_to = "variable",
               values_to = "value") %>%
  mutate(source = case_when(source == "Mean CMIP6" ~ "Mean",
                            source == "CMIP6" ~ "Weighted.mean",
                            TRUE ~ source),
         reference = case_when(source == "obs" ~ "yes",
                               TRUE ~ "no"))

df.diff <- df.diff.temp %>%
  filter(reference == "no") %>%
  dplyr::select(-reference) %>%
  rename(mod = value) %>%
  left_join(df.diff.temp %>%
              filter(reference == "yes") %>%
              rename(obs = value) %>%
              dplyr::select(-c("source","reference")),
            by = c("basin","lon","lat","variable")) %>%
  filter(!is.na(obs)) %>%
  group_by(source,variable,basin) %>%
  mutate(diff = obs - mod)

ggplot(data = df.diff,
       aes(x = mod,y = obs, fill = source,
           color = source)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0,
              linetype = 2, color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()


CF <- df.diff %>%
  filter(variable == "biome.num") %>%
  group_by(source,mod,obs) %>%
  summarise(N = n(),
            .groups = "keep")

ggplot(data = CF) +
  geom_tile(aes(x = obs,
                  y = 8 - mod,
                  fill = N),
              color = "black") +
  scale_fill_gradient(low = "white",high = "darkblue",na.value = "white") +
  facet_wrap(~ source) +
  scale_x_continuous(limits = c(0.5,7.5),expand = c(0,0)) +
  scale_y_continuous(limits = c(0.5,7.5),expand = c(0,0)) +
  geom_hline(yintercept = seq(0.5,7.5,1)) +
  geom_vline(xintercept = seq(0.5,7.5,1)) +
  theme_bw() +
  theme(panel.grid = element_blank())

CF.basin <- df.diff %>%
  filter(variable == "biome.num") %>%
  group_by(source,basin,mod,obs) %>%
  summarise(N = n(),
            .groups = "keep")

ggplot(data = CF.basin) +
  geom_tile(aes(x = obs,
                y = 8 - mod,
                fill = (N)),
            color = "black") +
  scale_fill_gradient(low = "white",high = "darkblue",na.value = "white") +
  facet_grid(basin ~ source) +
  scale_x_continuous(limits = c(0.5,7.5),expand = c(0,0)) +
  scale_y_continuous(limits = c(0.5,7.5),expand = c(0,0)) +
  geom_hline(yintercept = seq(0.5,7.5,1)) +
  geom_vline(xintercept = seq(0.5,7.5,1)) +
  theme_bw() +
  theme(panel.grid = element_blank())


df.diff %>%
  group_by(source,basin) %>%
  summarise(Acc = (confusionMatrix(factor(mod,levels = 1:7),
                                   factor(obs,levels = 1:7)))
            [["overall"]][1],
            .groups = "keep") %>%
  pivot_wider(names_from = source,
              values_from = c(Acc))


df.diff %>%
  filter(variable %in% c("MAP","MCWD","MAT")) %>%
  group_by(basin,source,variable) %>%
  summarise(r2 = summary(lm(formula = obs~mod))[["r.squared"]],
            slope = coef(lm(formula = obs~mod))[2],
            RMSE = sqrt(1/sum(!is.na(obs))*sum((obs-mod)**2)),
            .groups = "keep") %>%
  pivot_wider(names_from = source,
              values_from = c(r2,RMSE,slope))

ggplot() +
  geom_raster(data = df.diff %>%
                filter(variable == "biome.num"),
              aes(x = lon, y = lat,
                  fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ source, ncol = 1) +
  theme(legend.position = "bottom")


ggplot() +
  geom_raster(data = df.diff %>%
                filter(variable == "MCWD"),
              aes(x = lon, y = lat,
                  fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue",
                       limits = c(-500,500),
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ source, ncol = 1) +
  theme(legend.position = "bottom")


ggplot() +
  geom_raster(data = df.diff %>%
                filter(variable == "MAP"),
              aes(x = lon, y = lat,
                  fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue",
                       limits = c(-1000,1000),
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ source, ncol = 1) +
  theme(legend.position = "bottom")



ggplot() +
  geom_raster(data = df.diff %>%
                filter(variable == "MAT"),
              aes(x = lon, y = lat,
                  fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue",
                       limits = c(-2,2),
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ source, ncol = 1) +
  theme(legend.position = "bottom")

ggplot() +
  geom_density(data = df.diff,
               aes(x = diff,
                  fill = source),
           alpha = 0.5) +
  theme_bw() +
  facet_wrap(~ variable,
             scales = "free")+
  theme(legend.position = "bottom")



ggplot() +
  geom_tile(data = df.all.selected.sum %>%
              group_by(basin,source,lon,lat) %>%
              summarise(MAT = mean(MAT,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = lon, y = lat,
                fill = MAT)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(limits = c(20,30),
                       low = "darkblue",high = "darkred",
                       midpoint = 25,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  facet_wrap(~ source, ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot() +
  geom_tile(data = df.all.selected.sum,
            aes(x = lon, y = lat,
                fill = MCWD)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(limits = c(-1000,0),low = "darkred",high = "darkblue",
                      midpoint = -500,
                      oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  facet_wrap(~ source, ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot() +
  # geom_density(data = df.all.selected,
  #              aes(x = MCWD,
  #                  group = interaction(source,model)),
  #              fill = "grey",alpha = 0.05, color = NA) +
  geom_density(data = df.all.selected.sum,
               aes(x = MCWD,
                   fill = source), alpha = 0.5,
               color = NA) +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(-1000,0)) +
  theme_bw()

ggplot() +
  geom_density(data = df.all.selected,
               aes(x = MAP,
                   group = interaction(source,model)),
               fill = "grey",alpha = 0.05, color = NA) +
  geom_density(data = df.all.selected.sum,
               aes(x = MAP,
                   fill = source), alpha = 0.5,
               color = NA) +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(0,3000)) +
  theme_bw()

stop()

################################################################################
# Individual models

GS <- readRDS("~/Documents/projects/Santiago/outputs/Amazon.GuianaShield.coord.v12.RDS") %>%
  filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(lon,".",lat))

models.selected <- c(df.weights %>%
  slice_head(n = 5) %>%
  pull(model),"CRUJRA")

ggplot() +
  geom_density(data = df.all.selected %>%
                 filter(model != "CRUJRA") %>%
                 mutate(lon.lat = paste0(lon,".",lat)),
               aes(x = MAP,
                   fill = model), alpha = 0.5) +
  geom_density(data = df.all.selected %>%
                 filter(model == "CRUJRA") %>%
                 mutate(lon.lat = paste0(lon,".",lat)),
               aes(x = MAP),
               fill = "red",
               alpha = 0.5) +
  geom_vline(xintercept = 1700) +
  theme_bw()

ggplot() +
  geom_tile(data = df.all.selected %>%
              filter(model %in% models.selected) %>%
              mutate(lon.lat = paste0(lon,".",lat)) %>%
              filter(lon.lat %in% GS[["lon.lat"]]),
            aes(x = lon, y = lat,
                fill = MAP)) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +

  scale_fill_gradient(limits = c(1000,3000),low = "white",high = "darkblue",
                      oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ model)


ggplot() +
  geom_tile(data = df.all.selected %>%
              filter(model %in% models.selected),
            aes(x = lon, y = lat,
                fill = MAT)) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +

  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ model)

ggplot() +
  geom_tile(data = df.all.selected %>%
              filter(model %in% models.selected),
            aes(x = lon, y = lat,
                fill = MCWD)) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +

  scale_fill_gradient2(limits = c(-500,0),low = "darkred",high = "darkblue",
                       midpoint = -200,
                       oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ model)


ggplot() +
  geom_raster(data = df.all.selected %>%
                filter(model %in% models.selected),
              aes(x = lon, y = lat,
                  fill = as.factor(type))) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ model)

