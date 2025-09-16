rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)
library(sf)
library(SPEI)
library(caret)

models.selection <- readRDS("./outputs/models.selected.RDS")

# system2("rsync",
#         paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.classifications.*",
#               "./outputs/"))

coord <- expand.grid(lat = seq(-23.25,23.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

data <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.ERA5_coord.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat))

data.selected <- data %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  dplyr::select(lon,lat,MAP,MAT,MCWD) %>%
  mutate(MAT = MAT - 273.15) %>%
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

CMIP6.files <- list.files("./outputs","*CMIP6.classifications*",
                          full.names = TRUE)
CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]

df.all <- data.frame()
for (file in CMIP6.files){

  print(file)
  CMIP6 <- readRDS(file) %>%
    filter(scenario == "historical",
           period == 2000) %>%
    ungroup() %>%
    distinct()

  df.all <- bind_rows(df.all,
                      CMIP6)
}

model.selected <- df.all %>%
  filter(model %in% models.selection) %>%
  dplyr::select(lon,lat,model,MAP,MAT,MCWD) %>%
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
              dplyr::select(lon,lat,MAT,MAP,MCWD,biome) %>%
              pivot_longer(cols = c(MAT,MAP,MCWD,biome),
                           values_to = "Obs",
                           names_to = "variable") %>%
              mutate(source = "obs"),
            by = c("lon","lat","variable"))


model.and.data <- bind_rows(model.vs.data,
                            data.selected %>%
                              dplyr::select(lon,lat,MAT,MAP,MCWD,biome) %>%
                              pivot_longer(cols = c(MAT,MAP,MCWD,biome),
                                           values_to = "Obs",
                                           names_to = "variable") %>%
                              mutate(source = "obs") %>%
                              mutate(Model = Obs,
                                     model = "CRUJRA"))

models <- unique(model.and.data$model)

vars2test.all <- list(c("MAP"),
                      c("MCWD"),
                      c("MAP","MCWD"))

df.weights.all <- df.diff.all <-
  data.frame()

sigma_D_main = 0.8 # model performance
sigma_S_main = 0.75 # model similarities

for(ivar in seq(1,length(vars2test.all))){
  vars2test <- vars2test.all[[ivar]]

  print("===================")
  print(paste("-",paste0(vars2test,collapse = "_")))

  D <- data.frame() ; S <- data.frame()

  for (imodel in seq(1,length(models))){
    cimodel = models[imodel]

    print(paste0(cimodel))

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
                         group_by(variable) %>%
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
    ungroup() %>%
    arrange(desc(Acc))

  Snorm <- S %>%
    filter(!is.na(RMSE)) %>%
    group_by(variable) %>%
    mutate(delta = RMSE/mean(RMSE,na.rm = TRUE))

  ggplot(data = Snorm) +
    geom_density(aes(x = delta,
                     fill = variable),
                 alpha = 0.5) +
    theme_bw()

  Snorm.sum <- Snorm %>%
    group_by(cimodel,cjmodel) %>%
    summarise(delta = sum(delta,
                          na.rm = TRUE)*(2/length(vars2test)),    # To have similar deltas
              .groups = "keep") %>%
    mutate(sigma_S = sigma_S_main) %>%
    mutate(S = exp(-(delta/sigma_S)**2))

  # ggplot(data = Snorm.sum) +
  #   geom_raster(aes(x = cimodel,
  #                   y = cjmodel,
  #                   fill = delta),
  #               alpha = 0.5) +
  #   # scale_bre(palette = "OrRd") +
  #   theme_bw()

  w_u <- Snorm.sum %>%
    group_by(cimodel) %>%
    summarise(w_u = 1/(1 + sum(S,na.rm = TRUE)),
              .groups = "keep") %>%
    rename(model = cimodel)

  # ggplot(data = w_u) +
  #   geom_density(aes(x = w_u),
  #                alpha = 0.5) +
  #   theme_bw()

  w_q <- Snorm.sum %>%
    filter(cimodel == "CRUJRA") %>%
    group_by(cjmodel) %>%
    summarise(w_q = exp(-(delta/sigma_D_main)**2),
              .groups = "keep") %>%
    rename(model = cjmodel)

  df.weights <- bind_rows(w_q %>%
                            left_join(w_u,
                                      by = c("model")) %>%
                            group_by(model) %>%
                            summarise(w = w_q*w_u,
                                      .groups = "keep") %>%
                            ungroup() %>%
                            mutate(w = w/(sum(w,na.rm = TRUE))),
                          data.frame(model = c("CRUJRA"),
                                     w = 1))

  df.weights.all <- bind_rows(
    df.weights.all,
    df.weights %>%
      mutate(var.test = paste0(vars2test,collapse = "_"))
  )


  df.all.selected <- bind_rows(
    model.selected %>%
      mutate(source = "CMIP6"),
    data.selected %>%
      mutate(source = "obs",
             model = "CRUJRA")) %>%
    left_join(df.weights %>%
                dplyr::select(model,w),
              by = c("model")) %>%
    ungroup()

  df.all.selected.sum <- bind_rows(df.all.selected %>%
                                     group_by(source,lat,lon) %>%
                                     summarise(MAP = weighted.mean(MAP,w,na.rm = TRUE),
                                               MAT = weighted.mean(MAT,w,na.rm = TRUE),
                                               MCWD = weighted.mean(MCWD,w,na.rm = TRUE),
                                               .groups = "keep"),

                                   df.all.selected %>%
                                     filter(source == "CMIP6") %>%
                                     group_by(source,lat,lon) %>%
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


  df.diff.temp <- df.all.selected.sum %>%
    mutate(biome.num = as.numeric(biome)) %>%
    dplyr::select(source,lon,lat,source,biome.num,MAP,MCWD,MAT) %>%
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
              by = c("lon","lat","variable")) %>%
    filter(!is.na(obs)) %>%
    group_by(source,variable) %>%
    mutate(diff = obs - mod)

  df.diff.all <- bind_rows(df.diff.all,
                           df.diff %>%
                             mutate(var.test = paste0(vars2test,collapse = "_")))

}


df.formatted <- df.weights.all %>%
  pivot_wider(names_from = var.test,
              values_from = w) %>%
  pivot_longer(cols = c(MAP,MCWD),
               names_to = "variable",
               values_to = "w") %>%
  filter(model != "CRUJRA")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

df2plot <- df.diff.all %>%
  filter(source == "Weighted.mean",
         variable == "biome.num")

ggplot() +
  geom_raster(data = df2plot,
              aes(x = lon, y = lat,
                  fill = diff)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(low = "darkred",
                       high = "darkblue") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ var.test, ncol = 1) +
  theme(legend.position = "bottom")

ggplot(data = df.diff.all %>%
         filter(source == "Weighted.mean",
                variable %in% c("MAP","MCWD","biome.num"))) +
  geom_density(aes(x = abs(diff),fill = var.test), alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

df.diff.all %>%
  filter(source == "Weighted.mean",
         variable %in% c("MAP","MCWD","biome.num")) %>%
  group_by(var.test,variable) %>%
  summarise(diff.m = mean(diff),
            RMSE = sqrt(sum(1/length(diff)*(sum(diff**2)))),
            diff.abs.m = mean(abs(diff)),
            .groups = "keep")


ggplot(data = df.formatted,
       aes(x = MAP_MCWD,y = w, color = variable, fill = variable)) +
  geom_point() +
  stat_smooth(se = FALSE, method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  theme_bw() +
  theme(legend.position = c(0.8,0.2)) +
  labs(x = "Multivariate weight",
       y = "Univariate weight")

df.formatted %>%
  arrange(desc(MAP_MCWD))

saveRDS(df.formatted,
        "./outputs/df.weights.all_criteria.RDS")


confusionMatrix(factor(df.diff.all %>%
                         filter(variable == "biome.num",
                                source == "Weighted.mean",
                                var.test == "MCWD") %>%
                         pull(mod),
                       levels = 1:7),
                factor(df.diff %>%
                         filter(variable == "biome.num",
                                source == "Mean") %>%
                         pull(obs),
                       levels = 1:7))
