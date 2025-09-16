rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)
library(sf)
library(SPEI)
library(caret)

# models.selection <- readRDS("./outputs/models.selected.RDS")
models.selection <- readRDS("./outputs/models.all.RDS")
# models.selection <- readRDS("./outputs/models.penman.RDS")

system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.classifications.*",
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


CMIP6.files <- list.files("./outputs","CMIP6.classifications*",
                          full.names = TRUE)
CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]

df.all <- data.frame()
for (file in CMIP6.files){

  CMIP6 <- readRDS(file) %>%
    filter(scenario == "historical",
           period == "current")

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

vars2test <- c("MAP","MCWD","MAT","biome")

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

Snorm <- S %>%
  filter(!is.na(RMSE)) %>%
  group_by(variable,basin) %>%
  mutate(delta = RMSE/mean(RMSE,na.rm = TRUE))

saveRDS(Snorm,
        "./outputs/Data.vs.Model.RDS")
saveRDS(data.selected,
        "./outputs/data.selected.RDS")
saveRDS(model.selected,
        "./outputs/model.selected.RDS")
