rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)
library(sf)
library(SPEI)
library(caret)
library(reshape2)

models.selection <- readRDS("./outputs/models.selected.RDS")
scenarios <- c("historical","ssp126","ssp245","ssp370","ssp585")

coord <- expand.grid(lat = seq(-23.25,23.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

data <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.ERA5_coord.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat))

data.selected <- data %>%
  ungroup() %>%
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


df.biomass <- readRDS("./outputs/Biomass.per.biome.Tropics.RDS") %>%
  group_by(biome) %>%
  summarise(AGB = mean(AGB,na.rm = TRUE),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

CMIP6.files <- list.files("./outputs","*CMIP6.classifications*",
                          full.names = TRUE)
CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]

################################################################
# Reading all
# df.all <- data.frame()
# for (file in CMIP6.files){
#
#   print(file)
#   CMIP6 <- readRDS(file) %>%
#     filter(scenario == "historical",
#            period == 2000) %>%
#     ungroup() %>%
#     distinct()
#
#   df.all <- bind_rows(df.all,
#                       CMIP6)
# }
# saveRDS(df.all,
#         "./outputs/All.historical.RDS")

df.all <- readRDS("./outputs/All.historical.RDS")

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

D <- data.frame() ; S <- data.frame()

vars2test <- c("MAP","MCWD")
# vars2test <- c("biome")

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


# df.all.scenarios <- data.frame()
# for (file in CMIP6.files){
#
#   print(file)
#   temp <- readRDS(file)
#   CMIP6 <- bind_rows(temp %>%
#                        filter(scenario == "historical") %>%
#                        group_by(period,scenario,model,lon,lat) %>%
#                        slice_head(n = 1),
#                      temp %>%
#                        filter(scenario != "historical")) %>%
#     ungroup()
#
#   df.all.scenarios <- bind_rows(df.all.scenarios,
#                                 CMIP6)
# }
# saveRDS(df.all.scenarios,"./outputs/All.scenarios.RDS")

df.all.scenarios <- readRDS("./outputs/All.scenarios.RDS")

df.all.selected.scenarios <- df.all.scenarios %>%
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
                                   "Semiarid","Arid","Hyperarid")))



coord <- expand.grid(lat = seq(-30.25,30.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                  lat = as.vector(unique(coord$lat))) %>%
  melt() %>%
  mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
         Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
  rename(lon = Var1,
         lat = Var2,
         area = value) %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100) %>%
  left_join(LandFrac,
            by = c("lat","lon")) %>%
  mutate(area = area*value)

sigma_D_main_all <-
  seq(0.1,2,0.1)

sigma_S_main_all <-
  seq(0.15,2,0.1)

df.SS <- df.weights.all <- data.frame()
for (i in seq(1,length(sigma_D_main_all))){
  for (j in seq(1,length(sigma_S_main_all))){

    sigma_D_main = sigma_D_main_all[i] # model performance
    sigma_S_main = sigma_S_main_all[j] # model similarities

    print(paste(sigma_S_main,"-",sigma_D_main))

    Snorm <- S %>%
      filter(!is.na(RMSE)) %>%
      group_by(variable) %>%
      mutate(delta = RMSE/mean(RMSE,na.rm = TRUE))

    Snorm.sum <- Snorm %>%
      group_by(cimodel,cjmodel) %>%
      summarise(delta = sum(delta,
                            na.rm = TRUE),
                .groups = "keep") %>%
      mutate(sigma_S = sigma_S_main) %>%
      mutate(S = exp(-(delta/sigma_S)**2))

    w_u <- Snorm.sum %>%
      group_by(cimodel) %>%
      summarise(w_u = 1/(1 + sum(S,na.rm = TRUE)),
                .groups = "keep") %>%
      rename(model = cimodel)

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

    df.weights.all <- bind_rows(df.weights.all,
                                df.weights %>%
                                  mutate(sigma_D = sigma_D_main,
                                         sigma_S = sigma_S_main))

    ################################################################################
    # Weighting difference

    df.all.selected.weights <- df.all.selected.scenarios %>%
      filter(period %in% c(2000,2090)) %>%
      left_join(df.weights %>%
                  dplyr::select(model,w),
                by = c("model")) %>%
      mutate(w = case_when(is.na(w) ~ 0,
                           TRUE ~ w))

    df.all.selected.sum <- df.all.selected.weights %>%
      group_by(period,scenario,lat,lon) %>%
      summarise(MAP.m = mean(MAP,na.rm = TRUE),
                MAT.m = mean(MAT,na.rm = TRUE),
                MCWD.m = mean(MCWD,na.rm = TRUE),
                Etot.m = mean(Etot,na.rm = TRUE),

                MAP.w = weighted.mean(MAP[!is.na(w)],w[!is.na(w)],na.rm = TRUE),
                MAT.w = weighted.mean(MAT[!is.na(w)],w[!is.na(w)],na.rm = TRUE),
                MCWD.w = weighted.mean(MCWD[!is.na(w)],w[!is.na(w)],na.rm = TRUE),
                Etot.w = weighted.mean(Etot[!is.na(w)],w[!is.na(w)],na.rm = TRUE),

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
      mutate(biome = factor(biome,
                            levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                       "Semiarid","Arid","Hyperarid"))) %>%
      filter(!is.na(biome))

    # Transitions
    cscenarios <- scenarios[scenarios != "historical"]
    df.scenarios <- df.scenarios.all <-
      data.frame()
    for (cscenario in cscenarios){

      df.scenarios <- bind_rows(df.scenarios,
                                df.all.sum %>%
                                  filter(scenario == "historical") %>%
                                  mutate(scenario = cscenario) %>%
                                  distinct(),
                                df.all.sum %>%
                                  filter(scenario == cscenario))

    }

    df.scenarios <- df.scenarios %>%
      left_join(Gridarea,
                by = c("lon","lat")) %>%
      mutate(continent = case_when(lon <= -30 ~ "America",
                                   lon <= 55 ~ "Africa",
                                   TRUE ~ "Australasia"))

    df.AGB <- df.scenarios %>%
      mutate(biome = factor(as.character(biome),
                            levels = rev(levels(df.scenarios$biome)))) %>%
      left_join(df.biomass %>%
                  dplyr::select(-any_of("source")),
                by = c("biome"))

    df.AGB.sum <- df.AGB %>%
      group_by(period,scenario,weighting) %>%
      summarise(AGB = sum(AGB*area,
                          na.rm = TRUE)/1e12,
                .groups = "keep") %>%
      group_by(scenario,weighting) %>%
      arrange(period) %>%
      mutate(AGB.change = AGB - AGB[1])

    df.delta.AGB <- df.AGB %>%
      filter(period > 1990) %>%
      group_by(scenario,weighting,lat,lon) %>%
      arrange(period) %>%
      mutate(AGB.change = (AGB - AGB[1])*area/1e12,
             AGB.change.rel = 100*AGB.change/(AGB[1]*area/1e12))

    df.AGB.biome.sum <- df.AGB %>%
      group_by(biome,period,scenario,weighting) %>%
      summarise(AGB = sum(AGB*area,
                          na.rm = TRUE)/1e12,
                .groups = "keep") %>%
      group_by(biome,scenario,weighting) %>%
      arrange(period) %>%
      mutate(AGB.change = AGB - AGB[1])


    # df.AGB.biome.sum %>%
    #   group_by(period,scenario,weighting) %>%
    #   summarise(AGB.tot = sum(AGB)) %>%
    #   filter(period %in% c(2000,2090))

    df.SS <- bind_rows(df.SS,
                       df.AGB.biome.sum %>%
                         mutate(sigma_D = sigma_D_main,
                                sigma_S = sigma_S_main))

  }
}

saveRDS(df.SS,
        "./outputs/SS.sigma.RDS")
saveRDS(df.weights.all,
        "./outputs/df.weights.all.RDS")
