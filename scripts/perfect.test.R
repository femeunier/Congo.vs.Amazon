rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(modi)

vars2test <- c("MCWD","MAP")

models.selection <- readRDS("./outputs/models.selected.RDS")
data <- readRDS("./outputs/Data.used.RDS")

# scp ./outputs/models.selected.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp ./outputs/Data.used.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/

# Reading the model outputs
CMIP6.files <- list.files("./outputs","*CMIP6.classifications*",
                          full.names = TRUE)
CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]

df.all <- df.all.future <- data.frame()
for (file in CMIP6.files){

  print(file)
  CMIP6 <- readRDS(file)

  df.all <- bind_rows(df.all,
                      CMIP6 %>%
                        filter(scenario == "historical",
                               period == 2000) %>%
                        ungroup() %>%
                        distinct())

  df.all.future <- bind_rows(df.all.future,
                             CMIP6 %>%
                               filter(scenario %in% c("ssp245","ssp585"),
                                      period %in% c(2050,2090)) %>%
                               ungroup() %>%
                               distinct())

}

df.all.long <- bind_rows(
  df.all %>%
    filter(model %in% models.selection) %>%
    dplyr::select(-c(ETeq,period,scenario,Etot)),

  data %>%
    mutate(model = "data") %>%
    dplyr::select(-c(AI,biome))) %>%
  pivot_longer(cols = c(MAP,MAT,MCWD),
               names_to = "variable",
               values_to = "value") %>%
  filter(variable %in% vars2test)

models <- sort(unique(df.all.long %>%
                        filter(model %in% c("data",models.selection)) %>%
                        pull(model)))

df.all.long.future <- df.all.future %>%
  filter(model %in% models) %>%
  dplyr::select(-c(ETeq)) %>%
  pivot_longer(cols = c(MAP,MAT,MCWD,Etot),
               names_to = "variable",
               values_to = "value") %>%
  filter(variable %in% vars2test)

# First we compute all the distances
S.all <- data.frame()

for (imodel in seq(1,length(models))){
  cimodel = models[imodel]

  print(cimodel)

  for (jmodel in seq(1,length(models))){
    cjmodel = models[jmodel]

    if (cjmodel != cimodel){

      S.all <- bind_rows(S.all,
                         df.all.long %>%
                           filter(model %in% c(cimodel,cjmodel)) %>%
                           ungroup() %>%
                           mutate(type = case_when(model == cimodel ~ "Obs",
                                                   model == cjmodel ~ "Model")) %>%
                           dplyr::select(-model) %>%
                           pivot_wider(names_from = type,
                                       values_from = value) %>%
                           group_by(variable) %>%
                           summarise(RMSE = sqrt(1/length(Obs[which(!is.na(Obs))])*sum((Model - Obs)**2,
                                                                                       na.rm = TRUE)),
                                     .groups = "keep") %>%
                           mutate(cimodel,
                                  cjmodel))
    }
  }
}

sigma_D_main_all = seq(0.1,2,0.1) # model performance
sigma_S_main_all = 0.75 # model similarities

models <- models[models != "data"]

S.data <- S.all %>%
  filter(cimodel == "data") %>%
  arrange((RMSE))

S <- S.all %>%
  filter(cimodel != "data",
         cjmodel != "data")
S.M <- S %>%
  group_by(variable) %>%
  summarise(M = mean(RMSE,na.rm = TRUE))

saveRDS(S.M,
        "./outputs/intermodel.distance.RDS")

Snorm.data <- S.data %>%
  filter(!is.na(RMSE)) %>%
  group_by(variable) %>%
  left_join(S.M,
            by = "variable") %>%
  mutate(delta = RMSE/M)

Snorm.data.sum <- Snorm.data %>%
  group_by(cimodel,cjmodel) %>%
  summarise(delta = sum(delta,
                        na.rm = TRUE),
            .groups = "keep") %>%
  arrange(delta) %>%
  ungroup() %>%
  slice_head(n = 1)

Snorm <- S %>%
  filter(!is.na(RMSE)) %>%
  group_by(variable) %>%
  left_join(S.M,
            by = "variable") %>%
  mutate(delta = RMSE/M)

cSnorm.sum <- Snorm %>%
  group_by(cimodel,cjmodel) %>%
  summarise(delta = sum(delta,
                        na.rm = TRUE),
            .groups = "keep") %>%
  filter(delta >= Snorm.data.sum[["delta"]])


df.summ <- model.combin <-
  data.frame()

for (irefmodel in seq(1,length(models))){

  cmodel <- models[irefmodel]  # truth model
  print(cmodel)

  df.obs <-  df.all.long %>%
    filter(model == cmodel)

  df.obs.future <-  df.all.long.future %>%
    filter(model == cmodel)

  for (i in seq(1,length(sigma_D_main_all))){
    for (j in seq(1,length(sigma_S_main_all))) {

      sigma_D_main <- sigma_D_main_all[i]
      sigma_S_main <- sigma_S_main_all[j]

      print(paste("sigma_D = ",sigma_D_main,", sigma_S = ",sigma_S_main))

      Snorm.sum <- cSnorm.sum %>%
        mutate(sigma_S = sigma_S_main) %>%
        mutate(S = exp(-(delta/sigma_S)**2))

      w_u <- Snorm.sum %>%
        group_by(cimodel) %>%
        summarise(w_u = 1/(1 + sum(S,na.rm = TRUE)),
                  .groups = "keep") %>%
        rename(model = cimodel)


      w_q <- Snorm.sum %>%
        filter(cimodel == cmodel) %>%
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
                                mutate(w = w/(sum(w,na.rm = TRUE))))

      df.all.long.future.w <- df.all.long.future %>%
        left_join(df.weights,
                  by = "model") %>%
        filter(!is.na(w)) %>%
        group_by(period,scenario,lon,lat,variable) %>%
        summarise(pred.w = weighted.mean(value,w,na.rm = TRUE),
                  pred.w.low = weighted.quantile(value,w,prob = 0.1),
                  pred.w.high = weighted.quantile(value,w,prob = 0.9),
                  pred.m = mean(value,na.rm = TRUE),
                  .groups = "keep")


      obs.vs.model.future <- df.all.long.future.w %>%
        ungroup() %>%
        left_join(df.obs.future %>%
                    dplyr::select(-model),
                  by = c("period","scenario","variable","lon","lat"))


      frac <- obs.vs.model.future %>%
        ungroup() %>%
        rowwise() %>%
        mutate(success = (abs(value) >= min(abs(c(pred.w.low,
                                                  pred.w.high)))) &
                            (abs(value) <= max(abs(c(pred.w.low,
                                                     pred.w.high)))))
      frac.m <- frac %>%
        group_by(variable) %>%
        summarise(r = sum(success,na.rm = TRUE)/length(success[!is.na(success)]))

      df.summ <- bind_rows(df.summ,
                           frac.m %>%
                             mutate(sigma_D = sigma_D_main,
                                    sigma_S = sigma_S_main,
                                    model = cmodel))

      model.combin <- bind_rows(model.combin,
                                data.frame(ref.model = cmodel,
                                           model = df.weights$model))


    }
  }
}

saveRDS(df.summ,"./outputs/df.summ.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/perfect.test.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
