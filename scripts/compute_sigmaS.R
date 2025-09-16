rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(modi)

vars2test <- c("MCWD","MAP")

models.selection <- readRDS("./outputs/models.selected.RDS")

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
                               filter(scenario == "ssp585",
                                      period == 2090) %>%
                               ungroup() %>%
                               distinct())

}

df.all.long <-
  df.all %>%
    filter(model %in% models.selection) %>%
    dplyr::select(-c(ETeq,period,scenario,Etot)) %>%
  pivot_longer(cols = c(MAP,MAT,MCWD),
               names_to = "variable",
               values_to = "value") %>%
  filter(variable %in% vars2test)

models <- sort(unique(df.all.long %>%
                        filter(model %in% c(models.selection)) %>%
                        pull(model)))

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

sigma_S_main_all = seq(0.1,2,0.05) # model similarities

S <- S.all
S.M <- S %>%
  group_by(variable) %>%
  summarise(M = mean(RMSE,na.rm = TRUE))


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
            .groups = "keep")

ggplot(data = cSnorm.sum) +
  geom_tile(aes(x = cimodel,
                y = cjmodel,
                fill = delta)) +
  theme_bw() +
  scale_fill_gradient2(low = "darkblue",mid = "white",high = "darkred",
                       midpoint = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

df.weights <- data.frame()

for (j in seq(1,length(sigma_S_main_all))){

    sigma_S_main <- sigma_S_main_all[j]

    print(paste("sigma_S = ",sigma_S_main))

    Snorm.sum <- cSnorm.sum %>%
      mutate(sigma_S = sigma_S_main) %>%
      mutate(S = exp(-(delta/sigma_S)**2))

    w_u <- Snorm.sum %>%
      group_by(cimodel) %>%
      summarise(w_u = 1/(1 + sum(S,na.rm = TRUE)),
                .groups = "keep") %>%
      rename(model = cimodel)

    df.weights <- bind_rows(df.weights,
                            w_u %>%
                              mutate(sigma_S = sigma_S_main))
}

df.weights.group <-
  df.weights %>%
  mutate(group = case_when(grepl("ACCESS",model) ~ "ACCESS variants",
                           grepl("CanESM5",model) ~ "CANESM5 variants",
                           grepl("EC-Earth",model) ~ "EC-Earth variants",
                           grepl("MPI-ESM1",model) ~ "MPI-ESM1 variants",
                           grepl("NorESM2",model) ~ "NorESM2 variants",
                           grepl("INM-CM",model) ~ "INM-CM variants",
                           grepl("CMCC",model) ~ "CMCC variants",


                           TRUE ~ "other")) %>%
  mutate(grouping = case_when(grepl("variants",group) ~ "Variants",
                              TRUE ~ "Unique"))

ggplot(data = df.weights.group) +
  geom_line(aes(x = sigma_S, y = w_u,
                color = grouping,
                group = model)) +
  scale_x_continuous(limits = c(0,2)) +
  theme_bw()


ggplot(data = df.weights.group %>%
         filter(grouping == "Unique")) +
  geom_line(aes(x = sigma_S, y = w_u,
                color = model,
                group = model)) +
  scale_x_continuous(limits = c(0,2)) +
  theme_bw()

df.weights.group.sum <- df.weights.group %>%
  group_by(grouping,sigma_S) %>%
  summarise(w_u.m = mean(w_u,na.rm = TRUE),
            .groups = "keep")

RSE <- c()

for (j in seq(1,length(sigma_S_main_all))){
  cdf <- df.weights.group %>%
    filter(sigma_S == sigma_S_main_all[j])

  cw_u <- cdf$w_u
  cgrouping <- cdf$grouping
  target <- rep(1,length(cgrouping))
  target[cgrouping != "Unique"] <- 0.5

  RSE[j] <- sum((cw_u - target)**2)
}

final <- sigma_S_main_all[which.min(RSE)]

ggplot(data = df.weights.group.sum) +
  geom_line(data = df.weights.group,
            aes(x = sigma_S, y = w_u,
                color = grouping,
                group = model),
            size = 0.1) +
  geom_line(aes(x = sigma_S, y = w_u.m,
                color = grouping)) +
  geom_vline(xintercept = final,linetype = 2,
             color = "black") +
  scale_x_continuous(limits = c(0,2)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",
       y = "") +
  guides(color = "none")
