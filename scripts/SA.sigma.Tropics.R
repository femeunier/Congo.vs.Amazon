rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)

vars2test <- c("MCWD","MAP")
# vars2test <- c("biome")

models.selection <- readRDS("./outputs/models.selected.RDS")
# models.selection <- readRDS("./outputs/models.all.RDS")
# models.selection <- readRDS("./outputs/models.penman.RDS")

sigma_D_main_all = seq(0.1,2,0.05) # model performance
sigma_S_main_all = c(0.75) # model similarities

data.selected <- readRDS("./outputs/data.selected.Tropics.RDS")
model.selected <- readRDS("./outputs/model.selected.Tropics.RDS") %>%
  filter(model %in% models.selection)
Snorm <- readRDS("./outputs/Data.vs.Model.Tropics.RDS") %>%
  filter(cimodel %in% c(models.selection,"CRUJRA"),
         cjmodel %in% c(models.selection,"CRUJRA")) %>%
  filter(variable %in% vars2test)

df.Acc.all <- data.frame() ; df.rsq <- data.frame() ; df.N <-
  df.weights.all <- data.frame()

for (i in seq(1,length(sigma_D_main_all))){
  for (j in seq(1,length(sigma_S_main_all))) {

    sigma_D_main <- sigma_D_main_all[i]
    sigma_S_main <- sigma_S_main_all[j]

    print(paste("sigma_D = ",sigma_D_main,", sigma_S = ",sigma_S_main))

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

    df.N <- bind_rows(df.N,
                      bind_rows(df.weights %>%
                                  filter(model != "CRUJRA") %>%
                                  filter(w > 0.02) %>%
                                  ungroup() %>%
                                  summarise(N = n(),
                                            .groups = "keep")) %>%
                        mutate(sigma_D = sigma_D_main,
                               sigma_S = sigma_S_main))

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

    CF <- df.diff %>%
      filter(variable == "biome.num") %>%
      group_by(source,mod,obs) %>%
      summarise(N = n(),
                .groups = "keep")

    temp1 <- bind_rows(df.diff %>%
                         group_by(source) %>%
                         summarise(Acc = (confusionMatrix(factor(mod,levels = 1:7),
                                                          factor(obs,levels = 1:7)))
                                   [["overall"]][1],
                                   .groups = "keep") %>%
                         pivot_wider(names_from = source,
                                     values_from = c(Acc)))

    temp2 <- bind_rows(df.diff %>%
                         filter(variable %in% c("MAP","MCWD","MAT")) %>%
                         group_by(source,variable) %>%
                         summarise(r2 = summary(lm(formula = obs~mod))[["r.squared"]],
                                   RMSE = sqrt(1/sum(!is.na(obs))*sum((obs-mod)**2)),
                                   .groups = "keep") %>%
                         pivot_wider(names_from = source,
                                     values_from = c(r2,RMSE)))

    df.Acc.all <- bind_rows(df.Acc.all,
                            temp1 %>%
                              mutate(sigma_D = sigma_D_main,
                                     sigma_S = sigma_S_main))
    df.rsq <- bind_rows(df.rsq,
                        temp2 %>%
                          mutate(sigma_D = sigma_D_main,
                                 sigma_S = sigma_S_main))

  }
}

ggplot(data = df.N %>%
         filter(sigma_S == min(sigma_S))) +
  geom_line(aes(x = sigma_D,
                y = N)) +
  theme_bw()

ggplot(data = df.Acc.all %>%
         filter(sigma_S == min(sigma_S))) +
  geom_line(aes(x = sigma_D,
                y = Weighted.mean)) +
  geom_point(aes(x = sigma_D,
                 y = Weighted.mean)) +
  geom_line(aes(x = sigma_D,
                y = Mean),
            linetype = 2) +
  theme_bw()

ggplot(data = df.rsq %>%
         filter(sigma_S == min(sigma_S),
                variable %in% vars2test)) +
  geom_line(aes(x = sigma_D,
                y = RMSE_Weighted.mean/RMSE_Mean,
                color = variable)) +
  # geom_point(aes(x = sigma_D,
  #                y = RMSE_Weighted.mean)) +
  geom_line(aes(x = sigma_D,
                y = 1),
            linetype = 2) +
  geom_vline(xintercept = 0.85,linetype = 2) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = "none")

ggplot(data = df.Acc.all) +
  geom_raster(aes(x = sigma_D,
                  y = sigma_S,
                  fill = Weighted.mean)) +
  scale_fill_gradient2(low = "darkred",
                       high = "darkgreen",
                       midpoint = 0.65) +
  theme_bw()
