# Calculate weights

rm(list = ls())

library(dplyr)
library(ggplot2)
library(YGB)
library(raster)


# CMIP6
LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

all.CMIP6.MCWD <- readRDS("./outputs/All.CMIP6.states.MEM.RDS") %>%
  ungroup() %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  dplyr::select(-LC) %>%
  left_join(LC,
            by = c("lon","lat"))

all.CMIP6.MCWD.select <- all.CMIP6.MCWD %>%
  ungroup() %>%
  filter(period == "historical") %>%
  dplyr::select(model,lon,lat,month,pre,MCWD) %>%
  group_by(model,lon,lat) %>%
  summarise(MAP = sum(pre),
            MCWD = unique(MCWD),
            .groups = "keep") %>%
  ungroup()


# Reanalysis

MEM.climate <- readRDS("./outputs/Climate.CA.summ.RDS") %>%
  ungroup() %>%
  filter(model == "MEM") %>%
  dplyr::select(lon,lat,month,pre,MCWD)
MEM.climate.MCWD <- MEM.climate %>%
  group_by(lon,lat) %>%
  summarise(MAP.ref = sum(pre),
            MCWD.ref = unique(MCWD),
            .groups = "keep")

Nmodel <- all.CMIP6.MCWD.select %>%
  filter(model != "MEM") %>%
  pull(model) %>%
  unique()
Mean.models <- all.CMIP6.MCWD.select %>%
  filter(model != "MEM") %>%
  group_by(model) %>%
  summarise(MAP = mean(MAP),
            MCWD = mean(MCWD),
            .groups = "keep")

d_MAP <- mean(as.matrix(dist(Mean.models$MAP, method = "euclidean")))
d_MCWD <- mean(as.matrix(dist(Mean.models$MCWD, method = "euclidean")))

CMIP6.vs.reanal <- all.CMIP6.MCWD.select %>%
  left_join(MEM.climate.MCWD,
            by = c("lon","lat"))

CMIP6.vs.reanal.sum <- CMIP6.vs.reanal %>%
  group_by(model) %>%
  na.omit() %>%
  summarise(RMSE.MAP = sqrt(1/length(MAP)*(sum((MAP - MAP.ref)**2))),
            RMSE.MCWD = sqrt(1/length(MCWD)*(sum((MCWD - MCWD.ref)**2))),
            .groups = "keep") %>%
  mutate(rel.RMSE.MAP = RMSE.MAP/d_MAP,
         rel.RMSE.MCWD = RMSE.MCWD/d_MCWD) %>%
  mutate(Sigma = rel.RMSE.MAP + rel.RMSE.MCWD) %>%
  arrange((Sigma)) %>%
  ungroup() %>%
  mutate(w = exp(-(Sigma/0.7)**2)) %>%
  mutate(w = w/sum(w))

df.weights <- CMIP6.vs.reanal.sum %>%
  dplyr::select(model,w)

hist(CMIP6.vs.reanal.sum$w)

CMIP6.wMEM <- CMIP6 %>%
  left_join(df.weights,
            by = "model") %>%
  group_by(scenario,period,lon,lat,month) %>%
  summarise(pre = weighted.mean(pre,w,na.rm = TRUE),
            tas = weighted.mean(tas,w,na.rm = TRUE),
            tasmin = weighted.mean(tasmin,w,na.rm = TRUE),
            tasmax = weighted.mean(tasmax,w,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(model = "wMEM")

LC <- readRDS("./data/LC_Congo.RDS")

CMIP6.wMEM.MCWD <- CMIP6.wMEM %>%
  ungroup() %>%
  mutate(Ndays = dayspermonth[as.integer(month)]) %>%
  group_by(model,scenario,period,lon,lat) %>%
  mutate(Ndays = dayspermonth,
         E = SPEI::hargreaves(tasmin,
                              tasmax,
                              lat = unique(lat),
                              Ra = NULL,
                              na.rm = TRUE,
                              verbose = FALSE)/Ndays,
         Etot = E*Ndays) %>%
  ungroup() %>%
  group_by(model,scenario,period,lon,lat) %>%
  mutate(diff = pre - Etot) %>%
  mutate(wettest.month = which.max(pre)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,
                        CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD))

all.CMIP6.wMCWD <- bind_rows(all.CMIP6.MCWD,
                            CMIP6.wMEM.MCWD) %>%
  filter(lat >= - 15, lat <= 10) %>%
  left_join(LC,
            by = c("lon","lat"))

saveRDS(all.CMIP6.wMCWD,
        "./outputs/All.CMIP6.states.wMEM.RDS")

threshold.sum <- readRDS(
  "./outputs/Sensitivity.thresholds.sum.RDS")
MAP.threshold <- threshold.sum %>%
  filter(variable == "MAP") %>%
  pull(mean)
MCWD.threshold <- threshold.sum %>%
  filter(variable == "MCWD") %>%
  pull(mean)

df2test <- all.CMIP6.wMCWD %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  ungroup() %>%
  filter(period == "historical") %>%
  dplyr::select(scenario,model,lon,lat,month,pre,MCWD) %>%
  group_by(scenario,model,lon,lat) %>%
  summarise(MAP = sum(pre),
            N = length(unique(MCWD)),
            MCWD = unique(MCWD),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(LC.pred = as.factor(case_when(MCWD > MCWD.threshold ~ 2,
                                       MAP < MAP.threshold ~ 1,
                                       TRUE ~ 3))) %>%
  left_join(LC %>%
              mutate(lon = round(lon,2),
                     lat = round(lat,2)) ,
            by = c("lon","lat"))

CM <- df2test %>%
  filter(LC %in% c(1:3)) %>%
  ungroup() %>%
  mutate(LC = as.factor(LC),
         LC.pred = as.factor(LC.pred)) %>%
  group_by(model) %>%
  summarise(Acc = (confusionMatrix(LC.pred,LC))[["overall"]][1],
            precision = mean(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                             na.rm = TRUE),
            precision.min = min(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                na.rm = TRUE))

hist(CM$Acc)

CM %>%
  arrange(Acc)

CM %>%
  filter(model %in% c("MEM","wMEM"))
