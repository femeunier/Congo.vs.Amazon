rm(list = ls())

library(dplyr)
library(ggplot2)
library(YGB)
library(raster)

dayspermonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# system2("rsync",
#         c("-avz",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/All.CMIP6.states.timing.RDS",
#           "./outputs/"))

CMIP6 <- readRDS("./outputs/All.CMIP6.states.timing.RDS") %>%
  filter(!(model %in% c("CIESM"))) %>%
  rename(pre = Pmm,
         period = timing) %>%
  filter(Emodel %in% c("Hargreaves",
                       "Hargreaves_interp")) #%>%
# filter((scenario %in% c("ssp534-over")))

CMIP6.MEM <- CMIP6 %>%
  filter(!(model %in% c("CIESM","NESM"))) %>%
  group_by(scenario,period,lon,lat,month) %>%
  summarise(pre = mean(pre,na.rm = TRUE),
            tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(model = "MEM")

# LC <- as.data.frame(readRDS("./outputs/LC.rspld.RDS"),
#                     xy = TRUE) %>%
#   rename(lon = x,
#          lat = y)

LC <- readRDS("./data/LC_Congo.RDS")

CMIP6.MEM.MCWD <- CMIP6.MEM %>%
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

all.CMIP6.MCWD <- bind_rows(CMIP6,
                            CMIP6.MEM.MCWD) %>%
  filter(lat >= -15, lat <= 10,
         lon >= -15, lon <= 60) %>%
  left_join(LC,
            by = c("lon","lat"))

saveRDS(all.CMIP6.MCWD,
        "./outputs/All.CMIP6.states.timing.MEM.RDS")

all.CMIP6.MCWD.sum <- all.CMIP6.MCWD %>%
  group_by(model,scenario,period,lon,lat) %>%
  summarise(MAP = sum(pre,na.rm = TRUE),
            MCWD = unique(MCWD),
            .groups = "keep")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Add.MEM.2.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

