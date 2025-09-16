rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(sp)
library(YGB)

files <- c("All.CMIP6.ET.RDS",
           "All.CMIP6.ET.future.RDS")

# for (cfile in files){
#   system2("rsync",
#           c("-avz",
#             paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }


CMIP6 <- readRDS("./outputs/All.CMIP6.ET.RDS") %>%
  rename(pre = Pmm)

load("./data/IPCC-WGI-reference-regions-v4_R.rda")

selected.df <- IPCC_WGI_reference_regions_v4[
  IPCC_WGI_reference_regions_v4@data$Acronym %in% c("WAF","CAF","NEAF","SEAF",
                                                    "WSAF","ESAF","MDG",
                                                    "SAH"),]


reanalyses <- readRDS("./outputs/Pantropical.climate.recent.seasonal.rspld.RDS")
Search <- reanalyses %>%
  ungroup() %>%
  filter(model == model[1],
         month == month[1])

A <- Search
coordinates(A) <- ~lon + lat
proj4string(A) <- CRS("+proj=longlat +datum=WGS84")
matches <- over(A, selected.df)
Search$region <- matches$Acronym

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = Search %>%
         filter(!is.na(region))) +
  geom_raster(aes(x = lon, y = lat, fill = region)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-15, 55), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

reanalyses.region <- reanalyses %>%
  ungroup() %>%
  left_join(Search %>%
              dplyr::select(lon,lat,region),
            by = c("lon","lat"))

reanalyses.region.sum <- reanalyses.region %>%
  filter(!is.na(region)) %>%
  pivot_longer(cols = c(tas,tasmin,tasmax,pre),
               names_to = "variable",
               values_to = "value") %>%
  group_by(model,region,variable,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

reanalyses.region.sum.av <- reanalyses.region.sum %>%
  group_by(region,variable,month) %>%
  summarise(value.m.av = mean(value.m,na.rm = TRUE),
            value.m.low = min(value.m,na.rm = TRUE),
            value.m.high = max(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_ribbon(data = reanalyses.region.sum.av,
              aes(x = month, y = value.m.av,fill = region,
                  ymin = value.m.low, ymax = value.m.high),
              color = NA,
              alpha = 0.3) +
  geom_line(data = reanalyses.region.sum,
            aes(x = month, y = value.m, color = region,
                group = interaction(region, model)),
            size = 0.1) +
  geom_line(data = reanalyses.region.sum.av,
            aes(x = month, y = value.m.av, color = region)) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

# CMIP6
CMIP6.region <- CMIP6 %>%
  ungroup() %>%
  left_join(Search %>%
              dplyr::select(lon,lat,region),
            by = c("lon","lat"))

CMIP6.region.sum <- CMIP6.region %>%
  filter(!is.na(region)) %>%
  dplyr::select(lon,lat,month,model,region,tas,tasmin,tasmax,pre) %>%
  pivot_longer(cols = c(tas,tasmin,tasmax,pre),
               names_to = "variable",
               values_to = "value") %>%
  group_by(model,region,variable,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

CMIP6.region.sum.av <- CMIP6.region.sum %>%
  group_by(region,variable,month) %>%
  summarise(value.m.av = mean(value.m,na.rm = TRUE),
            value.m.low = min(value.m,na.rm = TRUE),
            value.m.high = max(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_ribbon(data = CMIP6.region.sum.av,
              aes(x = month, y = value.m.av,fill = region,
                  ymin = value.m.low, ymax = value.m.high),
              color = NA,
              alpha = 0.3) +
  geom_line(data = CMIP6.region.sum,
            aes(x = month, y = value.m, color = region,
                group = interaction(region, model)),
            size = 0.1) +
  geom_line(data = CMIP6.region.sum.av,
            aes(x = month, y = value.m.av, color = region)) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

# Debias
CMIP6.vs.reanalyses <- CMIP6.region.sum %>%
  left_join(reanalyses.region.sum.av %>%
              dplyr::select(region,variable,month,value.m.av),
            by = c("region","variable","month"))

df.biases <- CMIP6.vs.reanalyses %>%
  group_by(model,variable) %>%
  summarise(b = mean(value.m - value.m.av),
            slope = coef(lm(value.m.av ~ value.m))[2],
            intercept = coef(lm(value.m.av ~ value.m))[1],
            .groups = "keep")

ggplot(data = CMIP6.vs.reanalyses %>%
         filter(variable == "pre"),
       aes(y = value.m,
           x = value.m.av,
           color = region, fill = region)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",se = FALSE) +
  facet_grid(model ~ variable) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  theme_bw()

ggplot(data = CMIP6.vs.reanalyses %>%
         filter(variable != "pre"),
       aes(y = value.m,
           x = value.m.av,
           color = region, fill = region)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",se = FALSE) +
  facet_grid(model ~ variable) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  theme_bw()

# Future?

CMIP6.future <-
  readRDS("./outputs/All.CMIP6.ET.future.RDS") %>%
              rename(pre = Pmm)

CMIP6.future.region <- CMIP6.future %>%
  ungroup() %>%
  left_join(Search %>%
              dplyr::select(lon,lat,region),
            by = c("lon","lat"))

CMIP6.future.region.sum <- CMIP6.future.region %>%
  filter(!is.na(region)) %>%
  dplyr::select(scenario,lon,lat,period,month,model,region,tas,tasmin,tasmax,pre) %>%
  pivot_longer(cols = c(tas,tasmin,tasmax,pre),
               names_to = "variable",
               values_to = "value") %>%
  group_by(scenario,model,period,region,variable,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

# Unbiased future

CMIP6.future.region.sum.ts <- CMIP6.future.region.sum %>%
  left_join(CMIP6.region.sum %>%
              rename(ref = value.m),
            by = c("model","region","month","variable")) %>%
  left_join(df.biases %>%
              dplyr::select(model,variable,b,slope,intercept),
            by = c("model","variable")) %>%
  mutate(ref.db = slope*ref + intercept,
         value.m.db = slope*value.m + intercept)

CMIP6.future.region.sum.ts.av <- CMIP6.future.region.sum.ts %>%
  group_by(scenario,region,variable,month) %>%
  summarise(value.m.db.av = mean(value.m.db,na.rm = TRUE),
            value.m.db.low = min(value.m.db,na.rm = TRUE),
            value.m.db.high = max(value.m.db,na.rm = TRUE),

            value.m.av = mean(value.m,na.rm = TRUE),
            value.m.low = min(value.m,na.rm = TRUE),
            value.m.high = max(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_ribbon(data = CMIP6.future.region.sum.ts.av %>%
                filter(variable != "pre"),
              aes(x = month, y = value.m.db.av,fill = region,
                  ymin = value.m.db.low, ymax = value.m.db.high),
              color = NA,
              alpha = 0.3) +
  geom_line(data = CMIP6.future.region.sum.ts %>%
              filter(variable != "pre"),
            aes(x = month, y = value.m.db, color = region,
                group = interaction(region, model)),
            size = 0.1) +
  geom_line(data = CMIP6.future.region.sum.ts.av %>%
              filter(variable != "pre"),
            aes(x = month, y = value.m.db.av, color = region)) +
  facet_grid(scenario ~ variable, scales = "free") +
  theme_bw()



ggplot() +
  geom_ribbon(data = CMIP6.future.region.sum.ts.av %>%
                filter(variable == "pre"),
              aes(x = month, y = value.m.db.av,fill = region,
                  ymin = value.m.db.low, ymax = value.m.db.high),
              color = NA,
              alpha = 0.3) +
  geom_line(data = CMIP6.future.region.sum.ts %>%
              filter(variable == "pre"),
            aes(x = month, y = value.m.db, color = region,
                group = interaction(region, model)),
            size = 0.1) +
  geom_line(data = CMIP6.future.region.sum.ts.av %>%
              filter(variable == "pre"),
            aes(x = month, y = value.m.db.av, color = region)) +
  facet_grid(scenario ~ variable, scales = "free") +
  theme_bw()

ggplot() +
  geom_ribbon(data = CMIP6.future.region.sum.ts.av %>%
                filter(variable == "pre"),
              aes(x = month, y = value.m.av,fill = region,
                  ymin = value.m.low, ymax = value.m.high),
              color = NA,
              alpha = 0.3) +
  geom_line(data = CMIP6.future.region.sum.ts %>%
              filter(variable == "pre"),
            aes(x = month, y = value.m, color = region,
                group = interaction(region, model)),
            size = 0.1) +
  geom_line(data = CMIP6.future.region.sum.ts.av %>%
              filter(variable == "pre"),
            aes(x = month, y = value.m.av, color = region)) +
  facet_grid(scenario ~ variable, scales = "free") +
  theme_bw()

# Difference
CMIP6.future.region.sum.change <- CMIP6.future.region.sum.ts %>%
  mutate(diff.db = value.m.db - ref.db,
         diff = value.m - ref) %>%
  dplyr::select(-c(value.m,ref,
                   value.m.db,ref.db))

CMIP6.future.region.sum.change.av <- CMIP6.future.region.sum.change %>%
  group_by(scenario,region,variable,month) %>%
  summarise(diff.av = mean(diff,na.rm = TRUE),
            diff.low = min(diff,na.rm = TRUE),
            diff.high = max(diff,na.rm = TRUE),

            diff.db.av = mean(diff.db,na.rm = TRUE),
            diff.db.low = min(diff.db,na.rm = TRUE),
            diff.db.high = max(diff.db,na.rm = TRUE),
            .groups = "keep")

ggplot(data = CMIP6.future.region.sum.change.av %>%
         filter(variable == "pre")) +
  geom_point(aes(x = diff.av, y = diff.db.av, color = region)) +
  geom_hline(yintercept = 0,linetype = 2) +
  geom_abline(slope = 1,
              intercept = 0, linetype = 2) +
  facet_grid(scenario ~ variable, scales = "free") +
  theme_bw()


ggplot() +
  geom_ribbon(data = CMIP6.future.region.sum.change.av %>%
                filter(variable == "pre"),
              aes(x = month, y = diff.db.av,fill = region,
                  ymin = diff.db.low, ymax = diff.db.high),
              color = NA,
              alpha = 0.3) +
  geom_line(data = CMIP6.future.region.sum.change %>%
              filter(variable == "pre"),
            aes(x = month, y = diff.db, color = region,
                group = interaction(region, model)),
            size = 0.1) +
  geom_line(data = CMIP6.future.region.sum.change.av %>%
              filter(variable == "pre"),
            aes(x = month, y = diff.db.av, color = region)) +
  geom_hline(yintercept = 0,linetype = 2) +
  facet_grid(scenario ~ region, scales = "free") +
  theme_bw()

ggplot(data = CMIP6.future.region.sum.change.av %>%
         filter(variable != "pre")) +
  geom_point(aes(x = diff.av, y = diff.db.av, color = region)) +
  geom_hline(yintercept = 0,linetype = 2) +
  geom_abline(slope = 1,
              intercept = 0, linetype = 2) +
  facet_grid(scenario ~ variable, scales = "free") +
  theme_bw()

ggplot() +
  geom_ribbon(data = CMIP6.future.region.sum.change.av %>%
                filter(variable == "tas"),
              aes(x = month, y = diff.db.av,fill = region,
                  ymin = diff.db.low, ymax = diff.db.high),
              color = NA,
              alpha = 0.3) +
  geom_line(data = CMIP6.future.region.sum.change %>%
              filter(variable == "tas"),
            aes(x = month, y = diff.db, color = region,
                group = interaction(region, model)),
            size = 0.1) +
  geom_line(data = CMIP6.future.region.sum.change.av %>%
              filter(variable == "tas"),
            aes(x = month, y = diff.db.av, color = region)) +
  geom_hline(yintercept = 0,linetype = 2) +
  facet_grid(scenario ~ region, scales = "free") +
  theme_bw()

###############################################################################
# What it means in terms of forests

CMIP6.future.region.bias <- bind_rows(CMIP6.region,
                                      CMIP6.future.region) %>%
  dplyr::select(region,model,scenario,period,lon,lat,month,
                tas,tasmin,tasmax,pre) %>%
  pivot_longer(cols = c(tas,tasmin,tasmax,pre),
               names_to = "variable",
               values_to = "value") %>%
  left_join(df.biases %>%
              dplyr::select(model,variable,b,slope,intercept),
            by = c("model","variable"))

CMIP6.future.region.bias.db <- CMIP6.future.region.bias %>%
  ungroup() %>%
  filter(!is.na(region)) %>%
  mutate(value.db = slope*value + intercept) %>%
  dplyr::select(-c(b,slope,intercept,value)) %>%
  pivot_wider(names_from = "variable",
              values_from = "value.db") %>%
  group_by(period,scenario,model,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = SPEI::hargreaves(tasmin - 273.15,
                              tasmax - 273.15,
                              lat = unique(lat),
                              Ra = NULL,
                              na.rm = TRUE,
                              verbose = FALSE)/Ndays,
         Pmm = pre,
         Etot = E*Ndays) %>%
  dplyr::select(-pre) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD),
         MAP = sum(Pmm),
         Etot = sum(Etot),
         MAT = mean(tas)) %>%
  filter(month == 1)

CMIP6.future.region.bias <- CMIP6.future.region.bias %>%
  ungroup() %>%
  filter(!is.na(region)) %>%
  dplyr::select(-c(b,slope,intercept,value)) %>%
  pivot_wider(names_from = "variable",
              values_from = "value") %>%
  group_by(period,scenario,model,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = SPEI::hargreaves(tasmin - 273.15,
                              tasmax - 273.15,
                              lat = unique(lat),
                              Ra = NULL,
                              na.rm = TRUE,
                              verbose = FALSE)/Ndays,
         Pmm = pre,
         Etot = E*Ndays) %>%
  dplyr::select(-pre) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD),
         MAP = sum(Pmm),
         Etot = sum(Etot),
         MAT = mean(tas)) %>%
  filter(month == 1)

df.LC <- bind_rows(CMIP6.future.region.bias %>%
                     mutate(type = "Original"),
                   CMIP6.future.region.bias.db %>%
                     mutate(type = "Debiased")) %>%
  ungroup() %>%
  mutate(LC.pred = as.factor(case_when(MCWD > -450 ~ 2,
                                       MAP < 1000 ~ 1,
                                       TRUE ~ 3)))

# Changes
Cdf <- df.LC %>%
  filter(month == 1) %>%
  dplyr::select(type,model,scenario,lon,lat,Etot,MAP,MAT,MCWD,LC.pred)

df.change <- Cdf %>%
  ungroup() %>%
  filter(scenario != "historical") %>%
  left_join(Cdf %>%
              ungroup() %>%
              filter(scenario == "historical") %>%
              dplyr::select(-c(scenario)),
            by = c("type","lon","lat","model"))

saveRDS(df.change,
        "./outputs/df.change.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/data/IPCC-WGI-reference-regions-v4_R.rda hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/data/IPCC-WGI-reference-regions-v4_R.rda
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Pantropical.climate.recent.seasonal.rspld.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/analyze.change.ET.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
