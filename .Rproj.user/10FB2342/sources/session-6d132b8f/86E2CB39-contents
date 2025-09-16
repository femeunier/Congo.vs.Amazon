rm(list = ls())

library(YGB)
library(caret)
library(dplyr)
library(tidyr)

# system2("rsync",
#         c("-avz",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Africa.climate.rspld.RDS",
#           "./outputs/"))

dayspermonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)

raw <- readRDS("./outputs/Africa.climate.rspld.RDS") %>%
  ungroup() %>%
  filter(year %in% c(1981:2010))
A <- raw %>%
  mutate(lon = round(lon,digits = 2),
         lat = round(lat,digits = 2),
         month = as.integer(month)) %>%
  group_by(model,lon,lat,month) %>%
  summarise(tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            pre = mean(pre,na.rm = TRUE),
            .groups = "keep")

B <- A %>%
  group_by(model) %>%
  summarise(tas.inc = !all(is.na(tas)),
            tasmin.inc = !all(is.na(tasmin)),
            tasmax.inc = !all(is.na(tasmax)),
            pre.inc = !all(is.na(pre)),
            .groups = "keep")

GLEAM <- readRDS("/home/femeunier/Documents/data/GLEAM/GLEAM.pantropical.months.RDS") %>%
  mutate(lon = round(lon,digits = 2),
         lat = round(lat,digits = 2),
         month = as.integer(month))
GLEAM.sum <- GLEAM %>%
  filter(lon <= 60,lon>= -15,lat <= 20,lat >= -20) %>%
  group_by(lon,lat,month) %>%
  summarise(E.m = mean(E,na.rm = TRUE),
            .groups = "keep")

# Tropical.Forest.mask <- readRDS("./outputs/Tropical.Forests.mask.RDS")
# LC <- as.data.frame(terra::rast(readRDS("./outputs/LC.rspld.RDS")),
#                     xy = TRUE) %>%
#   rename(lon = x, lat = y)
LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)


LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)


Clim.Mask <- A %>%
  left_join(LandFrac,
            by = c("lon","lat")) %>%
  left_join(LC %>%
              mutate(lat = round(lat,digits = 2),
                     lon = round(lon,digits = 2)),
            by = c("lon","lat")) %>%
  filter(lon >= - 15 & lon <= 55) %>%
  filter(value >= 0.1)


products <- unique(Clim.Mask$model)
product.na.tas <- Clim.Mask %>% ungroup() %>%
  filter(is.na(tas)) %>% pull(model) %>% unique()
product.w.everything <- products[!(products %in% product.na.tas)]

Clim.Mask.filled <- data.frame(Clim.Mask %>%
                                 filter(model %in% product.w.everything))
Clim.Mask.filled.m <- Clim.Mask.filled %>%
  group_by(lon,lat,month) %>%
  summarise(tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            .groups = "keep")

for (cproduct in product.na.tas){

  print(cproduct)

  cdf <- Clim.Mask %>%
    filter(model == cproduct)

  Clim.Mask.filled <- bind_rows(Clim.Mask.filled,
                                cdf %>%
                                  dplyr::select(-c(tas,tasmin,tasmax)) %>%
                                  left_join(Clim.Mask.filled.m,
                                            by = c("lon","lat","month")))
}

Clim.Mask.filled <- bind_rows(Clim.Mask.filled,
                              Clim.Mask.filled %>%
                                group_by(lon,lat,month,LC) %>%
                                summarise(tas = mean(tas,na.rm = TRUE),
                                          tasmin = mean(tasmin,na.rm = TRUE),
                                          tasmax = mean(tasmax,na.rm = TRUE),
                                          value = unique(value),
                                          pre = mean(pre,na.rm = TRUE),
                                          .groups = "keep") %>%
                                mutate(model = "MEM"))

Clim.Mask.MCWD <- Clim.Mask.filled %>%
  ungroup() %>%
  arrange(model,lon,lat,month) %>%
  group_by(model,lon,lat) %>%
  mutate(Ndays = dayspermonth,
         E = SPEI::hargreaves(tasmin - 273.15,
                              tasmax  - 273.15,
                              lat = unique(lat),
                              Ra = NULL,
                              na.rm = TRUE,
                              verbose = FALSE)/Ndays,
         Etot = E*Ndays) %>%
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

df2plot <- Clim.Mask.MCWD
df2plot.sum <- df2plot %>%
  group_by(LC,model,month) %>%
  summarise(CWD.m = median(CWD),
            .groups = "keep")


ggplot() +
  geom_line(data = df2plot %>%
              filter(LC == 2),
            aes(x = month, y = CWD,group = interaction(lon,lat)),
            size = 0.1, color = "grey") +
  geom_line(data = df2plot.sum %>%
              filter(LC == 2),
            aes(x = month, y = CWD.m),
            color = "black") +
  facet_wrap(~model) +
  theme_bw()

ggplot() +
  geom_line(data = df2plot.sum %>%
              filter(LC %in% c(1:3)),
            aes(x = month, y = CWD.m, color = model)) +
  facet_wrap(~LC) +
  theme_bw()

Clim.Mask.MCWD.LC <- Clim.Mask.MCWD %>%
  group_by(LC,model,month) %>%
  summarise(pre = mean(pre,na.rm = TRUE),
            tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            Etot = mean(Etot,na.rm = TRUE),
            CWD = mean(CWD,na.rm = TRUE),
            .groups = "keep")

GLEAM.sum.LC <- GLEAM.sum %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(LC %in% c(1:3)) %>%
  mutate(hemisph = case_when(lat < 0 ~ "S",
                             TRUE ~ "N")) %>%
  group_by(hemisph,month,LC) %>%
  summarise(Etot = mean(E.m,na.rm = TRUE),
            .groups = "keep")



ggplot(data = Clim.Mask.MCWD.LC %>%
         filter(LC %in% c(1:3))) +
  geom_hline(yintercept = 0, color = "black",
             linetype = 2) +
  geom_line(aes(x = month, y = pre,
                color = model)) +
  geom_line(aes(x = month, y = -Etot,
                color = model)) +
  geom_line(data = GLEAM.sum.LC,
            aes(x = month, y = -Etot),
            color = "black") +
  geom_line(aes(x = month, y = CWD,
                color = model),
            linetype = 2) +
  facet_wrap(~ (LC)) +
  theme_bw()


ggplot(data = Clim.Mask.MCWD.LC %>%
         filter(LC %in% c(1:3),
                model %in% product.w.everything)) +
  geom_line(aes(x = month, y = tas - 273.15,
                color = model)) +
  geom_line(aes(x = month, y = tasmin - 273.15,
                color = model),
            linetype = 2) +
  geom_line(aes(x = month, y = tasmax - 273.15,
                color = model),
            linetype = 3) +
  facet_wrap(~ as.factor(LC)) +
  theme_bw()

GLEAM.vs.hargreaves <- Clim.Mask.MCWD %>%
  filter(LC %in% c(1,2,3)) %>%
  ungroup() %>%
  mutate(lon = round(lon,digits = 2),
         lat = round(lat,digits = 2),
         month = as.integer(month)) %>%
  dplyr::select(lon,lat,month,model,Etot) %>%
  left_join(GLEAM.sum %>%
              mutate(lon = round(lon,digits = 2),
                     lat = round(lat,digits = 2)),
            by = c("lon","lat","month"))

ggplot(data = GLEAM.vs.hargreaves %>%
         filter(model %in% c("MEM",product.w.everything)),
       aes(x = E.m,y = Etot)) +
  geom_hex() +
  stat_smooth(aes(color = model),
              method = "lm") +
  geom_abline(slope = 1, color = "black",linetype = 2) +
  scale_fill_gradient(low = "white", high = "darkgrey") +
  theme_bw()


ggplot() +
  geom_density(data = GLEAM.vs.hargreaves %>%
                 filter(model %in% c(product.w.everything)),
               aes(x = E.m - Etot,
                   fill = model),
               alpha = 0.25) +
  geom_density(data = GLEAM.vs.hargreaves %>%
                 filter(model %in% c("MEM")),
               aes(x = E.m - Etot,
                   fill = model),
               alpha = 0.5, fill = "black") +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw()



ggplot(data = GLEAM.vs.hargreaves %>%
         na.omit() %>%
         rename(GLEAM = E.m,
                Hargreaves = Etot) %>%
         pivot_longer(cols = c(GLEAM,Hargreaves))) +
  geom_density(aes(x = value, fill = name),
               alpha = 0.5) +
  theme_bw()


Clim.Mask.MCWD.sum <-
  Clim.Mask.MCWD %>%
  filter(LC %in% c(1,2,3)) %>%
  group_by(lon,lat,model) %>%
  summarise(MAP = sum(pre,na.rm = TRUE),
            Etot = sum(Etot,na.rm = TRUE),
            LC = unique(LC),
            MCWD = unique(MCWD),
            tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            .groups = "keep") %>%
  filter(Etot >= 1050) # Remove coastal regions


MCWD.gleam.vs.products <-
  Clim.Mask.MCWD %>%
  filter(LC %in% c(1:3)) %>%
  mutate(lat = round(lat,2),
         lon = round(lon,2)) %>%
  left_join(GLEAM.sum %>%
              mutate(lat = round(lat,2),
                     lon = round(lon,2)),
            by = c("lon","lat","month")) %>%
  na.omit() %>%
  group_by(model,lon,lat) %>%
  mutate(N = n()) %>%
  filter(N == 12) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         Etot = E.m) %>%
  mutate(diff = pre - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD.gleam = min(CWD)) %>%
  dplyr::select(model,lon,lat,MCWD,MCWD.gleam) %>%
  distinct()

ggplot() +
  geom_density(data = MCWD.gleam.vs.products,
               aes(x = MCWD.gleam - MCWD,
                   fill = model),
               alpha = 0.25) +
  geom_density(data = MCWD.gleam.vs.products %>%
                 filter(model %in% c("MEM")),
               aes(x = MCWD.gleam - MCWD,
                   fill = model),
               alpha = 0.5, fill = "black") +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw()


GLEAM.vs.hargreaves.year <- Clim.Mask.MCWD.sum %>%
  mutate(lon = round(lon, digits = 2), lat = round(lat, digits = 2)) %>%
  dplyr::select(lon, lat, model, Etot) %>%
  left_join(GLEAM.sum %>%
              mutate(lon = round(lon,digits = 2),
                     lat = round(lat,digits = 2)) %>%
              group_by(lon,lat) %>%
              summarise(E.m = sum(E.m),
                        .groups = "keep"),
            by = c("lon","lat"))
ggplot() +
  geom_density(data = GLEAM.vs.hargreaves.year %>%
                 filter(model %in% c(product.w.everything)),
               aes(x = E.m - Etot,
                   fill = model),
               alpha = 0.25) +
  geom_density(data = GLEAM.vs.hargreaves.year %>%
                 filter(model %in% c("MEM")),
               aes(x = E.m - Etot,
                   fill = model),
               alpha = 0.5, fill = "black") +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw()


ggplot(data = GLEAM.vs.hargreaves.year ,
       aes(x = E.m,y = Etot)) +
  geom_hex() +
  stat_smooth(aes(color = model),
              method = "lm") +
  geom_abline(slope = 1, color = "black",linetype = 2) +
  scale_fill_gradient(low = "white", high = "darkgrey") +
  theme_bw()


ggplot(data = GLEAM.vs.hargreaves.year %>%
         na.omit() %>%
         rename(GLEAM = E.m,
                Hargreaves = Etot) %>%
         pivot_longer(cols = c(GLEAM,Hargreaves))) +
  geom_density(aes(x = value, fill = name),
               alpha = 0.5) +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = GLEAM.vs.hargreaves.year %>%
         na.omit() %>%
         rename(GLEAM = E.m,
                Hargreaves = Etot) %>%
         pivot_longer(cols = c(GLEAM,Hargreaves))) +
  geom_density(aes(x = value, fill = name),
               alpha = 0.5) +
  theme_bw()

Clim.Mask.MCWD.sum.region <- Clim.Mask.MCWD %>%
  mutate(hemisph = case_when(lat < 0 ~ "S",
                             TRUE ~ "N")) %>%
  filter(LC %in% c(1,2,3)) %>%
  group_by(hemisph,LC,month,model) %>%
  summarise(pre.m = mean(pre,na.rm = TRUE),
            tas.m = mean(tas,na.rm = TRUE),
            tasmin.m = mean(tasmin,na.rm = TRUE),
            tasmax.m = mean(tasmax,na.rm = TRUE),
            Etot.m = mean(Etot,na.rm = TRUE),
            CWD.m = mean(CWD,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(model) %>%
  mutate(pre.cum.m = cumsum(pre.m))

GLEAM.sum.LC <- GLEAM.sum %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(LC %in% c(1,2,3)) %>%
  group_by(LC,month) %>%
  summarise(Etot.m = mean(E.m,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(LC = factor(LC,
                     levels = c(2,3,1)))

ggplot(data = Clim.Mask.MCWD.sum.region) +
  geom_line(aes(x = month, y =  pre.m,
                color = model)) +
  geom_line(aes(x = month, y =  -Etot.m,
                color = model)) +
  geom_line(data = GLEAM.sum.LC,
            aes(x = month, y =  -Etot.m),
            color = "black") +
  geom_hline(yintercept = 0,linetype = 2) +
  facet_wrap(~ (LC)) +
  theme_bw()

df.E2plot <- Clim.Mask.MCWD.sum.region %>%
  filter(model %in% c(product.w.everything,
                      "MEM")) %>%
  mutate(LC = factor(LC,
                     levels = c(2,3,1)))

df.E2plot.sum <- df.E2plot %>%
  group_by(hemisph,LC,month) %>%
  summarise(Etot.m.m = mean(Etot.m,na.rm = TRUE),
            Etot.m.min = min(Etot.m,na.rm = TRUE),
            Etot.m.max = max(Etot.m,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_ribbon(data = df.E2plot.sum,
            aes(x = month, y =  Etot.m.m,
                ymin = Etot.m.min, ymax = Etot.m.max), color = NA,
            alpha = 0.5, fill = "grey" ) +
  geom_line(data = df.E2plot.sum,
            aes(x = month, y =  Etot.m.m)) +
  geom_line(data = df.E2plot %>%
              filter(model == "ERA5"),
            aes(x = month, y =  Etot.m),
            color = "darkblue") +
  geom_line(data = GLEAM.sum.LC,
            aes(x = month, y =  Etot),
            color = "black",linetype = 2) +
  facet_grid(hemisph ~ LC) +
  theme_bw() +
  labs(x = "",y = "") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

GLEAM.vs.MEM <- df.E2plot.sum %>%
  dplyr::select(hemisph,LC,month,Etot.m.m) %>%
  left_join(GLEAM.sum.LC %>%
              mutate(LC = factor(LC,
                                 levels = c(2,3,1))),
            by = c("hemisph","LC","month"))

GLEAM.vs.MEM %>%
  group_by(LC,hemisph) %>%
  summarise(MAB = mean(abs(Etot.m.m - Etot)),
            RMSE = sqrt(1/11*(sum((Etot.m.m - Etot)**2))),
            rsq = summary(lm(Etot ~ Etot.m.m))[["r.squared"]],
            pval = summary(lm(Etot ~ Etot.m.m))[["coefficients"]][2,4],
            Etot.av = mean(Etot.m.m),
            .groups = "keep")
