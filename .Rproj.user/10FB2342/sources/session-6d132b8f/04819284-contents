rm(list = ls())

library(YGB)
library(caret)
library(dplyr)
library(tidyr)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Africa.climate.rspld.RDS",
          "./outputs/"))

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

Tropical.Forest.mask <- readRDS("./outputs/Tropical.Forests.mask.RDS")
# LC <- as.data.frame(terra::rast(readRDS("./outputs/LC.rspld.RDS")),
#                     xy = TRUE) %>%
#   rename(lon = x, lat = y)
LC <- readRDS("./data/LC_Congo.RDS")

LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


ggplot() +
  geom_raster(data = LandFrac %>% filter(value >= 0.3),
              aes(x = lon, y = lat,
                  fill = value )) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-15, 55), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

Clim.Mask <- A %>%
  left_join(Tropical.Forest.mask,
            by = c("lon","lat")) %>%
  left_join(LandFrac,
            by = c("lon","lat")) %>%
  left_join(LC %>%
              mutate(lat = round(lat,digits = 2),
                     lon = round(lon,digits = 2)),
            by = c("lon","lat")) %>%
  filter(lon >= - 15 & lon <= 55) %>%
  filter(value >= 0.1) # %>%
  # filter(!is.na(IFL))


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
                      IFL = unique(IFL),
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


saveRDS(Clim.Mask.MCWD,
        "./outputs/Climate.CA.summ.RDS")

Clim.Mask.MCWD.LC <- Clim.Mask.MCWD %>%
  group_by(LC,model,month) %>%
  summarise(pre = mean(pre,na.rm = TRUE),
            tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            Etot = mean(Etot,na.rm = TRUE),
            CWD = mean(CWD,na.rm = TRUE),
            .groups = "keep")


ggplot(data = Clim.Mask.MCWD.LC %>%
         filter(LC %in% c(1:3))) +
  geom_hline(yintercept = 0, color = "black",
             linetype = 2) +
  geom_line(aes(x = month, y = pre,
                color = model)) +
  geom_line(aes(x = month, y = -Etot,
                color = model)) +
  # geom_line(data = GLEAM.sum.LC,
  #           aes(x = month, y = -Etot),
  #           color = "black") +
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


Thresholds <- readRDS("./outputs/Sensitivity.thresholds.RDS")

MAP.threshold.MEM <- Thresholds %>%
  filter(model == "MEM") %>%
  pull(MAP.threshold)
MCWD.threshold.MEM <- Thresholds %>%
  filter(model == "MEM") %>%
  pull(MCWD.threshold)


test <- Clim.Mask.MCWD %>%
  ungroup() %>%
  filter(lon == lon[1],
         lat == lat[1],
         model == model[1])

sort(test$Etot,decreasing = TRUE,
     index.return = TRUE)[["ix"]]

wet_dry_quarters <- function(pre, tmp, Etot) {
  stopifnot(length(pre) == 12L,
            length(Etot) == 12L,
            length(tmp) == 12L)
  pre3 <- zoo::rollsum(c(pre, pre[1:2]), k = 3, align = "left")[1:12]  # circular 3-mo totals
  tmp3 <- zoo::rollmean(c(tmp, tmp[1:2]), k = 3, align = "left")[1:12]  # circular 3-mo totals

  wet_start <- which.max(pre3)
  dry_start <- which.min(pre3)

  hot_start <- which.max(tmp3)
  cold_start <- which.min(tmp3)

  idx_wet <- ((wet_start - 1) + 0:2) %% 12 + 1
  idx_dry <- ((dry_start - 1) + 0:2) %% 12 + 1

  idx_hot <- ((hot_start - 1) + 0:2) %% 12 + 1
  idx_cold <- ((cold_start - 1) + 0:2) %% 12 + 1

  tibble(
    Etot_quarter_wet  = sum(Etot[idx_wet], na.rm = TRUE),
    Etot_quarter_dry  = sum(Etot[idx_dry], na.rm = TRUE),

    Etot_quarter_hot  = sum(Etot[idx_hot], na.rm = TRUE),
    Etot_quarter_cold  = sum(Etot[idx_cold], na.rm = TRUE),

    wet_quarter_start = wet_start,   # 1..12
    dry_quarter_start = dry_start    # 1..12
  )
}


Clim.Mask.MCWD.sum.allE <-
  Clim.Mask.MCWD %>%
  filter(LC %in% c(1, 2, 3)) %>%
  group_by(lon, lat, model) %>%
  arrange(month, .by_group = TRUE) %>%
  summarise(
    MAP            = sum(pre,  na.rm = TRUE),
    Etot_monthmax  = max(Etot, na.rm = TRUE),
    Etot_monthmin  = min(Etot, na.rm = TRUE),
    wet_dry        = list(wet_dry_quarters(pre, tas, Etot)),

    Etot_wettest_month  = Etot[which.max(pre)],
    Etot_driest_month   = Etot[which.min(pre)],

    Etot_hotest_month  = Etot[which.max(tas)],
    Etot_coldest_month   = Etot[which.min(tas)],

    Etot_total     = sum(Etot, na.rm = TRUE),
    LC             = dplyr::first(LC),
    MCWD           = dplyr::first(MCWD),
    tas            = mean(tas,    na.rm = TRUE),
    tasmin         = mean(tasmin, na.rm = TRUE),
    tasmax         = mean(tasmax, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  unnest(wet_dry) %>%
  left_join(Thresholds %>% dplyr::select(-Acc), by = "model") %>%
  filter(Etot_total >= 1050)  # your coastal filter

hist(Clim.Mask.MCWD.sum.allE$Etot_quarter_hot  -
       Clim.Mask.MCWD.sum.allE$Etot_quarter_cold)

saveRDS(Clim.Mask.MCWD.sum.allE,
        "./outputs/Summary.climate.Etot.RDS")



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
  left_join(Thresholds %>%
              dplyr::select(-Acc),
            by = "model") %>%
  filter(Etot >= 1050) # Remove coastal regions

hist(Clim.Mask.MCWD.sum$Etot.max/3)

ggplot() +
  geom_raster(data = Clim.Mask.MCWD.sum,
              aes(x = lon, y = lat,
                  fill = MAP)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-15, 55), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ model) +
  scale_fill_gradient(low = "white", high = "darkblue",
                      limits = c(0,2000),oob = scales::squish) +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggplot() +
  geom_raster(data = Clim.Mask.MCWD.sum,
              aes(x = lon, y = lat,
                  fill = Etot)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-15, 55), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ model) +
  scale_fill_gradient2(low = "darkblue",mid = "white", high = "darkred",
                       midpoint = 1500,
                      limits = c(1000,2000),
                      oob = scales::squish) +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggplot() +
  geom_raster(data = Clim.Mask.MCWD.sum %>%
                filter(model == "MEM",
                       !is.na(LC)),
              aes(x = lon, y = lat,
                  fill = as.factor(LC))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-15, 55), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

Clim.Mask.MCWD.sum.region <- Clim.Mask.MCWD %>%
  filter(IFL == 1) %>%
  group_by(month,model) %>%
  summarise(pre.m = mean(pre,na.rm = TRUE),
            tas.m = mean(tas,na.rm = TRUE),
            tasmin.m = mean(tasmin,na.rm = TRUE),
            tasmax.m = mean(tasmax,na.rm = TRUE),
            Etot.m = mean(Etot,na.rm = TRUE),
            CWD.m = mean(CWD,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(model) %>%
  mutate(pre.cum.m = cumsum(pre.m))


ggplot(data = Clim.Mask.MCWD.sum.region) +
  geom_line(aes(x = month, y =  pre.m,
                color = model)) +
  geom_line(aes(x = month, y =  -Etot.m,
                color = model)) +
  # geom_line(data = GLEAM.sum.LC,
  #           aes(x = month, y =  -Etot.m),
  #           color = "black") +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw()


ggplot(data = Clim.Mask.MCWD.sum.region %>%
         filter(!is.na(tas.m))) +
  geom_line(aes(x = month, y = tas.m - 273.15, color = model)) +
  geom_line(aes(x = month, y = tasmin.m - 273.15, color = model),
            linetype = 2) +
  geom_line(aes(x = month, y = tasmax.m - 273.15, color = model),
            linetype = 3) +
  # scale_y_continuous(limits = c(0,40)) +
  # facet_wrap(~model) +
  theme_bw()


ggplot(data = Clim.Mask.MCWD.sum %>%
         filter(LC %in% c(1:3))) +
  geom_point(aes(x = MCWD,y = MAP,
                 color = as.factor(LC)),
             size = 0.1) +
  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  facet_wrap(~ model) +
  geom_hline(data = Thresholds,
             aes(yintercept = MAP.threshold), linetype = 2) +
  geom_vline(data = Thresholds,
             aes(xintercept = MCWD.threshold), linetype = 2) +
  theme_bw()

saveRDS(Clim.Mask.MCWD.sum,
        "./outputs/Summary.climate.RDS")



df.temp <- bind_rows(
  data.frame(model = product.na.tas,
             tas.included = "No"),
  data.frame(model = product.w.everything,
             tas.included = "Yes"))

product.na.tas <- Clim.Mask %>% ungroup() %>%
  filter(is.na(tas)) %>% pull(model) %>% unique()
product.w.everything <- products[!(products %in% product.na.tas)]


ConfusionM <- Clim.Mask.MCWD.sum %>%
  # mutate(MAP.threshold = 900,
  #        MCWD.threshold = -300) %>%
  filter(LC %in% c(1:3)) %>%
  ungroup() %>%
  mutate(LC = as.factor(LC),
         LC.pred = as.factor(case_when(MCWD > MCWD.threshold ~ 2,
                                       MAP < MAP.threshold ~ 1,
                                       TRUE ~ 3))) %>%
  group_by(model) %>%
  summarise(MAP.threshold = unique(MAP.threshold),
            MCWD.threshold = unique(MCWD.threshold),
            Acc = (confusionMatrix(LC.pred,LC))[["overall"]][1],
            precision = mean(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                             na.rm = TRUE),
            precision.min = min(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                             na.rm = TRUE),
            precision.max = max(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                na.rm = TRUE)) %>%
  left_join(df.temp,
            by = "model") %>%
  mutate(Acc = signif(Acc,digits = 2),
         Precision = paste0(signif(precision,digits = 2)," \r\n",
                            "(",signif(precision.min,digits = 2),
                            "-",signif(precision.max,digits = 2),
                            ")"))

hist(ConfusionM$Acc)
summary(ConfusionM$Acc)

df.period <- raw %>%
  group_by(model) %>%
  summarise(period = paste0(min(year),
                            "-",max(year)),
            .groups = "keep")

write.csv(ConfusionM %>%
          dplyr::select(model,
                        tas.included,
                        MAP.threshold,MCWD.threshold,
                        Acc,Precision) %>%
            left_join(df.period,
                      by = "model") %>%
            dplyr::select(model,period,
                          tas.included,MAP.threshold,MCWD.threshold,Acc,Precision) ,
          "./outputs/Reanalysis.table.csv")

##########################################################
# Product ensemble mean

Clim.Mask.MCWD.sum.av <- Clim.Mask.MCWD %>%
  group_by(lon,lat,month) %>%
  summarise(LC = unique(LC),
            IFL = unique(IFL),
            value = unique(value),
            pre = mean(pre,na.rm = TRUE),
            tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            .groups = "keep") # %>%
  # left_join(GLEAM.sum %>%
  #             mutate(lon = round(lon,digits = 2),
  #                    lat = round(lat,digits = 2)),
  #           by = c("lon","lat","month"))

Clim.Mask.MCWD.sum.av.MCWD <- Clim.Mask.MCWD.sum.av %>%
  arrange(lon,lat,month) %>%
  group_by(lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = SPEI::hargreaves(tasmin - 273.15,
                              tasmax  - 273.15,
                              lat = unique(lat),
                              Ra = NULL,
                              na.rm = TRUE,
                              verbose = FALSE)/Ndays,
         Etot = E*Ndays) %>%
  # mutate(Etot = E.m) %>%  # Use GLEAM insteaf of Hargeaves
  # filter(!is.na(Etot)) %>%
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
  mutate(MCWD = min(CWD))

ggplot() +
  geom_raster(data = Clim.Mask.MCWD.sum.av.MCWD,
              aes(x = lon, y = lat,
                  fill = Etot)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-15, 55), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))


Clim.Mask.MCWD.sum.av.MCWD.sum <- Clim.Mask.MCWD.sum.av.MCWD %>%
  group_by(lon,lat) %>%
  summarise(MAP = sum(pre,na.rm = TRUE),
            MCWD = unique(MCWD),
            LC =factor(LC,levels = c(1,2,3)),
            .groups = "keep") %>%
  mutate(LC.pred = factor(case_when(MCWD > MCWD.threshold.MEM ~ 2,
                                    MAP < MAP.threshold.MEM ~ 1,
                                       TRUE ~ 3),
                          levels = c(1,2,3)))

df.LC <- Clim.Mask.MCWD.sum.av.MCWD.sum %>%
  dplyr::select(lon,lat,LC,LC.pred) %>%
  pivot_longer(cols = c(LC,LC.pred),
               values_to = "LC",
               names_to = "source")

ggplot() +
  geom_raster(data = df.LC,
              aes(x = lon, y = lat,
                  fill = as.factor(LC))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-15, 55), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  facet_wrap(~ source) +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggplot() +
  geom_raster(data = Clim.Mask.MCWD.sum.av.MCWD.sum %>%
                filter(LC %in% c(1,2,3)),
              aes(x = lon, y = lat,
                  fill = (LC == LC.pred))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-15, 55), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

Clim.Mask.MCWD.sum.av.MCWD.sum %>%
  group_by(LC) %>%
  summarise(N = n(),
            .groups = "keep")


Clim.Mask.MCWD.sum.av.MCWD.sum %>%
  ungroup() %>%
  na.omit() %>%
  mutate(pred.default = factor(1,
                               levels = c(1,2,3))) %>%
  summarise(Acc = (confusionMatrix(pred.default,LC))[["overall"]][1],
            precision = mean(as.matrix(confusionMatrix(pred.default,LC)[["byClass"]])[,"Precision"],
                             na.rm = TRUE),
            precision.min = min(as.matrix(confusionMatrix(pred.default,LC)[["byClass"]])[,"Precision"],
                                na.rm = TRUE))


ggplot(data = Clim.Mask.MCWD.sum.av.MCWD.sum %>%
         filter(LC %in% c(1:3))) +
  geom_point(aes(x = MCWD,y = MAP, color = as.factor(LC)),
             size = 0.1) +
  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  geom_hline(yintercept = MAP.threshold.MEM, linetype = 2) +
  geom_vline(xintercept = MCWD.threshold.MEM, linetype = 2) +
  theme_bw()


# ggplot(data = Clim.Mask.MCWD.sum.av.MCWD.sum %>%
         # filter(LC %in% c(1:3),
         #        LC.pred != LC)) +
#   geom_point(aes(x = MCWD,y = MAP, color = as.factor(LC)),
#              size = 0.1) +
#   scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
#   geom_hline(yintercept = MAP.threshold.MEM, linetype = 2) +
#   geom_vline(xintercept = MCWD.threshold.MEM, linetype = 2) +
#   theme_bw()


ggplot() +
  geom_raster(data = Clim.Mask.MCWD.sum.av.MCWD.sum %>%
                filter(LC %in% c(1:3),
                       LC.pred != LC) %>%
                pivot_longer(cols = c(LC,LC.pred),
                             values_to = "value",
                             names_to = "origin"),
              aes(x = lon, y = lat,
                  fill = value)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  facet_wrap(~ origin) +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  coord_sf(xlim = c(-15, 55),
           ylim = c(-1, 1)*23.25,
           expand = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

confusionMatrix(Clim.Mask.MCWD.sum.av.MCWD.sum$LC.pred,
                Clim.Mask.MCWD.sum.av.MCWD.sum$LC)

ConfusionM.sum <- Clim.Mask.MCWD.sum.av.MCWD.sum %>%
  filter(LC %in% c(1:3)) %>%
  ungroup() %>%
  summarise(Acc = (confusionMatrix(LC.pred,LC))[["overall"]][1],
            precision = mean(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                             na.rm = TRUE),
            precision.min = min(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                na.rm = TRUE))
ConfusionM.sum

MEM.climate <- Clim.Mask.MCWD.sum.av.MCWD.sum %>%
  filter(LC %in% c(1:3)) %>%
  group_by(LC) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),
            .groups = "keep")


saveRDS(MEM.climate,
        "./outputs/Mean.Climate.RDS")

saveRDS(Clim.Mask.MCWD.sum.av.MCWD.sum %>%
          filter(lat <= 10,
                 lat >= -15),
        "./outputs/LC.pred.MEM.RDS")

