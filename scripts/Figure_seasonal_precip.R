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

Sensitivity.thresholds <- readRDS(
  "./outputs/Sensitivity.thresholds.RDS")

GridArea <- readRDS("./outputs/GridArea.RDS")

A <- readRDS("./outputs/Africa.climate.rspld.RDS") %>%
  ungroup() %>%
  filter(year %in% c(1981:2010)) %>%
  mutate(lon = round(lon,digits = 2),
         lat = round(lat,digits = 2),
         month = as.integer(month)) %>%
  group_by(model,lon,lat,month) %>%
  summarise(tas = mean(tas,na.rm = TRUE),
            tasmin = mean(tasmin,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            pre = mean(pre,na.rm = TRUE),
            .groups = "keep")


Tropical.Forest.mask <- readRDS("./outputs/Tropical.Forests.mask.RDS")

LC <- readRDS("./data/LC_Congo.RDS")

LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

Clim.Mask <- A %>%
  left_join(Tropical.Forest.mask,
            by = c("lon","lat")) %>%
  left_join(LandFrac,
            by = c("lon","lat")) %>%
  left_join(LC %>%
              mutate(lat = round(lat,digits = 2),
                     lon = round(lon,digits = 2)),
            by = c("lon","lat")) %>%
  filter(lon >= -15 & lon <= 60,
         lat >=-15 & lat <= 10) %>%
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

Clim.Mask.filled.all <-
  bind_rows(Clim.Mask.filled,
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


df.numbers <- Clim.Mask.filled.all %>%
  filter(LC %in% c(1,2,3),
         model == "MEM",
         lat > 0) %>%
  group_by(LC,month) %>%
  summarise(tas = mean(tas,na.rm = TRUE) - 273.15,
            tasmin = mean(tasmin,na.rm = TRUE) - 273.15,
            tasmax = mean(tasmax,na.rm = TRUE) - 273.15,
            pre = mean(pre,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(diff = tasmax-tasmin)

df.numbers %>%
  # filter(LC == 1) %>%
  group_by(LC) %>%
  summarise(MAP = sum(pre))


df.numbers %>%
  filter(LC == 1) %>%
  pull(pre) %>%
  sum()

df.numbers %>%
  filter(LC == 1) %>%
  pull(tas) %>%
  summary()

df.numbers %>%
  filter(LC == 3,
         pre < 110) %>%
  pull(pre) %>% mean()

df.numbers %>%
  filter(LC == 2) %>%
  pull(tas) %>%
  summary()

df.numbers %>%
  filter(LC == 2) %>%
  pull(diff) %>%
  plot(type = "l")

df.numbers %>%
  filter(LC == 2) %>%
  pull(tasmax) %>%
  plot(type = "l")

Clim.Mask.filled.all.MCWD <- Clim.Mask.filled.all %>%
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

Clim.Mask.filled.all.MCWD %>%
  filter(month == 1,
         LC == 3,
         model == "MEM") %>%
  pull(MCWD) %>%
  summary()

df.numbers2 <- Clim.Mask.filled.all.MCWD %>%
  filter(lat > 0) %>%
  group_by(LC,model,month) %>%
  summarise(Etot = mean(Etot,na.rm = TRUE),
            MCWD = mean(MCWD,na.rm = TRUE),
            .groups = "keep")

df.numbers2 %>%
  filter(LC == 1,
         model == "MEM") %>%
  pull(MCWD) %>%
  summary()
  # plot(type = "l")


df.numbers2 %>%
  filter(LC == 3,
         model == "MEM") %>%
  pull(Etot) %>%
  summary()

df.numbers2 %>%
  filter(LC == 2,
         model == "MEM",
         month == 1) %>%
  pull(MCWD)


df.numbers2 %>%
  filter(model == "MEM",
         month == 1) %>%
  pull(MCWD)


Clim.Mask.filled.all.MCWD %>%
  filter(month == 1) %>%
  ungroup() %>%
  mutate(hemisp = case_when(lat >= 0 ~ "N",
                            TRUE ~ "S")) %>%
  filter(model == "MEM") %>%
  left_join(GridArea,
            by = c("lon","lat")) %>%
  group_by(LC,hemisp) %>%
  summarise(area = sum(area*land.frac)/1e12)


df2plot <- Clim.Mask.filled.all.MCWD %>%
  ungroup() %>%
  mutate(hemisp = case_when(lat >= 0 ~ "N",
                            TRUE ~ "S")) %>%
  group_by(hemisp,LC,month,model) %>%
  summarise(pre.m = mean(pre),
            Etot.m = mean(Etot),
            CWD.m = mean(CWD),
            .groups = "keep") %>%
  mutate(deficit.m = pmin(0,pre.m - Etot.m)) %>%
  group_by(hemisp,LC,month) %>%
  summarise(pre.med = median(pre.m,na.rm = TRUE),
            pre.min = min(pre.m,na.rm = TRUE),
            pre.max = max(pre.m,na.rm = TRUE),
            pre.m = mean(pre.m,na.rm = TRUE),

            deficit.med = median(deficit.m,na.rm = TRUE),
            deficit.min = min(deficit.m,na.rm = TRUE),
            deficit.max = max(deficit.m,na.rm = TRUE),
            deficit.m = mean(deficit.m,na.rm = TRUE),

            Etot.med = median(Etot.m,na.rm = TRUE),
            Etot.min = min(Etot.m,na.rm = TRUE),
            Etot.max = max(Etot.m,na.rm = TRUE),
            Etot.m = mean(Etot.m,na.rm = TRUE),

            CWD.med = median(CWD.m,na.rm = TRUE),
            CWD.min = min(CWD.m,na.rm = TRUE),
            CWD.max = max(CWD.m,na.rm = TRUE),
            CWD.m = mean(CWD.m,na.rm = TRUE),

            .groups = "keep")

df2plot.NA <- df2plot %>%
  filter(LC %in% c(1:3)) %>%
  mutate(LC = factor(LC,
                     levels = c(2,3,1)))


ggplot() +
  geom_ribbon(data = df2plot.NA,
              aes(x = month, y = pre.m,
                  ymin = pre.min, ymax = pre.max,
                  fill = as.factor(LC)),
              alpha = 0.5) +
  geom_line(data = df2plot.NA,
             aes(x = month, y = pre.m,
                 color = as.factor(LC))) +

  geom_ribbon(data = df2plot.NA,
              aes(x = month, y = deficit.m,
                  ymin = deficit.min, ymax = deficit.max,
                  fill = as.factor(LC)),
              alpha = 0.5) +
  geom_line(data = df2plot.NA,
             aes(x = month, y = deficit.m,
                 color = as.factor(LC))) +
  geom_hline(data = df2plot.NA %>%
               group_by(hemisp) %>%
               summarise(Em = mean(Etot.m,na.rm = TRUE),
                         .groups = "keep"),
             aes(yintercept = Em),
             color = "black",
             linetype = 2) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  facet_wrap(~ hemisp, nrow = 2) +
  labs(x = "", y = "", fill = "", color = "") +
  geom_hline(yintercept = 0, linetype = 1, color = "grey17") +
  scale_color_manual(values =
                       c("#005401","#448704","#c49402","grey")) +
  scale_fill_manual(values =
                      c("#005401","#448704","#c49402","grey")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(color = "none", fill = "none")

dry_season_rects <- data.frame(
  LC = factor(c(1, 1, 2, 2, 2, 3, 3, 3),
              levels = c(2,3,1)),
  hemisp = c("N", "S", "N", "N", "S", "N", "N", "S"),
  xmin = c(1, 4, 12, 1, 6, 11, 1, 4) -0.1,
  xmax = c(12, 10, 12, 2, 8, 12, 4, 10) + 0.1)

df2plot.NA %>%
  filter(hemisp == "N",
         month == 3,
         LC == 2)

df2plot.NA %>%
  filter(hemisp == "S",
         month == 4,
         LC == 1)

df2plot.NA %>%
  filter(hemisp == "S",
         month == 4,
         LC == 3)


ggplot() +
  geom_rect(data = dry_season_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.4,
            inherit.aes = FALSE) +
  geom_ribbon(data = df2plot.NA,
              aes(x = month, y = pre.m,
                  ymin = pre.min, ymax = pre.max,
                  fill = as.factor(LC)),
              alpha = 0.5) +
  geom_point(data = df2plot.NA,
            aes(x = month, y = pre.m,
                color = as.factor(LC))) +

  geom_ribbon(data = df2plot.NA,
              aes(x = month, y = deficit.m,
                  ymin = deficit.min, ymax = deficit.max,
                  fill = as.factor(LC)),
              alpha = 0.5) +
  geom_point(data = df2plot.NA,
            aes(x = month, y = deficit.m,
                color = as.factor(LC))) +
  geom_hline(data = df2plot.NA %>%
               group_by(hemisp,LC) %>%
               summarise(Em = mean(Etot.m,na.rm = TRUE),
                         .groups = "keep"),
             aes(yintercept = Em),
             color = "black",
             linetype = 2) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  facet_grid(hemisp ~ LC) +
  labs(x = "", y = "", fill = "", color = "") +
  geom_hline(yintercept = 0, linetype = 1, color = "grey17") +
  scale_color_manual(values =
                       c("#005401","#448704","#c49402","grey")) +
  scale_fill_manual(values =
                      c("#005401","#448704","#c49402","grey")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20)) +
  guides(color = "none", fill = "none")

stop()

head(df2plot.NA)

saveRDS(df2plot.NA,
        "./outputs/SC.water.deficit.RDS")


ggplot() +
  geom_ribbon(data = df2plot.NA,
              aes(x = month, y = Etot.m,
                  ymin = Etot.min, ymax = Etot.max,
                  fill = as.factor(LC)),
              alpha = 0.5) +
  geom_point(data = df2plot.NA,
            aes(x = month, y = Etot.m,
                color = as.factor(LC))) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  geom_ribbon(data = df2plot.NA,
              aes(x = month, y = deficit.m,
                  ymin = deficit.min, ymax = deficit.max,
                  fill = as.factor(LC)),
              alpha = 0.5) +
  geom_point(data = df2plot.NA,
            aes(x = month, y = deficit.m,
                color = as.factor(LC))) +

  facet_wrap(~hemisp,ncol = 1) +
  labs(x = "", y = "", fill = "", color = "") +
  geom_hline(yintercept = 0, linetype = 1, color = "grey17") +
  scale_color_manual(values =
                       c("#005401","#448704","#c49402","grey")) +
  scale_fill_manual(values =
                      c("#005401","#448704","#c49402","grey")) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 20)) +
  guides(color = "none", fill = "none")



df.sum.all <- Clim.Mask.filled.all.MCWD %>%
  mutate(hemisp = case_when(lat >= 0 ~ "N",
                            TRUE ~ "S")) %>%
  group_by(hemisp,model,LC,lon,lat) %>%
  summarise(MAP = sum(pre,na.rm = TRUE),
            MCWD = unique(MCWD),
            .groups = "keep")


ggplot(data = df.sum.all %>%
         filter(LC %in% c(1:3))) +
  geom_density(aes(x = MCWD,
                   fill = as.factor(LC)),
               alpha = 0.5,
               color = NA) +
  facet_wrap(~hemisp) +
  geom_vline(data = Sensitivity.thresholds,
             aes(xintercept = MCWD.threshold),
             color = "black") +
  scale_fill_manual(values =
                      c("#c49402","#005401","#448704","grey")) +
  theme_bw()

ggplot(data = df.sum.all %>%
         filter(LC %in% c(1:3))) +
  geom_density(aes(x = MAP,
                   fill = as.factor(LC)),
               alpha = 0.5,
               color = NA) +
  scale_x_continuous(limits = c(0,3000)) +
  geom_vline(data = Sensitivity.thresholds,
             aes(xintercept = MAP.threshold),
             color = "black") +
  facet_wrap(~hemisp) +
  scale_fill_manual(values =
                      c("#c49402","#005401","#448704","grey")) +
  theme_bw()

threshold.sum <- readRDS("./outputs/Sensitivity.thresholds.sum.RDS")

df.sum.all.var <- df.sum.all %>%
  filter(model == "MEM") %>%
  filter(LC %in% c(1:3)) %>%
  pivot_longer(cols = c(MAP,MCWD),
               names_to = "variable",
               values_to = "value")


all.together <- bind_rows(df.sum.all.var,
                          df.sum.all.var %>%
                            mutate(hemisp = "Z")) %>%
                            filter(LC %in% c(1:3))

ggplot() +
  geom_rect(data = threshold.sum,
            aes(xmin = min,xmax = max,
                ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5, color = NA) +
  geom_vline(data = threshold.sum,
            aes(xintercept = mean),
            color = "grey17") +
  geom_density(data = all.together,
               aes(x = value,
                   fill = as.factor(LC)),
               alpha = 0.5,
               color = NA) +
  facet_grid(hemisp~variable,scales = "free") +
  scale_fill_manual(values =
                      c("#c49402","#005401","#448704","grey")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x = "", y = "") +
  guides(fill = "none")


# ERA5
ERA5 <- df.sum.all %>%
  filter(model %in% c("ERA5","MEM","CRUJRA")) %>%
  filter(LC %in% c(1:3)) %>%
  pivot_longer(cols = c(MAP,MCWD),
               names_to = "variable",
               values_to = "value")

ggplot() +
  geom_vline(data = Sensitivity.thresholds %>%
               dplyr::select(-Acc) %>%
             filter(model %in% c("MEM","ERA5","CRUJRA")) %>%
               pivot_longer(cols = c(MCWD.threshold,MAP.threshold),
                            names_to = "variable",
                            values_to = "mean") %>%
               mutate(variable = stringr::str_replace(variable,
                                                      ".threshold",
                                                      "")),
             aes(xintercept = mean),
             color = "grey17") +
  geom_density(data = ERA5,
               aes(x = value,
                   fill = as.factor(LC)),
               alpha = 0.5,
               color = NA) +
  facet_grid(model~variable,scales = "free") +
  scale_fill_manual(values =
                      c("#c49402","#005401","#448704","grey")) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "", y = "") +
  guides(fill = "none")
