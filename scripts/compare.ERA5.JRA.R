rm(list = ls())

rm(list = ls())

library(dplyr)
library(ggplot2)
library(pals)
library(cowplot)
library(pals)
library(lubridate)

cfile <- "df.ERA5.Tropics.RDS"
system2("rsync",
        c("-avz",
          paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
          "./outputs/"))

coord <- bind_rows(
  readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Amazon.coord.ILF.RDS"),
  readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Congo.coord.ILF.RDS")) %>%
  filter(model == "CABLE-POP") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))

climate <- readRDS(paste0("./outputs/",
                          cfile))

climate.select <- climate %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  mutate(basin = case_when(lon <= -10 ~ "Amazon",
                           TRUE ~ "Congo"))


Window = 12

climate.sum <- climate.select %>%
  group_by(basin,year,month) %>%
  summarise(tmp = mean(tmp),
            pre = mean(MAP),
            .groups = "keep") %>%
  pivot_longer(cols = -c(year,month,basin),
               names_to = "variable",
               values_to = "value") %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(variable,basin)


ERA5 <- climate.sum %>%
  group_by(basin,variable) %>%
  mutate(mean.obs = mean(value[time >= 1970 & time < 2000],
                         na.rm = TRUE)) %>%
  mutate(detrended = value - mean.obs) %>%
  group_by(basin,variable,month) %>%
  mutate(mean.month = mean(detrended[time >= 1970 & time < 2000],
                           na.rm = TRUE)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin,variable) %>%
  mutate(anomaly.m = anomaly/sd(anomaly[time >= 1970 & time < 2000],
                                na.rm = TRUE)) %>%
  dplyr::select(basin,year,month,variable,value,anomaly,anomaly.m)

# JRA
JRA <- readRDS("./outputs/JRA.basins.RDS")

all <- bind_rows(ERA5 %>%
                   mutate(source = "ERA5"),
                 JRA %>%
                   filter(variable %in% c("tmp","pre")) %>%
                   mutate(source = "JRA")) %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(basin,source,variable) %>%
  mutate(value.rm = rollapply(value, width=Window,
                              FUN=function(x) mean(x, na.rm=TRUE),
                              partial=TRUE, fill=NA, align="center"),
         anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="center"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center"))

################################################################################

ggplot(data = all %>%
         filter(variable == "tmp"),
       aes(x = time, y = value - 273.15, color = source)) +
  geom_line(size = 0.25,
            color = "grey") +
  geom_line(aes(y = value.rm - 273.15, color = source)) +
  facet_grid(~basin) +
  scale_x_continuous(limits = c(1970,2024)) +
  theme_bw()


ggplot(data = all %>%
         filter(variable == "pre"),
       aes(x = time, y = value, color = source)) +
  geom_line(size = 0.25,
            color = "grey") +
  geom_line(aes(y = value.rm, color = source)) +
  facet_grid(~basin) +
  scale_x_continuous(limits = c(1970,2024)) +
  theme_bw()

################################################################################


ggplot(data = all %>%
         filter(variable == "tmp"),
       aes(x = time, y = anomaly, color = source)) +
  geom_line(size = 0.25,
            color = "grey") +
  geom_line(aes(y = anomaly.rm, color = source)) +
  facet_grid(~basin) +
  scale_x_continuous(limits = c(1970,2024)) +
  theme_bw()

ggplot(data = all %>%
         filter(variable == "pre"),
       aes(x = time, y = anomaly, color = source)) +
  geom_line(size = 0.25,
            color = "grey") +
  geom_line(aes(y = anomaly.rm, color = source)) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(~basin) +
  scale_x_continuous(limits = c(1970,2024)) +
  theme_bw()

################################################################################

ggplot(data = all,
       aes(x = time, y = anomaly.m, group = source)) +
  geom_line(size = 0.25,
            color = "grey") +
  geom_line(aes(y = anomaly.m.rm,
                color = source)) +
  facet_grid(variable~basin) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_continuous(limits = c(1970,2024)) +
  theme_bw()
