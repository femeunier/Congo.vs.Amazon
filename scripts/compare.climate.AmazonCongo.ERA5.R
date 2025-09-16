rm(list = ls())

library(dplyr)
library(ggplot2)
library(pals)
library(cowplot)
library(pals)
library(lubridate)

coord <- bind_rows(
  readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Amazon.coord.ILF.RDS"),
  readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Congo.coord.ILF.RDS")) %>%
  filter(model == "CABLE-POP") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))

climate <- readRDS("/home/femeunier/Documents/data/monthly.climate.global.Tropics.RDS")

climate.select <- climate %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01"))),
         basin = case_when(lon <= -10 ~ "Amazon",
                           TRUE ~ "Congo"))

climate.sum <- climate.select %>%
  group_by(basin,year,month) %>%
  summarise(tmp = mean(tmp) - 273.15,
            spfh = mean(spfh),
            VPD = mean(VPD),
            dswrf = mean(dswrf),
            dlwrf = mean(dlwrf),
            tmin = mean(tmin) - 273.15,
            tmax = mean(tmax) - 273.15,
            pre = mean(pre*N*8),
            .groups = "keep") %>%
  pivot_longer(cols = -c(year,month,basin),
               names_to = "variable",
               values_to = "value") %>%
  mutate(time = year + (month - 1/2)/12)

ggplot(data = climate.sum) +
  geom_line(aes(x = time,
                y = value,
                color = basin)) +
  facet_wrap(~variable,scales = "free") +
  scale_x_continuous(limits = c(2000,2025)) +
  theme_bw()

Window <- 6
year.min = 1994 ; year.max = 2023

climate.sum.anomaly <- climate.sum %>%
  group_by(basin,variable) %>%
  mutate(mean.obs = mean(value[time >= year.min & time < year.max])) %>%
  mutate(detrended = value - mean.obs) %>%
  group_by(basin,variable,month) %>%
  mutate(mean.month = mean(detrended[time >= year.min & time < year.max])) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin,variable) %>%
  mutate(anomaly.m = anomaly/sd(anomaly[time >= year.min & time < year.max])) %>%
  mutate(value.rm = rollapply(value, width=Window,
                              FUN=function(x) mean(x, na.rm=TRUE),
                              partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         anomaly.rm = rollapply(anomaly, width=Window,
                               FUN=function(x) mean(x, na.rm=TRUE),
                               partial=TRUE, fill=NA, align="right")) %>%
  mutate(time = year + (month - 1/2)/12)


climate.SC <- climate.sum %>%
  filter(year >= year.min,
         year < year.max) %>%
  group_by(month,basin,variable) %>%
  summarise(value.m = mean(value),
            .groups = "keep")

climate.mean <- climate.sum %>%
  filter(year >= year.min,
         year < year.max) %>%
  group_by(basin,variable) %>%
  summarise(value.m = mean(value),
            .groups = "keep")

ggplot() +
  geom_line(data = climate.SC,
            aes(x = month,
                y = value.m,
                color = basin)) +
  geom_hline(data = climate.mean,
             aes(yintercept = value.m,
                 color = basin),
             linetype = 2) +
  facet_wrap(~ variable,scales = "free") +
  theme_bw()



ggplot() +
  geom_line(data = climate.SC %>%
              filter(variable %in% c("pre","tmp")),
            aes(x = month,
                y = value.m,
                color = basin)) +
  geom_hline(data = climate.mean %>%
               filter(variable %in% c("pre","tmp")),
             aes(yintercept = value.m,
                 color = basin),
             linetype = 2) +
  facet_wrap(~ variable,scales = "free") +
  theme_bw()


ggplot() +
  geom_line(data = climate.sum.anomaly %>%
              filter(year >= year.min,
                     year < year.max,
                     variable %in% c("tmp","pre")),
            aes(x = month,
                y = anomaly.m,
                group = interaction(basin,year)),
            color = "grey",
            linewidth = 0.1) +

  geom_line(data = climate.sum.anomaly %>%
              filter(((basin == "Amazon" & time >= 2023.5) |
                        ((basin == "Congo" & time >= 2023.5))),
                     variable %in% c("tmp","pre")),
            aes(x = month,
                y = anomaly.m,
                group = interaction(basin,year)),
            color = "black",
            linewidth = 1) +

  geom_hline(yintercept = 0,
             linetype = 2, color = "black") +
  facet_grid(variable ~ basin,scales = "free") +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "") +

  theme_bw()




ggplot() +
  geom_line(data = climate.sum.anomaly,
            aes(x = year + (month - 1/2)/12,
                y = anomaly.m,
                color = basin)) +
  facet_wrap(~ variable,scales = "free") +
  scale_x_continuous(limits = c(2000,2025),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(-1,1)*7) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  theme_bw()




saveRDS(climate.sum.anomaly %>%
          dplyr::select(basin,year,month,variable,value,
                        anomaly,anomaly.m,
                        anomaly.rm,anomaly.m.rm),
        "./outputs/ERA5.basins.RDS")

