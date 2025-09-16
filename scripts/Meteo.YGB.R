rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

all.files <- list.files("/home/femeunier/Documents/projects/Congo.vs.Amazon/data/Meteo_Data/",pattern = "*.csv",
                        full.names = TRUE)

all <- data.frame()

for (ifile in seq(1,length(all.files))){

  print(ifile/length(all.files))

  cfile <- all.files[ifile]
  if (!file.exists(cfile)){
    next()
  }

  cdata <- read.csv(cfile,header = TRUE,skip = 3)
  all <- bind_rows(all,
                   cdata[,c(1,3,6)] %>%
                     rename(timestamp = Smp,
                            temp = Smp.2,
                            precip = Tot) %>%
                     mutate(timestamp = sub("\\.","",as.character(timestamp))) %>%
                     mutate(file = cfile))

}

all <- all %>%
  mutate(year = as.numeric(substr(timestamp,1,4)),
         month = as.numeric(substr(timestamp,5,6)),
         day = as.numeric(substr(timestamp,7,8)))

all.day <- all %>%
  group_by(year,month,day) %>%
  summarise(temp = mean(temp,na.rm = TRUE),
            precip = sum(precip,na.rm = TRUE),
            N = n(),
            .groups = "keep") %>%
  mutate(full.day = (N>=(86400/30)),
         half.day = (N>=(86400/2/30)))

ggplot(data = all.day %>%
         filter(year == 2021,
                month == 4)) +
  geom_point(aes(x = day, y = precip)) +
  theme_bw()

all.month <- all.day %>%
  filter(half.day) %>%
  group_by(year,month) %>%
  mutate(Ndays.max = days_in_month(as.Date(paste0(year,"/",month,"/01")))) %>%
  summarise(Ndays = length(precip),
            temp = case_when(unique(Ndays/Ndays.max) > (1/6) ~ mean(temp,na.rm = TRUE),
                             TRUE ~ NA_real_),
            precip = case_when(year == 2021 & month == 4 ~ NA_real_,
                               unique(Ndays/Ndays.max) > (1/6) ~ sum(precip,na.rm = TRUE),
                               TRUE ~ NA_real_),
            .groups = "keep")

all.month.long <- all.month %>%
  pivot_longer(cols = c(temp,precip),
               names_to = "var")

ggplot(data = all.month.long) +
  geom_line(aes(x = year + (month - 1/2)/12,
                 y = value)) +
  geom_point(aes(x = year + (month - 1/2)/12,
                y = value)) +
  facet_wrap(~ var, scales = "free") +
  labs(x = "") +
  theme_bw()

seasonal.C <- all.month.long %>%
  group_by(var,month) %>%
  summarise(value.m = mean(value,
                           na.rm = TRUE),
            .groups = "keep")

ggplot(data = seasonal.C) +
  geom_line(data = all.month.long %>%
              filter(year == 2024),
            aes(x = month,
                y = value),
            color = "red") +
  geom_point(aes(x = month,
                y = value.m)) +
  geom_line(aes(x = month,
                 y = value.m),
            linewidth = 0.4) +
  # geom_point(aes(x = year + (month - 1/2)/12,
  #                y = value)) +
  facet_wrap(~ var, scales = "free") +
  labs(x = "") +
  theme_bw()

