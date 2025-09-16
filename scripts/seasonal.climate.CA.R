rm(list = ls())

library(dplyr)
library(tidyr)

system2("scp",
        c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/climate.CA.rspld.RDS",
          "./outputs/"))
A <- readRDS("./outputs/climate.CA.rspld.RDS")

A.long <- A %>%
  filter(year %in% c(1981:2010)) %>%
  pivot_longer(cols = c(pre,tas,tasmin,tasmax),
               names_to = "variable",
               values_to = "value") %>%
  filter(!is.na(value))

A.seasonal <- A.long %>%
  group_by(product,lon,lat,month,variable) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

saveRDS(A.seasonal,
        "./outputs/climate.CA.seasonal.rspld.RDS")

A <- readRDS("./outputs/climate.CA.seasonal.rspld.RDS")


