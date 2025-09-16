rm(list = ls())

library(dplyr)
library(ggplot2)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/all.TS.global.warming.RDS",
          "./outputs/"))

CMIP6.models <- read.csv("outputs/CMIP6.table.csv") %>%
  pull(model)

all.global.warming <- readRDS("./outputs/all.TS.global.warming.RDS") %>%
  filter(model %in% CMIP6.models) %>%
  filter(!(model %in% c("IPSL-CM6A-LR"))) %>%
  distinct() %>%
  filter(!(year > 2014 & scenario == "historical"))

# Complete overshoot
models <- all.global.warming %>%
  filter(scenario == "ssp534-over") %>%
  pull(model) %>%
  unique()

all.global.warming.compl <- all.global.warming %>%
  filter(scenario != "ssp534-over")
for (i in seq(1,length(models))){

  cmodel <- models[i]
  print(cmodel)

  cdf <- all.global.warming %>%
    filter(model == cmodel,
           scenario == "ssp534-over")

  cyears <- unique(cdf$year)
  missing.years <- c(2015:2100)[(!(c(2015:2100) %in% cyears))]

  missing <- any(missing.years)

  if (missing){
    cdf.mod <- all.global.warming %>%
      filter(model == cmodel,
             year %in% missing.years,
             scenario == "ssp585")


    mod <- bind_rows(cdf,
                     cdf.mod %>%
                       mutate(scenario = "ssp534-over")) %>%
      arrange(year)

    all.global.warming.compl <-
      bind_rows(all.global.warming.compl,
                mod)

  }
}

all.global.warming.compl.sum <- all.global.warming.compl %>%
  group_by(year,scenario) %>%
  summarise(tas.m = mean(tas,na.rm = TRUE),
            .groups = "keep")

all.global.warming.compl.sum.model <- all.global.warming.compl %>%
  filter(model %in% models) %>%
  group_by(year,scenario) %>%
  summarise(tas.m = mean(tas,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_line(data = all.global.warming.compl,
            aes(x = year, y = tas -273.15, color = scenario,
                group = interaction(scenario,model)),
            size = 0.1) +
  geom_line(data = all.global.warming.compl.sum,
            aes(x = year, y = tas.m - 273.15, color = scenario)) +
  theme_bw()


ggplot() +
  geom_line(data = all.global.warming.compl %>%
              filter(model %in% models),
            aes(x = year, y = tas -273.15, color = scenario,
                group = interaction(scenario,model)),
            size = 0.1) +
  geom_line(data = all.global.warming.compl.sum.model,
            aes(x = year, y = tas.m - 273.15, color = scenario)) +
  theme_bw()


MEM <- bind_rows(all.global.warming.compl,
                 all.global.warming.compl %>%
                   group_by(year,variant,scenario) %>%
                   summarise(tas = mean(tas,na.rm = TRUE),
                             .groups = "keep") %>%
                   mutate(model = "MEM"))

saveRDS(MEM,
        "./outputs/Global.warming.MEM.RDS")

MEM.sel <- bind_rows(all.global.warming.compl %>%
                       filter(model %in% models),
                     all.global.warming.compl %>%
                       filter(model %in% models) %>%
                       group_by(year,variant,scenario) %>%
                       summarise(tas = mean(tas,na.rm = TRUE),
                                 .groups = "keep") %>%
                       mutate(model = "MEM"))

saveRDS(MEM.sel,
        "./outputs/Global.warming.MEM.sel.RDS")


