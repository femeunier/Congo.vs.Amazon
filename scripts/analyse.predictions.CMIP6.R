rm(list = ls())

library(dplyr)
library(ggplot2)
library(pals)
library(cowplot)
library(pals)
library(sf)
library(tidyr)
library(zoo)

Prefix <- "Basin.Comp.PreTmp"

files2transfer <- paste0("all.CMIP6.predictions.",Prefix,".RDS")

# for (cfile in files2transfer){
#   system2("rsync",
#           c("-avz",
#             paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }

all.CMIP6.predictions <- readRDS(paste0("./outputs/all.CMIP6.predictions.",Prefix,".RDS"))
CMIP6models <- unique(all.CMIP6.predictions$CMIP6model)

all.CMIP6.predictions.EM <- all.CMIP6.predictions %>%
  filter(model == "JULES") %>%
  group_by(year,scenario,basin,var) %>%
  summarise(value.m.EM = mean(value.m),
            value.min.EM = min(value.m),
            value.max.EM = max(value.m),
            .groups = "keep") %>%
  group_by(basin,var) %>%
  mutate(value.m.EM.rel = value.m.EM/mean(value.m.EM[year %in% 1994:2023]))


ggplot(data = all.CMIP6.predictions.EM) +
  geom_ribbon(aes(x = year,
                ymin = value.min.EM,
                ymax = value.max.EM,
                fill = scenario,
                linetype = basin,
                group = interaction(scenario,basin)),
              alpha = 0.2) +
  geom_line(aes(x = year,
                y = value.m.EM,
                color = scenario,
                linetype = basin,
                group = interaction(scenario,basin))) +
  scale_x_continuous(limits = c(1980,2100)) +
  scale_color_manual(values = c('darkgrey',"#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  scale_fill_manual(values = c('darkgrey',"#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  facet_wrap(~ var,
             scales = "free") +
  theme_bw() +
  labs(x= "",y = "", color = "") +
  # geom_hline(yintercept = 1, linetype = 2, color = "black") +
  # geom_vline(xintercept = 2024) +
  guides(color = "none")
