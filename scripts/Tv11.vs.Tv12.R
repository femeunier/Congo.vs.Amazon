rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

Tv11 <- readRDS("./outputs/Basin.Comp.mean.sum.RDS")
Tv12 <- readRDS("./outputs/Basin.Comp.v12.mean.sum.RDS")


models.intersect <- intersect(unique(Tv11$model),
                              unique(Tv12$model))

all <- bind_rows(Tv11 %>%
                   mutate(source = "Tv11"),
                 Tv12 %>%
                   mutate(source = "Tv12")) %>%
  filter(year == 2022)

all.MEM <- bind_rows(all,
                     all %>%
                       group_by(var,year,month,basin,source) %>%
                       summarise(pred = mean(pred),
                                 obs = mean(obs),
                                 model = "MEM",
                                 .groups = "keep"))

all.wide <- all.MEM %>%
  pivot_wider(names_from = source,
              values_from = c(pred,obs))

ggplot(data = all.wide %>%
         dplyr::select(-obs_Tv11) %>%
         na.omit() %>%
         filter(model != "MEM"),
       aes(x = pred_Tv11,
           y = obs_Tv12,
           color = model,
           fill = model)) +
  geom_point() +
  stat_smooth(method="lm",
              formula = y ~ x) +
  geom_abline(intercept = 0,
              slope = 1) +
  facet_wrap(~ var,
             scales = "free") +
  theme_bw()


ggplot(data = all.wide %>%
         filter(model %in% c(models.intersect,"MEM")),
       aes(x = pred_Tv11,
           y = obs_Tv12,
           color = basin,
           fill = basin)) +
  geom_point() +
  stat_smooth(method="lm",
              formula = y ~ x) +
  geom_abline(intercept = 0,
              slope = 1) +
  facet_wrap(~ var,
             scales = "free") +
  theme_bw()

ggplot(data = all.wide %>%
         filter(basin == "Congo",
                model %in% c(models.intersect,"MEM")),
       aes(x = month,
           y = obs_Tv12,
           color = var)) +
  geom_line(   aes(x = month,
                   y = pred_Tv11,
                   color = var)) +
  geom_line(linetype = 2) +
  facet_wrap( ~ model) +
  theme_bw()


ggplot(data = all.wide %>%
         filter(model == "MEM"),
       aes(x = month,
           y = obs_Tv12,
           color = var)) +
  geom_line(aes(x = month,
                y = pred_Tv11,
                color = var)) +
  geom_line(linetype = 2) +
  facet_wrap( ~ basin,
              scales = "free") +
  theme_bw()
