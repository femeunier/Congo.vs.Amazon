rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

A <- readRDS("./outputs/CC.basins.RDS") %>%
  mutate(source = "Trendy")
B <- readRDS("./outputs/CC.basins.RS.RDS") %>%
  mutate(source = "RS")

all <- bind_rows(A,
                 B) %>%
  mutate(time = year + (month - 1/2)/12)


ggplot(data = all %>%
         filter(var == "gpp")) +
  geom_line(aes(x = time,
                y = anomaly.m.rm,
                color = basin,
                group = interaction(source, basin)),
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = anomaly.m,
                 color = basin),
             size = 0.25) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  facet_wrap(~ source) +
  scale_x_continuous(limits = c(1990,2025)) +
  theme_bw()



ggplot(data = all %>%
         filter(var == "gpp")) +
  geom_line(aes(x = time,
                y = pred.m.rm,
                color = basin,
                group = interaction(source, basin)),
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = pred.m,
                 color = basin),
             size = 0.25) +
  facet_wrap(~ source) +
  scale_x_continuous(limits = c(2020,2025)) +
  theme_bw()


ggplot(data = all %>%
         filter(var == "gpp")) +
  geom_line(aes(x = time,
                y = pred.m.rm,
                color = source,
                group = interaction(source, basin)),
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = pred.m,
                 color = source),
             size = 0.25) +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1994,2025)) +
  theme_bw()


ggplot(data = all %>%
         filter(year >= 1994) %>%
         filter(var == "gpp")) +
  geom_line(aes(x = time,
                y = anomaly.m.rm,
                color = source,
                group = interaction(source, basin)),
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = anomaly.m,
                 color = source),
             size = 0.25) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1994,2025)) +
  theme_bw()


ggplot(data = all %>%
         filter(year >= 1994) %>%
         filter(var == "gpp")) +
  geom_line(aes(x = time,
                y = anomaly.m.rm,
                color = source,
                group = interaction(source, basin)),
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = anomaly.m,
                 color = source),
             size = 0.25) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(2020,2025)) +
  theme_bw()

all.wide <- all %>%
  filter(var == "gpp") %>%
  dplyr::select(year,month,var,basin,source,
                pred.m,anomaly.rm,anomaly,anomaly.m) %>%
  pivot_wider(names_from = source,
              values_from = c(pred.m,anomaly,anomaly.m,
                              anomaly.rm))

ggplot(data = all.wide %>%
         filter(year >= 1994),
       aes(x = pred.m_Trendy,
           y = pred.m_RS,
           color = basin, fill = basin)) +
  geom_point(size = 0.25) +
  geom_abline(slope = 1, intercept = 0,
              linetype = 2) +
  stat_smooth(method = "lm") +
  theme_bw()

all.wide %>%
  filter(year >= 1994) %>%
  group_by(basin) %>%
  summarise(R2 = summary(lm(formula = pred.m_RS ~ pred.m_Trendy))[["r.squared"]],
            .groups = "keep")

ggplot(data = all.wide %>%
         filter(year >= 1994),
       aes(x = anomaly.rm_Trendy,
           y = anomaly.rm_RS,
           color = basin, fill = basin)) +
  geom_point(size = 0.25) +
  geom_abline(slope = 1, intercept = 0,
              linetype = 2) +
  stat_smooth(method = "lm") +
  theme_bw()

all.wide %>%
  filter(year >= 1994) %>%
  group_by(basin,var) %>%
  summarise(R2 = summary(lm(formula = anomaly.rm_RS ~ anomaly.rm_Trendy))[["r.squared"]],
            .groups = "keep")

