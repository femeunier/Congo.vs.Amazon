rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

CC.basins.RS <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/CC.basins.RS.RDS") %>%
  pivot_wider(names_from = "var",
              values_from = c("pred.m","pred.m.rm",
                              "mean.obs",
                              "anomaly","anomaly.rm",
                              "anomaly.m","anomaly.m.rm"))

CC.basins <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/CC.basins.RDS") %>%
  pivot_wider(names_from = "var",
              values_from = c("pred.m","pred.m.rm",
                              "mean.obs",
                              "anomaly","anomaly.rm",
                              "anomaly.m","anomaly.m.rm"))

all <- bind_rows(CC.basins %>%
                   mutate(source = "Trendy"),
                 CC.basins.RS %>%
                   mutate(source = "RS"))

cmonth = 5
droughts <- data.frame(x1 = c(1997 + 9/12,
                              # 2010 + 7/12,
                              2015 + 8/12,
                              2023 + 7/12) + 0.5/12,
                       x2 = c(1998 + 4/12 ,
                              # 2010 + 10/12,
                              2016 + 3/12,
                              2024 + cmonth/12) + 0.5/12)


ggplot(data = all %>%
         filter( (source == "Trendy" & year <= 2022))) +

  geom_rect(data = droughts %>%
              filter(x2 <= 2020),
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_point(aes(x = year + (month - 1/2)/12,
                y = pred.m_gpp*10,
                color = basin),
            size = 0.2) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = mean.obs_gpp*10,
                color = basin), linetype = 2) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m.rm_gpp*10,
                color = basin)) +

  geom_vline(aes(xintercept = 2022 + 11.5/12),
             linetype = 2) +

  scale_color_manual(values = c("#5ab4ac","#d8b365")) +
  labs(x = "",y = "", color = "") +
  scale_y_continuous(limits = c(2.5,3.5)*10) +
  scale_x_continuous(limits = c(1994,2025)) +
  guides(color = FALSE) +
  theme_bw() +
  labs(x = "", y = "GPP (MgC/ha/yr)") +
  facet_wrap(~ basin) +
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())


ggplot(data = all %>%
         filter( (source == "Trendy" & year <= 2022))) +

  geom_rect(data = droughts %>%
              filter(x2 <= 2020),
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_point(aes(x = year + (month - 1/2)/12,
                 y = pred.m_gpp*10,
                 color = basin),
             size = 0.2) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = mean.obs_gpp*10,
                color = basin), linetype = 2) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m.rm_gpp*10,
                color = basin)) +

  geom_vline(aes(xintercept = 2022 + 11.5/12),
             linetype = 2) +

  scale_color_manual(values = c("#5ab4ac","#d8b365")) +
  labs(x = "",y = "", color = "") +
  scale_y_continuous(limits = c(2.5,3.5)*10) +
  scale_x_continuous(limits = c(1994,2025)) +
  guides(color = FALSE) +
  theme_bw() +
  labs(x = "", y = "GPP (MgC/ha/yr)") +
  facet_wrap(~ basin) +
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())




ggplot(data = all %>%
         filter(source == "Trendy")) +

  geom_rect(data = droughts ,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_point(aes(x = year + (month - 1/2)/12,
                 y = pred.m_gpp*10,
                 color = basin),
             size = 0.2) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = mean.obs_gpp*10,
                color = basin), linetype = 2) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m.rm_gpp*10,
                color = basin)) +

  scale_color_manual(values = c("#5ab4ac","#d8b365")) +
  labs(x = "",y = "", color = "") +
  scale_y_continuous(limits = c(2.5,3.5)*10) +
  scale_x_continuous(limits = c(1994,2025)) +
  guides(color = FALSE) +
  theme_bw() +
  labs(x = "", y = "GPP (MgC/ha/yr)") +
  facet_wrap(~ basin) +
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())



ggplot(data = all %>%
         filter(source == "Trendy")) +

  geom_rect(data = droughts ,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_point(aes(x = year + (month - 1/2)/12,
                 y = anomaly.m_gpp,
                 color = basin),
             size = 0.2) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.m.rm_gpp,
                color = basin)) +

  scale_color_manual(values = c("#5ab4ac","#d8b365")) +
  labs(x = "",y = "", color = "") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  scale_y_continuous(limits = c(-6,3)) +
  scale_x_continuous(limits = c(1985,2025)) +
  guides(color = FALSE) +
  theme_bw() +
  labs(x = "", y = "GPP normalized anomaly (-)") +
  # facet_wrap(~ basin) +
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

