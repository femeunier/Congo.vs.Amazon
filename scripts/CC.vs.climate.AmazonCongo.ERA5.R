rm(list = ls())

library(dplyr)
library(ggplot2)

CC.basins.RS <- readRDS("./outputs/CC.basins.RS.RDS") %>%
  pivot_wider(names_from = "var",
              values_from = c("pred.m","pred.m.rm",
                              "mean.obs",
                              "anomaly","anomaly.rm",
                              "anomaly.m","anomaly.m.rm"))

CC.basins <- readRDS("./outputs/CC.basins.RDS") %>%
  pivot_wider(names_from = "var",
              values_from = c("pred.m","pred.m.rm",
                              "mean.obs",
                              "anomaly","anomaly.rm",
                              "anomaly.m","anomaly.m.rm"))
climate.basins <- readRDS("./outputs/ERA5.basins.RDS") %>%
  pivot_wider(names_from = "variable",
              values_from = c("value","anomaly","anomaly",
                              "anomaly.m","anomaly.m.rm",
                              "anomaly","anomaly.rm"))

all.CC <- bind_rows(CC.basins %>%
                      mutate(source = "Trendy"),
                    CC.basins.RS %>%
                      mutate(source = "RS"))

all.CC.SC <- all.CC %>%
  filter(year %in% c(1990:2020)) %>%
  group_by(source,month,basin) %>%
  summarise(pred.m_gpp = mean(pred.m_gpp),
            .groups = "keep")

all.CC.SC.m <- all.CC.SC %>%
  group_by(source,basin) %>%
  summarise(pred.m_gpp = mean(pred.m_gpp),
            .groups = "keep")


ggplot() +
  geom_line(data = all.CC.SC,
            aes(x = month,
                y = pred.m_gpp,
                color = basin)) +
  geom_hline(data = all.CC.SC.m ,
             aes(yintercept = pred.m_gpp,
                 color = basin),
             linetype = 2) +
  facet_wrap(~ source) +
  theme_bw()

CC.vs.climate <- all.CC %>%
  left_join(climate.basins,
            by = c("basin","year","month")) %>%
  filter(year >= 1994)


ggplot(data = CC.vs.climate,
       aes(x = anomaly.rm_tmp,
           y = anomaly.rm_gpp,
           color = source)) +
  geom_point(size = 0.1) +
  geom_point(data = CC.vs.climate %>%
               filter(year == 2024),
             size = 1) +
  geom_smooth(method = "gam",
              se = FALSE) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_wrap(~ basin) +
  theme_bw()

last.drought <- CC.vs.climate %>%
  filter( (year >= 2023 & month >= 8) |
            (year ==  2024))


hull_cyl <- last.drought  %>%
  group_by(basin, source) %>%
  filter(!is.na(anomaly.m_gpp)) %>%
  dplyr::slice(chull(anomaly.rm_tmp,anomaly.m.rm_gpp))

ggplot(data = CC.vs.climate %>%
         filter(source == "Trendy"),
       aes(x = anomaly.m.rm_tmp,
           y = anomaly.m.rm_gpp,
           color = basin,
           fill = basin)) +


  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +

  geom_polygon(data = hull_cyl  %>%
                 filter(source == "Trendy"),
               color = NA,
               alpha = 0.2, show.legend = FALSE) +

  geom_point(size = 0.1) +
  geom_point(data = last.drought %>%
               filter(source == "Trendy"),
             size = 1) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x,2),
              se = FALSE) +
  facet_wrap(~ source) +
  labs(x = "Temperature anomaly (°C)",
       y = "GPP normalized anomaly (-)") +
  scale_color_manual(values = c("#5ab4ac","#d8b365")) +
  scale_fill_manual(values = c("#5ab4ac","#d8b365")) +
  theme_bw() +
  theme(text = element_text(size = 14),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(color = "none",
         fill = "none")


ggplot(data = CC.vs.climate,
       aes(x = anomaly.rm_pre,
           y = anomaly.rm_gpp,
           color = basin)) +
  geom_point(size = 0.1) +
  geom_point(data = CC.vs.climate %>%
               filter(year == 2024),
             size = 1) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x,3),
              se = FALSE) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_wrap(~ source) +
  theme_bw()

CC.vs.climate %>%
  group_by(basin,source) %>%
  summarise(r2 = summary(lm(formula = anomaly.rm_gpp ~
                             poly(anomaly.rm_tmp,2) *
                              poly(anomaly.rm_pre,2)))[["adj.r.squared"]])

