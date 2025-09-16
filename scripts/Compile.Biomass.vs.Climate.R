rm(list = ls())

library(ggthemes)
library(dplyr)
library(ggplot2)
library(multcompView)
library(akima)
library(ggExtra)

df.biomass <- readRDS("./outputs/Map.biomass.RDS") %>%
  mutate(AGB = case_when(source == "Xu" ~ AGB*2,
                         TRUE ~ AGB))

df.biomass <- bind_rows(df.biomass,
                        df.biomass %>%
                          group_by(lon,lat) %>%
                          summarise(AGB = mean(AGB,na.rm = TRUE),
                                    .groups = "keep") %>%
                          mutate(source = "MEM")) %>%
  mutate(lon.lat = paste0(lon,".",lat))

saveRDS(df.biomass,
        "./outputs/All.biomass.maps.RDS")

LC <- readRDS("./outputs/LC.pred.MEM.RDS") %>%
  filter(lat <= 10, lat >= -15,
         lon >= -15, lon <= 60) %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  distinct()

Clim.Mask.MCWD.sum <- readRDS("./outputs/Summary.climate.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(model == "MEM")

df.biomass.vs.climate <- df.biomass %>%
  filter(lon.lat %in% Clim.Mask.MCWD.sum[["lon.lat"]]) %>%
  left_join(Clim.Mask.MCWD.sum %>%
              dplyr::select(-lon.lat),
            by = c("lon","lat")) %>%
  filter(LC %in% c(1,2,3))

ggplot(data = df.biomass.vs.climate %>%
         filter(!is.na(LC))) +
  geom_boxplot(aes(x = as.factor(LC),
                   y = AGB,
                   fill = as.factor(LC))) +
  # scale_y_log10() +
  facet_wrap(~source,
             nrow = 1) +
  theme_bw()

all.interp <- data.frame()
my_dupfun <- function(x) {
  cat("Custom dupfun called on:", x, "\n")
  quantile(x, 0.95, na.rm = TRUE)
}

res_x <- 50 ; res_y <- 50

for (csource in unique(df.biomass.vs.climate$source)){

  print(csource)

  df <- df.biomass.vs.climate %>%
    filter(source == csource) %>%
    rename(x = MCWD,
           y = MAP,
           z = AGB) %>%
    na.omit()

  df <- df %>%
    mutate(
      x_bin = floor(x / res_x) * res_x + res_x / 2,
      y_bin = floor(y / res_y) * res_y + res_y / 2
    )

  grid_mean <- df %>%
    group_by(x_bin, y_bin) %>%
    summarise(z = mean(z, na.rm = TRUE), .groups = "drop")

  grid_q95 <- df %>%
    group_by(x_bin, y_bin) %>%
    summarise(z = quantile(z, 0.95, na.rm = TRUE), .groups = "drop")

  all.interp <- bind_rows(all.interp,
                          grid_mean %>%
                            left_join(grid_q95 %>%
                                        rename(AGB.max = z),
                                      by = c("x_bin","y_bin")) %>%
                            rename(MCWD = x_bin, MAP = y_bin,
                                   AGB.m = z) %>%
                            mutate(source = csource))

}

ggplot(grid_mean,
       aes(x = x_bin, y = y_bin, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_fixed() +
  ggtitle("Mean Z per Grid Cell")

ggplot(all.interp,
       aes(x = MCWD,
           y = MAP,
           fill = AGB.m)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~ source, nrow = 1) +
  coord_fixed() +
  theme_bw()

all.interp.long <- all.interp %>%
  tidyr::pivot_longer(cols = c(AGB.m,AGB.max),
                      names_to = "variable",
                      values_to = "value")

MCWD.threshold <- -475 ; MAP.threshold <- 975

df_plot <- all.interp.long %>%
  filter(source == "MEM") %>%
  mutate(LC_pred = case_when(MAP < MAP.threshold ~ 1,
                             MCWD > MCWD.threshold ~ 2,
                             TRUE ~ 3)) %>%
  mutate(LC_pred = factor(LC_pred,
                          levels = c(2,3,1)))

ggplot(df_plot,
       aes(x = MCWD, y = MAP, fill = value/20)) +

  geom_hline(yintercept = MAP.threshold,color = "grey17") +
  geom_vline(xintercept = MCWD.threshold,color = "grey17") +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_bw() +
  labs(x = "",y= "", fill = "")+
  facet_grid(~ variable) +
  theme(    text = element_text(size = 20),
            strip.background = element_blank(),
            strip.text = element_blank(),
            axis.text.x = element_text(size = 14),  # Ensure tick labels show
            axis.text.y = element_text(size = 14)
  )

# Top marginal density
ggplot(df_plot %>%
         filter(variable == "AGB.m"),
       aes(x = as.factor(LC_pred), y = value/20,
           fill = as.factor(LC_pred))) +
  geom_boxplot(alpha = 1) +

  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme_bw() +
  # scale_y_log10() +
  scale_x_discrete(labels = c()) +
  labs(x = "", y = "",fill = "") +
  guides(fill = "none") +
  theme(text = element_text(size = 20))

df_plot %>%
  filter(variable == "AGB.m") %>%
  group_by(LC_pred) %>%
  summarise(AGB.m = mean(value)/20)

df_plot$LC_pred <- as.numeric(as.character(df_plot$LC_pred))
summary(lm(data = df_plot %>%
     ungroup() %>%
     filter(variable == "AGB.m"),
   formula = value ~ as.factor(LC_pred)))


summary(lm(data = df_plot %>%
             ungroup() %>%
             filter(variable == "AGB.max"),
           formula = value ~ as.factor(LC_pred)))


saveRDS(all.interp,
        "./outputs/Biomass.vs.climate.RDS")


