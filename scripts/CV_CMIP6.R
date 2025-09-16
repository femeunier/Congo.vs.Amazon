rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

CMIP6.files <- list.files("./outputs","*CMIP6.classifications*",
                          full.names = TRUE)
CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]

df.all <- data.frame()
for (file in CMIP6.files){

  CMIP6 <- readRDS(file) %>%
    filter(scenario == "historical",
           period == "current")

  df.all <- bind_rows(df.all,
                      CMIP6 %>%
                        filter(period == "current"))
}


# models.selection <- readRDS("./outputs/models.selected.RDS")

df.all.sum <- df.all %>%
  # filter(model %in% models.selection) %>%
  group_by(lon,lat) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),
            MAT.m = mean(MAT,na.rm = TRUE),
            Etot.m = mean(Etot,na.rm = TRUE),

            MAP.sd = sd(MAP,na.rm = TRUE),
            MCWD.sd = sd(MCWD,na.rm = TRUE),
            MAT.sd = sd(MAT,na.rm = TRUE),
            Etot.sd = sd(Etot,na.rm = TRUE),

            .groups = "keep") %>%
  ungroup() %>%
  mutate(MAP.CV = MAP.sd/MAP.m,
         Etot.CV = Etot.sd/Etot.m,
         MCWD.CV = MCWD.sd/abs(MCWD.m),
         MAT.CV = MAT.sd/MAT.m)

df.all.sum.long <- df.all.sum %>%
  dplyr::select(lon,lat,
                MAP.CV,MCWD.CV,MAT.CV,Etot.CV,
                MAP.m,MCWD.m,MAT.m,Etot.m,
                MAP.sd,MCWD.sd,MAT.sd,Etot.sd) %>%
  pivot_longer(cols = -c(lon,lat),
               names_to = "var",
               values_to = "value") %>%
  mutate(variable = case_when(grepl("CV",var) ~ "CV",
                              grepl("sd",var) ~ "sd",
                              TRUE ~ "mean")) %>%
  mutate(var = sub("\\..*", "", var))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df.all.sum,
              aes(x = lon, y = lat,
                  fill = MAP.sd)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient(low = "white",
                      high = "darkred",
                      limits = c(0,1000),
                      oob = scales::squish) +
  coord_sf(xlim = c(-120, 160),
           ylim = c(-1, 1)*23.25, expand = FALSE) +

  theme_bw() +
  theme(legend.position = "bottom")

ggplot() +
  geom_raster(data = df.all.sum.long %>%
                filter(variable == "CV"),
              aes(x = lon, y = lat,
                  fill = pmin(100,value*100))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient(low = "white",
                      high = "darkred",
                      limits = c(0,100),
                      oob = scales::squish) +
  coord_sf(xlim = c(-120, 160),
           ylim = c(-1, 1)*23.25, expand = FALSE) +
  facet_wrap(~ var,
             ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data = df.all.sum.long %>%
         filter(variable == "CV")) +
  geom_density(aes(x = 100*value,
                   fill = var),
               alpha = 0.5) +
  scale_x_continuous(limits = c(0,150)) +
  theme_bw()

ggplot(data = df.all.sum.long %>%
         filter(variable %in% c("sd","mean"))) +
  geom_density(aes(x = abs(value),
                   fill = variable),
               alpha = 0.5) +
  # scale_x_continuous(limits = c(0,150)) +
  facet_wrap(~ var, scales = "free",
             nrow = 1) +
  theme_bw() +
  theme(legend.position = "bottom")



