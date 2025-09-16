rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(sf)
library(ggthemes)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")

ILF <- bind_rows(
  readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Amazon.coord.ILF.RDS"),
  readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Congo.coord.ILF.RDS")) %>%
  filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat,digits = 2))) %>%
  mutate(basin = case_when(lon < 0 ~ "Amazon",
                           TRUE ~ "Congo"))

ILF %>%
  group_by(basin,model) %>%
  summarise(N = n())

ggplot(data = ILF %>%
         filter(model == "ORCHIDEE")) +
  geom_tile(aes(x=lon,y = lat,
                fill = 1),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  geom_sf(data = Congo.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, 45), ylim = c(-25, 10), expand = FALSE) +
  labs(x = "",y = "") +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


Trendy.data.rspld <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Trendy.data.rspld.pred.RDS") %>%
  mutate(basin = case_when(lon < 0 ~ "Amazon",
                           TRUE ~ "Congo")) %>%
  ungroup() %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat,digits = 2))) %>%
  filter(lon.lat %in% ILF[["lon.lat"]]) %>%
  dplyr::select(-lon.lat)

Trendy.data.rspld.map <- Trendy.data.rspld %>%
  filter(year == 2023) %>%
  group_by(lat,lon,basin) %>%
  summarise(MEM = mean(pred,na.rm = TRUE),
            .groups = "keep")

ggplot(data = Trendy.data.rspld.map) +
  geom_tile(aes(x=lon,y = lat,
                fill = MEM),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  geom_sf(data = Congo.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, 45), ylim = c(-25, 10), expand = FALSE) +
  scale_fill_gradient(limits = c(0,5),
                       oob = scales::squish,
                       low = "grey",high = "darkgreen") +
  labs(x = "",y = "") +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

ggplot(data = Trendy.data.rspld.map) +
  geom_density(aes(x = MEM,
                   fill = basin),
               alpha = 0.5, color = NA) +
  theme_bw()

Trendy.data.rspld.sum <- Trendy.data.rspld %>%
  group_by(basin,model,year,month) %>%
  summarise(pred.m = mean(pred),
            .groups = "keep")

Trendy.data.MEM <- Trendy.data.rspld.sum %>%
  group_by(basin,year,month) %>%
  summarise(pred.m = mean(pred.m),
            .groups = "keep")

Trendy.data.rspld %>%
  group_by(basin) %>%
  filter(year == year[1],
         month == month[1],
         model == "CLASSIC") %>%
  summarise(N = n())


ggplot(data = Trendy.data.MEM %>%
         filter(year >= 1994),
       aes(x = year + (month - 1/2)/12,
           y = pred.m,
           color = basin)) +
  geom_line(size = 0.25) +
  stat_smooth(method = "lm",linetype = 2,
              se = FALSE) +
  theme_bw()

SC <- Trendy.data.MEM %>%
  filter(year >= 1994) %>%
  group_by(basin,month) %>%
  summarise(pred.m = mean(pred.m),
            .groups = "keep")


ggplot(data = SC ,
       aes(x = month,
           y = pred.m,
           color = basin)) +
  geom_line() +
  geom_line(data = Trendy.data.MEM %>%
              filter(year == 2023),
            size = 0.25) +
  theme_bw()


YC <- Trendy.data.MEM %>%
  filter(year >= 1994) %>%
  group_by(basin,year) %>%
  summarise(pred.m = mean(pred.m),
            .groups = "keep")
Window = 12
Diff <- Trendy.data.MEM %>%
  ungroup() %>%
  pivot_wider(names_from = basin,
              values_from = pred.m) %>%
  mutate(diff = Amazon - Congo,
         time = year + (month - 1/2)/12) %>%
  mutate(diff.rm = rollapply(diff, width=Window,
                              FUN=function(x) mean(x, na.rm=TRUE),
                              partial=TRUE, fill=NA, align="center"))


ggplot(data = Diff) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(x = time,
                 y = diff),
            color = "grey") +
  geom_line(aes(x = time,
                y = diff.rm),
            color = "red") +
  stat_smooth(aes(x = time,
                  y = diff),
              method = "lm", color = "black", se = FALSE) +
  theme_bw()

summary(lm(data = Diff,
           formula = diff.rm ~ year))
