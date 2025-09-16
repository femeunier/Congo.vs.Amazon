rm(list = ls())

# system2("scp",
#         c("./outputs/df.LC.ESA.class.RDS",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

library(terra)
library(ggplot2)
library(raster)
library(dplyr)
library(ggthemes)

A <- readRDS("./outputs/df.LC.ESA.class.RDS")
A.select <- A %>%
  filter(new == 1) %>%
  mutate(hemisph = case_when(lat < 0 ~ "S",
                             TRUE ~ "N"))

ggplot(data = A.select) +
  geom_bar(aes(x = veg.type, fill = veg.type)) +
  facet_wrap(~ hemisph) +
  theme_bw()

df <- A.select %>%
  group_by(hemisph, veg.type) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  arrange(hemisph,desc(N))

df2raster <- A
  # mutate(veg.type.NA = case_when(new == 1 ~ veg.type,
  #                                TRUE ~ NA_character_)) %>%
  # dplyr::select(lon,lat,veg.type.NA) %>%
  # filter(!is.na(veg.type.NA))

cat2num <- df2raster %>%
  dplyr::select(num,veg.type,new) %>%
  distinct()

cr <- raster(SpatialPixelsDataFrame(points = df2raster[c("lon","lat")],
                                    data = df2raster["num"]))


df.cr <- as.data.frame(cr,
                           xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  left_join(cat2num,
            by = "num")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggpl <- ggplot() +
  geom_raster(data = df.cr %>%
                filter(new == 1),
              aes(x = lon,
                  y = lat,
                  fill = as.factor(veg.type))) +
  geom_segment(data = data.frame(x = -5, xend = 50,
                                 y = 0, yend = 0),
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linetype = 2, color = "black") +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  theme(text = element_text(size = 20))
  labs(x = "",y = "", fill = "") +
  guides(fill = guide_legend(nrow = 4)) +
  theme(legend.position = "bottom")


ggsave("./Figures/LC_centraAfrica_savannas.png",
       ggpl,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 300)


df <-df.cr %>%
  filter(new == 1)

df_props <- df %>%
  filter(!is.na(veg.type)) %>%
  mutate(hemisphere = ifelse(lat >= 0, "Northern", "Southern")) %>%
  group_by(hemisphere, veg.type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(hemisphere) %>%
  mutate(prop = count / sum(count))  # relative proportion within hemisphere
df_counts <- df %>%
  mutate(hemisphere = ifelse(lat >= 0, "Northern", "Southern")) %>%
  filter(!is.na(veg.type)) %>%  # Remove rows with missing veg.type
  group_by(hemisphere, veg.type) %>%
  summarise(count = n(), .groups = "drop")

pie <- ggplot(df_props, aes(x = "", y = prop, fill = veg.type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~ hemisphere) +
  theme_void() +
  labs(fill = "Vegetation Type")


ggsave("./Figures/LC_centraAfrica_savannas_pie.png",
       pie,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 300)



# system2("scp",
#         c("hpc:/data/gent/vo/000/gvo00074/felicien/R/Figures/LC_centraAfrica_savannas.png",
#           "./Figures"))

cr.agg <- aggregate(cr,
                    0.5/res(cr),
                    modal,
                    na.rm = TRUE)

plot(cr.agg)

df.cr.agg <- as.data.frame(cr.agg,
                           xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  left_join(cat2num,
            by = "num")

ggplot() +
  geom_raster(data = df,
              aes(x = lon,
                  y = lat,
                  fill = as.factor(veg.type))) +
  geom_segment(data = data.frame(x = -5, xend = 50,
                          y = 0, yend = 0),
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linetype = 2, color = "black") +
  geom_sf(data = world,fill = NA, color = "grey17") +
  coord_sf(xlim = c(-15, 60), ylim = c(-15, 10), expand = FALSE) +
  theme_map() +
  labs(x = "",y = "", fill = "") +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Figure_landcover_savannas.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

