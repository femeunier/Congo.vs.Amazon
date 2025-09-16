rm(list = ls())

library(raster)
library(ggplot2)
library(tidyr)
library(ggthemes)

files <- list.files("~/Documents/projects/Precip.Africa/data/Anomalies_2023/",
                    full.names = TRUE)
file.names <- basename(files)

all.attributes <- strsplit(file.names,split = "\\.")
products <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                        function(i){
                                                          data.frame(var = all.attributes[[i]][2])}))))
types <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                         function(i){
                                                           data.frame(var = all.attributes[[i]][7])}))))
df <- data.frame(product = products,
                 type = types,
                 file = files)

df.select <- df %>%
  dplyr::select(-file) %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = type,
              values_from = present,values_fill = FALSE) %>%
  filter(dry & hot)


final <- df %>%
  filter(product %in% df.select$product)


df.all <- data.frame()

for (i in seq(1,nrow(final))){

  print(i/nrow(final))

  cr <- raster(final$file[i])
  names(cr) <- "sigma"
  cdf <- as.data.frame(cr,
                       xy = TRUE) %>%
    rename(lon = x,
           lat = y) %>%
    filter(!is.na(sigma))

  df.all <- bind_rows(df.all,
                      cdf %>%
                        mutate(type = final$type[i],
                               product = final$product[i]))

}

df.all.wide <- df.all %>%
  pivot_wider(names_from = type,
              values_from = sigma) %>%
  mutate(dry_and_hot = case_when(dry > 1 & hot > 1 ~ 4,
                                 dry > 1 | hot > 1 ~ 3,
                                 dry > 0 & hot > 0 ~ 2,
                                 dry > 0 | hot > 0 ~ 1,
                                 TRUE ~ 0),
         dry_and_hot2 = case_when(dry > 1 & hot > 1 ~ 3,
                                 dry > 0 & hot > 0 ~ 2,
                                 dry > 0 | hot > 0 ~ 1,
                                 TRUE ~ 0))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df.all.wide,
              aes(x = lon, y = lat,
                  fill = as.factor(dry_and_hot))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 60),
           ylim = c(-1, 1)*23.25,
           expand = FALSE) +
  theme_map() +
  facet_wrap(~ product, ncol = 1) +
  scale_fill_brewer(palette = "Reds") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank())


ggplot() +
  geom_raster(data = df.all.wide,
              aes(x = lon, y = lat,
                  fill = as.factor(dry_and_hot2))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 60),
           ylim = c(-1, 1)*23.25,
           expand = FALSE) +
  theme_map() +
  facet_wrap(~ product, ncol = 1) +
  scale_fill_brewer(palette = "Reds") +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        strip.text = element_blank(),
        strip.background = element_blank())

df.all.wide.MEM <- df.all.wide %>%
  filter(dry_and_hot == 2) %>%
  group_by(lon,lat) %>%
  summarise(N = length(unique(product)),
            .groups = "keep")

ggplot() +
  geom_raster(data = df.all.wide.MEM,
              aes(x = lon, y = lat,
                  fill = as.factor(1))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 60),
           ylim = c(-1, 1)*23.25,
           expand = FALSE) +
  scale_fill_manual(values = c("darkred")) +
  theme_map() +
  theme(legend.position = "bottom",
        text = element_text(size = 20))


