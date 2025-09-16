rm(list = ls())

library(sf)
library(ggthemes)
IFL <- read_sf(dsn = "/home/femeunier/Documents/projects/Congo.ED2/data/IFL/",
                      layer = "ifl_2020")
IFL$type = 1
N = 1
r <- raster(ncol=720*N, nrow=360*N)
extent(r) <- extent(-180,
                    180,
                    -90,90)

rp <- rasterize(IFL, r,
                'type',
                fun = mean)
plot(rp)

rp.df <- as.data.frame(rp,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         is.undisturbed = layer) %>%
  mutate(is.undisturbed.factor = case_when(is.na(is.undisturbed) ~ 0,
                                           is.undisturbed < 0.5 ~ 0,
                                           is.undisturbed >= 0.5 ~1))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")


ggplot(data = rp.df %>%
         filter(is.undisturbed.factor == 1)) +
  geom_tile(aes(x=lon,y = lat,
                fill = is.undisturbed.factor),alpha = 1) +
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
