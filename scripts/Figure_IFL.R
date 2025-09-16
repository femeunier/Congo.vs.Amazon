rm(list = ls())

IFL <- readRDS("./outputs/Amazon.ERA5.IFL.RDS")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "congo_basin_polyline")

ggplot() +
  geom_raster(data = IFL %>%
                filter(model == "ORCHIDEE"),
              aes(x = lon, y = lat,
                  fill = as.factor(1)),show.legend = FALSE) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  scale_fill_manual(values = c("darkgreen")) +
  scale_y_continuous(limits = c(-23.5,10)) +
  scale_x_continuous(limits = c(-90,-30),expand = c(0,0)) +
  scale_size_continuous(range = c(0.1, 2)) +
  labs(x = "", y = "") +
  theme_map() +
  guides(size = "none") +
  theme(text = element_text(size = 20))
