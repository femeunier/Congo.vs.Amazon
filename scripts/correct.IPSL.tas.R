system2("rsync",
        paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/df.monthly.tas.pantropical.historical.IPSL-CM6A-LR.RDS",
        " ",
        "./outputs/"
        ))


A <- readRDS("./outputs/df.monthly.tas.pantropical.historical.IPSL-CM6A-LR.RDS")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")
A.select <- A %>%
  filter(tas < 500)
ggplot() +
  geom_point(data = A.select %>%
              filter(year == 1900,
                     month == 1),
            aes(x = lon, y = lat,
                fill = tas)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +
  coord_sf(xlim = c(-90, 55), ylim = c(-30, 20), expand = FALSE) +
  theme_bw()
