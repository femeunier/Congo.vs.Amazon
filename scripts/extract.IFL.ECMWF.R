rm(list = ls())

library(sf)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(rgeos)

ILF.df  <- readRDS("~/Documents/projects/Congo.ED2//outputs/ILF2020.df")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")
Congo <- as_Spatial(Congo.shp)

ggplot() +

  geom_tile(data = ILF.df,
            aes(x = lon, y = lat,
                fill = as.factor(is.undisturbed.factor)), alpha = 0.5,
            linewidth = 0.5, color = "black",
            show.legend = FALSE) +

  geom_sf(data = world,
          fill = NA, color = "black") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black", fill = NA) +

  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-90, -30),
           ylim = c(-25, 12)) +
  labs(x = "",y = "") +
  # facet_wrap(~ model) +
  theme_map() +
  labs(fill = "") +
  scale_fill_manual(values = c("grey","white")) +
  theme(text = element_text(size = 20),
        legend.position = "top")

################################################################################

climate <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/monthly.climate.pantropical.Tropics.ECMWF.RDS") %>%
  mutate(source = "ECMWF")
models <- sort(unique(climate$source))

all.coord <- all.coord2 <-
  data.frame()

for (cmodel in models){

  print(cmodel)

  cclimate <- climate %>%
    dplyr::filter(source == cmodel) %>%
    ungroup()

  # grid <- rasterFromXYZ((cclimate %>%
  #                          ungroup() %>%
  #                          dplyr::select(c("lat","lon","tmp")))[,c("lon","lat","tmp")])


  grid <- raster(suppressWarnings(SpatialPixelsDataFrame(points = cclimate[c("lon","lat")],
                                                         data = cclimate["tmp"],
                                                         tolerance = 0.0357143 )))

  cILF <- resample.df.all.col(bigdf = ILF.df %>%
                                mutate(is.undisturbed = case_when(is.na(is.undisturbed) ~ 0,
                                                                  TRUE ~ is.undisturbed)) %>%
                                dplyr::select(lat,lon,is.undisturbed) %>%
                                mutate(model = "ILF"),

                              raster2resample = grid,
                              var.names = c("is.undisturbed"),
                              res = 0.0357143) %>%
    dplyr::select(lon,lat,is.undisturbed) %>%
    mutate(is.undisturbed.factor = case_when(is.na(is.undisturbed) ~ 0,
                                             is.undisturbed < 0.5 ~ 0,
                                             is.undisturbed >= 0.5 ~1))

  test <- cclimate %>%
    dplyr::filter(lon <= -30, lon >= -90,
                  lat <= 10, lat >= -25)

  test2 <-cclimate %>%
    dplyr::filter(source == cmodel) %>%
    dplyr::filter(lon >= 4, lon <= 40,
                  lat <= 12, lat >= -15)

  sp <- SpatialPoints(test[,c("lon","lat")])
  sp2 <- SpatialPoints(test2[,c("lon","lat")])

  out = as.data.frame(gIntersection(sp,Amazon))
  out2 = as.data.frame(gIntersection(sp2,Congo))

  all.coord <- bind_rows(all.coord,
                         data.frame(model = cmodel,
                                    lon = round(out$x,digits = 2),
                                    lat = round(out$y,digits = 2)) %>%
                           left_join(cILF %>%
                                       mutate( lon = round(lon,digits = 2),
                                               lat = round(lat,digits = 2)),
                                     by = c("lat","lon")) %>%
                           dplyr::filter(is.undisturbed.factor == 1) %>%
                           mutate(model.lon.lat =
                                    paste0(cmodel,".",lon,".",lat)))

  all.coord2 <- bind_rows(all.coord2,
                          data.frame(model = cmodel,
                                     lon = round(out2$x,digits = 2),
                                     lat = round(out2$y,digits = 2)) %>%
                            left_join(cILF %>%
                                        mutate( lon = round(lon,digits = 2),
                                                lat = round(lat,digits = 2)),
                                      by = c("lat","lon")) %>%
                            dplyr::filter(is.undisturbed.factor == 1) %>%
                            mutate(model.lon.lat =
                                     paste0(cmodel,".",lon,".",lat)))
}

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = ILF.df,
              aes(x = lon, y = lat,
                  fill = as.factor(is.undisturbed.factor)), alpha = 1,
              linewidth = 0.5,
              show.legend = FALSE) +
  geom_point(data = all.coord2 %>%
               filter(model == "ECMWF"),
             aes(x = lon, y = lat), shape = "+") +

  geom_point(data = all.coord %>%
               filter(model == "ECMWF"),
             aes(x = lon, y = lat), shape = "+", color = "red") +

  geom_sf(data = world,
          fill = NA) +
  geom_sf(data = Amazon.shp,fill = NA, color = "red", fill = NA) +
  geom_sf(data = Congo.shp,fill = NA, color = "red", fill = NA) +

  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-100, -40),
           ylim = c(-25, 25)) +
  labs(x = "",y = "") +
  # facet_wrap(~ model) +
  theme_map() +
  labs(fill = "") +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme(text = element_text(size = 20),
        legend.position = "top")

saveRDS(all.coord,
        "./outputs/Amazon.coord.ILF.ECWMF.RDS")
saveRDS(all.coord2,
        "./outputs/Congo.coord.ILF.ECWMF.RDS")


# system2("scp",
#         c("./outputs/Amazon.coord.ILF.ECWMF.RDS",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
#
# system2("scp",
#         c("./outputs/Congo.coord.ILF.ECWMF.RDS",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

