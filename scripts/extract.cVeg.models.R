rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(raster)
library(terra)
library(TrENDY.analyses)

# system2("scp",c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.*.cC.v11.recent.RDS",
#                 "./outputs/"))

models <- get.model.names.TRENDY(version = "v11")

scenarios <- c("S2")
vars <- c("cVeg","cSoil")
years <- 2003:2020

all <- data.frame()

for (cvar in vars){
for (cmodel in models){
  for (cscenario in scenarios){

    cfile <- paste0("./outputs/Trendy.Africa.",
                    cscenario,".",
                    cmodel,".cC.v11.recent.RDS")

    if (!file.exists(cfile)) next()

    A <- readRDS(cfile)

    print(c(min(A$year),max(A$year)))

    A.sum <- A %>%
      mutate(year = round(year)) %>%
      filter(year %in% round(years)) %>%
      group_by(lon,lat) %>%
      summarise(value.m = mean(get(cvar),na.rm = TRUE),
                .groups = "keep") %>%
      filter(!is.na(value.m))

    all <- bind_rows(all,
                     A.sum %>%
                       mutate(scenario = cscenario,
                              var = cvar))
    #
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    #
    # ggplot(data = A.sum) +
    #   geom_raster(aes(x=lon,y = lat,
    #                   fill = value.m),alpha = 1) +
    #   geom_sf(data = world,fill = NA, color = "grey") +
    #   labs(x = "",y = "") +
    #   scale_x_continuous(limits = c(-30,60)) +
    #   scale_y_continuous(limits = c(-35,35)) +
    #   theme_map()
    #
    # ggplot(A.sum) +
    #   geom_density(aes(x = value.m)) +
    #   theme_bw()

    # cdf <- A
    # cr <- tryCatch(rasterFromXYZ(cdf %>%
    #                                ungroup() %>%
    #                                dplyr::select(lon,lat,value.m)),
    #                error = function(e) NULL)

    r <- stack()
    for (cyear in years){

      print(paste(cvar,"-",cmodel," - ",cscenario," - ",cvar," - ",cyear))


      cdf <- A %>%
        filter(year == cyear) %>%
        filter(lon >= -35, lon <= 60) %>%
        filter(lat <= 30,lat >= -50)

      if ((nrow(cdf) == 0)) next()

      cr <- tryCatch(rasterFromXYZ(cdf %>%
                                     ungroup() %>%
                                     dplyr::select(lon,lat,!!cvar)),
                     error = function(e) NULL)

      if (is.null(cr)){
        cr <- raster(suppressWarnings(SpatialPixelsDataFrame(points = cdf[c("lon","lat")],
                                                             data = cdf[cvar],
                                                             tolerance = 1e-5)))

      }
      r <- stack(r, cr)
    }

    if (length(r) == 0) next()
    names(r) <- paste0(cvar,".",as.character(years))

    terra::writeRaster(rast(r),
                       filename= paste0("./outputs/",cmodel,".",cscenario,".",cvar,".raster.tif"),
                       overwrite=TRUE)

  }
}
}
ggplot(all) +
  geom_density(aes(x = value.m,
                   fill = scenario), alpha = 0.5) +
  theme_bw()

all.wide <- all %>%
  pivot_wider(names_from = scenario,
              values_from = value.m) %>%
  mutate(diff = S3-S2)

ggplot(data = all.wide) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-30,60)) +
  scale_y_continuous(limits = c(-35,35)) +
  scale_fill_gradient2(low = "darkred",
                       high = "darkgreen") +
  theme_map()

# system2("scp",c("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/rasters.zip",
#                 "./outputs/"))
# A <- stack("./outputs/JSBACH.S2.gpp.tif")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/convertdf2raster.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/extract.cVeg.models.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
