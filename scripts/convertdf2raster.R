rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(raster)
library(terra)
library(TrENDY.analyses)

# system2("scp",
#         c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.CLM5.0.S2.CC.pantropical.v11.RDS",
#                 "./outputs/"))

models <- get.model.names.TRENDY(version = "v11")

scenarios <- c("S2","S3")
vars <- c("gpp","nbp","nep","npp","ra","rh")
years <- 2000:2021

for (cmodel in models){
  for (cscenario in scenarios){

    cfile <- paste0("./outputs/Trendy.",
                    cmodel,".",
                    cscenario,".CC.pantropical.v11.update.RDS")

    if (!file.exists(cfile)) next()

    A <- readRDS(cfile) %>%
      filter(year >= 2000)

    A.sum <- A %>%
      pivot_longer(cols = any_of(c("gpp","npp","rh","nbp","ra","nep")),
                   names_to = "variable",
                   values_to = "value") %>%
      group_by(year,variable,lon,lat) %>%
      summarise(value.m = mean(value,na.rm = TRUE)*86400*365,
                .groups = "keep") %>%
      filter(!is.na(value.m))

    # world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    #
    # ggplot(data = A %>%
    #          filter(year == 2000)) +
    #   geom_raster(aes(x=lon,y = lat,
    #                   fill = gpp),alpha = 1) +
    #   geom_sf(data = world,fill = NA, color = "grey") +
    #   labs(x = "",y = "") +
    #   # scale_x_continuous(limits = c(-30,60)) +
    #   scale_y_continuous(limits = c(-35,35)) +
    #   # facet_wrap(~ variable) +
    #   theme_map()

    A.sum.wide <- A.sum %>%
      pivot_wider(names_from = "variable",
                  values_from = value.m)

    for (cvar in vars){
      r <- stack()
      for (cyear in years){

        print(paste(cmodel," - ",cscenario," - ",cvar," - ",cyear))

        if (!(cvar %in% colnames(A.sum.wide))) next()

        cdf <- A.sum.wide %>%
          filter(year == cyear) %>%
          dplyr::select(year,lat,lon,!!cvar)

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

# rsync -avz hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/*.raster.tif .

# system2("scp",c("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/rasters.zip",
#                 "./outputs/"))
# A <- stack("./outputs/JSBACH.S2.gpp.tif")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/convertdf2raster.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
