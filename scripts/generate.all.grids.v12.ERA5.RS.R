rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(rgdal)
library(YGB)
library(TrENDY.analyses)
library(stringr)
library(randomForest)
library(ggpointdensity)

df.all <- readRDS("./data/GPP.products.RDS")

Tropics.sum <- readRDS("./outputs/monthly.climate.global.ERA5.RDS")
CN <- colnames(Tropics.sum)
Var.names <- CN[!(CN %in% c("lat","lon","year","month"))]

products <- unique(df.all$product)
products <- c("SIF","VOD","TwoLeaf","NIR")
for (cproduct in products){

  print(cproduct)

  cdf <- df.all %>%
    filter(product == cproduct) %>%
    filter(month == month[1],
           year == year[1])

  OPfile <- paste0("./data/grid.",cproduct,".ERA5.v12.RDS")

  cvar <- "value"
  years <- unique(Tropics.sum$year)

  cgrid <- data.frame()
  for(cyear in years){

    print(cyear)
    test <- resample.df.all.col(bigdf = Tropics.sum %>%
                                  filter(year == cyear) %>%
                                  mutate(model = "ERA5"),

                                raster2resample = rasterFromXYZ((cdf %>%
                                                                   ungroup() %>%
                                                                   filter(year == year[1],
                                                                          month == month[1]) %>%
                                                                   dplyr::select(c("lat","lon",cvar)))[,c("lon","lat",cvar)]),
                                var.names = Var.names,
                                NULL)
    cgrid <- bind_rows(cgrid,
                       test %>%
                         mutate(product = cproduct,
                                year = cyear))

  }

  saveRDS(cgrid,
          OPfile)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/generate.all.grids.v12.ERA5.RS.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
