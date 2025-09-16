rm(list = ls())

library(dplyr)
library(TrENDY.analyses)
library(raster)

coord <- expand.grid(lon = seq(-179.75,179.75,0.5),
                     lat = seq(-30.25,30.25,0.5))
rast <- rasterFromXYZ(coord %>%
                        mutate(value = 1))

Dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/"

files <- list.files(Dir,
                    pattern = "Trendy.*.cAGB.pantropical.v13.RDS",
                    full.names = TRUE)
# files <- files[!grepl("rspld",files)]
overwrite = TRUE

for (cfile in files){

  cmodel <- strsplit(basename(cfile),"\\.")[[1]][2]

  print(cmodel)

  OPfile <- file.path(Dir,
                      paste0(tools::file_path_sans_ext(basename(cfile)),".rspld.RDS"))

  if (!overwrite & file.exists(OPfile)){
    next()
  }

  A <- readRDS(cfile)
  colNames <- colnames(A)

  A.rspld <- TrENDY.analyses::resample.df.all.col(bigdf = A,
                                                  raster2resample = rast,
                                                  var.names = colNames[!(colNames %in% c("year","month","lat","lon"))])


  saveRDS(A.rspld %>%
            filter(!is.na(cVeg) | !is.na(cRoot)),
          OPfile)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Resample.all.vars.cAGB.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
