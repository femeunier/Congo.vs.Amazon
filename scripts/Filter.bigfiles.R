rm(list = ls())

library(dplyr)

coord.list <- bind_rows(readRDS("./outputs/Amazon.coord.ILF.v12.RDS"),
                        readRDS("./outputs/Congo.coord.ILF.v12.RDS")) %>%
  mutate(model.lon.lat =
           paste0(model,".",round(lon,digits = 2),".",round(lat,digits = 2)))

models <- TrENDY.analyses::get.model.names.TRENDY(version = "v12")

for (cmodel in models){

  print(cmodel)

  grid.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",cmodel,".ERA5.v12.RDS")
  model.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",cmodel,".S2.CC.global.v12.RDS")

  if (file.exists(grid.file)){
    cgrid <- readRDS(grid.file) %>%
      mutate(model.lon.lat =
               paste0(cmodel,".",round(lon,digits = 2),".",round(lat,digits = 2))) %>%
      filter(model.lon.lat %in% (coord.list %>%
                                   filter(model == cmodel) %>%
                                   pull(model.lon.lat)))
    mod.grid.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",cmodel,".ERA5.v12.IFL.RDS")

    saveRDS(cgrid,
            mod.grid.file)
  }

  if (file.exists(model.file)){
  cmodel.op <- readRDS(model.file) %>%
    mutate(model.lon.lat =
             paste0(cmodel,".",round(lon,digits = 2),".",round(lat,digits = 2))) %>%
    filter(model.lon.lat %in% (coord.list %>%
                                 filter(model == cmodel) %>%
                                 pull(model.lon.lat)))

  mod.model.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",cmodel,".S2.CC.global.v12.IFL.RDS")

  saveRDS(cmodel.op,
          mod.model.file)

  }
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Filter.bigfiles.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

