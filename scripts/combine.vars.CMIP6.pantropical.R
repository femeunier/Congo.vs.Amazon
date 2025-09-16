rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(ggthemes)
library(TrENDY.analyses)
library(raster)

biomes <- readRDS("./outputs/biome.ERA5.1940.2023_global.RDS") %>%
  filter(model == "ORCHIDEE")

grid <- rasterFromXYZ((biomes %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","MAT")))[,c("lon","lat","MAT")])

variables <- c("tas","tasmin","tasmax","pr")
scenarios <- c("ssp534-over")
variants = "r1i1p1f1" # NULL

overwrite <- FALSE

for (variable in variables){

  l.files <- list.files("./outputs/",pattern = paste0("CMIP6.monthly.",variable,".pantropical*"))
  l.files <- l.files[!grepl("rspld",l.files)]
  OP.files.no.ext <- tools::file_path_sans_ext((l.files))
  all.attributes <- strsplit(OP.files.no.ext,split = "\\.")

  cscenars <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                           function(i){
                                                             data.frame(var = all.attributes[[i]][5])}))))
  cmodels <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                          function(i){
                                                            data.frame(var = all.attributes[[i]][6])}))))

  cvariants <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                            function(i){
                                                              data.frame(var = all.attributes[[i]][7])}))))

  data.sum <-
    data.frame()

  for (ifile in seq(1,length(l.files))){

    print(ifile/length(l.files))
    cscenario <- cscenars[ifile] ; cvariant = cvariants[ifile] ; cmodel = cmodels[ifile]

    if (!(cvariant %in% variants)){
      next()
    }

    if (!(cscenario %in% scenarios)){
      next()
    }

    cfile <- paste0("./outputs/",l.files[ifile])

    print(cfile)

    cfile.op <- paste0("./outputs/",
                       tools::file_path_sans_ext(l.files[ifile]),"_rspld.RDS")

    if (!overwrite & file.exists(cfile.op)){
      next()
    }

    cdata <- readRDS(paste0("./outputs/",l.files[ifile])) %>%
      mutate(model = cmodel)

    cdata.rspld <- data.frame()

    for (cyear in sort(unique(cdata$year))){
      # print(paste0("- ",cyear))
      cdata.rspld <-
        bind_rows(cdata.rspld,
                  resample.df.all.col(bigdf = cdata %>%
                                        filter(year == cyear) %>%
                                        dplyr::select(-any_of("day")),

                                      raster2resample = grid,
                                      var.names = c(variable),
                                      res = 0.00001,
                                      verbose = FALSE) %>%
                    rename(value = !!variable) %>%
                    filter(!is.na(value)) %>%
                    mutate(var = variable))
    }

    saveRDS(cdata.rspld,
            cfile.op)
  }
}

# scp /home/femeunier/Documents/projects/AI.Fluxnet/scripts/combine.vars.CMIP6.pantropical.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


