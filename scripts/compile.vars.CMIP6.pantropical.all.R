rm(list = ls())

library(epwshiftr)
library(dplyr)
library(RNetCDF)
library(stringr)
library(ncdf4)
library(ncdf4.helpers)
library(reshape2)
library(lubridate)
library(CongoAS)
library(tidyr)

scenarios <- c("ssp534-over","ssp585")
variables <- c("pr","tas","tasmin","tasmax")
main.dir <- c("/data/gent/vo/000/gvo00074/felicien/CMIP6")
year.min <- 1900 ; year.max <- 2100
variants <- c("r1i1p1f1")

overwrite <- TRUE

for (iscenario in seq(1,length(scenarios))){
  cscenario <- scenarios[iscenario]
  for (ivariable in seq(1,length(variables))){
    cvariable <- variables[ivariable]

    cdir <- file.path(main.dir,cscenario,cvariable)

    l.files <- list.files(cdir,pattern = ".nc$")
    OP.files.no.ext <- tools::file_path_sans_ext((l.files))
    all.attributes <- strsplit(OP.files.no.ext,split = "\\_")


    cmodels <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                             function(i){
                                                               data.frame(var = all.attributes[[i]][3])}))))
    cvariants <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                            function(i){
                                                              data.frame(var = all.attributes[[i]][5])}))))
    cdates <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                              function(i){
                                                                data.frame(var = all.attributes[[i]][7])}))))

    init.years <- as.numeric(substr(cdates,start = 1,4))
    end.years <- as.numeric(substr(cdates,start = 8,11))

    call <- data.frame(file = l.files,
                       variable = cvariable,
                       scenario = cscenario,
                       model = cmodels,
                       variant = cvariants,
                       init.year = init.years,
                       end.year = end.years) %>%
      filter(variant %in% variants) %>%
      filter(init.year <= year.max)

    models <- sort(unique(call %>% pull(model)))

    for (imodel in seq(1,length(models))){

      cmodel <- models[imodel]
      cdf <- call %>%
        filter(model == cmodel)
      cvariants <- cdf %>% pull(variant) %>% unique() %>% sort()

      for (ivariant in seq(1,length(cvariants))){

        cvariant <- cvariants[ivariant]
        print(paste0(cscenario," - ", cvariable," - ",
                     cmodel," - ",cvariant))

        ccdf <- cdf %>%
          filter(variant == cvariant)

        OP.file <- paste0("./outputs/","CMIP6.monthly.",cvariable,".pantropical.",cscenario,".",cmodel,".",cvariant,".RDS")

        if (!overwrite & file.exists(OP.file)){
          next()
        }

        dest.files <- file.path(cdir,
                                ccdf %>% pull(file))

        if (all(file.exists(dest.files))){

          tmp <- tryCatch(read.and.filter.ncfiles(ncfiles = dest.files,
                                                  coord.analysis = continent2coord("World"),
                                                  var = cvariable,
                                                  aggr = FALSE) %>%
                            filter(year %in% 1900:2100) %>%
                            filter(abs(lat) <= 30) %>%
                            dplyr::select(-time0),
                          error = function(e) NULL)

          if(is.null(tmp)) next()

          if (nrow(tmp) == 0) next()

          df.month <- tmp %>%
            mutate(scenario = cscenario,
                   variant = cvariant)

          saveRDS(df.month,
                  OP.file)
        }
      }
    }
  }
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/compile.vars.CMIP6.pantropical.all.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


