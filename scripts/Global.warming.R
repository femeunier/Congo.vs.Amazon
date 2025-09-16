rm(list = ls())

library(epwshiftr)
library(dplyr)
library(CongoAS)
library(RNetCDF)
library(stringr)
library(ncdf4)
library(ncdf4.helpers)
library(reshape2)
library(lubridate)
library(tidyr)

scenarios = c("historical","ssp126","ssp245","ssp370","ssp585",
              "ssp534-over")
variables = c("tas")
variants = "r1i1p1f1" # NULL
main.dir <- c("/data/gent/vo/000/gvo00074/felicien/CMIP6")
year.min <- 1900 ; year.max <- 2100
variants <- c("r1i1p1f1")

############################################################################


overwrite <- FALSE

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

    all.dates <- strsplit(cdates,split = "\\-")
    init.years <- as.numeric(substr(as.vector(unlist(purrr::map_dfr(1:length(all.dates),
                                                               function(i){
                                                                 data.frame(var = all.dates[[i]][1])}))),1,4))
    end.years <- as.numeric(substr(as.vector(unlist(purrr::map_dfr(1:length(all.dates),
                                                                   function(i){
                                                                     data.frame(var = all.dates[[i]][2])}))),1,4))

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

        op.file <- paste0("./outputs/","Global.warming.",cscenario,".",cmodel,".",cvariant,".RDS")

        if (!overwrite & file.exists(op.file)){
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

          if (nrow(tmp) ==0){
            next()
          }

          coord <- tmp %>%
            ungroup() %>%
            filter(year == year[1],
                   month == month[1])

          Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                            lat = as.vector(unique(coord$lat))) %>%
            melt() %>%
            mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
                   Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
            rename(lon = Var1,
                   lat = Var2) %>%
            mutate(lon = round(100*lon)/100,
                   lat = round(100*lat)/100)

          tmp.GW <- tmp %>%
            mutate(lon = round(100*lon)/100,
                   lat = round(100*lat)/100) %>%
            left_join(Gridarea %>%
                        rename(area = value),
                      by = c("lat","lon")) %>%
            group_by(year) %>%
            summarise(tas = weighted.mean(tas,area,na.rm = TRUE),
                      .groups = "keep")

          df.GW <- tmp.GW %>%
            mutate(model = cmodel,
                   variant = cvariant,
                   cscenario = scenarios[iscenario])

          saveRDS(df.GW,
                  op.file)

        }
      }
    }
  }
}

# saveRDS(df.GW,
#         "./outputs/Global.warming.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Global.warming.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
