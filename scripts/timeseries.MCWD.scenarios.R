rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(YGB)

vars <- c("pr","tas","tasmin","tasmax")
variants <- "r1i1p1f1"
overwrite <- TRUE

scenarios = c("historical",
              "ssp126", "ssp245","ssp370","ssp585")

df.files <- data.frame()
for (var in vars){

  l.files.tas <- list.files("./outputs",pattern = paste0("CMIP6.monthly.",var,".pantropical*"))
  l.files.tas <- l.files.tas[grepl("rspld",l.files.tas) &
                               grepl(paste(scenarios,collapse = "|"),
                                     l.files.tas)]

  OP.files.no.ext <- tools::file_path_sans_ext((l.files.tas))
  all.attributes <- strsplit(OP.files.no.ext,split = "\\.")

  scenars <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                          function(i){
                                                            data.frame(var = all.attributes[[i]][5])}))))
  models <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                         function(i){
                                                           data.frame(var = all.attributes[[i]][6])}))))

  variants <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                           function(i){
                                                             data.frame(var = all.attributes[[i]][7])}))))

  df.files <- bind_rows(df.files,
                        data.frame(variable = var,
                                   model = models,
                                   scenario = scenars,
                                   variant = variants))
}


df.files.wide <- df.files %>%
  ungroup() %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = variable,
              values_from = present,
              values_fill = FALSE) %>%
  filter(pr)

models <- (sort(unique(df.files.wide[["model"]])))

for (cmodel in models){

  print(paste0("model: ",cmodel))

  cdf.files.wide <- df.files.wide %>%
    filter(model == cmodel,
           scenario != "historical")

  cdf.files.wide.hist <- df.files.wide %>%
    filter(model == cmodel,
           scenario == "historical")

  if (nrow(cdf.files.wide) == 0){
    next()
  }



  for (irow in seq(1,nrow(cdf.files.wide))){

    cvars <- cdf.files.wide[irow,] %>%
      dplyr::select(-c(model,scenario,variant)) %>%
      pivot_longer(cols = -c()) %>%
      filter(value) %>% pull(name) %>% unique()
    cscenario <- as.character(cdf.files.wide[irow,"scenario"])
    cmodel <- as.character(cdf.files.wide[irow,"model"])
    cvariant <- as.character(cdf.files.wide[irow,"variant"])

    print(paste0("- ",irow/nrow(cdf.files.wide)," - ",cscenario))


    OP.file <- paste0("./outputs/Timeseries.MCWD.",cmodel,".",cscenario,".RDS")

    if (!overwrite & file.exists(OP.file)){
      next()
    }

    cdata.all <- data.frame()

    for (ivar in seq(1,length(cvars))){

      cvar = cvars[ivar]
      print(cvar)

      cfile <- paste0("./outputs/","CMIP6.monthly.",cvar,".pantropical.",cscenario,".",cmodel,".",cvariant,".RDS")
      cfile.hist <- paste0("./outputs/","CMIP6.monthly.",cvar,".pantropical.historical.",cmodel,".",cvariant,".RDS")

      all.files <- c(cfile,cfile.hist)


      if (!all(file.exists(all.files))){
        next()
      } else{
        cdf.tempA <- bind_rows(readRDS(cfile.hist),
                               readRDS(cfile))
      }

      if (cvar == "tas"){
        cdata.tas <- cdf.tempA %>%
          rename(tas = value) %>%
          mutate(tas = tas - 273.15) %>%
          ungroup() %>%
          dplyr::select(-var)
      } else if (cvar == 'tasmin') {
        cdata.tas <- cdf.tempA %>%
          rename(tasmin = value) %>%
          mutate(tasmin = tasmin - 273.15) %>%
          ungroup() %>%
          dplyr::select(-var)
      } else if (cvar == 'tasmax') {
        cdata.tas <- cdf.tempA %>%
          rename(tasmax = value) %>%
          mutate(tasmax = tasmax - 273.15) %>%
          ungroup() %>%
          dplyr::select(-var)
      } else if (cvar == 'pr') {
        cdata.tas <- cdf.tempA %>%
          rename(pr = value) %>%
          ungroup() %>%
          dplyr::select(-var)
      }

      if (ivar == 1){
        cdata.all <- cdata.tas %>%
          ungroup() %>%
          mutate(scenario = case_when(year <= 2014 ~ "historical",
                                      TRUE ~ cscenario)) %>%
          mutate(period = year) %>%
          filter(!is.na(period))
      } else{
        cdata.all <- cdata.all %>%
          left_join(cdata.tas %>%
                      filter(!is.na(year)),
                    by = c("lon","lat","year","month","model","scenario","variant")
          )
      }
    }

    if (nrow(cdata.all) == 0){
      next()
    }

    saveRDS(cdata.all %>%
              distinct(),
            OP.file)

    if (irow == 1){
      saveRDS(cdata.all %>%
                filter(year <= 2014) %>%
                distinct(),
              paste0("./outputs/Timeseries.MCWD.",cmodel,".historical.RDS")
      )
    }

  }
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/timeseries.MCWD.scenarios.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


