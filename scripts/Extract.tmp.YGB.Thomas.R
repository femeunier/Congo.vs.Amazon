rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(YGB)

vars <- c("tas","tasmin","tasmax")
variants <- "r1i1p1f1"
overwrite <- TRUE

scenarios = c("historical","ssp126","ssp245","ssp370","ssp585")

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
              values_fill = FALSE)

models <- sort(unique(df.files.wide[["model"]]))

for (cmodel in models){

  print(paste0("model: ",cmodel))
  df.all.class <- data.frame()

  cdf.files.wide <- df.files.wide %>%
    filter(model == cmodel)

  if (nrow(cdf.files.wide) == 0){
    next()
  }

  OP.file <- paste0("./outputs/YGB.tmp.",cmodel,".RDS")

  if (!overwrite & file.exists(OP.file)){
    next()
  }

  df.all.scenario <- data.frame()

  for (irow in seq(1,nrow(cdf.files.wide))){

    print(paste0("- ",irow/nrow(cdf.files.wide)))

    cvars <- cdf.files.wide[irow,] %>%
      dplyr::select(-c(model,scenario,variant)) %>%
      pivot_longer(cols = -c()) %>%
      filter(value) %>% pull(name) %>% unique()
    cscenario <- as.character(cdf.files.wide[irow,"scenario"])
    cmodel <- as.character(cdf.files.wide[irow,"model"])
    cvariant <- as.character(cdf.files.wide[irow,"variant"])

    cdata.all <- data.frame()

    for (ivar in seq(1,length(cvars))){

      cvar = cvars[ivar]
      print(cvar)

     cfile <- paste0("./outputs/","CMIP6.monthly.",cvar,".pantropical.",cscenario,".",cmodel,".",cvariant,".RDS")

     if (!file.exists(cfile)){
       next()
     }

     cdf.tempA <-  readRDS(cfile)

      dist <- cdf.tempA %>%
        filter(year == year[1], month == month[1]) %>%
        mutate(dist = (lon - 24.46)**2 + (lat - 0.82)**2) %>%
        arrange(dist) %>%
        slice_head(n = 9) %>%
        mutate(lon.lat = paste0(lon,".",lat))

      cdf.tempA <- cdf.tempA %>%
        ungroup() %>%
        mutate(lon.lat = paste0(lon,".",lat)) %>%
        filter(lon.lat %in%
                 dist[["lon.lat"]]) %>%
        dplyr::select(-lon.lat)

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
      }

      if (ivar == 1){
        cdata.all <- cdata.tas %>%
          ungroup() %>%
          filter(!is.na(year))

      } else{
        cdata.all <- cdata.all %>%
          left_join(cdata.tas %>%
                      filter(!is.na(year)),
                    by = c("lon","lat","year","month","model","scenario","variant")
          )
      }
    }

    cdf.all.scenario <- cdata.all

    df.all.scenario <- bind_rows(df.all.scenario,
                                 cdf.all.scenario)

  }

  saveRDS(df.all.scenario %>%
            distinct(),
          OP.file)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Extract.tmp.YGB.Thomas.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


