rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(YGB)

vars <- c("pr","tas","tasmin","tasmax")
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
              values_fill = FALSE) %>%
  filter(pr)

models <- sort(unique(df.files.wide[["model"]]))
# models <- rev(models)

for (cmodel in models){

  print(paste0("model: ",cmodel))
  df.all.class <- data.frame()

  cdf.files.wide <- df.files.wide %>%
    filter(model == cmodel)

  all.df <- data.frame()

  OP.file <- paste0("./outputs/All.tas.",cmodel,".RDS")

  for (irow in seq(1,nrow(cdf.files.wide))){

    print(paste0("- ",irow/nrow(cdf.files.wide)))

    cvars <- cdf.files.wide[irow,] %>%
      dplyr::select(-c(model,scenario,variant)) %>%
      pivot_longer(cols = -c()) %>%
      filter(value) %>% pull(name) %>% unique()
    cscenario <- cdf.files.wide[irow,"scenario"]
    cvariant <- cdf.files.wide[irow,"variant"]

    if (!overwrite & file.exists(OP.file)){
      next()
    }

    for (ivar in seq(1,length(cvars))){

      cvar = cvars[ivar]
      cfile <- paste0("./outputs/","CMIP6.monthly.",cvar,".pantropical.",cscenario,".",cmodel,".",cvariant,".RDS")
      if (!file.exists(cfile)){
        next()
      }
      if (cvar == "tas"){
        cdata.tas <- readRDS(cfile) %>%
          rename(tas = value) %>%
          mutate(tas = tas - 273.15) %>%
          ungroup() %>%
          dplyr::select(-var)
      } else if (cvar == 'tasmin') {
        cdata.tas <- readRDS(cfile) %>%
          rename(tasmin = value) %>%
          mutate(tasmin = tasmin - 273.15) %>%
          ungroup() %>%
          dplyr::select(-var)
      } else if (cvar == 'tasmax') {
        cdata.tas <- readRDS(cfile) %>%
          rename(tasmax = value) %>%
          mutate(tasmax = tasmax - 273.15) %>%
          ungroup() %>%
          dplyr::select(-var)
      } else if (cvar == 'pr') {
        cdata.tas <- readRDS(cfile) %>%
          rename(pr = value) %>%
          ungroup() %>%
          dplyr::select(-var)
      }

      if (ivar == 1){
        cdata.all <- cdata.tas %>%
          mutate(period = case_when(year %in% 1901:1930 ~ "historical1",
                                    year %in% 1950:1979 ~ "historical2",
                                    year %in% 1980:2009 ~ "current",
                                    year %in% 2015:2040 ~ "near_future",
                                    year %in% 2041:2070 ~ "mid_future",
                                    year %in% 2071:2100 ~ "long_future",
                                    TRUE ~ NA_character_)) %>%
          filter(!is.na(period))
      } else{
        cdata.all <- cdata.all %>%
          left_join(cdata.tas %>%
                      mutate(period = case_when(year %in% 1901:1930 ~ "historical1",
                                                year %in% 1950:1979 ~ "historical2",
                                                year %in% 1980:2009 ~ "current",
                                                year %in% 2015:2040 ~ "near_future",
                                                year %in% 2041:2070 ~ "mid_future",
                                                year %in% 2071:2100 ~ "long_future",
                                                TRUE ~ NA_character_)) %>%
                      filter(!is.na(period)),
                    by = c("lon","lat","year","month","model","scenario","period")
          )
      }
    }

    if (nrow(cdata.all) == 0){
      next()
    }

    cdata.all.mean <- cdata.all %>%
      ungroup() %>%
      pivot_longer(cols = any_of(cvars),
                   names_to = "variable") %>%
      group_by(period,variable,scenario,model,lon,lat,month) %>%
      summarise(value.m = mean(value,
                               na.rm = TRUE),
                .groups = "keep") %>%
      pivot_wider(names_from = "variable",
                  values_from = "value.m")

    all.df <- bind_rows(all.df,
                        cdata.all.mean)

  }

  saveRDS(all.df,
          OP.file)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/compile.all.tas.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


