rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(YGB)

vars <- c("pr","tas")
variants <- "r1i1p1f1"
overwrite <- TRUE

scenarios = c("historical","ssp126","ssp245","ssp370","ssp585")

df.files <- data.frame()
for (var in vars){

  l.files.tas <- list.files("./outputs",pattern = paste0("CMIP6.monthly.",var,".pantropical*"))
  l.files.tas <- l.files.tas[grepl("rspld",l.files.tas) &
                               grepl(paste(scenarios,collapse = "|"),l.files.tas)]

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
  filter(pr & tas) %>%
  mutate(present = TRUE) %>%
  dplyr::select(-c(pr,tas)) %>%
  pivot_wider(names_from = scenario,
              values_from = present,
              values_fill = FALSE) %>%
  filter(historical & ssp245 & ss585)

models <- sort(unique(df.files.wide[["model"]]))

for (cmodel in models){

  print(paste0("model: ",cmodel))
  df.all.class <- data.frame()

  cdf.files.wide <- df.files.wide %>%
    filter(model == cmodel)

  for (irow in seq(1,nrow(cdf.files.wide))){

    print(paste0("- ",irow/nrow(cdf.files.wide)))

    cvars <- cdf.files.wide[irow,] %>%
      dplyr::select(-c(model,scenario,variant)) %>%
      pivot_longer(cols = -c()) %>%
      filter(value) %>% pull(name) %>% unique()
    cscenario <- cdf.files.wide[irow,"scenario"]
    cmodel <- cdf.files.wide[irow,"model"]
    cvariant <- cdf.files.wide[irow,"variant"]

    OP.file <- paste0("./outputs/subset.CMIP6.classifications.",cmodel,".RDS")

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


    if (all(c("tasmin","tasmax")) %in% cvars){
      cdata.all.conv <- cdata.all.mean %>%
        group_by(period,scenario,model,lon,lat) %>%
        mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
               E = SPEI::penman(tasmin, tasmax,
                                tsun = daylength(unique(lat), seq(15,365,30),
                                                 notimes.as.na = FALSE)[["Daylength"]],
                                lat = unique(lat),
                                z = 0,
                                na.rm = TRUE,
                                verbose = FALSE)/Ndays,

               Pmm = Ndays*pr*86400,
               Etot = E*Ndays) %>%
        dplyr::select(-pr) %>%
        mutate(diff = Pmm - Etot) %>%
        mutate(wettest.month = which.max(diff)) %>%
        mutate(month.ord = 1 + (month - wettest.month)) %>%
        mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                                     TRUE ~ month.ord)) %>%
        arrange(month.ord) %>%
        mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                               TRUE ~ NA_real_)) %>%
        mutate(CWD = calc.CWD(diff,CWD[1])) %>%
        arrange(month) %>%
        mutate(MCWD = min(CWD),
               MAP = sum(Pmm),
               MAT = mean(tas),
               Etot = sum(Etot),
               ETeq = "penman")
    } else if (all(c("tas") %in% cvars)){
      cdata.all.conv <- cdata.all.mean %>%
        group_by(period,scenario,model,lon,lat) %>%
        mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
               E = SPEI::thornthwaite(tas,
                                      lat = unique(lat),
                                      na.rm = TRUE,
                                      verbose = FALSE)/Ndays,
               Pmm = Ndays*pr*86400,
               Etot = E*Ndays) %>%
        dplyr::select(-pr) %>%
        mutate(diff = Pmm - Etot) %>%
        mutate(wettest.month = which.max(diff)) %>%
        mutate(month.ord = 1 + (month - wettest.month)) %>%
        mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                                     TRUE ~ month.ord)) %>%
        arrange(month.ord) %>%
        mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                               TRUE ~ NA_real_)) %>%
        mutate(CWD = calc.CWD(diff,CWD[1])) %>%
        arrange(month) %>%
        mutate(MCWD = min(CWD),
               MAP = sum(Pmm),
               MAT = mean(tas),
               Etot = sum(Etot),
               ETeq = "thornthwaite")
    } else {
      cdata.all.conv <- cdata.all.mean %>%
        group_by(period,scenario,model,lon,lat) %>%
        mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
               E = 3.33,
               Pmm = Ndays*pr*86400,
               Etot = E*Ndays) %>%
        dplyr::select(-pr) %>%
        mutate(diff = Pmm - Etot) %>%
        mutate(wettest.month = which.max(diff)) %>%
        mutate(month.ord = 1 + (month - wettest.month)) %>%
        mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                                     TRUE ~ month.ord)) %>%
        arrange(month.ord) %>%
        mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                               TRUE ~ NA_real_)) %>%
        mutate(CWD = calc.CWD(diff,CWD[1])) %>%
        arrange(month) %>%
        mutate(MCWD = min(CWD),
               MAP = sum(Pmm),
               MAT = NA_real_,
               Etot = sum(Etot),
               ETeq = "constant")
    }

    cdf.class <- cdata.all.conv %>%
      ungroup() %>%
      filter(month == 1) %>%
      mutate(basin = case_when(lon >= -120 & lon <= -30 ~ "Amazon",
                               lon <= 55 ~ "Congo",
                               lon <= 160 ~ "Australasia",
                               TRUE ~ NA_character_)) %>%
      filter(!is.na(basin)) %>%
      ungroup() %>%
      dplyr::select(period,scenario,model,lon,lat,
                    MAP,MAT,MCWD,Etot,ETeq)

    df.all.class <- bind_rows(df.all.class,
                              cdf.class)

  }

  saveRDS(df.all.class,
          OP.file)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/summarise.vars.CMIP6.subset.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


