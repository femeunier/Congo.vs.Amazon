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
models <- rev(models)
models <- c("FGOALS-f3-L")

for (cmodel in models){

  print(paste0("model: ",cmodel))
  df.all.class <- data.frame()

  cdf.files.wide.ref <- df.files.wide %>%
    filter(model == cmodel,
           scenario == "historical")
  cdf.files.wide <- df.files.wide %>%
    filter(model == cmodel,
           scenario != "historical")

  if (nrow(cdf.files.wide) == 0){
    next()
  }

  OP.file <- paste0("./outputs/CMIP6.classifications.hargreaves",cmodel,".RDS")

  if (!overwrite & file.exists(OP.file)){
    next()
  }

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

      cfile.hist <- paste0("./outputs/","CMIP6.monthly.",cvar,".pantropical.historical.",cmodel,".",cvariant,".RDS")
      cfile <- paste0("./outputs/","CMIP6.monthly.",cvar,".pantropical.",cscenario,".",cmodel,".",cvariant,".RDS")
      cfiles2read <- c(cfile.hist,cfile)
      if (!all(file.exists(cfiles2read))){
        cfiles2read <- cfiles2read[file.exists(cfiles2read)]
        cdf.tempA <- readRDS(cfiles2read)
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
                      mutate(scenario = case_when(year <= 2014 ~ "historical",
                                                  TRUE ~ cscenario)) %>%
                      mutate(period = year) %>%
                      filter(!is.na(period)),
                    by = c("lon","lat","year","month","model","scenario","period","variant")
          )
      }
    }

    if (nrow(cdata.all) == 0){
      next()
    }

    for (cperiod in seq(1900,2100,10)){

      print(cperiod)

      ctemp <- cdata.all %>%
        filter(period %in% c((cperiod-15):(cperiod+14)))

      if (nrow(ctemp) == 0){
        next()
      }

      cdata.all.mean <- ctemp %>%
        ungroup() %>%
        pivot_longer(cols = any_of(cvars),
                     names_to = "variable") %>%
        group_by(variable,model,lon,lat,month) %>%
        summarise(value.m = mean(value,
                                 na.rm = TRUE),
                  .groups = "keep") %>%
        pivot_wider(names_from = "variable",
                    values_from = "value.m") %>%
        ungroup() %>%
        mutate(period = cperiod,
               scenario = case_when(cperiod <= 2014 ~ "historical",
                                    TRUE ~ cscenario))


      if (all(c("tasmin","tasmax") %in% cvars)){

        print("hargreaves")

        ctemp.df <- cdata.all.mean %>%
          filter(!is.na(tasmax),!is.na(tasmin),!is.na(pr))

        if (nrow(ctemp.df) == 0){
          next()
        }

        cdata.all.conv <- ctemp.df %>%
          group_by(period,scenario,model,lon,lat) %>%
          mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
                 # E = SPEI::penman(tasmin, tasmax,
                 #              tsun = daylength(unique(lat), seq(15,365,30),
                 #                               notimes.as.na = FALSE)[["Daylength"]],
                 #              lat = unique(lat),
                 #              z = 0,
                 #              na.rm = TRUE,
                 #              verbose = FALSE)/Ndays,
                 E = SPEI::hargreaves(tasmin,
                                      tasmax,
                                      lat = unique(lat),
                                      Ra = NULL,
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
                 ETeq = "hargreaves")

      } else if (all(c("tas") %in% cvars) & !all(is.na(cdata.all.mean$tas))){

        print("thornwaite")

        ctemp.df <- cdata.all.mean %>%
          filter(!is.na(tas),!is.na(pr))

        if (nrow(ctemp.df) == 0){
          next()
        }

        cdata.all.conv <- ctemp.df %>%
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

        print("constant")

        ctemp.df <- cdata.all.mean %>%
          filter(!is.na(pr))

        if (nrow(ctemp.df) == 0){
          next()
        }

        cdata.all.conv <- ctemp.df %>%
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
  }

  saveRDS(df.all.class %>%
            distinct(),
          OP.file)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/summarise.vars.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


