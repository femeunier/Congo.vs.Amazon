rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(YGB)

thresholds <-
  data.frame(basin = c("Amazon","Congo"),
             MCWD = c(-200,-250),
             MAP = c(1500,1000))


vars <- c("pr","tas")

df.files <- data.frame()
for (var in vars){

  l.files.tas <- list.files("./outputs",pattern = paste0("df.monthly.",var,".pantropical*"))

  OP.files.no.ext <- tools::file_path_sans_ext((l.files.tas))
  all.attributes <- strsplit(OP.files.no.ext,split = "\\.")

  scenars <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                          function(i){
                                                            data.frame(var = all.attributes[[i]][5])}))))
  models <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                         function(i){
                                                           data.frame(var = all.attributes[[i]][6])}))))

  df.files <- bind_rows(df.files,
                        data.frame(variable = var,
                                   model = models,
                                   scenario = scenars))
}


df.files.wide <- df.files %>%
  ungroup() %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = variable,
              values_from = present,
              values_fill = FALSE)


models <- unique(df.files.wide[["model"]])
overwrite <- TRUE

for (cmodel in models){

  print(paste0("model: ",cmodel))
  df.all.class <- data.frame()

  cdf.files.wide <- df.files.wide %>%
    filter(model == cmodel,
           scenario == "historical")

  OP.file <- paste0("./outputs/CMIP6.",cmodel,".classifications.pr.tas.historical.RDS")

  if (!overwrite & file.exists(OP.file)){
    next()
  }


  for (irow in seq(1,nrow(cdf.files.wide))){

    print(paste0("- ",irow/nrow(cdf.files.wide)))

    cvars <- vars[vars %in% colnames(cdf.files.wide[irow,])]
    cscenario <- cdf.files.wide[irow,"scenario"]
    cmodel <- cdf.files.wide[irow,"model"]

    for (ivar in seq(1,length(cvars))){

      cvar = cvars[ivar]
      if (cvar == "tas"){
        cdata.tas <- readRDS(paste0("./outputs/","df.monthly.",cvar,".pantropical.",cscenario,".",cmodel,".RDS")) %>%
          rename(tas = value) %>%
          mutate(tas = tas - 273.15) %>%
          ungroup() %>%
          dplyr::select(-var)
      }  else if (cvar == 'pr') {
        cdata.tas <- readRDS(paste0("./outputs/","df.monthly.",cvar,".pantropical.",cscenario,".",cmodel,".RDS")) %>%
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
             MAT = mean(tas),
             Etot = sum(Etot))


    cdf.class <- cdata.all.conv %>%
      ungroup() %>%
      filter(month == 1) %>%
      mutate(basin = case_when(lon >= -120 & lon =< -30 ~ "Amazon",
                               lon > -30 & lon <= 55 ~ "Congo",
                               lon <= 160 ~ "Australasia",
                               TRUE ~ NA_character_)) %>%
      filter(!is.na(basin)) %>%
      ungroup() %>%
      mutate(MCWD.threshold = thresholds$MCWD[match(basin,thresholds$basin)],
             MAP.threshold = thresholds$MAP[match(basin,thresholds$basin)]) %>%
      mutate(type = case_when(MCWD >= MCWD.threshold ~ 2,
                              MAP <= MAP.threshold ~ 1,
                              TRUE ~ 3)) %>%
      dplyr::select(period,scenario,model,lon,lat,
                    MAP,MAT,MCWD,Etot,type)

    df.all.class <- bind_rows(df.all.class,
                              cdf.class)

  }
  saveRDS(df.all.class,
          OP.file)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/summarise.vars.CMIP6.pr.tas.historical.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


