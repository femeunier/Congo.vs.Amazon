rm(list = ls())

library(dplyr)
library(feather)
library(Rcpp)
library(data.table)

files <- list.files("./outputs/",
                    "^Timeseries.MCWD.",full.names = TRUE)
files <- (files[!grepl(pattern = "historical|ssp534-over",
                       files)])

states <- list()

for (ifile in seq(1,length(files))){

  cfile <- files[ifile]
  cmodel <- strsplit(basename(cfile),"\\.")[[1]][3]
  cscenario <- strsplit(basename(cfile),"\\.")[[1]][4]

  print(paste0(basename(cfile),": ",cmodel," - ",cscenario," - ",
               ifile/length(files)))

  if (file.exists(cfile)){
    A <- readRDS(cfile) %>%
      ungroup() %>%
      filter(abs(lat) <= 23.25) %>%
      filter(lon >= -20, lon <= 60)
  } else {
    next()
  }

  cvars <- colnames(A)

  if (!all(c("pr") %in% cvars)){
    next()
  }

  if (!(all(c("tasmin","tasmax","tas") %in% cvars))){
    next()
  }


  A.cat <- A %>%
    filter(year %in% seq(1901,2100,20)) %>%
    mutate(scenario = case_when(year <= 2014 ~ "historical",
                                TRUE ~ cscenario)) %>%
    dplyr::select(any_of(c("lon","lat","model","scenario",
                           "year","month",
                           "tas","tasmin","tasmax","pr")))

  cvars <- colnames(A)

  if (!all(c("pr") %in% cvars)){
    next()
  }

 states[[ifile]] <- A.cat

}

combined_df <-  rbindlist(states,
                          use.names = TRUE,
                          fill = TRUE) %>%
  distinct()

write_feather(combined_df,
              "./outputs/All.tas.feather")


# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Compile.tas.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
