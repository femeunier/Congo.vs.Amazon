rm(list = ls())

library(dplyr)
library(Rcpp)

files <- list.files("./outputs/",
                    "^Roll.Timeseries.MCWD.",full.names = TRUE)
files <- (files[!grepl(pattern = "historical",
                       files)])

Variance <- data.frame()

LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

overwrite <- FALSE

for (ifile in seq(1,length(files))){

  cfile <- files[ifile]
  cmodel <- strsplit(basename(cfile),"\\.")[[1]][4]
  cscenario <- strsplit(basename(cfile),"\\.")[[1]][5]

  print(paste0(basename(cfile),": ",cmodel," - ",cscenario," - ",
               ifile/length(files)))


  op.file <- paste0("./outputs/CMIP6.variance.",
                    cmodel,".",cscenario,".RDS")

  if (!overwrite & file.exists(op.file)){
    next()
  }

  A <- readRDS(cfile) %>%
    ungroup()

  A.sel <- A %>%
    filter(lon >= -15, lon <= 60,
           lat >= -15, lat <= 10) %>%
    filter(year %in% c(1981:2010,
                       2011:2040,
                       2041:2070,
                       2071:2100)) %>%
    mutate(timing = case_when(year <= 2014 ~ "historical",
                              year <= 2040 ~ "Near_future",
                              year <= 2070 ~ "Mid_future",
                              TRUE ~ "Long_future"))

  A.sel.LC <- A.sel %>%
    left_join(LC,
              by = c("lon","lat")) %>%
    group_by(LC,lon,lat,timing) %>%
    summarise(MAP.m = mean(MAP,na.rm = TRUE),
              MCWD.m = mean(MCWD,na.rm = TRUE),

              MAP.sd = sd(MAP,na.rm = TRUE),
              MCWD.sd = sd(MCWD,na.rm = TRUE),

              MAP.min = min(MAP,na.rm = TRUE),
              MCWD.min = min(MCWD,na.rm = TRUE),

              .groups = "keep")


  saveRDS(A.sel.LC %>%
            mutate(model = cmodel,
                   scenario = cscenario),
          op.file)

}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/CMIP6.state.var.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

