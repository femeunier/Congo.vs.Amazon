rm(list = ls())

library(dplyr)
library(YGB)
library(Rcpp)
library(ggplot2)
library(zoo)
library(roll)
library(data.table)
library(tidyr)

files1 <- list.files("./outputs/",
                     pattern = "TS.Global.warming*",full.names = TRUE)
df.all <-
  data.frame()
for (ifile in seq(1,length(files1))){

  print(ifile/length(files1))

  cdf <- readRDS(files1[ifile]) %>%
    distinct()

  df.all <- bind_rows(df.all,
                      cdf)

}

saveRDS(df.all %>%
          rename(scenario = cscenario),
        "./outputs/all.TS.global.warming.RDS")


# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Gather.global.warming.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

