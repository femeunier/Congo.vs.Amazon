rm(list = ls())

library(dplyr)
library(YGB)
library(Rcpp)
library(ggplot2)
library(zoo)
library(roll)
library(data.table)
library(tidyr)

system2("rm",
        "./outputs/All.CMIP6.states.timing.RDS")

files1 <- list.files("./outputs/",
                     pattern = "^All.CMIP6.states.timing.*",full.names = TRUE)

df.all <-
  data.frame()
for (ifile in seq(1,length(files1))){

  print(ifile/length(files1))

  cdf <- readRDS(files1[ifile]) %>%
    distinct()

  df.all <- bind_rows(df.all,
                      cdf)

}

saveRDS(df.all,
        "./outputs/All.CMIP6.states.timing.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Gather.CMIP6.states.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

