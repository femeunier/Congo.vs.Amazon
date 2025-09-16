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
                    pattern = "^Changes.*",full.names = TRUE)
files2 <- list.files("./outputs/",pattern = "^Transitions.*",full.names = TRUE)

df.all <- df.all2 <-
  data.frame()
for (ifile in seq(1,length(files1))){

  print(ifile/length(files1))

  cdf <- readRDS(files1[ifile]) %>%
    distinct()

  df.all <- bind_rows(df.all,
                      cdf)

}

for (ifile in seq(1,length(files2))){

  print(ifile/length(files2))

  cdf2 <- readRDS(files2[ifile]) %>%
    distinct()

  df.all2 <- bind_rows(df.all2,
                       cdf2)

}

saveRDS(df.all,
        "./outputs/all.changes2.RDS")

saveRDS(df.all2,
        "./outputs/transitions2.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Gather.transitions.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

