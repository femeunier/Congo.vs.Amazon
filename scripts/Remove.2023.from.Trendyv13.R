rm(list = ls())

library(dplyr)

files <- list.files("./outputs/",pattern = "*CC.pantropical.completed.v13*",
                    full.names = TRUE)
files <- files[!grepl("nodrought",files)]

for (cfile in files){

  print(basename(cfile))
  A <- readRDS(cfile)
  Amod <- A %>%
    filter(year < 2023)

  newfilename <- paste0(dirname(cfile),"/",
                        tools::file_path_sans_ext(basename(cfile)),".nodrought.RDS")

  saveRDS(Amod,
          newfilename)

}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Remove.2023.from.Trendyv13.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

