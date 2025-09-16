rm(list = ls())

library(dplyr)

Dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/"

files <- list.files(Dir,
                    pattern = "Trendy.*S3.cAGB.pantropical.v13.rspld.RDS",
                    full.names = TRUE)

df.all <- data.frame()

for (cfile in files){

  cmodel <- strsplit(basename(cfile),"\\.")[[1]][2]

  print(cmodel)

  A <- readRDS(cfile)

  df.all <- bind_rows(df.all,
                      A %>% mutate(model = cmodel))

}

saveRDS(df.all,
        file.path(Dir,
                  "All.S3.cAGB.pantropical.v13.rspld.RDS"))

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/combine.all.cAGB.Trendy.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

