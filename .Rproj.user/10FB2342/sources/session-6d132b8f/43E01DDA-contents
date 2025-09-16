rm(list = ls())

library(dplyr)

years <- c(seq(1910,2020,10),2023)

df.all <- data.frame()
for (cyear in years){

  print(cyear)
  cfile <- paste0("./outputs/monthly.climate.pantropical.",cyear,".RDS")
  cdata <- readRDS(cfile)

  df.all <- bind_rows(df.all,
                      cdata)
}

saveRDS(df.all,
        "./outputs/monthly.climate.pantropical.CRUJRA.RDS")
        
# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/compile.monthly.climate.pantropical.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
