rm(list = ls())

library(dplyr)

all.files <- list.files("./outputs/",pattern = "^CMIP6.ET.*.RDS",full.names = TRUE)

df.all <- data.frame()
df.all.future <- data.frame()
df.all.futures <- data.frame()
for (cfile in all.files){

  print(basename(cfile))
  cdf <- readRDS(cfile)
  df.all <- bind_rows(df.all,
                      cdf %>%
                        filter(period == 1985))

  df.all.future <- bind_rows(df.all.future,
                      cdf %>%
                        filter(period == 2085))

  df.all.futures <- bind_rows(df.all.futures,
                             cdf %>%
                               filter(period >= 2025))

}

saveRDS(df.all,
        "./outputs/All.CMIP6.ET.RDS")
saveRDS(df.all.future,
        "./outputs/All.CMIP6.ET.future.RDS")
saveRDS(df.all.futures,
        "./outputs/All.CMIP6.ET.futures.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Combine.all.ET.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

