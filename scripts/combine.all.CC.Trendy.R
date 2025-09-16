rm(list = ls())

library(dplyr)

Dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/"

files <- list.files(Dir,
                    pattern = "Trendy.*S2.CC.pantropical.v13.rspld.RDS",
                    full.names = TRUE)

coord <- bind_rows(readRDS("outputs/Amazon.coord.ILF.v13.RDS"),
                   readRDS("outputs/Congo.coord.ILF.v13.RDS")) %>%
  filter(model == "ORCHIDEE") %>%
  mutate(lon = round(lon,digits = 2),
         lat = round(lat,digits = 2)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

df.all <- data.frame()

for (cfile in files){

  cmodel <- strsplit(basename(cfile),"\\.")[[1]][2]

  print(cmodel)

  A <- readRDS(cfile)

  df.all <- bind_rows(df.all,
                      A %>% mutate(model = cmodel) %>%
                        mutate(lon = round(lon,digits = 2),
                               lat = round(lat,digits = 2)) %>%
                        mutate(lon.lat = paste0(lon,".",lat)) %>%
                        filter(lon.lat %in% coord[["lon.lat"]]) %>%
                        filter(year >= 1960))

}

saveRDS(df.all,
        file.path(Dir,
                  "All.S2.CC.pantropical.v13.rspld.RDS"))

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/combine.all.CC.Trendy.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

