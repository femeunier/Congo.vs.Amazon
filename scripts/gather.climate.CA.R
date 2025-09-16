rm(list = ls())

library(dplyr)

cdir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Precip.Tropics"
files <- list.files(cdir,
                    pattern = "^df.*rspld.RDS",
                    full.names = TRUE)

df.all <- data.frame()
for (ifile in seq(1,length(files))){

  cfile <- files[ifile]

  all.attributes <- strsplit(cfile,split = "\\.")

  cproduct <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                          function(i){
                                                            data.frame(var = all.attributes[[i]][3])}))))


  cdata <- readRDS(cfile)
  print(paste(cproduct," - ",min(cdata$year)," - ",
              max(cdata$year)))

  df.all <- bind_rows(df.all,
                      cdata %>%
                        mutate(product = cproduct) %>%
                        filter(lon >= -15, lon <= 60,
                               lat <= 20,lat >= -20) %>%
                        filter(year %in% c(1961:2010)))

}

saveRDS(df.all,
        "./outputs/climate.CA.rspld.RDS")


# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/gather.climate.CA.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
