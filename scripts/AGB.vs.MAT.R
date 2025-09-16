rm(list = ls())

library(dplyr)
library(Rcpp)
library(ggplot2)
library(zoo)
library(roll)
library(data.table)
library(tidyr)

files <- list.files("./outputs/",
                    "^Roll.Timeseries.MCWD",full.names = TRUE)
files <- (files[!grepl(pattern = "historical",
                       files)])

models <- c("ACCESS-CM2","ACCESS-ESM1-5","AWI-ESM-1-REcoM",
            "CanESM5","CMCC-ESM2","EC-Earth3-Veg","FGOALS-g3",
            "IPSL-CM6A-LR","MIROC6","MRI-ESM2-0")
files <- files[grepl(paste0(models,collapse = "|"),
                     files)]

AGB <- readRDS("./outputs/Biomass.per.biome.Tropics.avg.RDS")

overwite <- FALSE

df.ts <- df.ts.all <-
  data.frame()
for (ifile in seq(1,length(files))){

  print(ifile/length(files))

  cfile <- files[ifile]
  cmodel <- strsplit(basename(cfile),"\\.")[[1]][4]
  cscenario <- strsplit(basename(cfile),"\\.")[[1]][5]

  A <- readRDS(cfile)
  A.cat <- A %>%
    ungroup() %>%
    filter(month == 1) %>%
    mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
    mutate(biome = case_when(MAP > 1700 & AI < -3.8 ~ "Humid_large",
                             AI < -3.8 ~ "Humid_low",
                             AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
                             AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
                             AI < -0.25 & AI >= -1 ~ "Semiarid",
                             AI < -0.05 & AI >= -0.25 ~ "Arid",
                             AI < 0 & AI >= -0.05 ~ "Hyperarid",
                             TRUE ~ NA_character_)) %>%
    mutate(biome = factor(biome,
                          levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                     "Semiarid","Arid","Hyperarid"))) %>%
    left_join(AGB,
              by = "biome")

  if (!c("MAT") %in% colnames(A.cat)){
    A.ts <- A.cat %>%
      group_by(scenario,model,year) %>%
      summarise(AGB.tot = mean(AGB.m,na.rm = TRUE),
                .groups = "keep")
  } else {
    A.ts <- A.cat %>%
      group_by(scenario,model,year) %>%
      summarise(AGB.tot = mean(AGB.m,na.rm = TRUE),
                MAT = mean(MAT,na.rm = TRUE),
                .groups = "keep")
  }

  df.ts <- bind_rows(df.ts,
                     A.ts)

  df.ts.all <- bind_rows(df.ts.all,
                         A.cat %>%
                           dplyr::select(lon,lat,year,scenario,model,
                                         MAP,MCWD,AI,biome,AGB.m))

}

saveRDS(df.ts,
        "./outputs/All.ts.sum.RDS")

saveRDS(df.ts.all,
        "./outputs/All.ts.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/AGB.vs.MAT.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
