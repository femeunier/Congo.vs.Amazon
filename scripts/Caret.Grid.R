rm(list = ls())

library(CausalAI)
library(Congo.vs.Amazon)
library(ED2scenarios)
library(dplyr)
library(purrr)
library(raster)
library(ggplot2)
library(ggthemes)

###############################################################
# Settings

models <- TrENDY.analyses::get.model.names.TRENDY("v13")

main.config <- list(
  x_var = c("tmp","tmin","tmax","dswrf","vpd","co2anomaly","pre"),
  y_var = "gppanomaly",
  fac.CC = 86400*365,
  year.min = 1980,
  year.max = 2025,
  lags = 12,
  initial = 432,
  horizon = 12,
  skip = 11,

  Grid = expand.grid(
    nrounds = c(200, 400, 800),
    max_depth = c(3, 6, 9),
    eta = c(0.03, 0.1),
    gamma = c(0),
    colsample_bytree = c(0.8),
    min_child_weight = c(1),
    subsample = c(0.8)),

  include.past.lag = FALSE,

  suffix = "noLag",

  climate.location = "/data/gent/vo/000/gvo00074/felicien/R/outputs/ERA5/anomaly.",
  Ntest.month = 48)


dir.name <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/GPP_caret/"
dir.create(dir.name,showWarnings = FALSE)


suffix <- main.config[["suffix"]]
list_dir <- list() ; job.names <- c()

for (cmodel in models){

  print(paste0(cmodel))

  dir.create(file.path(dir.name,cmodel),showWarnings = FALSE)

  model.config <- main.config
  model.config[["CC.location"]] <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/DGVM/",cmodel,"/",main.config[["y_var"]],".",cmodel)
  model.config[["IFL"]] <- readRDS("./outputs/Amazon.coord.ILF.v13.RDS") %>%
    filter(model == "ORCHIDEE")
  model.config[["dest.dir"]] <- file.path(dir.name,cmodel,cmodel)


  modelconfig.file <- file.path(dir.name,cmodel,
                                paste0("config.",cmodel,".",suffix,".RDS"))

  saveRDS(model.config,
          modelconfig.file)

  write.Caret.script(file.path(dir.name, cmodel),
                     paste0("Rscript.",suffix,".R"),
                     modelconfig.file)

  cjobname <- paste0("job_",cmodel,".pbs")
  ED2scenarios::write_jobR(file = file.path(dir.name,cmodel,cjobname),
                           nodes = 1,ppn = 16,mem = 100,walltime = 3,
                           prerun = "ml purge ; ml R-bundle-Bioconductor/3.20-foss-2024a-R-4.4.2",
                           CD = file.path(dir.name,cmodel),
                           Rscript = paste0("Rscript.",suffix,".R"))
  job.names <- c(job.names,
                 cjobname)
  list_dir[[cmodel]] = file.path(dir.name,
                                 cmodel)
}

dumb <- write_bash_submission(file = file.path(getwd(),
                                               "All.IFL.sh"),
                              list_files = list_dir,
                              job_name = job.names)

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Caret.Grid.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
