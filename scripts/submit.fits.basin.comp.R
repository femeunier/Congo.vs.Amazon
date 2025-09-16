rm(list = ls())

library(dplyr)
library(tidyr)
library(ED2scenarios)
library(Congo.ED2)
library(purrr)

overwrite = TRUE
models <- TrENDY.analyses::get.model.names.TRENDY(version = "v12")

suffixes <- c("Congo","Amazon")
scenarios <- c("S2")
GridSuffix = ".ERA5.v12.IFL"
all.vars <- c("gpp","nep","npp","nbp")

main.dir <- "/data/gent/vo/000/gvo00074/felicien/R"

grid <- base::expand.grid(
  list(
    model = models,
    scenario = scenarios
  ))

list_dir <- list() ; jobname <- "job.sh"

for (suffix in suffixes){

  prefix <- paste0("Basin.Comp.v12.",suffix,".")
  coord.list.file <- paste0("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/",suffix,".coord.ILF.v12.RDS")

  for (irow in seq(1,nrow(grid))){

    crow <- grid[irow,]
    cdir <- file.path(main.dir,"outputs",
                      cname <- (gsub("\\/","",gsub(" ","",
                                                   paste0("Basin.Comp.v12.",suffix,".",
                                                          crow[["model"]],".",
                                                          crow[["scenario"]])))))
    dir.create(cdir,showWarnings = FALSE)

    Rscript.name <- file.path(cdir,script.name <- "Rscript.R")
    write.script.fit.coordlist(
      file = Rscript.name,
      model = crow[["model"]],
      scenario = crow[["scenario"]],
      vars = all.vars,
      CC.suffix = "CC.global.v12.IFL",
      coord.list = coord.list.file,
      xgb.model.prefix = gsub("\\/","",gsub(" ","",
                                            paste0(prefix,
                                                   crow[["model"]],".",
                                                   crow[["scenario"]]))),
      grid.suffix = GridSuffix,
      frac.train = 0.6,
      overwrite = overwrite,
      transition.suffix = NULL,
      climate.vars = c("year","month","lon","lat",
                       "tmp","tmin","tmax","spfh","VPD","pre","dswrf","dlwrf"))

    # Create job file
    ED2scenarios::write_jobR(file = file.path(cdir,jobname),
                             nodes = 1,ppn = 16,mem = 200,walltime = 12,
                             prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                             CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                             Rscript = Rscript.name)
    list_dir[[cname]] = cdir

  }
}

dumb <- write_bash_submission(file = file.path(getwd(),
                                               "All.fits.CompBasin.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/submit.fits.basin.comp.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
