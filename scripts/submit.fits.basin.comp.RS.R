rm(list = ls())

library(dplyr)
library(tidyr)
library(ED2scenarios)
library(Congo.ED2)
library(purrr)

overwrite = TRUE
products <- c("TwoLeaf","NIR","VOD","SIF")

suffixes <- c("Congo","Amazon")
scenarios <- c("S2")
GridSuffix = "ERA5"

grid <- base::expand.grid(
  list(
    product = products,
    scenario = "S2"
  ))

main.dir <- "/data/gent/vo/000/gvo00074/felicien/R"

list_dir <- list() ; jobname <- "job.sh"

for (suffix in suffixes){

  prefix <- paste0("Basin.Comp.RS.",suffix,".")
  coord.list.file <- paste0("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/",suffix,".coord.ILF.RDS")

  for (irow in seq(1,nrow(grid))){

    crow <- grid[irow,]
    cdir <- file.path(main.dir,"outputs",
                      cname <- (gsub("\\/","",gsub(" ","",
                                                   paste0("Basin.Comp.RS.",suffix,".",
                                                          crow[["product"]])))))

    dir.create(cdir,showWarnings = FALSE)

    Rscript.name <- file.path(cdir,
                              script.name <- "Rscript.R")
    write.script.fit.RS.coordlist(
      file = Rscript.name,
      product = crow[["product"]],
      coord.list = coord.list.file,
      xgb.model.prefix = gsub("\\/","",gsub(" ","",
                                            paste0(prefix,
                                                   crow[["product"]]))),
      grid.suffix = GridSuffix,
      frac.train = 0.6,
      overwrite = overwrite,
      climate.vars = c("year","month","lon","lat",
                       "tmp","tmin","tmax","spfh","VPD","pre","dswrf","dlwrf"))

    # Create job file
    ED2scenarios::write_jobR(file = file.path(cdir,jobname),
                             nodes = 1,ppn = 16,mem = 64,walltime = 1,
                             prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                             CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                             Rscript = Rscript.name)
    list_dir[[cname]] = cdir

  }
}

dumb <- write_bash_submission(file = file.path(getwd(),
                                               "All.fits.CompBasin.RS.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/submit.fits.basin.comp.RS.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
