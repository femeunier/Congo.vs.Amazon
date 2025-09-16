rm(list = ls())

###############################################################################
# Rscript files

years <- 2000:2025

input_file <- "/data/gent/vo/000/gvo00074/felicien/R/dowload.GLDAS.daily.R"
lines <- readLines(input_file)

for (cyear in years){
  output_file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/DL.GLDAS/dowload.GLDAS.daily.",
                        cyear,".R")
  clines <- lines
  # Modify line 5 (change index as needed)
  clines[10] <- paste0("years <-",cyear)
  # clines[181:182] <- ""

  writeLines(clines, output_file)
}

###############################################################################
# Job files

years <- 2001:2025

input_file <- "/data/gent/vo/000/gvo00074/felicien/R/DL.GLDAS/jobR2000.pbs"
lines <- readLines(input_file)

for (cyear in years){
  output_file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/DL.GLDAS/jobR",
                        cyear,
                        ".pbs")
  clines <- lines
  # Modify line 5 (change index as needed)
  clines[12] <- paste0("Rscript dowload.GLDAS.daily.",cyear,".R")

  writeLines(clines, output_file)
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Split.download.years.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

