rm(list = ls())

library(dplyr)

files <- list.files("./outputs/",
                    "^Timeseries.MCWD",full.names = TRUE)
files <- (files[grepl(pattern = "ssp534-over",
                       files)])

# overwrite <- FALSE
for (i in seq(1,length(files))){

  cfile <- files[i]
  print(cfile)

  # op.file <- paste0(tools::file_path_sans_ext(cfile),".RDS")
  #
  # if (!overwrite & file.exists(op.files)){
  #   next()
  # }

  A <- readRDS(cfile)

  cyears <- unique(A$year)
  missing.years <- c(1900:2100)[(!(c(1900:2100) %in% cyears))]

  missing <- any(missing.years)

  if (missing){
    cfile.mod <- sub(pattern = "ssp534-over",
                     replacement = "ssp585",
                     cfile)

    if (file.exists(cfile.mod)){
      B <- readRDS(cfile.mod) %>%
        filter(year %in% missing.years)
    }

    mod <- bind_rows(A,B) %>%
      arrange(year)

    saveRDS(mod,
            cfile)

  }
}


# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Complete.Overshoot.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
