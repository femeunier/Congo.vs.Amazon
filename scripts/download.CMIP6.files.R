rm(list = ls())

library(epwshiftr)
library(dplyr)
library(tidyr)
library(lubridate)

scenarios = rev(c("historical","ssp126","ssp245","ssp370","ssp585",
               "ssp534-over"))
variables = c("tas","pr","tasmin","tasmax")
variants = "r1i1p1f1"
year.min = 1900 ; year.max = 2100
overwrite = FALSE

########################################################################################################

activities <- rep("ScenarioMIP",length(scenarios))
activities[scenarios %in% c("historical","1pctCO2")] <- "CMIP"
activities[grepl("land",scenarios)] <- "LUMIP"
activities[grepl("esm-ssp534-over",scenarios)] <- "CDRMIP"
activities[grepl("ssp534-over-bgc",scenarios)] <- "C4MIP"

df.check <- data.frame()
for (iscenario in seq(1,length(scenarios))){
  cdf.check <- data.frame()
  for (cvariable in variables){

    print(paste0(scenarios[iscenario]," - ",cvariable))

    # pr <- init_cmip6_index()

    pr <- esgf_query_all(
      activity = activities[iscenario],
      variable = cvariable,
      frequency = "mon",
      experiment = scenarios[iscenario],
      type = "File",
      source = NULL,
      variant = NULL,
      replica = FALSE,
      latest = TRUE,
      resolution = NULL,
      limit = 10000L,
      data_node = NULL) %>%
      distinct() %>%
      filter(year(datetime_start) <= year.max,
             year(datetime_end) >= year.min)

   if (nrow(pr) == 0){
     next()
   }

    cdf.check <- bind_rows(cdf.check,
                           pr %>%
                            dplyr::select(experiment_id,source_id,variable_id) %>%
                            distinct() )
  }


   if (nrow(cdf.check) == 0){
     next()
   }



  df.check <- bind_rows(df.check,
                        cdf.check %>%
    mutate(present = TRUE) %>%
    pivot_wider(names_from = c(variable_id),
                values_from = present,
                values_fill = FALSE) %>%
    mutate(all.present = (tas & pr) | (tasmin & tasmax & pr))) #%>%
    # filter(all.present))
}

df.check.wide <- df.check %>%
  dplyr::select(-c(tas,tasmin,tasmax,pr)) %>%
  pivot_wider(names_from = experiment_id,
              values_from = all.present,
              values_fill = FALSE)

models2download <- df.check.wide %>%
  # dplyr::filter(historical & ssp126 & ssp245 & ssp370 & ssp585) %>%
  pull(source_id) %>%
  unique() %>% sort()


########################################################################################################
# Functions

download_size <- function(url) {
  as.numeric(httr::HEAD(url)$headers$`content-length`)
}

retry.func <- function (expr, isError = function(x) inherits(x, "try-error"),
                        maxErrors = 5, sleep = 0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]",
                    utils::capture.output(utils::str(retval)))
      # warning(msg)
      return(0)
    }
    else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]",
                    attempts, maxErrors, utils::capture.output(utils::str(retval)))
      # warning(msg)
    }
    if (sleep > 0)
      Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(1)
}

options(timeout=6000)

# data.nodes.status <- get_data_node()

######################################################################################################################


for (iscenario in seq(1,length(scenarios))){
  for (ivar in seq(1,length(variables))){

    print(paste("--- Downloading files for scenario ",scenarios[iscenario], " and variable ",variables[ivar]))

    dest.dir.scenar <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6",
                                 scenarios[iscenario])
    dir.create(dest.dir.scenar,showWarnings = FALSE)

    dest.dir <- file.path(dest.dir.scenar,variables[ivar])
    dir.create(dest.dir,showWarnings = FALSE)


    # pr <- init_cmip6_index(activity = activities[iscenario],
    #                        variable = variables[ivar],
    #                        frequency = 'mon',
    #                        experiment = scenarios[iscenario],
    #                        source = NULL,
    #                        variant = variants,
    #                        replica = FALSE,
    #                        latest = TRUE,
    #                        resolution = NULL,
    #                        limit = 10000L,
    #                        data_node = NULL)

    pr <- esgf_query_all(
      activity = activities[iscenario],
      variable = variables[ivar],
      frequency = 'mon',
      experiment = scenarios[iscenario],
      source = NULL,
      type = "File",
      variant = variants,
      replica = FALSE,
      latest = TRUE,
      resolution = NULL,
      limit = 10000L,
      data_node = NULL) %>%
      distinct() %>%
      filter(year(datetime_start) <= year.max,
             year(datetime_end) >= year.min)


    if (nrow(pr) == 0) next()


    files2download.prc <- pr %>%
      # filter(member_id %in% variants) %>%
      filter(source_id %in% c(models2download))

    if (nrow(files2download.prc) == 0) next()

    models <- sort(unique(files2download.prc$source_id))
    print(unique(files2download.prc$source_id))

    for (imodel in seq(1, length(models))){

      cmodel <- models[imodel]
      cdf <- files2download.prc %>%
        filter(source_id == cmodel)

      if (nrow(cdf) == 0) next()

      for (i in seq(1,nrow(cdf))){

        dest.file <- file.path(dest.dir,
                               basename(cdf$file_url[i]))

        if (overwrite | !file.exists(dest.file) | file.info(dest.file)[["size"]] != cdf$file_size[i]){

          dumb <- retry.func(utils::download.file(url = cdf$file_url[i],
                                                  destfile = dest.file),
                             maxErrors = 2,
                             sleep = 0)

          if (dumb == 0){
            break
          }
        }
      }
    }
  }
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/download.CMIP6.files.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
