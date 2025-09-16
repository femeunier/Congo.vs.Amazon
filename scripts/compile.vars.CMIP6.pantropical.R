rm(list = ls())

library(epwshiftr)
library(dplyr)
library(RNetCDF)
library(stringr)
library(ncdf4)
library(ncdf4.helpers)
library(reshape2)
library(lubridate)
library(CongoAS)
library(tidyr)

scenarios = (c("ssp534-over"))
variables = (c("tasmax","pr","tas","tasmin"))
variants = "r1i1p1f1" # NULL

overwrite = FALSE

################################################################################

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
  return(retval)
}

################################################################################

activities <- rep("ScenarioMIP",length(scenarios))
activities[scenarios %in% c("historical","1pctCO2")] <- "CMIP"
activities[grepl("land",scenarios)] <- "LUMIP"
df.all <- data.frame()

# for (iscenario in seq(1,length(scenarios))){
#   for (ivar in seq(1,length(variables))){
#
#     variable <- variables[ivar]
#
#     dest.dir.scenar <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6",
#                                  scenarios[iscenario])
#     dest.dir <- file.path(dest.dir.scenar,variable)
#
#     cscenario <- scenarios[iscenario]
#
#     pr <- init_cmip6_index(activity = activities[iscenario],
#                            variable = variable,
#                            frequency = 'mon',
#                            experiment = scenarios[iscenario],
#                            source = NULL,
#                            variant = variants,
#                            replica = FALSE,
#                            latest = TRUE,
#                            resolution = NULL,
#                            limit = 10000L,
#                            data_node = NULL)
#
#     if (nrow(pr) == 0) next()
#
#     files2download.prc <- pr
#
#     # Add model loop
#
#     print(paste("--- Looking for files for scenario ",scenarios[iscenario], " and variable ",variable))
#
#     models <- unique(files2download.prc$source_id)
#
#     for (imodel in seq(1, length(models))){
#
#       cmodel <- models[imodel]
#       print(cmodel)
#       cdf <- files2download.prc %>%
#         filter(source_id == cmodel)
#
#       if (nrow(cdf) == 0) next()
#
#       dest.files <- file.path(dest.dir,
#                               basename(cdf$file_url))
#
#       if (all(file.exists(dest.files))){
#         df.all <- bind_rows(df.all,
#                             data.frame(model = cmodel,
#                                        var = variable,
#                                        scenario = cscenario))
#       }
#     }
#   }
# }
#
# df.all.wide <- df.all %>%
#   mutate(exist = TRUE) %>%
#   pivot_wider(values_from = exist,
#               names_from = scenario)
#
# # df.selected <- df.all.wide %>%
# #   filter(historical,
# #          ssp126,
# #          ssp245,
# #          ssp370,
# #          ssp585) %>%
# #   ungroup() %>%
# #   dplyr::select(model,var) %>%
# #   mutate(exist = TRUE) %>%
# #   pivot_wider(names_from = var,
# #               values_from = exist) %>%
# #   filter(tas,pr,tasmin,tasmax)
#
# # sources <- df.selected[["model"]]
#
# sources <- unique(df.all.wide[["model"]])

for (iscenario in seq(1,length(scenarios))){
  for (ivar in seq(1,length(variables))){

    variable <- variables[ivar]

    dest.dir.scenar <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6",
                                 scenarios[iscenario])

    dest.dir <- file.path(dest.dir.scenar,variable)

        pr <- esgf_query(
      activity = activities[iscenario],
      variable = variable,
      frequency = 'mon',
      experiment = scenarios[iscenario],
      source = NULL,
      type = "File",
      variant = variants,
      replica = FALSE,
      latest = TRUE,
      resolution = NULL,
      limit = 10000L,
      data_node = NULL)



    if (nrow(pr) == 0) next()

    files2download.prc <- pr

    print(paste("--- Looking for files for scenario ",scenarios[iscenario], " and variable ",variable))

    models <- unique(files2download.prc$source_id)

    for (imodel in seq(1,length(models))){

      cmodel <- models[imodel]
      cvariants <- sort(unique(files2download.prc %>%
                                 filter(source_id == cmodel) %>%
                                 pull(member_id)))

      for (ivariant in seq(1,length(cvariants))){

        cvariant <- cvariants[ivariant]
        print(paste0(cmodel," - ",cvariant))

        OP.file <- paste0("./outputs/","CMIP6.monthly.",variable,".pantropical.",scenarios[iscenario],".",cmodel,".",cvariant,".RDS")
        # OP.file <- paste0("./outputs/","df.monthly.",variable,".pantropical.",scenarios[iscenario],".",cmodel,".RDS")

        if (!overwrite & file.exists(OP.file)){
          next()
        }

        dest.files <- file.path(dest.dir,
                                basename(files2download.prc %>%
                                           filter(source_id == cmodel,
                                                  member_id == cvariant) %>%
                                           pull(file_url)))

        if (all(file.exists(dest.files))){

          tmp <- tryCatch(read.and.filter.ncfiles(ncfiles = dest.files,
                                         coord.analysis = continent2coord("World"),
                                         var = variable,
                                         aggr = FALSE) %>%
            filter(year %in% 1900:2100) %>%
            filter(abs(lat) <= 30) %>%
            dplyr::select(-time0),
            error = function(e) NULL)

          if(is.null(tmp)) next()

          if (nrow(tmp) == 0) next()

          df.month <- tmp %>%
            mutate(scenario = scenarios[iscenario],
                   variant = cvariant)

          saveRDS(df.month,
                  OP.file)

        }
      }
    }
  }
}


# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/compile.vars.CMIP6.pantropical.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
