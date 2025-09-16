rm(list = ls())

library(dplyr)
library(YGB)
library(ggplot2)
library(zoo)
library(Rcpp)
library(xgboost)

files <- list.files("./outputs/",
                    pattern = "^Timeseries.MCWD.*",
                    full.names = TRUE)
files <- (files[!grepl(pattern = "historical",
                       files)])
# files <- rev(files)
# files <- (files[grepl(pattern = "TaiESM",
#                       files)])

Train <- readRDS("./outputs/tas2tasmin.RDS")
modeltas2tasmin <- Train$finalModel

Train <- readRDS("./outputs/tas2tasmax.RDS")
modeltas2tasmax <- Train$finalModel


cppFunction('
double computeMCWD(NumericVector diff) {
  int n = diff.size();
  if (n != 12) stop("Input must be length 12.");

  // Step 1: Find the wettest month (max diff)
  int wettest_month = 0;
  double max_val = diff[0];
  for (int i = 1; i < n; ++i) {
    if (diff[i] > max_val) {
      max_val = diff[i];
      wettest_month = i;
    }
  }

  // Step 2: Reorder months so that wettest month is first
  NumericVector diff_ord(n);
  for (int i = 0; i < n; ++i) {
    int m = (i - wettest_month + n) % n;  // shifted index
    diff_ord[i] = diff[m];
  }

  // Step 3: Compute CWD from reordered diff
  NumericVector CWD(n);
  CWD[0] = std::min(0.0, diff_ord[0]);
  double prev = CWD[0];
  for (int i = 1; i < n; ++i) {
    CWD[i] = std::min(0.0, diff_ord[i] + prev);
    prev = CWD[i];
  }

  // Step 4: Return MCWD (minimum of CWD)
  double mcwd = CWD[0];
  for (int i = 1; i < n; ++i) {
    if (CWD[i] < mcwd) {
      mcwd = CWD[i];
    }
  }
  return mcwd;
}
')

overwrite <- FALSE
for (ifile in seq(1,length(files))){

  cfile <- files[ifile]

  op.file <-paste0("./outputs/Roll.",basename(cfile))

  print(paste0(basename(cfile)," - ",ifile/length(files)))

  if (!overwrite & file.exists(op.file)){
    next()
  }

  A <- tryCatch(readRDS(cfile) %>%
                  arrange(lon,lat,year,month),
                 error = function(e) NULL)

  if (is.null(A)){
    next()
  }

  cvars <- colnames(A)

  if (!all(c("pr") %in% cvars)){
    next()
  }

  if (all(c("tasmin","tasmax","tas") %in% cvars)){

    print("Hargreaves")

    A.MCWD <- A %>%
      group_by(year,model,lon,lat) %>%
      mutate(N = n()) %>%
      filter(N == 12) %>%
      mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
             E = SPEI::hargreaves(tasmin,
                                  tasmax,
                                  lat = unique(lat),
                                  Ra = NULL,
                                  na.rm = TRUE,
                                  verbose = FALSE)/Ndays,
             Pmm = Ndays*pr*86400,
             Etot = E*Ndays) %>%
      dplyr::select(-pr) %>%
      mutate(diff = Pmm - Etot) %>%
      group_by(model,lon,lat) %>%
      mutate(MCWD = rollapply(diff, 12,
                              function(x) computeMCWD(x),
                              fill = NA,
                              align = c("right")))  %>%
      mutate(MAP = rollapply(Pmm, 12, sum,align = "right", fill = NA),
             Etot = rollapply(Etot, 12, sum,align = "right", fill = NA),
             MAT = rollapply(tas, 12, mean,align = "right", fill = NA),
             MinAT = rollapply(tasmin, 12, mean,align = "right", fill = NA),
             MaxAT = rollapply(tasmax, 12, mean,align = "right", fill = NA))

    saveRDS(A.MCWD %>%
              dplyr::select(-c(variant,period,
                               tas,tasmin,tasmax,Ndays,E,Pmm,diff)),
            op.file)


  } else if (all(c("tasmin","tasmax") %in% cvars)){

    print("Hargreaves 2")

    A.MCWD <- A %>%
      group_by(year,model,lon,lat) %>%
      mutate(N = n()) %>%
      filter(N == 12) %>%
      mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
             E = SPEI::hargreaves(tasmin,
                                  tasmax,
                                  lat = unique(lat),
                                  Ra = NULL,
                                  na.rm = TRUE,
                                  verbose = FALSE)/Ndays,
             Pmm = Ndays*pr*86400,
             Etot = E*Ndays) %>%
      dplyr::select(-pr) %>%
      mutate(diff = Pmm - Etot) %>%
      group_by(model,lon,lat) %>%
      mutate(MCWD = rollapply(diff, 12,
                              function(x) computeMCWD(x),
                              fill = NA,
                              align = c("right")))  %>%
      mutate(MAP = rollapply(Pmm, 12, sum,align = "right", fill = NA),
             Etot = rollapply(Etot, 12, sum,align = "right", fill = NA),
             MinAT = rollapply(tasmin, 12, mean,align = "right", fill = NA),
             MaxAT = rollapply(tasmax, 12, mean,align = "right", fill = NA))

    saveRDS(A.MCWD %>%
              dplyr::select(-c(variant,period,
                               tasmin,tasmax,Ndays,E,Pmm,diff)),
            op.file)
    } else if (all(c("tas") %in% cvars)) {

      print("Hargreaves 3")

      dnew <- xgb.DMatrix(data = as.matrix(A[, c("lon", "lat", "year", "month", "pr", "tas")]))


      A[["tasmin"]] <- predict(modeltas2tasmin,
                               dnew)

      A[["tasmax"]] <- predict(modeltas2tasmax,
                               dnew)

      # stop()

      A.MCWD <- A %>%
        group_by(year,model,lon,lat) %>%
        mutate(N = n()) %>%
        filter(N == 12) %>%
        mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
               E = SPEI::hargreaves(tasmin,
                                    tasmax,
                                    lat = unique(lat),
                                    Ra = NULL,
                                    na.rm = TRUE,
                                    verbose = FALSE)/Ndays,
               Pmm = Ndays*pr*86400,
               Etot = E*Ndays) %>%
        dplyr::select(-pr) %>%
        mutate(diff = Pmm - Etot) %>%
        group_by(model,lon,lat) %>%
        mutate(MCWD = rollapply(diff, 12,
                                function(x) computeMCWD(x),
                                fill = NA,
                                align = c("right")))  %>%
        mutate(MAP = rollapply(Pmm, 12, sum,align = "right", fill = NA),
               Etot = rollapply(Etot, 12, sum,align = "right", fill = NA),
               MAT = rollapply(tas, 12, mean,align = "right", fill = NA),
               MinAT = rollapply(tasmin, 12, mean,align = "right", fill = NA),
               MaxAT = rollapply(tasmax, 12, mean,align = "right", fill = NA))

      saveRDS(A.MCWD %>%
                dplyr::select(-c(variant,period,
                                 tas,tasmin,tasmax,Ndays,E,Pmm,diff)),
              op.file)


  } else {

    print("Constant")

    A.MCWD <- A %>%
      group_by(year,model,lon,lat) %>%
      mutate(N = n()) %>%
      filter(N == 12) %>%
      mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
             Pmm = Ndays*pr*86400,
             Etot = 100) %>%
      dplyr::select(-pr) %>%
      mutate(diff = Pmm - Etot) %>%
      group_by(model,lon,lat) %>%
      mutate(MCWD = rollapply(diff, 12,
                              function(x) computeMCWD(x),
                              fill = NA,
                              align = c("right")))  %>%
      mutate(MAP = rollapply(Pmm, 12, sum,align = "right", fill = NA),
             Etot = rollapply(Etot, 12, sum,align = "right", fill = NA))

    saveRDS(A.MCWD %>%
              dplyr::select(-c(variant,period,Ndays,Pmm,diff)),
            op.file)

  }
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Rolling.timeseries.MCWD.scenarios.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R




