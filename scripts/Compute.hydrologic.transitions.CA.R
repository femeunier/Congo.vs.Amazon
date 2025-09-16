rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xgboost)
library(Rcpp)

files <- list.files("./outputs/",
                    "^Timeseries.MCWD",full.names = TRUE)
files <- (files[!grepl(pattern = "historical",
                       files)])

MCWD.threshold <- -450 ; MAP.threshold <- 1000

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


Emodel <- "Hargreaves"

df.all <- data.frame()

for (ifile in seq(1,length(files))){

  print(ifile/length(files))

  cfile <- files[ifile]

  cmodel <- strsplit(basename(cfile),"\\.")[[1]][3]
  cscenario <- strsplit(basename(cfile),"\\.")[[1]][4]
  cEmodel <- Emodel

  Timeseries <-readRDS(cfile) %>%
    ungroup() %>%
    filter(lon >= -20, lon <= 60,
           lat <= 10, lat >= -15) %>%
    filter(year %in% c(1981:2010,
                       2071:2100)) %>%
    mutate(timing = case_when(year < 2050 ~ "init",
                              TRUE ~ "end"))

  if (!all(c("init","end") %in% c(unique(Timeseries$timing)))){
    next()
  }

  cvars <- colnames(Timeseries)

  if (!all(c("pr") %in% cvars)){
    next()
  }

  if (!all(c("tasmax") %in% cvars)){
    if (!(c("tas") %in% cvars)){
      next()
    }

    dnew <- xgb.DMatrix(data = as.matrix(Timeseries[, c("lon", "lat", "year", "month", "pr", "tas")]))
    Timeseries[["tasmax"]] <- predict(modeltas2tasmax,dnew)
    cEmodel <- "Hargreaves_interp"
  }

  if (!all(c("tasmin") %in% cvars)){
    if (!(c("tas") %in% cvars)){
      next()
    }

    dnew <- xgb.DMatrix(data = as.matrix(Timeseries[, c("lon", "lat", "year", "month", "pr", "tas")]))
    Timeseries[["tasmin"]] <- predict(modeltas2tasmin,dnew)
    cEmodel <- "Hargreaves_interp"
  }

  Timeseries.MCWD <- Timeseries %>%
    arrange(timing,year,month,lon,lat) %>%
    group_by(timing,model,lon,lat,year) %>%
    mutate(N = n()) %>%
    filter(N == 12) %>%
    group_by(timing,lon,lat,year) %>%
    mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31)) %>%
    group_by(timing,lon,lat,month) %>%
    summarise(tasmin = mean(tasmin,na.rm = TRUE),
              tasmax = mean(tasmax,na.rm = TRUE),
              pre = mean(Ndays)*86400*mean(pr,na.rm = TRUE),
              Ndays = mean(Ndays,na.rm = TRUE),
              .groups = "keep") %>%
    mutate(E = SPEI::hargreaves(tasmin,
                                tasmax,
                                lat = unique(lat),
                                Ra = NULL,
                                na.rm = TRUE,
                                verbose = FALSE)/Ndays,
           Etot = E*Ndays) %>%
  mutate(diff = pre - Etot) %>%
  group_by(timing,lon,lat) %>%
  mutate(MCWD = computeMCWD(diff),
         MAP = sum(pre,na.rm = TRUE))

  Timeseries.MCWD.cat <- Timeseries.MCWD %>%
    filter(month == 1) %>%
    dplyr::select(-c(month,Ndays)) %>%
    mutate(LC = case_when(MAP <= MAP.threshold ~ 1,
                          MCWD >= MCWD.threshold ~ 2,
                          TRUE ~ 3))

  Timeseries.MCWD.cat.change <- Timeseries.MCWD.cat %>%
    dplyr::select(timing,lon,lat,LC) %>%
    pivot_wider(names_from = timing,
                values_from = LC) %>%
    mutate(change = case_when(init == end ~ FALSE,
                              TRUE ~ TRUE))

  variable.changes <- Timeseries.MCWD.cat %>%
    ungroup() %>%
    pivot_wider(names_from = timing,
                values_from = -c(lon,lat,timing)) %>%
    mutate(LC_change = (LC_init != LC_end),
           Delta_MCWD = MCWD_end-MCWD_init,
           Delta_MAP = MAP_end - MAP_init) %>%
    mutate(seasonality = case_when(Delta_MCWD < 0 ~ "Increase",
                                   Delta_MCWD >= 0 ~ "Decrease"))

  df.all <- bind_rows(df.all,
                      variable.changes %>%
                        mutate(model = cmodel,
                               Emodel = cEmodel,
                               scenario = cscenario))

}

saveRDS(df.all,
        "./outputs/All.var.changes.CB.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Compute.hydrologic.transitions.CA.R hpc:/data/gent/vo/000/gvo00074/felicien/R/


