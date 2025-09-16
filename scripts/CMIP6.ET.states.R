rm(list = ls())

library(dplyr)
library(Rcpp)

files <- list.files("./outputs/",
                    "^Timeseries.MCWD.",full.names = TRUE)
files <- (files[!grepl(pattern = "historical",
                       files)])

states <- data.frame()

modeltas2tasmin <- readRDS("./outputs/tas2tasmin.RDS")
modeltas2tasmax <- readRDS("./outputs/tas2tasmax.RDS")

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

# states <- data.frame()

overwrite <- FALSE

for (ifile in seq(1,length(files))){

  cfile <- files[ifile]
  cmodel <- strsplit(basename(cfile),"\\.")[[1]][3]
  cscenario <- strsplit(basename(cfile),"\\.")[[1]][4]

  print(paste0(basename(cfile),": ",cmodel," - ",cscenario," - ",
               ifile/length(files)))


  op.file <- paste0("./outputs/All.CMIP6.states.timing.",
                    cmodel,".",cscenario,".RDS")

  if (!overwrite & file.exists(op.file)){
    next()
  }

  # ccdataset <- states %>%
  #   filter(model == cmodel,
  #          scenario == cscenario)
  #
  # if (nrow(ccdataset) > 0){
  #   next()
  # }



  if (file.exists(cfile)){
    A <- readRDS(cfile) %>%
      ungroup() %>%
      filter(abs(lat) <= 23.25) %>%
      filter(lon >= -20, lon <= 60)
  }

  # A.cat <- A %>%
  #   filter(year %in% c(1981:2010,
  #                      2071:2100)) %>%
  #   mutate(timing = case_when(year <= 2014 ~ "historical",
  #                             TRUE ~ "scenario"))

  A.cat <- A %>%
    filter(year %in% c(1981:2010,

                       2011:2040,
                       2041:2070,
                       2071:2100)) %>%
    mutate(timing = case_when(year <= 2014 ~ "historical",
                              year <= 2040 ~ "Near_future",
                              year <= 2070 ~ "Mid_future",
                              TRUE ~ "Long_future"))


  cvars <- colnames(A)

  if (!all(c("pr") %in% cvars)){
    next()
  }

  if (all(c("tasmin","tasmax","tas") %in% cvars)){

    print("Hargreaves")

    A.MCWD <- A.cat %>%
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
      group_by(timing,model,lon,lat,month) %>%
      summarise(Etot = mean(Etot,na.rm = TRUE),
                Pmm = mean(Pmm,na.rm = TRUE),
                tas = mean(tas,na.rm = TRUE),
                tasmin = mean(tasmin,na.rm = TRUE),
                tasmax = mean(tasmax,na.rm = TRUE),
                .groups = "keep") %>%
      ungroup() %>%
      mutate(diff = Pmm - Etot) %>%
      group_by(timing,model,lon,lat) %>%
      mutate(MCWD = computeMCWD(diff),
             MAP = sum(Pmm,na.rm = TRUE),
             Etot.yr = sum(Etot,na.rm = TRUE),
             MAT = mean(tas, na.rm = TRUE),
             MinAT =  mean(tasmin, na.rm = TRUE),
             MaxAT =  mean(tasmax, na.rm = TRUE),
             Emodel = "Hargreaves")


  } else if (all(c("tasmin","tasmax") %in% cvars)){

    print("Hargreaves2")

    A.MCWD <- A.cat %>%
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
      group_by(timing,model,lon,lat,month) %>%
      summarise(Etot = mean(Etot,na.rm = TRUE),
                Pmm = mean(Pmm,na.rm = TRUE),
                tasmin = mean(tasmin,na.rm = TRUE),
                tasmax = mean(tasmax,na.rm = TRUE),
                .groups = "keep") %>%
      ungroup() %>%
      mutate(diff = Pmm - Etot) %>%
      group_by(timing,model,lon,lat) %>%
      mutate(MCWD = computeMCWD(diff),
             MAP = sum(Pmm,na.rm = TRUE),
             Etot.yr = sum(Etot,na.rm = TRUE),
             MinAT =  mean(tasmin, na.rm = TRUE),
             MaxAT =  mean(tasmax, na.rm = TRUE),
             Emodel = "Hargreaves")


  } else if (all(c("tas") %in% cvars)) {

    print("Hargreaves3")

    A.cat[["tasmin"]] <- predict(modeltas2tasmin,
                                 data.frame(lon = A.cat$lon,
                                            lat = A.cat$lat,
                                            year = A.cat$year,
                                            month = A.cat$month,
                                            pr = A.cat$pr,
                                            tas = A.cat$tas))

    A.cat[["tasmax"]] <- predict(modeltas2tasmax,
                                 data.frame(lon = A.cat$lon,
                                            lat = A.cat$lat,
                                            year = A.cat$year,
                                            month = A.cat$month,
                                            pr = A.cat$pr,
                                            tas = A.cat$tas))

    A.MCWD <- A.cat %>%
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
      group_by(timing,model,lon,lat,month) %>%
      summarise(Etot = mean(Etot,na.rm = TRUE),
                Pmm = mean(Pmm,na.rm = TRUE),
                tas = mean(tas,na.rm = TRUE),
                tasmin = mean(tasmin,na.rm = TRUE),
                tasmax = mean(tasmax,na.rm = TRUE),
                .groups = "keep") %>%
      ungroup() %>%
      mutate(diff = Pmm - Etot) %>%
      group_by(timing,model,lon,lat) %>%
      mutate(MCWD = computeMCWD(diff),
             MAP = sum(Pmm,na.rm = TRUE),
             Etot.yr = sum(Etot,na.rm = TRUE),
             MAT = mean(tas, na.rm = TRUE),
             MinAT =  mean(tasmin, na.rm = TRUE),
             MaxAT =  mean(tasmax, na.rm = TRUE),
             Emodel = "Hargreaves_interp")

  } else {

    print("Constant")

    A.MCWD <- A.cat %>%
      group_by(year,model,lon,lat) %>%
      mutate(N = n()) %>%
      filter(N == 12) %>%
      mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
             Pmm = Ndays*pr*86400,
             Etot = 100) %>%
      dplyr::select(-pr) %>%
      group_by(timing,model,lon,lat,month) %>%
      summarise(Etot = mean(Etot,na.rm = TRUE),
                Pmm = mean(Pmm,na.rm = TRUE),
                .groups = "keep") %>%
      ungroup() %>%
      mutate(diff = Pmm - Etot) %>%
      group_by(timing,model,lon,lat) %>%
      mutate(MCWD = computeMCWD(diff),
             MAP = sum(Pmm,na.rm = TRUE),
             Etot.yr = sum(Etot,na.rm = TRUE),
             Emodel = "Constant")

  }

  # states <- bind_rows(states,
  #                     A.MCWD %>%
  #                       mutate(model = cmodel,
  #                              scenario = cscenario))

  saveRDS(A.MCWD %>%
            mutate(model = cmodel,
                   scenario = cscenario),
          op.file)
}

# saveRDS(states,
#         "./outputs/All.CMIP6.states.timing.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/CMIP6.ET.states.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
