rm(list = ls())

library(dplyr)
library(YGB)
library(Rcpp)
library(ggplot2)
library(zoo)
library(roll)
library(data.table)
library(tidyr)

files <- list.files("./outputs/",
                    "^Roll.Timeseries.MCWD",full.names = TRUE)
files <- (files[!grepl(pattern = "historical",
                       files)])

roll_freq <- function(vec, val, window) {
  vec <- as.integer(vec)
  is_valid <- !is.na(vec)
  match_val <- as.integer(vec == val)
  match_val[!is_valid] <- 0
  is_valid_int <- as.integer(is_valid)

  # Cumulative sums
  cs_match <- c(0, cumsum(match_val))
  cs_valid <- c(0, cumsum(is_valid_int))

  # Indices
  n <- length(vec)
  idx_start <- pmax(1, seq_len(n) - window + 1)

  # Vectorized window sums using start and end index
  count <- cs_match[seq_len(n) + 1] - cs_match[idx_start]
  total <- cs_valid[seq_len(n) + 1] - cs_valid[idx_start]

  # Frequency (with NA where total is 0)
  freq <- ifelse(total == 0, NA_real_, count / total)
  return(freq)
}

overwite <- FALSE

for (ifile in seq(1,length(files))){

  cfile <- files[ifile]
  cmodel <- strsplit(basename(cfile),"\\.")[[1]][4]
  cscenario <- strsplit(basename(cfile),"\\.")[[1]][5]

  print(paste0(basename(cfile),": ",cmodel," - ",cscenario," - ",
               ifile/length(files)))


  op.file <- file.path("./outputs/",
                       paste0("Transitions.",cmodel,".",cscenario,".RDS"))
  op.file2 <- file.path("./outputs/",
                        paste0("Changes.",cmodel,".",cscenario,".RDS"))

  if (!overwite & all(file.exists(op.file,op.file2))){
    next()
  }

  if (file.exists(cfile)){
    A <- readRDS(cfile) %>%
      ungroup() %>%
      filter(abs(lat) <= 23.25) %>%
      filter(lat <= 10,lat >= -15,
           lon > -20,lon <= 60)
  }

  A.cat <- A %>%
    # filter(year == 2000, month == 1) %>%
    # mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
    mutate(LC = case_when(MAP <= 1000 ~ 3,
                          MCWD >= -400 ~ 1,
                          TRUE ~ 2))

  A.sum <- A.cat %>%
    dplyr::select(lon,lat,year,month,scenario,model,LC,MAP,MCWD)

  A.cat.dt <- as.data.table(A.sum)

  setorder(A.cat.dt,
           model, scenario, lon, lat, year, month)

  A.cat.dt[, paste0("LC.", 1:3) := lapply(1:3, function(x) roll_freq(LC, x, 360)),
           by = .(model, lon, lat)]

  A.cat.dt.lon <- melt(
    A.cat.dt[, !"LC"],                           # Drop original `cat` column
    measure.vars = patterns("^LC\\.[1234567]$"), # Select cat.1, cat.2, cat.3
    variable.name = "LC",                        # Name for new 'cat' column
    value.name = "value"                            # Name for values
  )


  A.cat.dt.lon[, LC := as.integer(gsub("LC\\.", "", LC))]


  if (!all(c(2000,2085) %in% unique(A.cat$year))){
    next()
  }

  transitions <- A.cat %>%
    filter((year == 2000 & month == 1) |
             (year == 2085 & month == 1)) %>%
    mutate(period = case_when(year == 2000 ~ "init",
                              TRUE ~ "end")) %>%
    dplyr::select(-any_of(c("year","month","scenario","N",
                            "Etot","MCWD","MAP","MAT",
                            "MinAT","MaxAT","AI","Etotr"))) %>%
    pivot_wider(names_from = period,
                values_from = LC)

  nochanges <- transitions %>%
    filter(init == end) %>%
    mutate(lon.lat = paste0(lon,".",lat))

  changes <- transitions %>%
    filter(init != end) %>%
    mutate(lon.lat = paste0(lon,".",lat))

  # all.changes <- bind_rows(all.changes,
  #                          bind_rows(changes %>%
  #                                      mutate(type = "changes"),
  #                                    nochanges %>%
  #                                      mutate(type = "nochanges")) %>%
  #                            mutate(model = cmodel,
  #                                   scenario = cscenario))

  saveRDS(bind_rows(changes %>%
                      mutate(type = "changes"),
                    nochanges %>%
                      mutate(type = "nochanges")) %>%
            mutate(model = cmodel,
                   scenario = cscenario),
          op.file2)

  A.cat.dt.lon[, lon.lat := paste0(lon, ".", lat)]  # Efficient column creation
  filtered_dt <- A.cat.dt.lon[lon.lat %in%
                                c(nochanges[["lon.lat"]])] %>%
    left_join(nochanges %>%
                dplyr::select(lon,lat,end),
              by = c("lon","lat"))

  A.cat.dt.lon.sum.forests <- filtered_dt[, .(
    value.m = mean(value, na.rm = TRUE),
    value.sd = sd(value, na.rm = TRUE),
    MCWD.m = mean(MCWD,na.rm = TRUE),
    MAP.m = mean(MAP,na.rm = TRUE)
  ), by = .(year, month, end,LC)]

  saveRDS(A.cat.dt.lon.sum.forests %>%
            mutate(model = cmodel,
                   scenario = cscenario),
          op.file)

}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Compute.transitions.CA.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
