run.Caret.IFL <- function(config.file,
                          shap.test = TRUE){

  # x_var <- c("tmp","tmin","tmax","dswrf","vpd","co2anomaly","pre")
  # y_var <- "gppanomaly"
  # fac.CC <- 86400*365
  # year.min <- 1980
  # year.max <- 2025
  # lags <- 12
  # initial <- 432
  # horizon <- 12
  # skip <- 11

  set.seed(1234)

  config <- readRDS(config.file)

  x_var <- config[["x_var"]]
  y_var <- config[["y_var"]]
  fac.CC <- config[["fac.CC"]]
  year.min <- config[["year.min"]]
  year.max <- config[["year.max"]]
  lags <- config[["lags"]]
  initial <- config[["initial"]]
  horizon <- config[["horizon"]]
  skip <- config[["skip"]]

  Grid <- config[["Grid"]]

  climate.location <- config[["climate.location"]]
  CC.location <- config[["CC.location"]]

  IFL <- config[["IFL"]]

  Ntest.month <- config[["Ntest.month"]]

  dest.dir <- config[["dest.dir"]]

  include.past.lag <- config[["include.past.lag"]]
  if (is.null(include.past.lag)){
    include.past.lag <- FALSE
  }

  suffix <- config[["suffix"]]
  if (is.null(suffix)){
    suffix <- ""
  }

  rolls <- config[["rolls"]]
  sums <- config[["sums"]]

  pos <- config[["pos"]]

  ##############################################################################

  climate.list <- list()
  all_x_var <- c("co2anomaly","CO2")
  for (cclimate.location in climate.location){

    all_x_var <- c(all_x_var,
                   paste0(basename(cclimate.location),x_var))
    for (cvar in x_var){
      climate.files <- list.files(path = dirname(cclimate.location),
                                  pattern = paste0("^",
                                                   basename(cclimate.location),cvar,
                                                   ".*.tif$"),
                                  full.names = TRUE,
                                  ignore.case = TRUE)
      if (length(climate.files) == 0) next()
      cclimate <- rast(climate.files)
      climate.list[[paste0(basename(cclimate.location),cvar)]] <-  cclimate
    }
  }

  climate <- rast(climate.list)
  names(climate) <- tolower(sapply(strsplit(names(climate),"\\_"),"[[",1))
  climate.years <- as.numeric(unlist(lapply(strsplit(names(cclimate),"_|\\."),"[[",1)))
  climate.months <- as.numeric(unlist(lapply(strsplit(names(cclimate),"_|\\."),"[[",2)))

  cc.files <- list.files(path = dirname(CC.location),
                         pattern = paste0("^",
                                          basename(CC.location),
                                          ".*.tif$"),
                         full.names = TRUE)
  cc.rspld <- rast(cc.files)
  cnames <- names(cc.rspld)

  cc.years <- as.numeric(unlist(lapply(strsplit((basename(cnames)),"_|\\."),"[[",1)))
  cc.months <- as.numeric(unlist(lapply(strsplit((basename(cnames)),"_|\\."),"[[",2)))

  names(cc.rspld) <- rep(y_var,nlyr(cc.rspld))

  CO2 <- read.table("/kyukon/data/gent/vo/000/gvo00074/felicien/R/data/global_co2_ann_1700_2024.txt") %>%
    rename(year = V1,
           CO2 = V2)

  monthly_df <- expand.grid(
    month = 1:12,
    year = CO2$year) %>%
    arrange(year, month) %>%
    mutate(year_decimal = year + (month - 0.5) / 12)

  f <- splinefun(CO2$year, CO2$CO2, method = "natural")
  monthly_df$CO2 <- f(monthly_df$year_decimal)

  monthly_df <- deseason_detrend(monthly_df, year.min = 1960, year.max = 1990) %>%
    dplyr::select(year,month,CO2,co2detrended,co2anomaly)

  # We extract

  loc.coords <- terra::vect(IFL %>%
                              dplyr::select(lon,lat),
                            geom = c("lon", "lat"))

  temp.climate <- terra::extract(climate,
                                 loc.coords)

  temp.climate.df <- as.data.frame(t(temp.climate)[-1,])
  temp.climate.df[["variable"]] <- paste0(sapply(strsplit(rownames(temp.climate.df),"\\."),"[[",1),
                                          ".",
                                          sapply(strsplit(rownames(temp.climate.df),"\\."),"[[",2))

  cdf.climate <- temp.climate.df %>%
    pivot_longer(cols = -variable,
                 names_to = "site",
                 values_to = "value") %>%
    mutate(site.num = as.numeric(str_extract(site, "\\d+"))) %>%
    mutate(lon = crds(loc.coords)[site.num,"x"],
           lat = crds(loc.coords)[site.num,"y"]) %>%
    group_by(variable,lon,lat) %>%
    mutate(timing = 1:n()) %>%
    ungroup() %>%
    mutate(year = climate.years[timing],
           month = climate.months[timing]) %>%
    dplyr::select(-c(site,site.num,timing))

  cdf.climate.wide <- cdf.climate %>%
    pivot_wider(names_from = "variable",
                values_from = "value")

  cdf.climate.sum <- cdf.climate %>%
    group_by(year,month,variable) %>%
    summarise(m = mean(value),
              .groups = "keep")

  temp.cc <- terra::extract(cc.rspld,
                            loc.coords)

  temp.cc.df <- as.data.frame(t(temp.cc)[-1,])
  temp.cc.df[["variable"]] <- sapply(strsplit(rownames(temp.cc.df),"\\."),"[[",1)

  cdf.cc <- temp.cc.df %>%
    pivot_longer(cols = -variable,
                 names_to = "site",
                 values_to = "value") %>%
    mutate(site.num = as.numeric(str_extract(site, "\\d+"))) %>%
    mutate(lon = crds(loc.coords)[site.num,"x"],
           lat = crds(loc.coords)[site.num,"y"]) %>%
    group_by(variable,lon,lat) %>%
    mutate(timing = 1:n()) %>%
    ungroup() %>%
    mutate(year = cc.years[timing],
           month = cc.months[timing]) %>%
    dplyr::select(-c(site,site.num,timing)) %>%
    mutate(value = value*fac.CC)

  cdf.cc.wide <- cdf.cc %>%
    pivot_wider(names_from = "variable",
                values_from = "value")

  cdf.cc.sum <- cdf.cc %>%
    group_by(year,month,variable) %>%
    summarise(m = mean(value),
              .groups = "keep")

  merged <- cdf.climate.wide %>%
    left_join(cdf.cc.wide,
              by = c("year","month","lon","lat")) %>%
    left_join(monthly_df,
              by = c("year","month")) %>%
    dplyr::filter(year >= year.min,
                  year <= year.max) %>%
    arrange(year, month, lon, lat) %>%
    mutate(tnum = year*12L + month)  %>%
    arrange(tnum,lon,lat)

  time_vals <- sort(unique(merged$tnum))
  all <- merged  %>%
    dplyr::select(any_of(c("lon","lat","tnum",all_x_var,y_var)))

  df <- all %>%
    dplyr::select(-any_of(c("lon_lat","year","month"))) %>%
    arrange(tnum,lon,lat)

  if (!is.null(rolls) | !is.null(sums)){
    temp.df <- df
  }


  if (!is.null(rolls)){
    for (croll in rolls){

      print(paste0("Adding rolling mean variable -",croll))

      df <- cbind(df,
                  temp.df %>%
                    group_by(lon, lat) %>%
                    arrange(tnum, .by_group = TRUE) %>%
                    mutate(across(
                      -any_of(c("lon", "lat", "tnum")),
                      ~ slide_dbl(.x, mean, .before = (croll - 1), .complete = FALSE),
                      .names = paste0("{.col}_roll",croll))) %>%
                    ungroup() %>%
                    dplyr::select(ends_with(paste0("_roll",croll))))
    }
  }


  if (!is.null(sums)){
    for (csum in sums){

      print(paste0("Adding rolling sum variable -",csum))

      df <- cbind(df,
                  temp.df %>%
                    group_by(lon, lat) %>%
                    arrange(tnum, .by_group = TRUE) %>%
                    mutate(across(
                      -any_of(c("lon", "lat", "tnum")),
                      ~ slide_dbl(.x, sum, .before = (csum - 1), .complete = FALSE),
                      .names = paste0("{.col}_sum",csum))) %>%
                    ungroup() %>%
                    dplyr::select(ends_with(paste0("_sum",csum))))
    }
  }

  all.tnum <- df[["tnum"]]

  if (is.null(pos)){
    pos <- sample((lags+1):(length(time_vals)-(Ntest.month-1)),1)
  }

  all.test.pos <- pos:(pos+Ntest.month-1)
  test_ind <- which(all.tnum %in% time_vals[all.test.pos])
  all.buffer.pos <- c((pos-lags):(pos-1),
                      (max(all.test.pos)+1):(max(all.test.pos)+lags))
  all.train.pos <- setdiff(1:length(unique(time_vals)),
                           c(all.test.pos,all.buffer.pos))  # 4 final years = 1 buffer, 2 test, 1 projection
  train_ind <- which(all.tnum %in% time_vals[all.train.pos])

  mu  <- sapply(df[train_ind, , drop = FALSE], mean,na.rm = TRUE); mu[c(y_var,"lon","lat","tnum")]  <- 0
  sdv <- sapply(df[train_ind, , drop = FALSE], sd,na.rm = TRUE);   sdv[c(y_var,"lon","lat","tnum")] <- 1

  # df <- scale_z(df, mu, sdv) %>%
  #   arrange(tnum,lon,lat)

  if (include.past.lag){
    dfl.all <- make_lags_by_group(df,
                                  max_lag = lags,
                                  group = c("lon","lat"), order_by = "tnum",
                                  drop_rows_with_na_lags = FALSE) %>%
      dplyr::select(-c(starts_with("lon_L"),
                       starts_with("lat_L"),
                       starts_with("tnum_L"))) %>%
      arrange(tnum,lon,lat) %>%
      mutate(type = case_when(
        tnum %in% time_vals[1:lags] ~ "initial",
        tnum %in% time_vals[all.train.pos] ~ "train",
        tnum %in% time_vals[all.test.pos] ~ "test",
        TRUE ~ "buffer")) %>%
      filter(type != "initial")

  } else{
    dfl.all <- make_lags_by_group(df,
                                  max_lag = lags,
                                  group = c("lon","lat"), order_by = "tnum",
                                  drop_rows_with_na_lags = FALSE) %>%
      dplyr::select(-c(starts_with("lon_L"),
                       starts_with("lat_L"),
                       starts_with(paste0(y_var,"_")),
                       starts_with("tnum_L"))) %>%
      arrange(tnum,lon,lat) %>%
      mutate(type = case_when(
        tnum %in% time_vals[1:lags] ~ "initial",
        tnum %in% time_vals[all.train.pos] ~ "train",
        tnum %in% time_vals[all.test.pos] ~ "test",
        TRUE ~ "buffer")) %>%
      filter(type != "initial")

  }

  dfl <- dfl.all %>%
    na.omit()

  dfl.train <- as.matrix((dfl %>%
                            dplyr::filter(type == "train"))[,setdiff(colnames(dfl), c("type",y_var))] )
  y.train <- as.matrix((dfl %>%
                          dplyr::filter(type == "train"))[,y_var] )


  dfl.test <- as.matrix((dfl %>%
                           dplyr::filter(type == "test"))[,setdiff(colnames(dfl), c("type",y_var))] )

  y.test <- as.matrix((dfl %>%
                         dplyr::filter(type == "test"))[,y_var] )

  fit <- tuneModel(train = data.matrix(dfl.train),
                   y = as.numeric(y.train),
                   grid = Grid,
                   target = y_var,
                   lags = lags,
                   initial = initial, horizon = horizon, skip = skip)


  bestTune <- fit$bestTune
  bestModel <- fit$finalModel

  params <- list(
    objective = "reg:squarederror",
    eta = bestTune$eta, max_depth = bestTune$max_depth, gamma = bestTune$gamma,
    colsample_bytree = bestTune$colsample_bytree, min_child_weight = bestTune$min_child_weight,
    subsample = bestTune$subsample
  )

  features <- bestModel$feature_names
  dtrain <- xgb.DMatrix(as.matrix(dfl.train[, features, drop = FALSE]),
                        label = as.numeric(y.train))
  final_model <- xgb.train(params, dtrain, nrounds = bestTune$nrounds, verbose = 2)

  if (shap.test){
    shap_test <- predict(
      final_model,
      newdata = as.matrix(dfl.test[, features, drop = FALSE]),
      predcontrib = TRUE,          # SHAP values
      approxcontrib = FALSE        # exact TreeSHAP (set TRUE if speed/memory needed)
    )

    X_test <- as.matrix(dfl.test[, features, drop = FALSE])
    sv <- shapviz(final_model, X_test, pred_contrib = TRUE)  # uses TreeSHAP

  } else {
    shap_test <- sv <- NULL
  }


  saveRDS(list(final_model = final_model,
               dfl = dfl,
               dfl.train = dfl.train,
               y.train = y.train,
               train_ind = train_ind,
               dfl.test = dfl.test,
               y.test = y.test,
               test_ind = test_ind,
               shap_test = shap_test,
               sv = sv),
          paste0(dest.dir,"_",suffix,".RDS"))
}
