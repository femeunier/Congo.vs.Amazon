rm(list = ls())

library(dplyr)
library(stringr)
library(zoo)
library(raster)
library(TrENDY.analyses)

products <- c("NIR","SIF","TwoLeaf","VOD")

Prefix <- "Basin.Comp.RS."
years <- 1970:2024
suffixes <- c("Amazon","Congo")

# CO2
dataC02.all <- read.table("./data/global_co2_ann_1700_2024.txt",
                          stringsAsFactors = FALSE) %>%
  rename(year = V1,
         CO2 = V2)

coord.list <- bind_rows(readRDS("./outputs/Amazon.coord.ILF.RDS"),
                        readRDS("./outputs/Congo.coord.ILF.RDS")) %>%
  filter(model == "DLEM") %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  dplyr::select(-any_of(c("model","model.lon.lat")))

overwrite <- TRUE

for (csuffix in suffixes){

  for (cmodel in products){

    df.all <- df.test <- data.frame()

    print(cmodel)

    file <- paste0("./data/grid.",cmodel,".ERA5.v12.RDS")

    if (any(!file.exists(file))){
      next()
    }

    climate <- readRDS(file) %>%
      filter(abs(lat) <= 25)


    inputs <- climate %>%
      ungroup() %>%
      filter(year %in% years) %>%
      mutate(xgb.model = paste0(Prefix,csuffix,".",cmodel)) %>%
      left_join(dataC02.all,
                by = c("year")) %>%
      mutate(model.lon.lat = paste0(cmodel,".",round(lon,digits = 2),".",round(lat,digits = 2)),
             lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
      filter(lon.lat %in% coord.list[["lon.lat"]])

    xgb.models <- expand.grid(model = unique(inputs$xgb.model),
                              var = "gpp") %>%
      mutate(xgb.model = paste0(model,".RDS"))

    opfile.name <- paste0("./outputs/",Prefix,cmodel,".",csuffix,".RDS")
    opfile.name2 <- paste0("./outputs/",Prefix,"test.",cmodel,".",csuffix,".RDS")


    if (!overwrite & all(file.exists(c(opfile.name,
                                       opfile.name2)))){
      next()
    }

    for (i in seq(1,nrow(xgb.models))){

      cxgb.model <- as.character(xgb.models[["xgb.model"]][i])
      cvar <- as.character(xgb.models[["var"]][i])
      cxgb.model.novar <- as.character(xgb.models[["model"]][i])

      print(paste0("- ",cxgb.model.novar," - ",cvar))

      cfile <- file.path("/data/gent/vo/000/gvo00074/felicien/R/outputs/",
                         paste0(cxgb.model))

      if (!file.exists(cfile)) next()

      cXGBmodel <- readRDS(cfile)

      all.data <- bind_rows(
        bind_cols(cXGBmodel$test.data,
                  !!as.character(cvar) := cXGBmodel$test.labels),

        bind_cols(cXGBmodel$validation.data,
                  !!as.character(cvar) := cXGBmodel$validation.labels),

        bind_cols(cXGBmodel$training.data,
                  !!as.character(cvar) := cXGBmodel$labels)) %>%
        filter(year %in% years) %>%
        mutate(model.lon.lat = paste0(cmodel,".",round(lon,digits = 2),".",round(lat,digits = 2)))

      test.data <- bind_rows(
        bind_cols(cXGBmodel$test.data,
                  !!as.character(cvar) := cXGBmodel$test.labels)) %>%
        filter(year %in% years) %>%
        mutate(model.lon.lat = paste0(cmodel,".",round(lon,digits = 2),".",round(lat,digits = 2)))

      cdf <- inputs %>%
        filter(xgb.model == cxgb.model.novar) %>%
        filter(model.lon.lat %in% all.data[["model.lon.lat"]]) %>%
        dplyr::select(-any_of(c("model","xgb.model","model.lon.lat","lon.lat")))

      predicted <- predict(cXGBmodel,
                           cdf[,cXGBmodel$finalModel$feature_names])

      predicted_test <- predict(cXGBmodel,
                                cXGBmodel$test.data[,cXGBmodel$finalModel$feature_names])

      all_test <- bind_cols(cXGBmodel$test.data[,cXGBmodel$finalModel$feature_names],
                            pred = predicted_test,
                            obs = cXGBmodel$test.labels) %>%
        filter(year %in% years) %>%
        mutate(var = cvar)

      all.predicted <- inputs %>%
        filter(xgb.model == cxgb.model.novar) %>%
        filter(model.lon.lat %in% all.data[["model.lon.lat"]]) %>%
        dplyr::select(c("model","xgb.model","year",
                        "model.lon.lat",
                        "month","lat","lon")) %>%
        mutate(var = cvar,
               pred = predicted)

      cpredicted <- all.predicted %>%
        left_join(all.data %>%
                    dplyr::select(model.lon.lat,year,month,cvar) %>%
                    rename(obs := !!cvar),
                  by = c("model.lon.lat","year","month"))

      df.all <- bind_rows(df.all,
                          cpredicted)
      df.test <- bind_rows(df.test,
                           all_test)

    }

    saveRDS(df.all,
            opfile.name)
    saveRDS(df.test,
            opfile.name2)
  }
}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/post.processing.XGBmodels.RS.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

