rm(list = ls())

library(dplyr)
library(stringr)
library(zoo)
library(raster)
library(TrENDY.analyses)

Prefix <- "Basin.Comp.RS."

products <- c("NIR","VOD","SIF","TwoLeaf")

df.all.ts <- df.all.year <-
  predictions.Amazon <- predictions.Congo <-
  predictions.Amazon.test <- predictions.Congo.test <-
  predictions.Amazon.sum <- predictions.Congo.sum <-
  df.all.ts.MEM <- df.all.year.MEM <-
  all.predictions <-
  data.frame()

Amazon.coord <- readRDS("./outputs/Amazon.coord.ILF.RDS") %>%
  filter(model == "DLEM") %>%
  mutate(lon.lat = paste0(lon,".",lat))
Congo.coord <- readRDS("./outputs/Congo.coord.ILF.RDS") %>%
  filter(model == "DLEM") %>%
  mutate(lon.lat = paste0(lon,".",lat))

for (cmodel in products){

  print(cmodel)
  cfile <- paste0("./outputs/",Prefix,cmodel,".Amazon.RDS")
  cfile2 <- paste0("./outputs/",Prefix,"test.",cmodel,".Amazon.RDS")

  if (!file.exists(cfile)){
    next()
  }

  predictions.XGB <- readRDS(cfile)
  predictions.XGB.test <- readRDS(cfile2)

  if (nrow(predictions.XGB) == 0) next()

  all.predictions <- bind_rows(all.predictions,
                               predictions.XGB %>%
                                 mutate(model = cmodel))

  predictions.Amazon <- bind_rows(predictions.Amazon,
                                  predictions.XGB %>%
                                    mutate(model = cmodel) %>%
                                    mutate(model.lon.lat =
                                             paste0(model,".",round(lon,digits = 2),".",round(lat,digits = 2)),
                                           lon.lat =
                                             paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
                                    filter(lon.lat %in% Amazon.coord[["lon.lat"]]))

  predictions.Amazon.test <- bind_rows(predictions.Amazon.test,
                                       predictions.XGB.test %>%
                                         mutate(model = cmodel) %>%
                                         mutate(model.lon.lat =
                                                  paste0(model,".",round(lon,digits = 2),".",round(lat,digits = 2)),
                                                lon.lat =
                                                  paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
                                         filter(lon.lat %in% Amazon.coord[["lon.lat"]]))


  # timeseries
  predictions.XGB.sum <- predictions.XGB %>%
    group_by(model,year,month,var) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              .groups = "keep")

  df.all.ts <- bind_rows(list(df.all.ts,
                              predictions.XGB.sum %>%
                                mutate(model = cmodel,
                                       basin = "Amazon")))


  # Yearly
  predictions.XGB.sum.year <- predictions.XGB %>%
    group_by(model,year,var) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              .groups = "keep")

  df.all.year <- bind_rows(list(df.all.year,
                                predictions.XGB.sum.year %>%
                                  mutate(model = cmodel,
                                         basin = "Amazon")))

}

predictions.Amazon.sum <- predictions.Amazon %>%
  group_by(model,var,year,month) %>%
  summarise(pred = mean(pred,
                        na.rm = TRUE),
            obs = mean(obs,
                       na.rm = TRUE),
            .groups = "keep")

################################################################################
# Congo

for (cmodel in products){

  print(cmodel)
  cfile <- paste0("./outputs/",Prefix,cmodel,".Congo.RDS")
  cfile2 <- paste0("./outputs/",Prefix,"test.",cmodel,".Congo.RDS")

  if (!file.exists(cfile)){
    next()
  }

  predictions.XGB <- readRDS(cfile)
  predictions.XGB.test <- readRDS(cfile2)

  if (nrow(predictions.XGB) == 0) next()

  all.predictions <- bind_rows(all.predictions,
                               predictions.XGB %>%
                                 mutate(model = cmodel))


  predictions.Congo <- bind_rows(predictions.Congo,
                                 predictions.XGB %>%
                                   mutate(model = cmodel) %>%
                                   mutate(model.lon.lat =
                                            paste0(model,".",
                                                   round(lon,digits = 2),".",
                                                   round(lat,digits = 2)),
                                          lon.lat =
                                            paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
                                   filter(lon.lat %in% Congo.coord[["lon.lat"]]))

  predictions.Congo.test <- bind_rows(predictions.Congo.test,
                                      predictions.XGB.test %>%
                                        mutate(model = cmodel) %>%
                                        mutate(model.lon.lat =
                                                 paste0(model,".",round(lon,digits = 2),".",round(lat,digits = 2)),
                                               lon.lat =
                                                 paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
                                        filter(lon.lat %in% Congo.coord[["lon.lat"]]))




  # timeseries
  predictions.XGB.sum <- predictions.XGB %>%
    group_by(model,year,month,var) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              .groups = "keep")

  df.all.ts <- bind_rows(list(df.all.ts,
                              predictions.XGB.sum %>%
                                mutate(model = cmodel,
                                       basin = "Congo")))


  # Yearly
  predictions.XGB.sum.year <- predictions.XGB %>%
    group_by(model,year,var) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              .groups = "keep")

  df.all.year <- bind_rows(list(df.all.year,
                                predictions.XGB.sum.year %>%
                                  mutate(model = cmodel,
                                         basin = "Congo")))

}

predictions.Congo.sum <- predictions.Congo %>%
  group_by(model,var,year,month) %>%
  summarise(pred = mean(pred,
                        na.rm = TRUE),
            obs = mean(obs,
                       na.rm = TRUE),
            .groups = "keep")


MEM <- df.all.ts %>%
  group_by(basin,year,month,var) %>%
  summarise(obs.MEM.m = mean(obs.m,na.rm = TRUE),
            pred.MEM.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")

MEM.year <- df.all.year %>%
  group_by(basin,year,var) %>%
  summarise(obs.MEM.m = mean(obs.m,na.rm = TRUE),
            pred.MEM.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")

saveRDS(df.all.ts,
        paste0("./outputs/",Prefix,".ts.RDS"))
saveRDS(df.all.year,
        paste0("./outputs/",Prefix,"year.RDS"))
saveRDS(MEM,
        paste0("./outputs/",Prefix,"MEM.ts.RDS"))
saveRDS(MEM.year,
        paste0("./outputs/",Prefix,"MEM.year.RDS"))

saveRDS(bind_rows(predictions.Amazon %>%
                    mutate(basin = "Amazon"),
                  predictions.Congo %>%
                    mutate(basin = "Congo")),
        paste0("./outputs/",Prefix,"mean.RDS"))

saveRDS(bind_rows(predictions.Amazon.test %>%
                    mutate(basin = "Amazon"),
                  predictions.Congo.test %>%
                    mutate(basin = "Congo")),
        paste0("./outputs/",Prefix,"mean.test.RDS"))

saveRDS(bind_rows(predictions.Amazon.sum %>%
                    mutate(basin = "Amazon"),
                  predictions.Congo.sum %>%
                    mutate(basin = "Congo")),
        paste0("./outputs/",Prefix,"mean.sum.RDS"))

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Compile.XGBmodel.RS.outputs.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
