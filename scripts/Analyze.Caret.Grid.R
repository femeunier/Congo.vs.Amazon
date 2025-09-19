rm(list = ls())

library(terra)
library(dplyr)
library(CausalAI)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(tibble)
library(yardstick)
library(xgboost)
library(shapviz)

models <- TrENDY.analyses::get.model.names.TRENDY("v13")
dir.name <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/GPP_caret/"

df.QoF <- df.test <- df.SHAP <-
  data.frame()

suffix <- "noLag_CRUJRA"

for (cmodel in models){
  cdir <- file.path(dir.name,cmodel)

  cmodel.op <- file.path(cdir,paste0(cmodel,"_",suffix,".RDS"))

  if (!file.exists(cmodel.op)) next

  print(cmodel)

  cop <- readRDS(cmodel.op)

  final_model <- cop[["final_model"]]
  dfl.test <- cop[["dfl.test"]]
  y.test <- cop[["y.test"]]
  test_ind <- cop[["test_ind"]]
  dfl <- cop[["dfl"]]

  if (is.null(dfl)){
    next()
  }

  # print(dim(dfl %>% na.omit()))
  # print(dim(dfl.test %>% na.omit()))

  features <- setdiff(colnames(dfl.test), "tnum")
  y.pred <- predict(final_model,
                    as.matrix(dfl.test[, features, drop = FALSE]))

  RMSE <- caret::RMSE(y.test[!is.na(y.test)],y.pred[!is.na(y.test)])
  RSQ <- rsq_vec(as.vector(y.test),
                 as.numeric(y.pred))
  MAE <- mean(abs(y.test - y.pred),na.rm = TRUE)

  XYtest <- as.data.frame(cbind(dfl.test,
                                gppanomaly = y.test,
                                pred =y.pred))

  df.test <- bind_rows(df.test,
                       XYtest %>%
                         mutate(model = cmodel))

  df2plot <- XYtest %>%
    group_by(tnum) %>%
    summarise(obs.m = mean(gppanomaly,na.rm = TRUE),
              pred.m = mean(pred),
              .groups = "keep") %>%
    na.omit()

  RMSE.region <- caret::RMSE(df2plot$obs.m,df2plot$pred.m)
  RSQ.region <- rsq_vec(as.vector(df2plot$obs.m),
                        as.numeric(df2plot$pred.m))
  MAE.region <- mean(abs(df2plot$obs.m - df2plot$pred.m),na.rm = TRUE)


  df.QoF <- bind_rows(df.QoF,
                      data.frame(RMSE.test = RMSE,
                                 R2.test = RSQ,
                                 MAE.test = MAE,
                                 RMSE.region = RMSE.region,
                                 R2.region = RSQ.region,
                                 MAE.region = MAE.region,
                                 model = cmodel))


  shap_test <- cop[['shap_test']]

  colnames(shap_test) <- c(features, "BIAS")
  pred_from_shap <- rowSums(shap_test[, features, drop = FALSE]) + shap_test[, "BIAS"]

  imp_feat <- colMeans(abs(shap_test[, features, drop = FALSE]))
  imp_tbl  <- tibble(feature = names(imp_feat),
                     mean_abs_shap = as.numeric(imp_feat)) %>%
    arrange(desc(mean_abs_shap))


  # If you want to aggregate across lags (e.g., tmp_L1..tmp_L12 → "tmp"):
  imp_var <- imp_tbl %>%
    mutate(var = str_replace(feature, "_L\\d+$", "")) %>%
    group_by(var) %>%
    summarise(mean_abs_shap = sum(mean_abs_shap), .groups = "drop") %>%
    arrange(desc(mean_abs_shap))

  df.SHAP <- bind_rows(df.SHAP,
                       imp_var %>%
                         mutate(model = cmodel))


  # X_test <- as.matrix(dfl.test[, features, drop = FALSE])
  # sv <- cop[["sv"]]
  # sv_importance(sv)                # global importance
  # sv_dependence(sv, "gppanomaly_L1", color_var = "gppanomaly_L1")  # dependence plot

}

saveRDS(df.QoF,
        paste0("./outputs/QoF.allModels.Caret_",suffix,".RDS"))
saveRDS(df.test,
        paste0("./outputs/Alltests.allModels.Caret_",suffix,".RDS"))
saveRDS(df.SHAP,
        paste0("./outputs/AllSHAPS.allModels.Caret_",suffix,".RDS"))

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Analyze.Caret.Grid.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

