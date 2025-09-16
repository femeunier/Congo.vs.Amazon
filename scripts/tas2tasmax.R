rm(list = ls())

################################################################################
# XGBoost
# Libraries

library(dplyr)
library(ggplot2)
library(ggthemes)
library(caret)
library(xgboost)
library(zoo)
library(feather)

rawdata <- read_feather("./outputs/All.tas.feather") %>%
  ungroup()

####################################################################"
# Fit machine learning model

# Climate variables
climate.vars <- c("lon","lat","year","month","pr","tas")

# Name of the y to fit
target.var <- c("tasmin")

rawdata <- rawdata %>%
  dplyr::select(any_of(c(climate.vars,target.var)))

# Machine learning settings
xgb_trcontrol <- caret::trainControl(
  method = "cv",
  number = 8,
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE
)

# Hyperparameters to explore for the machine learning algorithm
xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))

# Fraction of the data that will be used for training, note (1-frac.train)/2 will be used for completely independent test
frac.train <- 0.6

# Adding a id column for each row
cdf <- rawdata %>%
  ungroup() %>%
  mutate(id = 1:n())

# Split the data into training/validatin/test
ccdf <-  cdf %>%
  mutate(group = sample(
    c("train", "validation", "test"),
    size = n(),
    replace = TRUE,
    prob = c(as.numeric(frac.train),
             (1-as.numeric(frac.train))/2,
             (1-as.numeric(frac.train))/2))) %>%
  ungroup()

train <- ccdf %>%
  filter(group == "train") %>%
  dplyr::select(-c(any_of(c("group",target.var))))

validation <- ccdf %>%
  filter(group == "validation") %>%
  dplyr::select(-c(any_of(c("group",target.var))))

test <- ccdf %>%
  filter(group == "test") %>%
  dplyr::select(-c(any_of(c("group",target.var))))

# Training data formating
training.data <- as.matrix(train %>%
                             dplyr::select(-id))
training.labels <- ccdf %>%
  filter(id %in% (train[["id"]])) %>%
  pull(!!target.var)

# Validation data formating
validation.data <- as.matrix(validation %>%
                               dplyr::select(-id))
validation.labels <- ccdf %>%
  filter(id %in% (validation[["id"]])) %>%
  pull(!!target.var)

# Test data formating
test.data <- as.matrix(test %>%
                         dplyr::select(-id))
test.labels <- ccdf %>%
  filter(id %in% (test[["id"]])) %>%
  pull(!!target.var)

# First we optimize the machine learning model hyperparameters
xgb_model <- caret::train(
  training.data,training.labels,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 16,
  verbosity = 1)

# Then we rerun with the best set of hyperparameters
# with test and validation data merged together
xgb_best_model <- caret::train(
  x = rbind(training.data,
            validation.data),
  y = c(training.labels,
        validation.labels),
  trControl = xgb_trcontrol,
  tuneGrid = xgb_model$bestTune,
  method = "xgbTree",
  nthread = 16,
  verbosity = 1)

saveRDS(xgb_best_model,
        "./outputs/tas2tasmin.RDS")

# We add the dataset for further use later
xgb_best_model$training.data <- training.data
xgb_best_model$training.labels <- training.labels

xgb_best_model$validation.data <- validation.data
xgb_best_model$validation.labels <- validation.labels

xgb_best_model$test.data <- test.data
xgb_best_model$test.labels <- test.labels

# Make model predictions
predicted <- predict(xgb_best_model,
                     ccdf[,xgb_model$finalModel$feature_names])

all.predicted <- cbind(ccdf,
                       predicted)


write_feather(all.predicted,
              "./outputs/all.predicted.tasmin.feather")

# Model predictions specifically for the test data
predicted_test <- predict(xgb_best_model,
                          test.data[,xgb_best_model$finalModel$feature_names])

all_test <- bind_cols(test.data,
                      pred = predicted_test,
                      obs = xgb_best_model$test.labels)


write_feather(all_test,
        "./outputs/all.test.tasmin.feather")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/tas2tasmax.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
