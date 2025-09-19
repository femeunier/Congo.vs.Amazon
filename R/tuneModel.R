tuneModel <- function(train, y, target,
                      lags = 6, initial = 200, horizon = 12, skip = 6,
                      grid = expand.grid(
                        nrounds = c(200, 400, 800),
                        max_depth = c(3, 6, 9),
                        eta = c(0.03, 0.1),
                        gamma = c(0),
                        colsample_bytree = c(0.8),
                        min_child_weight = c(1),
                        subsample = c(0.8)
                      )) {

  if (requireNamespace("future", quietly = TRUE)) {
    Ncores <- as.numeric(future::availableCores())
    future::plan("multisession", workers = Ncores)
  } else {
    Ncores <- 1
  }

  # Coerce and clean
  dfx <- if (is.matrix(train)) as.data.frame(train) else train
  logical_cols <- vapply(dfx, is.logical, logical(1))
  if (any(logical_cols)) dfx[logical_cols] <- lapply(dfx[logical_cols], as.integer)

  stopifnot("tnum" %in% names(dfx))  # must carry time key
  x <- data.matrix(dfx); storage.mode(x) <- "double"
  y <- as.numeric(y)
  stopifnot(is.numeric(y), is.vector(y), length(y) == nrow(x))
  if (any(!is.finite(x))) x[!is.finite(x)] <- NA_real_
  if (any(!is.finite(y))) stop("y contains non-finite values")

  # --- Build rows-by-time using only times that actually exist in TRAIN ---
  t_train <- as.integer(dfx[["tnum"]])
  time_vals_eff <- sort(unique(t_train))
  # factor with all levels keeps empty bins as integer(0) instead of NULL
  f <- factor(match(t_train, time_vals_eff), levels = seq_along(time_vals_eff))
  rows_by_time_idx <- split(seq_len(nrow(x)), f, drop = FALSE)

  # Create rolling time slices on the EFFECTIVE time axis
  slices <- createTimeSlices(
    1:length(time_vals_eff),
    initialWindow = initial,
    horizon       = horizon,
    fixedWindow   = TRUE,
    skip          = skip
  )

  folds_train <- lapply(slices$train, function(ids)
    unlist(rows_by_time_idx[ids], use.names = FALSE))
  folds_test <- lapply(slices$test, function(ids)
    unlist(rows_by_time_idx[ids], use.names = FALSE))

  # Drop folds with empty train or test (caret cannot handle them)
  keep <- which(lengths(folds_train) > 0 & lengths(folds_test) > 0)
  folds_train <- folds_train[keep]
  folds_test  <- folds_test[keep]

  ctrl <- trainControl(
    method          = "cv",
    index           = folds_train,
    indexOut        = folds_test,
    summaryFunction = defaultSummary,
    classProbs      = FALSE,
    savePredictions = "final",
    verboseIter     = TRUE,
    allowParallel   = TRUE
  )

  # Don’t pass tnum as a feature
  x_nontime <- as.data.frame(x)[, setdiff(colnames(x), c("tnum")), drop = FALSE]

  fit <- train(
    x = x_nontime,
    y = y,
    method = "xgbTree",
    trControl = ctrl,
    tuneGrid = grid,
    metric = "RMSE",
    verbosity = 1,
    nthread = Ncores
  )

  future::plan("sequential")
  fit
}
