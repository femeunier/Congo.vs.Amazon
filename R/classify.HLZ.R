
classify.HLZ <- function(abt,tap,per){

  lgth <- length(abt)

  hlzDefSubset <- macroBiome:::hlzDefSubset

  vegCls <- rep(NA_character_, length = lgth)
  distBds <- matrix(nrow = lgth, ncol = nrow(hlzDefSubset),
                    dimnames = list(NULL, hlzDefSubset$ID))
  for (i_vcl in 1:nrow(hlzDefSubset)) {
    optVabt <- log2(hlzDefSubset$abt[i_vcl]) + 0.5
    tmpVabt <- ifelse(abt == 0, NA, (log2(abt) - optVabt)^2)
    optVtap <- log2(hlzDefSubset$tap[i_vcl]) + 0.5
    tmpVtap <- ifelse(tap == 0, NA, (log2(tap) - optVtap)^2)
    optVper <- log2(hlzDefSubset$per[i_vcl]) + 0.5
    tmpVper <- ifelse(per == 0, NA, (log2(per) - optVper)^2)
    distBds[, i_vcl] <- sqrt(tmpVabt + tmpVtap + tmpVper)
  }
  minVal <- do.call(pmin, as.data.frame(distBds))
  psblVegCls <- t(sapply(1:nrow(distBds), function(i) {
    distBds[i, ] == minVal[i]
  }))
  n_pvc <- rowSums(psblVegCls)
  plTL <- 1.5
  mn_tap <- 62.5
  mn_per <- 0.125
  frTL <- 2^(log2(12) + 0.5)
  WtCls <- seq(17, 23)
  names(WtCls) <- c("WtD", "WtDs", "WtTs", "WtDf", "WtMf",
                    "WtWf", "WtRf")
  StCls <- seq(24, 30)
  names(StCls) <- c("StD", "StDs", "StTw", "StDf", "StMf",
                    "StWf", "StRf")
  gr <- !is.na(n_pvc)
  grI <- gr & (tap < mn_tap | per < mn_per)
  grII <- gr & (tap >= mn_tap & per >= mn_per)
  grIIA <- grII & abt < plTL
  grIIB <- grII & abt >= plTL
  grIIB1 <- grIIB & n_pvc == 1
  grIIB2 <- grIIB & n_pvc != 1
  grIIB2a <- grIIB2 & apply(psblVegCls[, c(WtCls, StCls),
                                       drop = FALSE], 1, function(x) !any(x))
  grIIB2b <- grIIB2 & apply(psblVegCls[, c(WtCls, StCls),
                                       drop = FALSE], 1, any)
  grIIB2bwt <- grIIB2b & abt < frTL
  grIIB2bst <- grIIB2b & abt >= frTL
  selVegCls <- function(x) {
    names(which(x))[1]
  }
  vegCls[grI] <- "BaSl"
  vegCls[grIIA] <- "PD"
  vegCls[grIIB1] <- apply(psblVegCls[grIIB1, , drop = FALSE],
                          1, function(x) names(which(x)))
  vegCls[grIIB2a] <- apply(psblVegCls[grIIB2a, , drop = FALSE],
                           1, selVegCls)
  vegCls[grIIB2bwt] <- apply(psblVegCls[grIIB2bwt, -c(StCls),
                                        drop = FALSE], 1, selVegCls)
  vegCls[grIIB2bst] <- apply(psblVegCls[grIIB2bst, -c(WtCls),
                                        drop = FALSE], 1, selVegCls)
  return(vegCls)
}
