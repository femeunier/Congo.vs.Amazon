scale_z <- function(df, mu, sdv) {
  as.data.frame(mapply(function(z, m, s) (z - m) / s, df, mu, sdv))
}
