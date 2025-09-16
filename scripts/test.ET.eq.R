rm(list = ls())

# library(reticulate)
library(chillR)

# use_python("/home/femeunier/miniconda3/bin/python", required = T)
# py_config()
#
# source_python("~/Downloads/convert.py")
# source_python("~/Downloads/fao.py")
# source_python("~/Downloads/thornthwaite.py")
# source_python("~/Downloads/_check.py")

tasmin = rep(24,12) ; tasmax = rep(32,12) ; lat = 0.25

# monthly_mean_dlh0 <- monthly_mean_daylight_hours(deg2rad(lat),year = 2001)
monthly_mean_dlh <- daylength(lat, seq(15,365,30),
                              notimes.as.na = FALSE)[["Daylength"]]

E <- SPEI::thornthwaite((tasmin + tasmax)/2,lat,na.rm = TRUE,verbose = FALSE)
E1 <- SPEI::hargreaves(tasmin,tasmax,lat,Ra = NULL,na.rm = TRUE,verbose = FALSE)
E2 <- SPEI::penman(tasmin,tasmax,tsun = monthly_mean_dlh,
                   lat = lat, z = 0, na.rm = TRUE,verbose = FALSE)

plot(E2,type = "l",ylim = c(0,400))
lines(E1,lty = 2)
lines(E,lty = 3)
lines(c(31,28,31,30,31,30,31,31,30,31,30,31)*3.33,
      col = "red")

################################################################################
all.tas <- seq(20,35)

lat = 0 ; Delta = 3
monthly_mean_dlh <- daylength(lat, seq(15,365,30),
                              notimes.as.na = FALSE)[["Daylength"]]

E <- E1 <- E2 <- c()
for (itas in seq(1,length(all.tas))){

  ctas <- all.tas[itas]
  E[itas] <- mean(SPEI::thornthwaite(rep(ctas,12),lat,
                                na.rm = TRUE,verbose = FALSE))

  E1[itas] <- SPEI::hargreaves(ctas - Delta,ctas + Delta,lat,Ra = NULL,na.rm = TRUE,verbose = FALSE)
  E2[itas] <- mean(
    SPEI::penman(rep(ctas - Delta,12), rep(ctas + Delta,12),
              tsun = monthly_mean_dlh, lat = lat, z = 0, na.rm = TRUE,verbose = FALSE))

}

plot(all.tas,E,type = "l",ylim = c(80,200))
lines(all.tas,E1, col = "red")
lines(all.tas,E2, col = "blue")
abline(h = 100,lty = 2)
