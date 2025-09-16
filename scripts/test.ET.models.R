rm(list = ls())

library(dplyr)
library(ggplot2)
library(xgboost)


system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/tas2tasmax.RDS",
          "./outputs/"))


Train <- readRDS("./outputs/tas2tasmax.RDS")
modeltas2tasmax <- Train$finalModel
A <- data.frame(lon = 30,
                lat = 5,
                year = 2100,
                month = 1:12,
                pr = 1e-8,
                tas = 25)
dnew <- xgb.DMatrix(data = as.matrix(A[, c("lon", "lat", "year", "month", "pr", "tas")]))
predict(modeltas2tasmax, newdata = dnew)

plot(predict(modeltas2tasmax,newdata = dnew),
     ylim = c(20,40),
     type = "l")
lines(predict(modeltas2tasmax,
             data.frame(lon = 30,
                        lat = 0,
                        year = 2000,
                        month = 1:12,
                        pr = 1e-8,
                        tas = 25)),
     type = "l", col = "red")


Delta <- 2
tas <- rep(27,12) ; tasmod <- tas + Delta
tasmin = tas - 3 ; tasminmod <- tasmin + Delta
tasmax = tas + 3; tasmaxmod <- tasmax + Delta
lat <- -15

PET.th <- SPEI::thornthwaite(tas,
                          lat = lat,
                          na.rm = TRUE,
                          verbose = FALSE)
PET.th.mod <- SPEI::thornthwaite(tasmod,
                             lat = lat,
                             na.rm = TRUE,
                             verbose = FALSE)
PET.hr <-  SPEI::hargreaves(tasmin,
                             tasmax,
                             lat = lat,
                             Ra = NULL,
                             na.rm = TRUE,
                             verbose = FALSE)
PET.hr.mod <-  SPEI::hargreaves(tasminmod,
                            tasmaxmod,
                            lat = lat,
                            Ra = NULL,
                            na.rm = TRUE,
                            verbose = FALSE)


df <- bind_rows(data.frame(month = 1:12,
                           ET = PET.th,
                           Emodel = "Thornwaite",
                           timing = "historical"),
                data.frame(month = 1:12,
                           ET = PET.th.mod,
                           Emodel = "Thornwaite",
                           timing = "future"),
                data.frame(month = 1:12,
                           ET = PET.hr,
                           Emodel = "hargreaves",
                           timing = "historical"),
                data.frame(month = 1:12,
                           ET = PET.hr.mod,
                           Emodel = "hargreaves",
                           timing = "future"))

ggplot(data = df) +
  geom_line(aes(x = month,
                y = ET,
                color = timing)) +
  facet_wrap(~ Emodel) +
  theme_bw()


#######################################################


CMIP6 <- readRDS("./outputs/All.CMIP6.states.RDS") %>%
  rename(pre = Pmm,
         period = timing) %>%
  filter(Emodel %in% c("Hargreaves",
                       "Hargreaves_interp")) %>%
  filter(!(scenario %in% c("ssp534-over")))

ggplot(data = CMIP6,
       aes(x = tas, y = tasmin)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  stat_smooth(method = "lm") +
  theme_bw()

ggplot(data = CMIP6,
       aes(x = tas, y = tasmax)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  stat_smooth(method = "lm") +
  theme_bw()

ggplot(data = CMIP6 %>%
         ungroup()) +
  geom_density(aes(x = Etot, fill = period),
               alpha = 0.5) +
  facet_wrap(~ Emodel) +
  theme_bw()

ggplot(data = CMIP6 %>%
         ungroup() %>%
         filter(Etot <= 500)) +
  geom_boxplot(aes(x = Emodel,y = Etot, fill = period),
               alpha = 0.5) +
  theme_bw()
