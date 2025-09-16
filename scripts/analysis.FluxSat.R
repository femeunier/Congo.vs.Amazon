rm(list = ls())

library(dplyr)
library(ggplot2)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/ts.GPP.IFL.basin.RDS",
          "./outputs/"))


# Observations

df <- readRDS("./outputs/ts.GPP.IFL.basin.RDS") %>%
  filter(year > 0) %>%
  arrange(year,month)

df.slope <- df %>%
  na.omit() %>%
  mutate(time = year + (month -1/2)/12) %>%
  filter(year <= 2020) %>%
  group_by(basin) %>%
  summarise(slope = coef(lm(value.m ~ time))[2],
            intercept = coef(lm(value.m ~ time))[1],
            .groups = "keep") %>%
  dplyr::select(basin,
                slope,intercept)

Window <- 3

df.anomaly <- df %>%
  ungroup() %>%
  mutate(time = year + (month -1/2)/12) %>%
  left_join(df.slope,
            by = c("basin")) %>%
  group_by(basin) %>%
  mutate(mean.obs = slope*(time) + intercept) %>%
  # mutate(mean.obs = mean(value.m,na.rm = TRUE)) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(basin,month) %>%
  mutate(mean.month = mean(detrended,
                           na.rm = TRUE)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin,month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly,
                                na.rm = TRUE)) %>%
  group_by(basin) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=FALSE, fill=NA, align="right"),
         value.m.rm = rollapply(value.m, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"))


ggplot(data = df.anomaly) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m.rm,
                color = basin)) +
  theme_bw()

ggplot(data = df.anomaly) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.rm,
                color = basin)) +
  theme_bw()

ggplot(data = df.anomaly) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.m.rm,
                color = basin)) +
  theme_bw()

#
# # Model
# Model <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/df.all.ts.ERA5.IFL.v13.RDS") %>%
#   filter(basin == "Amazon",
#          var == "gpp")
#
# ggplot(data = Model %>%
#          filter(var == "gpp"))+
#   geom_line(aes(x = year + month/12, y = pred.m,
#                 color = model)) +
#   theme_bw()
#
#
# Model.sum <- Model %>%
#   group_by(year,month) %>%
#   summarise(pred.m = mean(pred.m,na.rm = TRUE),
#             .groups = "keep")
#
# Model.anomaly <- Model.sum %>%
#   ungroup() %>%
#   filter(year >= 2000) %>%
#   mutate(mean.obs = mean(pred.m,
#                          na.rm = TRUE)) %>%
#   mutate(detrended = pred.m - mean.obs) %>%
#   group_by(month) %>%
#   mutate(mean.month = mean(detrended,
#                            na.rm = TRUE)) %>%
#   mutate(anomaly = detrended - mean.month) %>%
#   mutate(reconstructed = mean.obs + mean.month) %>%
#   group_by(month) %>%
#   mutate(anomaly.m = anomaly/sd(anomaly,
#                                 na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(anomaly.rm = rollapply(anomaly, width=Window,
#                                 FUN=function(x) mean(x, na.rm=TRUE),
#                                 partial=TRUE, fill=NA, align="right"),
#          anomaly.m.rm = rollapply(anomaly.m, width=Window,
#                                   FUN=function(x) mean(x, na.rm=TRUE),
#                                   partial=TRUE, fill=NA, align="right"),
#          value.m.rm = rollapply(pred.m, width=Window,
#                                 FUN=function(x) mean(x, na.rm=TRUE),
#                                 partial=TRUE, fill=NA, align="right"))
#
#
# ggplot(data = Model.anomaly) +
#   geom_line(aes(x = year + (month - 1/2)/12,
#                 y = value.m.rm)) +
#   theme_bw()
#
# ggplot(data = Model.anomaly) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_line(aes(x = year + (month - 1/2)/12,
#                 y = anomaly.rm)) +
#   theme_bw()
#
# ggplot(data = Model.anomaly) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_line(aes(x = year + (month - 1/2)/12,
#                 y = anomaly.m.rm,
#                 color = basin)) +
#   theme_bw()
#
#
#
saveRDS(df.anomaly,
        "./outputs/FluxSat.GPP.Amazon_anomalies.RDS")
