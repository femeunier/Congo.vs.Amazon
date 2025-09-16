rm(list = ls())

coord.list <- bind_rows(readRDS("~/Documents/projects/Congo.ED2/outputs/Amazon.coord.ILF.RDS") %>%
                          mutate(basin = "Amazon"),
                        readRDS("~/Documents/projects/Congo.ED2/outputs/Congo.coord.ILF.RDS") %>%
                          mutate(basin = "Congo"))


mask <- coord.list %>%
  dplyr::select(lat,lon,basin) %>%
  mutate(lon.lat = paste0(lon,".",lat))

GPP.products <- readRDS("./data/GPP.products.RDS")
GPP.products.basin <- GPP.products %>%
  mutate(lon.lat = paste0(lon,".",lat)) %>%
  filter(lon.lat %in% mask[["lon.lat"]]) %>%
  dplyr::select(-lon.lat) %>%
  left_join(mask,
            by = c("lon","lat"))

GPP.products.basin.sum <- GPP.products.basin %>%
  group_by(basin,year,month,product) %>%
  summarise(value.m = mean(value,na.rm = TRUE)*365/1000,
            .groups = "keep")


ggplot(data = GPP.products.basin.sum) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = basin)) +
  facet_wrap(~ product) +
  scale_x_continuous(limits = c(2005,2015)) +
  theme_bw()

ggplot(data = GPP.products.basin.sum) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = product)) +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(2005,2015)) +
  theme_bw()


GPP.products.basin.sum.SC <- GPP.products.basin.sum %>%
  group_by(product,basin,month) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = GPP.products.basin.sum.SC) +

  geom_line(aes(x = month,
                y = value.m,
                color = basin)) +
  facet_wrap(~ product) +
  theme_bw()


################################################################################
# MEM


GPP.products.basin.sum.m <- GPP.products.basin.sum %>%
  filter(year %in% c(2001:2018)) %>% # Common years
  group_by(basin,year,month) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = GPP.products.basin.sum.m) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = basin)) +
  theme_bw()


GPP.products.basin.sum.SC <- GPP.products.basin.sum %>%
  filter(year %in% c(2001:2018)) %>%
  group_by(basin,month) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = GPP.products.basin.sum.SC) +
  geom_line(aes(x = month,
                y = value.m,
                color = basin)) +
  theme_bw()



# Anomalies
year.min=1994;year.max=2023;Window=6

GPP.products.basin.sum.anomalies <- GPP.products.basin.sum.m %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(basin) %>%
  mutate(slope = coef(lm(value.m ~ time))[2],
         intercept = coef(lm(value.m ~ time))[1]) %>%
  mutate(mean.obs =  intercept + slope *time) %>%
  # mutate(mean.obs = mean(value.m[time >= year.min & time < year.max])) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(basin,month) %>%
  mutate(mean.month = mean(detrended[time >= year.min & time < year.max])) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin) %>%
  mutate(anomaly.m = anomaly/sd(anomaly[time >= year.min & time < year.max])) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="center"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center"),
         value.m.rm = rollapply(value.m, width=Window,
                               FUN=function(x) mean(x, na.rm=TRUE),
                               partial=TRUE, fill=NA, align="center"))


ggplot(data = GPP.products.basin.sum.anomalies) +
  geom_line(aes(x = time,
                y = anomaly.m.rm,
                color = basin),
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = anomaly.m,
                 color = basin),
             size = 0.25) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  # scale_x_continuous(limits = c(2020,2025)) +
  theme_bw()

# Per product


GPP.products.basin.sum.anomalies.pr <- GPP.products.basin.sum %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(basin,product) %>%
  mutate(slope = coef(lm(value.m ~ time))[2],
         intercept = coef(lm(value.m ~ time))[1]) %>%
  mutate(mean.obs =  intercept + slope *time) %>%
  # mutate(mean.obs = mean(value.m[time >= year.min & time < year.max])) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(basin,product,month) %>%
  mutate(mean.month = mean(detrended[time >= year.min & time < year.max])) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin,product) %>%
  mutate(anomaly.m = anomaly/sd(anomaly[time >= year.min & time < year.max])) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="center"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center"),
         value.m.rm = rollapply(value.m, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="center"))


ggplot(data = GPP.products.basin.sum.anomalies.pr %>%
         filter(basin == "Amazon")) +
  geom_line(aes(x = time,
                y = anomaly.m.rm,
                color = product),
            linetype = 1, linewidth = 1) +
  geom_line(data = GPP.products.basin.sum.anomalies %>%
              filter(basin == "Amazon"),
            aes(x = time,
                y = anomaly.m.rm),
            color = "black",
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = anomaly.m,
                 color = product),
             size = 0.25) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  # facet_wrap(~ product) +
  # scale_x_continuous(limits = c(2020,2025)) +
  theme_bw()

