rm(list = ls())

library(dplyr)
library(ggplot2)
library(pals)
library(cowplot)
library(pals)
library(sf)
library(tidyr)
library(zoo)
library(segmented)

Prefix <- "Basin.Comp.RS."
models2keep <- c("NIR","SIF","VOD","TwoLeaf")

files2transfer <- paste0(Prefix,
                         c("mean.test.RDS",
                           "mean.sum.RDS"))

# for (cfile in files2transfer){
#   system2("rsync",
#           c("-avz",
#             paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }

Trendy.data <- readRDS(paste0("./outputs/",
                              Prefix,
                              "mean.test.RDS"))

Trendy.data.sum <- Trendy.data %>%
  filter(model %in% models2keep) %>%
  group_by(model,basin,var,year,month) %>%
  summarise(pred.m = mean(pred),
            obs.m = mean(obs),
            .groups = "keep")


palette <- kelly(n=17)[2:17]

ggscater <- ggplot(Trendy.data %>%
                     filter(basin == "Congo",
                            var == "gpp"),
                   aes(x = pred,
                       y = obs,
                       color = model)) +
  geom_point(shape = NA) +
  geom_hex(color = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = 1, color = "black") +
  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(100,100000), oob = scales::squish,
    trans = "log10") +
  scale_color_manual(values = palette) +
  coord_equal() +
  # facet_wrap(~ basin) +
  theme_bw() +
  labs(x = "", y = "", color = "", fill = "") +
  theme(legend.position = c(0.2,0.75)) +
  guides(color = "none") +
  theme(text = element_text(size = 20))

ggExtra::ggMarginal(ggscater,
                    type = "density",
                    groupColour = TRUE)

df.r2 <- Trendy.data %>%
  group_by(model,var,basin) %>%
  summarise(r2 = summary(lm(formula = obs ~ pred))[["r.squared"]],
            obs.m = mean(obs,na.rm = TRUE),
            rmse = sqrt(1/length(obs)*(sum((obs-pred)**2,na.rm = TRUE))),
            .groups = "keep") %>%
  mutate(rel.RMSE = rmse/obs.m)


ggplot(data = df.r2) +
  geom_density(aes(x = r2,
                   fill = basin),
               alpha = 0.5, color = NA) +
  facet_wrap(~ var) +
  theme_bw()


df.r2.m <- Trendy.data.sum %>%
  group_by(model,var,basin) %>%
  summarise(r2 = summary(lm(formula = obs.m ~ pred.m))[["r.squared"]],
            obs.m = mean(obs.m,na.rm = TRUE),
            rmse = sqrt(1/length(obs.m)*(sum((obs.m-pred.m)**2,na.rm = TRUE))),
            .groups = "keep") %>%
  mutate(rel.RMSE = rmse/obs.m)


ggplot(data = df.r2.m) +
  geom_density(aes(x = r2,
                   fill = basin),
               alpha = 0.5, color = NA) +
  facet_wrap(~ var) +
  theme_bw()

################################################################################

Coord <- bind_rows(readRDS("./data/Amazon.coord.ILF.RDS") %>%
                     mutate(basin = "Amazon"),
                   readRDS("./data/Congo.coord.ILF.RDS") %>%
                     mutate(basin = "Congo")) %>%
  filter(model == "DLEM")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")

Coord %>%
  group_by(model,basin) %>%
  summarise(N = n()) %>%
  pivot_wider(names_from = basin,
              values_from = N) %>%
  mutate(ratio = Congo/Amazon)

ggplot(data = Coord) +
  geom_tile(aes(x=lon,y = lat, fill = basin),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  geom_sf(data = Congo.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, 45), ylim = c(-25, 10), expand = FALSE) +
  labs(x = "",y = "") +
  scale_fill_manual(values = c("darkgreen","#72a83d")) +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

################################################################################

Basin.Comp.mean.sum <- readRDS(paste0("./outputs/",
                                      Prefix,
                                      "mean.sum.RDS"))
MEM <- Basin.Comp.mean.sum %>%
  group_by(year,month,var,basin) %>%
  summarise(pred.m = mean(pred),
            obs.m = mean(obs),
            .groups = "keep") %>%
  mutate(time = year + (month - 1/2)/12)

ggplot(data = MEM,
       aes(x = time,
           y = pred.m,
           color = basin)) +
  # geom_line(aes(x = time,
  #               y = obs.m,
  #               group = basin),
  #           color = "grey") +
  geom_line(linetype = 1) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ var,scales = "free") +
  scale_x_continuous(limits = c(2000,2025)) +
  theme_bw()

Window = 6
year.min = 1994
year.max = 2023

MEM.reference <- MEM %>%
  filter(year >= year.min,
         year < year.max) %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(basin,var) %>%
  mutate(slope = coef(lm(pred.m ~ time))[2],
         intercept = coef(lm(pred.m ~ time))[1]) %>%
  ungroup() %>%
  dplyr::select(basin,var,slope,intercept) %>%
  distinct()

MEM.anomaly <- MEM %>%
  group_by(basin,var) %>%
  left_join(MEM.reference,
            by = c("basin","var")) %>%
  mutate(mean.obs =  intercept + slope *time) %>%
  # mutate(mean.obs = mean(pred.m[time >= year.min & time < year.max],
  #                        na.rm = TRUE)) %>%
  mutate(detrended = pred.m - mean.obs) %>%
  group_by(basin,var,month) %>%
  mutate(mean.month = mean(detrended[time >= year.min & time < year.max])) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(basin,var) %>%
  mutate(anomaly.m = anomaly/sd(anomaly[time >= year.min & time < year.max])) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         pred.m.rm = rollapply(pred.m, width=Window,
                               FUN=function(x) mean(x, na.rm=TRUE),
                               partial=TRUE, fill=NA, align="right"))

ggplot(data = MEM.anomaly %>%
         filter(var == "gpp")) +
  geom_line(aes(x = time,
                y = anomaly.m.rm,
                color = basin),
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = anomaly.m,
                 color = basin),
             size = 0.25) +
  # stat_smooth(aes(x = time,
  #                 y = anomaly.m,
  #                 color = basin),
  #             method = "lm", se = FALSE,
  #             linewidth = 0.25, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  facet_wrap(~ var) +
  scale_x_continuous(limits = c(1960,2025)) +
  theme_bw()

ggplot(data = MEM.anomaly %>%
         filter(var == "gpp")) +
  geom_line(aes(x = time,
                y = anomaly.m.rm,
                color = basin),
            linetype = 1, linewidth = 1) +
  geom_point(aes(x = time,
                 y = anomaly.m,
                 color = basin),
             size = 0.25) +
  # stat_smooth(aes(x = time,
  #                 y = anomaly.m,
  #                 color = basin),
  #             method = "lm", se = FALSE,
  #             linewidth = 0.25, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  facet_wrap(~ var) +
  scale_x_continuous(limits = c(1994,2025)) +
  theme_bw()

MEM.synchro <- MEM.anomaly %>%
  mutate(sign.anomaly = sign(anomaly.m)) %>%
  dplyr::select(time,year,month,var,basin,sign.anomaly,anomaly) %>%
  pivot_wider(names_from = basin,
              values_from = c(sign.anomaly,anomaly)) %>%
  mutate(synchro = (sign.anomaly_Amazon == sign.anomaly_Congo),
         mean.anomaly = (anomaly_Amazon + anomaly_Congo)/2,
         mean.anomaly.w = (5*anomaly_Amazon + anomaly_Congo)/6) %>%
  mutate(sign.synchro = case_when(synchro == FALSE ~ 0,
                                  synchro == TRUE & sign.anomaly_Amazon <= 0 ~ 1,
                                  TRUE ~ 2))

ggplot(data = MEM.synchro %>%
         filter(var == "gpp")) +
  geom_rect(data = data.frame(xmin = year.min,
                              xmax = year.max,
                              ymin = -Inf, ymax = Inf),
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill="grey",alpha = 0.5) +
  geom_line(aes(x = time,
                y = mean.anomaly.w),
            linewidth = 0.25) +
  geom_point(aes(x = time,
                 y = mean.anomaly.w,
                 color = as.factor(sign.synchro)),
             linetype = 1, linewidth = 1) +
  scale_x_continuous(limits = c(1970,2025)) +
  scale_color_manual(values = c("black","darkred","darkgreen")) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  facet_wrap(~ var) +
  theme_bw()


Diff <- MEM %>%
  filter(year >= 1941) %>%
  ungroup() %>%
  dplyr::select(-obs.m) %>%
  pivot_wider(names_from = basin,
              values_from = c(pred.m)) %>%
  mutate(diff = Amazon - Congo,
         diff.rel = 100*(Amazon - Congo)/Congo,
         time = year + (month - 1/2)/12) %>%
  group_by(var) %>%
  mutate(diff.rm = rollapply(diff, width=Window,
                             FUN=function(x) mean(x, na.rm=TRUE),
                             partial=TRUE, fill=NA, align="right"),
         diff.rel.rm = rollapply(diff.rel, width=Window,
                                 FUN=function(x) mean(x, na.rm=TRUE),
                                 partial=TRUE, fill=NA, align="right"))

ggplot(data = MEM ,
       aes(x = time,
           y = pred.m,
           color = basin)) +
  geom_line(size = 0.25, alpha = 0.5) +
  scale_x_continuous(limits = c(2000,2025)) +
  facet_wrap(~ var, scales = "free") +
  guides(color = "none") +
  scale_color_manual(values = c("darkgreen","#72a83d")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(text = element_text(size = 20))

ggplot(data = Diff) +
  geom_line(aes(x = time,
                y = diff),
            color = "lightgrey") +
  geom_line(aes(x = time,
                y = diff.rm),
            color = "black") +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_continuous(limits = c(1980,2025)) +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(x = "", y = "") +
  theme(text = element_text(size = 20))

saveRDS(MEM.anomaly %>%
          dplyr::select(year,month,var,basin,
                        pred.m,pred.m.rm,
                        mean.obs,
                        anomaly,anomaly.rm,
                        anomaly.m,anomaly.m.rm),
        "./outputs/CC.basins.RS.RDS")
