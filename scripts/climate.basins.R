rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(zoo)
library(ggthemes)

# all.coord <- bind_rows(readRDS("./outputs/Amazon.coord.ERA5.RDS"),
#                        readRDS("./outputs/Congo.coord.ERA5.RDS")) %>%
#   mutate(lon.lat = paste0(lon,".",lat))
# climate <- readRDS("/home/femeunier/Documents/data/monthly.climate.pantropical.ERA5.RDS") %>%
#   mutate(lon.lat = paste0(lon,".",lat))
#
# climate %>%
#   filter(year == 2024) %>%
#   pull(month) %>% unique()
#
# climate.filt <- climate %>%
#   filter(lon.lat %in% all.coord[["lon.lat"]])
# saveRDS(climate.filt,
#         "./outputs/monthly.climate.global.ERA5.basins.RDS")

# scp  hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/monthly.climate.global.ERA5.basins.RDS /home/femeunier/Documents/data/

climate <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/monthly.climate.global.ERA5.basins.RDS") %>%
  mutate(basin = case_when(lon <= -20 ~ "Amazon",
                           TRUE ~ "Congo")) %>%
  dplyr::select(-c(lon.lat))


climate.Amazon <- climate %>%
  filter(basin == "Amazon")

climate.Amazon.sum <- climate.Amazon %>%
  filter(year %in% 1971:2000) %>%
  group_by(lon,lat,month) %>%
  summarise(tmp.m = mean(tmp,na.rm = TRUE),
            tmp.sd = sd(tmp,na.rm = TRUE),
            pre.m = mean(pre,na.rm = TRUE),
            pre.sd = sd(pre,na.rm = TRUE),
            .groups = "keep")

plot(climate.Amazon.sum$pre.m[1:12]*8*30)

climate.Amazon.anomal <- climate.Amazon %>%
  filter(year >= 1997) %>%
  left_join(climate.Amazon.sum,
            by = c("lon","lat","month")) %>%
  mutate(anomaly.tmp = tmp >= (tmp.m + 2*tmp.sd),
         anomaly.pre = pre <= (pre.m - 2*pre.sd)) %>%
  mutate(anomaly = anomaly.tmp & anomaly.pre) %>%
  # mutate(timing = case_when(year %in% c(1997:1998) ~ "1997",
  #                           year %in% c(2015:2016) ~ "2015",
  #                           year %in% c(2023:2024) ~ "2023",
  #                           TRUE ~ NA_character_)) %>%
  # filter(!is.na(timing)) %>%
  # group_by(timing,lon,lat) %>%
  # summarise(anomaly = any(anomaly),
  #           .groups = "keep") %>%
  # group_by(timing) %>%
  group_by(year,month) %>%
  summarise(frac = sum(anomaly)/length(anomaly),
            .groups = "keep")

plot(climate.Amazon.anomal$frac)

climate.Amazon.anomal %>%
  ungroup() %>%
  filter(year %in% 1997:1998) %>%
  # pull(frac) %>%
  filter(frac == max(frac))

climate.Amazon.anomal %>%
  ungroup() %>%
  filter(year %in% 2015:2016) %>%
  filter(frac == max(frac))


climate.Amazon.anomal %>%
  ungroup() %>%
  filter(year %in% 2023:2024) %>%
    filter(frac == max(frac))

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

climate.Amazon.anomal <- climate.Amazon %>%
  filter(year >= 1997) %>%
  left_join(climate.Amazon.sum,
            by = c("lon","lat","month")) %>%
  mutate(anomaly.tmp = tmp >= (tmp.m + 1*tmp.sd),
         anomaly.pre = pre <= (pre.m - 1*pre.sd),
         anomaly.tmp2 = tmp >= (tmp.m + 2*tmp.sd),
         anomaly.pre2 = pre <= (pre.m - 2*pre.sd)) %>%
  mutate(anomaly = case_when(anomaly.tmp2 & anomaly.pre2 ~ 3,
                             anomaly.tmp & anomaly.pre ~ 2,
                             anomaly.tmp | anomaly.pre ~ 1,
                             TRUE ~ 0)) %>%
  filter((year == 1997 & month == 10) |
           (year == 2015 & month == 9) |
           (year == 2023 & month == 10))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = climate.Amazon.anomal) +
  geom_raster(aes(x = lon,y = lat,
                  fill = as.factor(anomaly))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  scale_x_continuous(limits = c(-90,-30)) +
  scale_y_continuous(limits = c(-20,10)) +
  scale_fill_brewer(palette = "Reds") +
  facet_wrap(~ as.factor(year),ncol = 1) +
  theme_map() +
  # guides(fill = "none") +
  labs(x = "",y = "") +
  theme(strip.background = element_blank(),
        legend.position = "right",
        strip.text = element_blank(),
        text = element_text(size = 20))


climate.2023 <- climate %>%
  filter( (year == 2024) | (year == 2023 & month >= 6)) %>%
  group_by(basin) %>%
  summarise(tmp.m = mean(tmp,na.rm = TRUE) - 273.15,
            tmin.m = mean(tmin,na.rm = TRUE)  - 273.15,
            tmax.m = mean(tmax,na.rm = TRUE)  - 273.15,
            .groups = "keep")

climate.basin <- climate %>%
  pivot_longer(cols = -c(lon,lat,basin,year,month),
               values_to = "value",
               names_to = "variable") %>%
  group_by(basin,variable,year,month) %>%
  summarise(value.m = mean(value),
            value.min = mean(value) - 2,
            value.max = mean(value) + 1.5,
            .groups = "keep") %>%
  filter(!is.na(value.m))

year.min = 1970 ; year.max = 1999
climate.basin.anomaly <- climate.basin %>%
  mutate(time = year + (month -1/2)/12) %>%
  group_by(basin,variable) %>%
  mutate(mean.obs = mean(value.m[year %in% c(year.min:year.max)],
                         na.rm = TRUE)) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(variable,basin,month) %>%
  mutate(mean.month = mean(detrended,na.rm = TRUE)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(variable,basin,month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly[year %in% c(year.min:year.max)],na.rm = TRUE))


# climate.basin.anomaly <- bind_rows(
#   climate.basin.anomaly,
#   data.frame(year = 2024,
#              month = 6,
#              time = 2024+(6-1/2)/12,
#              variable = "tmp",
#              basin = c("Amazon","Congo"),
#              value.m = NA),
#   data.frame(year = 2024,
#              month = 6,
#              time = 2024+(6-1/2)/12,
#              variable = "pre",
#              basin = c("Amazon","Congo"),
#              value.m = NA)
# )

ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     year <= 2022),
            aes(x = month,
                y = value.m - 273.15,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly %>%
              filter(year == year.min,
                     variable == "tmp"),
            aes(x = month,
                y = reconstructed - 273.15),
            linewidth = 0.8,
            linetype = 1) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +

  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n temperature (°C)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank())


climatogram <- climate.basin.anomaly %>%
  filter(variable %in% c("pre","tmp")) %>%
  filter(year == 2000) %>%
  dplyr::select(basin,variable,month,reconstructed) %>%
  pivot_wider(names_from = variable,
              values_from = reconstructed) %>%
  mutate(pre = pre*8*30,
         tmp = tmp - 273.15)

ggplot(climatogram,
       aes(x= month)) +
  geom_bar(aes(y= pre/10, fill = "Precipitation"), stat="identity",
           fill = "lightblue")  +
  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     year >= 1970,
                     year <= 2022),
            aes(x = month,
                y = value.m - 273.15,
                group = year),
            color = "grey", size = 0.15) +
  geom_bar(data = climate.basin.anomaly %>%
             filter(variable == "pre",
                    time >= (2023 + (5/12))),
           aes(y= value.m*8*30/10, fill = "Precipitation"),
           alpha = 0.5,
           fill = "darkblue",
           stat="identity")  +
  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     time >= (2023 + (5/12))),
            aes(x = month,
                y = value.m - 273.15,
                group = as.factor(year)),
            color = "#d95f02",
            linewidth =  0.5,
            show.legend = FALSE) +
  geom_line(aes(y = tmp, color = "Temperature"),
            show.legend = FALSE) +
  labs(x = "") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_y_continuous(
    expression("Average Monthly Temperature " ( degree*C)),
    sec.axis = sec_axis(~ . * 10, name = "Monthly Precipitation (mm)")) +
  scale_colour_manual("", values = c("Temperature" = "black")) +
  # scale_fill_manual("", values = "lightblue") +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.position = "none") +
  facet_wrap(~ basin) +
  theme_bw()


thrshd <- climate.basin.anomaly %>%
  filter(variable == "tmp",
         year >= 1940, year < 1970) %>%
  group_by(basin,year) %>%
  summarise(MAT = mean(value.m) - 273.15,
            .groups = "keep") %>%
  group_by(basin) %>%
  summarise(MATm = mean(MAT),
            .groups = "keep")


thrshd2 <- climate.basin.anomaly %>%
  filter(variable %in% c("tmp","tmax")) %>%
  dplyr::select(basin,variable,value.m,year,month) %>%
  pivot_wider(names_from = variable,
              values_from = value.m) %>%
  mutate(tmp = tmp - 273.15,
         tmax = tmax - 273.15)

ggplot(data = thrshd2,
       aes(x = tmp,y = tmax, color = basin)) +
  geom_point() +
  stat_smooth(method = "lm") +
  # facet_wrap(~basin) +
  theme_bw()

LM <- thrshd2 %>%
  group_by(basin) %>%
  summarise(s = coef(lm(tmp ~ tmax))[2],
            i = coef(lm(tmp ~ tmax))[1]) %>%
  mutate(Teco = s*29 + i)

ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     year >= 1970,
                     year <= 2023),
            aes(x = month,
                y = value.m - 273.15,
                color = year,
                group = year), size = 0.15) +

  geom_line(data = climate.basin.anomaly %>%
              filter(year == year.min,
                     variable == "tmp"),
            aes(x = month,
                y = reconstructed - 273.15),
            linewidth = 0.8,
            linetype = 1) +


  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     time >= (2023 + (7/12))),
            aes(x = month,
                y = value.m - 273.15,
                group = as.factor(year)),
            color = "darkred",
            linewidth =  0.8,
            show.legend = FALSE) +

  geom_rect(data = thrshd,
            aes(xmin = -Inf,xmax = Inf,
                ymin = MATm + 2,ymax = Inf),
             linetype = 2,
             color = NA, fill = "lightgrey",
            alpha = 0.25) +

  geom_hline(data = thrshd,
             aes(yintercept = MATm + 3.5),
             linetype = 3,
             color = "grey2") +

  geom_hline(data = LM,
             aes(yintercept = Teco),
             linetype = 2,
             color = "grey2") +


  scale_x_continuous(breaks = 1:12,
                     limits = c(1,12),
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +

  scale_y_continuous(limits = c(21,28)) +

  facet_wrap(~ basin) +
  theme_bw() +
  scale_color_gradient2(low = "darkblue",mid = "grey",midpoint = 1995,
                       high = "darkred",
                       breaks = c(1970,2020)) +
  labs(y = "Monthly mean temperature (°C)", x = "",
       color = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "top",
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

df2plot <- climate.basin.anomaly %>%
  filter(variable == "tmp")

Window <- 12
df2plot.sum <- df2plot %>%
  group_by(basin) %>%
  mutate(value.m.rm = rollapply(value.m, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="center"),
         value.min.rm = rollapply(value.min, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="center"),
         value.max.rm = rollapply(value.max, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center"))


CMIP6 <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/CMIP6.RDS") %>%
  mutate(tas.m = tas.m - 273.15)

CMIP6.models <- CMIP6 %>%
  filter(scenario == "historical",
         year %in% c(1970:2000)) %>%
  group_by(model) %>%
  filter(mean(tas.m) > 23, mean(tas.m) < 26)


CMIP6.models2plot <- CMIP6 %>%
  filter(scenario != 'historical',
         model %in% CMIP6.models[["model"]]) %>%
  group_by(basin,scenario,model) %>%
  mutate(tas.m.rm = rollapply(tas.m, width=Window,
                              FUN=function(x) mean(x, na.rm=TRUE),
                              partial=TRUE, fill=NA, align="center")) %>%
  group_by(basin,year,scenario) %>%
  summarise(tas.m.rm.m = case_when(basin == "Amazon" ~ mean(tas.m.rm,na.rm = TRUE) - 0.8,
                                   basin == "Congo" ~ mean(tas.m.rm,na.rm = TRUE) - 0.2,
                                   TRUE ~ mean(tas.m.rm,na.rm = TRUE)),
            tas.m.rm.min = case_when(basin == "Amazon" ~ quantile(tas.m.rm,0.25,
                                                                  na.rm = TRUE) - 0.8,
                                     basin == "Amazon" ~ quantile(tas.m.rm,0.25,
                                                                  na.rm = TRUE) - 0.2,

                                   TRUE ~ quantile(tas.m.rm,0.25,
                                                   na.rm = TRUE)),
            tas.m.rm.max = case_when(basin == "Amazon" ~ quantile(tas.m.rm,0.75,
                                                                  na.rm = TRUE) - 0.8,
                                     basin == "Amazon" ~ quantile(tas.m.rm,0.75,
                                                                  na.rm = TRUE) - 0.2,

                                     TRUE ~ quantile(tas.m.rm,0.75,
                                                     na.rm = TRUE)),
            .groups = "keep")


df.before <- df2plot.sum %>%
  filter(year >= 1940, year < 1970)

ggplot() +

  geom_rect(data = thrshd,
            aes(xmin = -Inf,xmax = Inf,
                ymin = MATm + 2,ymax = MATm + 6),
            color = NA, fill = "lightgrey",
            alpha = 0.5) +

  geom_hline(data = thrshd,
             aes(yintercept = MATm + 3.5),
             linetype = 2,
             color = "grey2") +

  geom_hline(data = LM,
             aes(yintercept = Teco),
             linetype = 3,
             color = "grey2") +

  geom_line(data = df2plot %>%
              filter(year >= 1970),
            aes(x = time,
                y = value.m - 273.15), size = 0.15) +

  stat_smooth(data = df2plot %>%
                filter(year >= 1970),
              aes(x = time,
                  y = value.m - 273.15), linetype = 1,
              method = "lm", se = FALSE, linewidth = 0.4,
              fill = NA, color = "black") +

  geom_line(data = df2plot.sum %>%
              filter(year >= 1970),
            aes(x = time,
                y = value.m.rm - 273.15)) +
  geom_ribbon(data = CMIP6.models2plot %>%
              filter(year > 2024),
            aes(x = year, fill = scenario,
                ymin = tas.m.rm.min,ymax = tas.m.rm.max),
            alpha = 0.2) +
  geom_line(data = CMIP6.models2plot %>%
              filter(year > 2024),
            aes(x = year, color = scenario, y = tas.m.rm.m)) +
  # geom_segment(data = climate.2023,
  #              aes(x = -Inf, xend = Inf,
  #                  y = tmp.m, yend = tmp.m), linetype = 1,
  #              color = "black") +

  facet_wrap(~ basin) +
  # scale_y_continuous(limits = c(22,30)) +
  scale_x_continuous(limits = c(1970,2100),
                     breaks = seq(1980,2100,20),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Mean temperature (°C)",
       fill = "SSP") +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        axis.title.y =
          element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))


########################################################################


ggplot() +

  geom_boxplot(data = df.before %>%
                 filter(basin == "Amazon"),
               aes(x = 1960, y = value.m.rm - 273.15),
               outlier.shape = NA,
               width = 10, fill = "lightgrey") +

  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1950,2100),
                     breaks = seq(1960,2100,20),
                     labels = c("1940-1970",seq(1980,2100,20)),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(21.5,32),
                     breaks = seq(22,32,2),expand = c(0,0)) +
  labs(x = "",
       y = "Mean temperature (°C)",
       fill = "SSP") +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.margin = margin(0.5,1,0.5,0.5, "cm"))




ggplot() +

  geom_boxplot(data = df.before %>%
                 filter(basin == "Amazon"),
               aes(x = 1960, y = value.m.rm - 273.15),
               outlier.shape = NA,
               width = 10, fill = "lightgrey") +

  geom_rect(data = thrshd %>%
              filter(basin == "Amazon"),
            aes(xmin = -Inf,xmax = Inf,
                ymin = MATm + 2,ymax = MATm + 6),
            color = NA, fill = "lightgrey",
            alpha = 0.5) +

  geom_hline(data = thrshd %>%
               filter(basin == "Amazon"),
             aes(yintercept = MATm + 3.5),
             linetype = 2,
             color = "grey2") +

  geom_hline(data = LM %>%
               filter(basin == "Amazon"),
             aes(yintercept = Teco),
             linetype = 3,
             color = "grey2") +

  geom_line(data = df2plot %>%
              filter(year >= 1970,year < 2023,
                     basin == "Amazon"),
            aes(x = time,
                y = value.m - 273.15), size = 0.15) +

  stat_smooth(data = df2plot %>%
                filter(year >= 1970,year < 2023,
                       basin == "Amazon"),
              aes(x = time,
                  y = value.m - 273.15), linetype = 1,
              method = "lm", se = FALSE, linewidth = 0.4,
              fill = NA, color = "black") +

  geom_line(data = df2plot.sum %>%
              filter(year >= 1970, year < 2023,
                     basin == "Amazon"),
            aes(x = time,
                y = value.m.rm - 273.15)) +


  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1950,2100),
                     breaks = seq(1960,2100,20),
                     labels = c("1940-1970",seq(1980,2100,20)),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(21.5,32),
                     breaks = seq(22,32,2),expand = c(0,0)) +
  labs(x = "",
       y = "Mean temperature (°C)",
       fill = "SSP") +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.margin = margin(0.5,1,0.5,0.5, "cm"))



ggplot() +

  geom_boxplot(data = df.before %>%
                 filter(basin == "Amazon"),
               aes(x = 1960, y = value.m.rm - 273.15),
               outlier.shape = NA,
               width = 10, fill = "lightgrey") +

  geom_rect(data = thrshd %>%
              filter(basin == "Amazon"),
            aes(xmin = -Inf,xmax = Inf,
                ymin = MATm + 2,ymax = MATm + 6),
            color = NA, fill = "lightgrey",
            alpha = 0.5) +

  geom_hline(data = thrshd %>%
               filter(basin == "Amazon"),
             aes(yintercept = MATm + 3.5),
             linetype = 2,
             color = "grey2") +

  geom_hline(data = LM %>%
               filter(basin == "Amazon"),
             aes(yintercept = Teco),
             linetype = 3,
             color = "grey2") +

  geom_line(data = df2plot %>%
              filter(year >= 1970,
                     basin == "Amazon"),
            aes(x = time,
                y = value.m - 273.15), size = 0.15) +

  stat_smooth(data = df2plot %>%
                filter(year >= 1970,
                       basin == "Amazon"),
              aes(x = time,
                  y = value.m - 273.15), linetype = 1,
              method = "lm", se = FALSE, linewidth = 0.4,
              fill = NA, color = "black") +

  geom_line(data = df2plot.sum %>%
              filter(year >= 1970,
                     basin == "Amazon"),
            aes(x = time,
                y = value.m.rm - 273.15)) +


  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1950,2100),
                     breaks = seq(1960,2100,20),
                     labels = c("1940-1970",seq(1980,2100,20)),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(21.5,32),
                     breaks = seq(22,32,2),expand = c(0,0)) +
  labs(x = "",
       y = "Mean temperature (°C)",
       fill = "SSP") +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.margin = margin(0.5,1,0.5,0.5, "cm"))



ggplot() +

  geom_boxplot(data = df.before %>%
                 filter(basin == "Amazon"),
               aes(x = 1960, y = value.m.rm - 273.15),
               outlier.shape = NA,
               width = 10, fill = "lightgrey") +

  geom_rect(data = thrshd %>%
              filter(basin == "Amazon"),
            aes(xmin = -Inf,xmax = Inf,
                ymin = MATm + 2,ymax = MATm + 6),
            color = NA, fill = "lightgrey",
            alpha = 0.5) +

  geom_hline(data = thrshd %>%
               filter(basin == "Amazon"),
             aes(yintercept = MATm + 3.5),
             linetype = 2,
             color = "grey2") +

  geom_hline(data = LM %>%
               filter(basin == "Amazon"),
             aes(yintercept = Teco),
             linetype = 3,
             color = "grey2") +

  geom_line(data = df2plot %>%
              filter(year >= 1970,
                     basin == "Amazon"),
            aes(x = time,
                y = value.m - 273.15), size = 0.15) +

  stat_smooth(data = df2plot %>%
                filter(year >= 1970,
                       basin == "Amazon"),
              aes(x = time,
                  y = value.m - 273.15), linetype = 1,
              method = "lm", se = FALSE, linewidth = 0.4,
              fill = NA, color = "black") +

  geom_line(data = df2plot.sum %>%
              filter(year >= 1970,
                     basin == "Amazon"),
            aes(x = time,
                y = value.m.rm - 273.15)) +
  geom_ribbon(data = CMIP6.models2plot %>%
                filter(year > 2024,
                       basin == "Amazon"),
              aes(x = year, fill = scenario,
                  ymin = tas.m.rm.min,ymax = tas.m.rm.max),
              alpha = 0.2) +
  geom_line(data = CMIP6.models2plot %>%
              filter(year > 2024,
                     basin == "Amazon"),
            aes(x = year, color = scenario, y = tas.m.rm.m)) +

  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1950,2100),
                     breaks = seq(1960,2100,20),
                     labels = c("1940-1970",seq(1980,2100,20)),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(21.5,32),
                     breaks = seq(22,32,2),expand = c(0,0)) +
  labs(x = "",
       y = "Mean temperature (°C)",
       fill = "SSP") +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        plot.margin = margin(0.5,1,0.5,0.5, "cm"))


ggplot() +

  geom_boxplot(data = df.before %>%
                 filter(basin == "Congo"),
               aes(x = 1960, y = value.m.rm - 273.15),
               outlier.shape = NA,
               width = 10, fill = "lightgrey") +

  geom_rect(data = thrshd %>%
              filter(basin == "Congo"),
            aes(xmin = -Inf,xmax = Inf,
                ymin = MATm + 2,ymax = MATm + 6),
            color = NA, fill = "lightgrey",
            alpha = 0.5) +

  geom_hline(data = thrshd %>%
               filter(basin == "Congo"),
             aes(yintercept = MATm + 3.5),
             linetype = 2,
             color = "grey2") +

  geom_hline(data = LM %>%
               filter(basin == "Congo"),
             aes(yintercept = Teco),
             linetype = 3,
             color = "grey2") +

  geom_line(data = df2plot %>%
              filter(year >= 1970,
                     basin == "Congo"),
            aes(x = time,
                y = value.m - 273.15), size = 0.15) +

  stat_smooth(data = df2plot %>%
                filter(year >= 1970,
                       basin == "Congo"),
              aes(x = time,
                  y = value.m - 273.15), linetype = 1,
              method = "lm", se = FALSE, linewidth = 0.4,
              fill = NA, color = "black") +

  geom_line(data = df2plot.sum %>%
              filter(year >= 1970,
                     basin == "Congo"),
            aes(x = time,
                y = value.m.rm - 273.15)) +
  geom_ribbon(data = CMIP6.models2plot %>%
                filter(year > 2024,
                       basin == "Congo"),
              aes(x = year, fill = scenario,
                  ymin = tas.m.rm.min,ymax = tas.m.rm.max),
              alpha = 0.2) +
  geom_line(data = CMIP6.models2plot %>%
              filter(year > 2024,
                     basin == "Congo"),
            aes(x = year, color = scenario, y = tas.m.rm.m)) +

  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1950,2100),
                     breaks = seq(1960,2100,20),
                     labels = c("1940-1970",seq(1980,2100,20)),
                     expand = c(0,0)) +
  labs(x = "",
       y = "Mean temperature (°C)",
       fill = "SSP") +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  scale_color_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.margin = margin(0.5,1,0.5,0.5, "cm"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))



summary(lm(data = df.before %>%
             filter(year <= 1970,
                    basin == "Amazon"),
   formula = value.m.rm ~ year))

ggplot(data = df.before,
       aes(x = year + (month - 1/2)/12,
           y = value.m - 273.15)) +
  geom_line() +
  stat_smooth(method = "lm") +
  facet_wrap(~basin) +
  theme_bw()

CMIP6.df <- CMIP6 %>%
  filter(scenario != 'historical',
         model %in% CMIP6.models[["model"]]) %>%
  mutate(period = case_when(year %in% c(2021:2050) ~ 2040,
                            year %in% c(2071:2100) ~ 2060,
                            TRUE ~ NA)) %>%
  na.omit() %>%
  group_by(basin,year,scenario,model,period) %>%
  summarise(tas.m = mean(tas.m),
            .groups = "keep") %>%
  ungroup()




ggplot() +
  geom_ribbon(data = df2plot.sum %>%
                filter(year >= 1970,
                       basin == "Amazon"),
              aes(x = time,
                  ymin = value.min.rm - 273.15,
                  ymax = value.max.rm - 273.15),
              alpha = 0.5, fill = "darkgrey", color = NA) +
  geom_line(data = df2plot.sum %>%
              filter(year >= 1970,
                     basin == "Amazon"),
            aes(x = time,
                y = value.m.rm - 273.15),
            linetype = 2) +
  geom_line(data = df2plot.sum %>%
              filter(year %in% c(1990:2020),
                     basin == "Amazon"),
            aes(x = time,
                y = value.m.rm - 273.15)) +
  # scale_y_continuous(limits = c(22,30),
  #                    breaks = c()) +
  scale_x_continuous(limits = c(1970,2025),
                     breaks = c(1980,2000,2020),
                     labels = c("","","")) +
  labs(x = "",
       y = "") +
  theme_bw() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        # axis.text.x = element_text(angle = 0, vjust = 1, hjust=1),
        # legend.position = c(0.15,0.8),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))




ggplot() +

  geom_rect(data = thrshd,
            aes(xmin = -Inf,xmax = Inf,
                ymin = MATm + 2,ymax = MATm + 6),
            color = NA, fill = "lightgrey",
            alpha = 0.5) +

  geom_hline(data = thrshd,
             aes(yintercept = MATm + 3.5),
             linetype = 2,
             color = "grey2") +

  geom_hline(data = LM,
             aes(yintercept = Teco),
             linetype = 3,
             color = "grey2") +

  geom_line(data = df2plot %>%
              filter(year >= 1970),
            aes(x = time,
                y = value.m - 273.15), size = 0.15) +

  stat_smooth(data = df2plot %>%
                filter(year >= 1970),
            aes(x = time,
                y = value.m - 273.15), linetype = 1,
            method = "lm", se = FALSE, linewidth = 0.4,
            fill = NA, color = "black") +

  geom_line(data = df2plot.sum %>%
              filter(year >= 1970),
            aes(x = time,
                y = value.m.rm - 273.15)) +

  # geom_boxplot(data = CMIP6.df %>%
  #                filter(period == min(period)),
  #              width = 10,
  #              aes(x = (period), y = tas.m, fill = scenario),
  #              position=position_dodge(),
  #              alpha = 0.7,
  #              outlier.shape = NA) +
  #
  # geom_boxplot(data = CMIP6.df %>%
  #                filter(period == max(period)),
  #              width = 10,
  #              aes(x = (period), y = tas.m, fill = scenario),
  #              position=position_dodge(),
  #              alpha = 0.7,
  #              outlier.shape = NA) +

  facet_wrap(~ basin) +
  scale_y_continuous(limits = c(22,30)) +
  scale_x_continuous(limits = c(1970,2025),
                     breaks = c(1980,2000,2020),
                     labels = c("1980","2000","2020")) +
  labs(x = "",
       y = "Mean temperature (°C)",
       fill = "SSP") +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        # axis.text.x = element_text(angle = 0, vjust = 1, hjust=1),
        # legend.position = c(0.15,0.8),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))



ggplot() +
  geom_boxplot(data = CMIP6.df %>%
                 filter(period == min(period)),
               width = 10,
               aes(x = (period), y = tas.m, fill = scenario),
               position=position_dodge(),
               alpha = 0.7,
               outlier.shape = NA) +
  labs(fill = "") +
  theme_bw() +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31"),
                    labels = c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"))


ggplot() +
  geom_boxplot(data = CMIP6.df %>%
                 filter(period == min(period)),
               width = 10,
               aes(x = (period), y = tas.m, fill = scenario),
               position=position_dodge(),
               alpha = 0.7,
               outlier.shape = NA) +

  geom_boxplot(data = CMIP6.df %>%
                 filter(period == max(period)),
               width = 10,
               aes(x = (period), y = tas.m, fill = scenario),
               position=position_dodge(),
               alpha = 0.7,
               outlier.shape = NA) +

  facet_wrap(~ basin) +
  # scale_y_continuous(limits = c(21,36),breaks = c()) +
  scale_x_continuous(breaks = c(2040,2060),
                     labels = c("2021-2050","2071-2100")) +
  labs(x = "",y = "",
       fill = "SSP") +
  scale_fill_manual(values = c("#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     year <= 2022),
            aes(x = month,
                y = value.m - 273.15,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly %>%
              filter(year == year.min,
                     variable == "tmp"),
            aes(x = month,
                y = reconstructed - 273.15),
            linewidth = 0.8,
            linetype = 1) +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     time >= (2023 + (7/12))),
            aes(x = month,
                y = value.m - 273.15,
                group = as.factor(year),
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = FALSE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_color_manual(values = c("#fea25b","#d95f02")) +

  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n temperature (°C)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank())



ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "pre",
                     year <= 2022),
            aes(x = month,
                y = value.m*30.5*8,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly %>%
              filter(year == year.min,
                     variable == "pre"),
            aes(x = month,
                y = reconstructed*30.5*8),
            linewidth = 0.8,
            linetype = 1) +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "pre",
                     time >= (2023 + (7/12))),
            aes(x = month,
                y = value.m*30.5*8,
                group = as.factor(year),
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = FALSE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_color_manual(values = c("#fea25b","#d95f02")) +

  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n precipitation (mm)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank())



ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     year <= 2022),
            aes(x = month,
                y = anomaly,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     time >= (2023 + (7/12))),
            aes(x = month,
                y = anomaly,
                group = as.factor(year),
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = FALSE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_color_manual(values = c("#fea25b","#d95f02")) +

  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n temperature anomaly (°C)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank())



ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "pre",
                     year <= 2022),
            aes(x = month,
                y = anomaly,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "pre",
                     time >= (2023 + (7/12))),
            aes(x = month,
                y = anomaly,
                group = as.factor(year),
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = FALSE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_color_manual(values = c("#fea25b","#d95f02")) +

  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n precip anomaly (mm)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank())



climate.basin.anomaly %>%
  filter(variable == "tmp",
         time >= (2023 + (7/12))) %>%
  mutate(diff = (value.m) - c(reconstructed)) %>%
  group_by(basin) %>%
  summarise(m = mean(diff,na.rm = TRUE))


climate.basin.anomaly %>%
  filter(year == year.min,
         variable == "tmp") %>%
  group_by(basin) %>%
  summarise(m = mean(reconstructed - 273.15),
            Sd = sd(reconstructed),
            range = max(reconstructed) - min(reconstructed),
            .groups = "keep")


climate.MAT <- climate.basin.anomaly %>%
  filter(year >= 1970,
         variable == "tmp") %>%
  group_by(year,basin) %>%
  summarise(MAT = mean(value.m -273.15,
                       na.rm = TRUE),
            .groups = "keep")

climate.MAT.all <- climate.basin.anomaly %>%
  filter(variable %in% c("tmax","tmp")) %>%
  group_by(variable,year,basin) %>%
  summarise(MAT = mean(value.m -273.15,
                       na.rm = TRUE),
            .groups = "keep")

ggplot(data = climate.MAT.all %>%
         filter(year <= 2023,
                variable == "tmax"),
       aes(x = year, y = MAT,
           group = interaction(variable,basin))) +
  # geom_rect(aes(xmin = -Inf,xmax = Inf,
  #               ymin = 26,ymax = 32),
  #           fill = "lightgrey", alpha = 0.5, color = NA) +
  geom_hline(yintercept = 29, linetype = 2, linewidth = 0.5, color = "black") +
  geom_point(color = "black", size = 0.5) +
  geom_line(color = "black",linewidth = 0.25) +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1970,2025)) +
  # geom_smooth() +
  # geom_smooth(method = "lm", linetype = 2,
  #             linewidth = 0.5, se = FALSE) +
  theme_bw()

climate.MAT %>%
  filter(year >= 2000) %>%
  group_by(basin) %>%
  summarise(s = 10*coef(lm(formula = MAT ~ year))[2], # °C/decade
            p.val = summary(lm(formula = MAT ~ year))[["coefficients"]][2,4],
            .groups = "keep")


climate.MAT %>%
  filter(year %in% 1970:2000) %>%
  group_by(basin) %>%
  summarise(m = mean(MAT))

climate.MAT %>%
  filter(year <= 2023) %>%
  group_by(basin) %>%
  summarise(Sd = sd(MAT),
            CV = 100*sd(MAT)/mean(MAT))


#######################################################################
cmonth <- 6
climate.basin.anomaly.groups <- climate.basin.anomaly %>%
  mutate(groups = case_when(year == 2023 & month %in% c(8:12) ~ "2023",
                            year == 2024 ~ "2023",

                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(8:12) ~ "2015",

                            year == 1997 & month %in% 9:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",

                            TRUE ~ NA)) %>%
  mutate(value.m = case_when(!is.na(groups) ~ value.m,
                             TRUE ~ NA_real_))%>%
  mutate(value.m = case_when((year == 1998 & month == 5) |
                             (year == 2016 & month == 4) |
                             (year == 2010 & month == 5) ~ NA,
                           TRUE ~ value.m)) %>%
  arrange(year,month,variable)

ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     year <= 2022),
            aes(x = month,
                y = value.m - 273.15,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly %>%
              filter(year == year.min,
                     variable == "tmp"),
            aes(x = month,
                y = reconstructed - 273.15),
            linewidth = 0.8,
            linetype = 1) +

  geom_line(data = climate.basin.anomaly.groups %>%
              filter(!is.na(groups),
                     variable == "tmp"),
            aes(x = month,
                y = value.m - 273.15,
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = TRUE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#9be7d1","#1b9e77",
                                "#aca7ed","#7570b3",
                                "#fea25b","#d95f02")) +

  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n temperature (°C)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(color = "none")



ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "pre",
                     year <= 2022),
            aes(x = month,
                y = value.m*30.5*8,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly %>%
              filter(year == year.min,
                     variable == "pre"),
            aes(x = month,
                y = reconstructed*30.5*8),
            linewidth = 0.8,
            linetype = 1) +

  geom_line(data = climate.basin.anomaly.groups %>%
              filter(!is.na(groups),
                     variable == "pre"),
            aes(x = month,
                y = value.m*30.5*8,
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = TRUE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#9be7d1","#1b9e77",
                                "#aca7ed","#7570b3",
                                "#fea25b","#d95f02")) +

  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n precipitation (mm)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(color = "none")


climate.basin.anomaly.groups %>%
  filter(!is.na(groups),
         variable == "tmp") %>%
  group_by(basin,groups) %>%
  summarise(m = mean(value.m - reconstructed,
                     na.rm = TRUE))

climate.basin.anomaly.groups %>%
  filter(!is.na(groups),
         variable == "pre") %>%
  group_by(basin,groups) %>%
  summarise(m = 100*mean((value.m - reconstructed)/reconstructed,
                          na.rm = TRUE))



ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     year <= 2022),
            aes(x = month,
                y = anomaly,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly.groups %>%
              filter(!is.na(groups),
                     variable == "tmp"),
            aes(x = month,
                y = anomaly,
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = TRUE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#9be7d1","#1b9e77",
                                "#aca7ed","#7570b3",
                                "#fea25b","#d95f02")) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n temperature anomaly (°C)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(color = "none")


ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "pre",
                     year <= 2022),
            aes(x = month,
                y = anomaly*30.5*8,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly.groups %>%
              filter(!is.na(groups),
                     variable == "pre"),
            aes(x = month,
                y = anomaly*30.5*8,
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = TRUE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#9be7d1","#1b9e77",
                                "#aca7ed","#7570b3",
                                "#fea25b","#d95f02")) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n precipitation anomaly (mm)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(color = "none")



ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "tmp",
                     year <= 2022),
            aes(x = month,
                y = anomaly.m,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly.groups %>%
              filter(!is.na(groups),
                     variable == "tmp"),
            aes(x = month,
                y = anomaly.m,
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = TRUE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#9be7d1","#1b9e77",
                                "#aca7ed","#7570b3",
                                "#fea25b","#d95f02")) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n temperature norm. anomaly (°C)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(color = "none")


ggplot() +

  geom_line(data = climate.basin.anomaly %>%
              filter(variable == "pre",
                     year <= 2022),
            aes(x = month,
                y = anomaly.m,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.basin.anomaly.groups %>%
              filter(!is.na(groups),
                     variable == "pre"),
            aes(x = month,
                y = anomaly.m,
                color = as.factor(year)),
            linewidth =  0.8,
            show.legend = TRUE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#9be7d1","#1b9e77",
                                "#aca7ed","#7570b3",
                                "#fea25b","#d95f02")) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.8) +
  facet_wrap(~ basin) +
  theme_bw() +
  labs(y = "Monthly mean \r\n precipitation anomaly (mm)", x = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(color = "none")


climate.basin.anomaly.groups %>%
  filter(!is.na(groups)) %>%
  filter(variable %in% c("pre","tmp")) %>%
  group_by(basin,groups,variable) %>%
  summarise(Anomal.perc = mean(anomaly/reconstructed)*100,
            Anomal = mean(anomaly)) %>%
  arrange(basin,variable,groups)
