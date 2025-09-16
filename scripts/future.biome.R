rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)
library(sf)
library(SPEI)

thresholds <-
  data.frame(basin = c("Amazon","Congo"),
             MCWD = c(-200,-250),
             MAP = c(1500,1000))

system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.*.classifications*",
              "./outputs/"))

coord <- bind_rows(readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Amazon.coord.RDS") %>%
  mutate(lon.lat = paste0(lon,".",lat)),
  readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/Congo.coord.RDS") %>%
    mutate(lon.lat = paste0(lon,".",lat)))
coord <- expand.grid(lat = seq(-23.25,23.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")

CMIP6.files <- list.files("./outputs","*classifications*",
                          full.names = TRUE)
CMIP6.files.selected <- (CMIP6.files[grepl("_rspld", CMIP6.files) &
                                       !grepl("constantE", CMIP6.files)])

df.all <- data.frame()
for (file in CMIP6.files.selected){

  CMIP6 <- readRDS(file)

  df.all <- bind_rows(df.all,
                      CMIP6)
}

df.all$MAT[df.all$MAT>500] <- NA

df.all.selected <-
  df.all %>%
    dplyr::select(period,scenario,lon,lat,model,MAP,MAT,MCWD) %>%
    mutate(source = "CMIP6") %>%
    mutate(lon.lat = paste0(lon,".",lat)) %>%
    filter(lon.lat %in% coord[["lon.lat"]]) %>%
  mutate(basin = case_when(lon <= -30 ~ "Amazon",
                           TRUE ~ "Congo")) %>%
  mutate(MCWD.threshold = thresholds$MCWD[match(basin,thresholds$basin)],
         MAP.threshold = thresholds$MAP[match(basin,thresholds$basin)]) %>%
  mutate(type = case_when(MCWD >= MCWD.threshold ~ 2,
                          MAP <= MAP.threshold ~ 1,
                          TRUE ~ 3)) %>%
  ungroup()

df.weights <- readRDS("./outputs/CMIP6.weights.RDS")
df.all.selected.weights <- df.all.selected %>%
  left_join(df.weights %>%
              dplyr::select(model,w),
            by = "model")

df.all.selected.sum <- df.all.selected.weights %>%
  group_by(basin,source,period,scenario,lat,lon) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            MAT.m = mean(MAT,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),

            MAP.w = weighted.mean(MAP,w,na.rm = TRUE),
            MAT.w = weighted.mean(MAT,w,na.rm = TRUE),
            MCWD.w = weighted.mean(MCWD,w,na.rm = TRUE),

            .groups = "keep") %>%
  mutate(MCWD.threshold = thresholds$MCWD[match(basin,thresholds$basin)],
         MAP.threshold = thresholds$MAP[match(basin,thresholds$basin)]) %>%
  mutate(type.m = case_when(MCWD.m >= MCWD.threshold ~ 2,
                            MAP.m <= MAP.threshold ~ 1,
                            TRUE ~ 3),
         type.w = case_when(MCWD.w >= MCWD.threshold ~ 2,
                            MAP.w <= MAP.threshold ~ 1,
                            TRUE ~ 3)) %>%
  ungroup()

df.all.selected.sum.long <-
  df.all.selected.sum %>% dplyr::select(-c(MCWD.threshold, MAP.threshold)) %>%
  pivot_longer(cols = c(MAP.m,MAT.m,MCWD.m,type.m,
                        MAP.w,MAT.w,MCWD.w,type.w),
               names_to = "var",
               values_to = "value") %>%
  mutate(variable = sub("\\..*", "", var),
         weighting = sub(".*\\.", "", var))


ggplot() +
  geom_raster(data = df.all.selected.sum.long %>%
                filter(variable == "type",
                       period %in% c("current","long_future")),
              aes(x = lon, y = lat,
                  fill = as.factor(value))) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  coord_sf(xlim = c(-90, 55), ylim = c(-30, 20), expand = FALSE) +
  theme_bw() +
  facet_grid(weighting ~ scenario)


df2plot <- df.all.selected.sum.long %>%
  filter(variable == "type",
         period %in% c("current","long_future")) %>%
  dplyr::select(lon,lat,basin,scenario,weighting,value)  %>%
  group_by(basin,scenario,weighting,value) %>%
  mutate(N = n()) %>%
  group_by(weighting,basin,scenario) %>%
  arrange(lon,lat) %>%
  mutate(frac = N/sum(N)) %>%
  mutate(gridcell = 1:length(N))


ggplot(data = df2plot,
       aes(x = scenario, stratum = as.factor(value), alluvium = gridcell,
           y = frac,
           fill = as.factor(value))) +
  # scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  # geom_text(stat = "stratum", size = 3) +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  facet_grid(weighting ~ basin) +
  theme_bw()

