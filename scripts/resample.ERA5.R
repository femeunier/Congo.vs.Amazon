rm(list = ls())

library(TrENDY.analyses)
library(chillR)
library(raster)

years.select <- 1985:2014

# data <- readRDS("/home/femeunier/Documents/data/monthly.climate.global.ERA5.RDS") %>%
#   filter(abs(lat) <= 30,
#          year %in% years.select)
# variables <- colnames(data)[!(colnames(data) %in% c("year",'month',"lon","lat"))]

biomes <- readRDS("./outputs/biome.ERA5.1940.2023_global.RDS") %>%
  filter(model == "ORCHIDEE") %>%
  filter(abs(lat) <= 30)

grid <- rasterFromXYZ((biomes %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","MAT")))[,c("lon","lat","MAT")])

altitude <- readRDS("./data/Altitude.map.RDS")

# years <- sort(unique(data$year))

# cdata.rspld <- data.frame()
# for (cyear in years){
#   print(paste0("- ",cyear))
#   cdata.rspld <-
#     bind_rows(cdata.rspld,
#               resample.df.all.col(bigdf = data %>%
#                                     filter(year == cyear),
#
#                                   raster2resample = grid,
#                                   var.names = variables,
#                                   res = 0.00001,
#                                   verbose = FALSE))
# }
#
# saveRDS(cdata.rspld,
#         "./outputs/monthly.climate.pantropical.ERA5_coord.RDS")

cdata.rspld <- readRDS("./outputs/monthly.climate.pantropical.ERA5_coord.RDS")

cdata.rspld <- cdata.rspld %>%
  left_join(altitude,
            by = c("lon","lat"))
data.sum <- cdata.rspld %>%
  filter(year %in% years.select) %>%
  group_by(lon,lat,month) %>%
  summarise(tmp = mean(tmp,
                       na.rm = TRUE),
            tmin = mean(tmin,
                        na.rm = TRUE),
            tmax = mean(tmax,
                        na.rm = TRUE),
            pre = mean(pre,
                       na.rm = TRUE),
            z = unique(z),
            .groups = "keep") %>%
  ungroup() %>%
  filter(!is.na(tmp) & !is.na(pre) & !is.na(tmin) & !is.na(tmax)) %>%
  group_by(lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         # E = SPEI::thornthwaite(tmp - 273.15,
         #                        lat = unique(lat),
         #                        na.rm = TRUE,
         #                        verbose = FALSE)/Ndays,
         E = SPEI::hargreaves(tmin - 273.15,
                              tmax - 273.15,
                              lat = unique(lat),
                              Ra = NULL,
                              na.rm = TRUE,
                              verbose = FALSE)/Ndays,
         # E = SPEI::penman(tmin - 273.15,
         #                  tmax - 273.15,
         #                  tsun = daylength(unique(lat), seq(15,365,30),
         #                                   notimes.as.na = FALSE)[["Daylength"]],
         #                  lat = unique(lat),
         #                  z = unique(z),
         #                  na.rm = TRUE,
         #                  verbose = FALSE)/Ndays,
         Pmm = Ndays*pre*8,
         Etot = E*Ndays) %>%
  dplyr::select(-pre) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD),
         MAP = sum(Pmm),
         MAT = mean(tmp),
         Etot = sum(Etot),
         ETeq = "Penman") %>%
  filter(month == 1) %>%
  dplyr::select(-month)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")


ggplot() +
  geom_raster(data = data.sum,
              aes(x = lon, y = lat,
                  fill = Etot)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(limits = c(1000,2000),
                      low = 'darkblue',high = "darkred",midpoint = 1500,
                      oob = scales::squish) +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot() +
  geom_raster(data = data.sum,
              aes(x = lon, y = lat,
                  fill = MAP)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient(limits = c(0,3000),
                      low = 'white',high = "darkblue",
                      oob = scales::squish) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot() +
  geom_raster(data = data.sum,
              aes(x = lon, y = lat,
                  fill = MCWD)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(limits = c(-700,0),
                       midpoint = -350,
                      high = 'darkblue',mid = "white",low = "darkred",
                      oob = scales::squish) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot() +
  geom_raster(data = data.sum,
              aes(x = lon, y = lat,
                  fill = MAT - 273.15)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient(limits = c(0,30),
                      low = 'white',high = "darkred",
                      oob = scales::squish) +
  theme_bw() +
  theme(legend.position = "bottom")



ggplot() +
  geom_raster(data = data.sum,
              aes(x = lon, y = lat,
                  fill = z)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  # scale_fill_gradient(limits = c(0,30),
  #                     low = 'white',high = "darkred",
  #                     oob = scales::squish) +
  theme_bw() +
  theme(legend.position = "bottom")


saveRDS(data.sum,
        "./outputs/biome.pantropical.ERA5_coord.RDS")
