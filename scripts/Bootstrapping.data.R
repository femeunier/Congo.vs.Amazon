rm(list = ls())

library(reshape2)

coord <- expand.grid(lat = seq(-30.25,30.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                  lat = as.vector(unique(coord$lat))) %>%
  melt() %>%
  mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
         Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
  rename(lon = Var1,
         lat = Var2,
         area = value) %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100) %>%
  left_join(LandFrac,
            by = c("lat","lon")) %>%
  mutate(area = area*value)

data <- readRDS("./outputs/Data.used.RDS") %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  ungroup()


Nboot <- 1000

df.biomass <- readRDS("./outputs/Biomass.per.biome.all.RDS") %>%
  mutate(biome = as.numeric(biome))
sources <- unique(df.biomass$source)
Biomes <- unique(data$biome)

summ <- map <-
  data.frame()

for (iboot in seq(1,Nboot)){

  print(paste0("Iteration: ",iboot,"/",Nboot))

  csource <- sample(sources,1)
  cdf.biomass <- df.biomass
  cdf.all <- data

  newAGB <- newAGB.w <- rep(NA,nrow(data))
  for (cBiome in Biomes){
    # print(cBiome)
    cvec <- cdf.biomass %>%
      filter(biome == cBiome)

    cDF <- data %>%
      filter(biome == cBiome)

    N = nrow(cDF); pos <- which(cdf.all$biome == cBiome)

    ccAGB <- sample(cvec$AGB,size = N, replace = TRUE)

    newAGB[pos] <- ccAGB
  }

  cdf.all$AGB <- newAGB

  # map <- bind_rows(map,
  #                  cdf.all %>%
  #                    mutate(iter = iboot))

  summ <- bind_rows(summ,
                    cdf.all %>%
    group_by(biome) %>%
    summarise(AGB = sum(AGB*area,na.rm = TRUE)/1e12,
              Area.tot = sum(area)/1e12,
              .groups = "keep") %>%
      mutate(iter = iboot))
}

saveRDS(summ,
        "./outputs/ERA5.bootstrap.RDS")

stop()

summ.m <- summ %>%
  group_by(biome) %>%
  summarise(AGB.m = mean(AGB,na.rm = TRUE))


# map <- bind_rows(A,
#                  map %>%
#                    mutate(iter = iter + 500) %>%
#                    filter(iter > 500,
#                           iter <= 1000))
#
#
# map.m <- map %>%
#   group_by(lon,lat) %>%
#   summarise(AGB.low = quantile(AGB,0.025,na.rm = TRUE),
#             AGB.m = mean(AGB,na.rm = TRUE),
#             AGB.high = quantile(AGB,0.975,na.rm = TRUE),
#             .groups = "keep")
#
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot() +
#   geom_raster(data = map.m,
#               aes(x = lon, y = lat,
#                   fill = AGB.m)) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
#   scale_fill_gradient(low = "white",
#                       high = "darkgreen") +
#   #                      limits = c(-100,100),
#   #                      breaks = c(-100,100),
#   #                      oob = scales::squish) +
#   theme_bw() +
#   labs(x = "", y = "", fill = "") +
#   theme(legend.position = "bottom",
#         strip.background = element_blank(),
#         strip.text = element_blank(),
#         text = element_text(size = 20),
#         panel.spacing = unit(2, "lines"))




# saveRDS(map.m,
#         "./outputs/ERA5.map.RDS")


coord <- expand.grid(lat = seq(-30.25,30.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                  lat = as.vector(unique(coord$lat))) %>%
  melt() %>%
  mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
         Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
  rename(lon = Var1,
         lat = Var2,
         area = value) %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100) %>%
  left_join(LandFrac,
            by = c("lat","lon")) %>%
  mutate(area = area*value)

ERA5.map <- readRDS("./outputs/ERA5.map.RDS")
ERA5.map.sum <- ERA5.map %>%
  mutate(continent = case_when(lon <= -30 ~ "America",
                               lon <= 55 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  left_join(Gridarea %>%
              dplyr::select(lon,lat,area),
            by = c("lon","lat")) %>%
  group_by(continent) %>%
    summarise(AGB.tot = sum(AGB.m*area,
                            na.rm = TRUE)/1e12,
              .groups = "keep")

sum(ERA5.map.sum$AGB.tot)
