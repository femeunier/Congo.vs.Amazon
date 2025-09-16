rm(list = ls())

library(raster)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(sf)
library(reshape2)
library(tidyr)

# https://zenodo.org/records/4161694
# file:///home/femeunier/Downloads/dataset_readme.pdf

files <- c("~/Downloads/deforestation_emission_0119_v2_inTg.tif",
           "~/Downloads/degradation_emission_0119_v2_inTg.tif",
           "~/Downloads/fireforest_emission_0119_v2_inTg.tif",
           "~/Downloads/firenonforest_emission_0119_v2_inTg.tif")
types <- c("deforestation",
           "degradation",
           "fireforest",
           "firenonforest")

df.all <- data.frame()

for (ifile in seq(1,length(files))){
  r <- stack(files[ifile])

  print(types[ifile])

  cname <- tools::file_path_sans_ext(basename(files[ifile]))

  for (i in 1:19){

    print(paste0("- ",i))
    df <- as.data.frame(r[[i]],xy = TRUE) %>%
      rename(lon = x,
             lat = y,
             value = !!paste0(cname,"_",i)) %>%
      filter(!is.na(value)) %>%
      filter(abs(lat) <= 23.25) %>%
      mutate(lat = round(lat,2),
             lon = round(lon,2))

    df.all <- bind_rows(df.all,
                        df %>%
                        mutate(year = i + 1999) %>%
                        mutate(type = types[ifile]))
  }

}

df.all.wide <- df.all %>%
  pivot_wider(names_from = type,
              values_from = value)

df.all.wide.sum <- df.all.wide %>%
  group_by(lon,lat) %>%
  summarise(deforestation.m =mean(deforestation,na.rm = TRUE),
            degradation.m =mean(degradation,na.rm = TRUE),
            fireforest.m = mean(fireforest,na.rm = TRUE),
            firenonforest.m = mean(firenonforest,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(all = deforestation.m + degradation.m + fireforest.m + firenonforest.m,
         all.forest = deforestation.m + degradation.m + fireforest.m)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")

ggplot(data = df.all.wide.sum) +
  geom_tile(aes(x=lon,y = lat,
                fill = all),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  geom_sf(data = Congo.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, 45), ylim = c(-25, 10), expand = FALSE) +
  labs(x = "",y = "") +
  theme_map() +
  scale_fill_gradient(low = "white", high = "darkred") +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

df.all.year <- df.all.wide %>%
  group_by(year) %>%
  summarise(deforestation.tot = sum(deforestation,na.rm = TRUE)/1000, # TgC
            degradation.tot = sum(degradation,na.rm = TRUE)/1000,
            fireforest.tot = sum(fireforest,na.rm = TRUE)/1000, # TgC
            firenonforest.tot = sum(firenonforest,na.rm = TRUE)/1000,
            .groups = "keep") %>%
  mutate(loss.tot = deforestation.tot + degradation.tot + fireforest.tot) %>%
  pivot_longer(cols = -c(year),
               values_to = "value",
               names_to = "variable")

r <- stack("~/Downloads/test10a_cd_ab_pred_corr_2000_2019_v2.tif")
df.biomass <- data.frame()
for (i in 1:19){

  print(paste0("Biomass - ",i))
  df <- as.data.frame(r[[i]],xy = TRUE) %>%
    rename(lon = x,
           lat = y,
           value = paste0("test10a_cd_ab_pred_corr_2000_2019_v2_",i)) %>%
    filter(!is.na(value)) %>%
    filter(abs(lat) <= 23.25) %>%
    mutate(lat = round(lat,2),
           lon = round(lon,2))

  df.biomass <- bind_rows(df.biomass,
                      df %>%
                        mutate(year = i + 1999))
}

hist(df.biomass$value/10)

df.biomass.sum <- df.biomass %>%
  group_by(lon,lat) %>%
  summarise(biomass.m = mean(value,na.rm = TRUE)/10,
            .groups = "keep")

ggplot(data = df.biomass.sum) +
  geom_tile(aes(x = lon,y = lat,
                fill = biomass.m),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  geom_sf(data = Congo.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, 45), ylim = c(-25, 10), expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  # guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

ggplot(data = df.all.year) +
  geom_line(aes(x = year, y = value, color = variable)) +
  theme_bw()

summary(lm(data = df.all.year %>%
     filter(variable == "loss.tot"),
   formula = value ~ year))

df.all.year %>%
  filter(variable %in% c("deforestation.tot",
                         "degradation.tot")) %>%
  pull(value) %>%
  sum()

ecoregions <- stack("~/Downloads/global_ecoregions.tif")[[1]]
df.ecoregions <- as.data.frame(ecoregions,
                               xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  mutate(lat = round(lat,2),
         lon = round(lon,2))

df.biomass.eco <- df.biomass %>%
  left_join(df.ecoregions,
            by = c("lon","lat"))


coord <- expand.grid(lat = seq(-30.25,30.25,0.1),
                     lon = seq(-179.75,179.75,0.1)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

LandFrac <- readRDS("~/Documents/projects/Congo.vs.Amazon//outputs/landFrac.small.RDS") %>%
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

df.biomass.eco.sum <- df.biomass.eco %>%
  rename(biomass = value) %>%
  left_join(Gridarea,
            by = c("lon","lat")) %>%
  group_by(year,global_ecoregions) %>%
  summarise(tot = sum(biomass/10*area,na.rm = TRUE)/1e12, # kgC/m2*m2
            .groups = "keep")

ggplot(data = df.biomass.eco.sum %>%
         filter(global_ecoregions %in% 101:103)) +
  geom_line(aes(x = year, y = tot)) +
  facet_wrap(~global_ecoregions,scale = "free") +
  theme_bw()

df.biomass.eco.sum %>%
  filter(year == 2000,
         global_ecoregions %in% 101:103)


df.all.wide.sum.eco <- df.all.wide.sum %>%
  left_join(df.ecoregions,
            by = c("lon","lat")) %>%
  filter(global_ecoregions %in% 101:103) %>%
  group_by(global_ecoregions) %>%
  summarise(all = sum(all)/1000,
            .groups = "keep")

################################################################################
# Figure
df.ecoregions.tropics <- df.ecoregions %>%
  filter(abs(lat) <= 23.25,
         global_ecoregions %in% c(101:109))


ggplot(data = df.ecoregions.tropics) +
  geom_raster(aes(x = lon,y = lat,
                  fill = as.factor(global_ecoregions)),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +

  coord_sf(xlim = c(-120, 160), ylim = c(-23.25, 23.25),
           expand = FALSE) +
  labs(x = "",y = "") +
  theme_map() +
  scale_fill_manual(values = c(rep("#253b10",3),
                               rep("#448704",3),
                               rep("#c49402",3))) +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

df2plot <- df.biomass.eco.sum %>%
  filter(global_ecoregions %in% c(101:109)) %>%
  mutate(continent = case_when(global_ecoregions %in% c(101,104,107) ~ "Americas",
                               global_ecoregions %in% c(102,105,108) ~ "Africa",
                               global_ecoregions %in% c(103,106,109) ~ "Australasia")) %>%
  mutate(continent = factor(continent,
                            levels = c("Americas","Africa","Australasia"))) %>%
  mutate(type = case_when(global_ecoregions %in% c(101,102,103) ~ "A",
                          global_ecoregions %in% c(104,105,106) ~ "B",
                          global_ecoregions %in% c(107,108,109) ~ "C")) %>%
  mutate(type = factor(type,
                       levels = c("C","B","A"))) %>%
  group_by(continent,type) %>%
  summarise(tot = mean(tot),
            .groups = "keep")

ggplot(data = df2plot) +
  geom_bar(aes(x = continent, fill = as.factor(type),
               y = tot),
           stat = "identity") +
  labs(x = "", y = "",fill = "") +
  guides(fill = "none") +
  scale_fill_manual(values = rev(c(rep("#253b10",1),
                               rep("#448704",1),
                               rep("#c49402",1)))) +
  theme_minimal() +
  # facet_grid(~ continent) +
  theme(text = element_text(size = 20))

d2plot.final <- df.all.wide.sum %>%
  left_join(Gridarea,
            by = c("lon","lat")) %>%
  mutate(all.forest.area = all.forest/area*1e9) %>% # TgC/m2 --> GgC/100 km2
  filter(is.finite(all.forest.area)) %>%
  filter(all.forest.area > 0,
         all.forest.area < 20)


CongoBasin <- st_read("/home/femeunier/Desktop/FWO/congo_basin_polyline.shp")

ggplot(data = d2plot.final) +
  geom_tile(aes(x=lon,y = lat,
                fill = (all.forest.area)*10),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  # geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  # geom_sf(data = Congo.shp,fill = NA, color = "black") +
  geom_sf(data = CongoBasin,color = "black",
          linewidth = 0.25,
          fill = NA) +
  coord_sf(xlim = c(-20, 50), ylim = c(-23.25, 23.25), expand = FALSE) +
  labs(x = "",y = "",fill = "") +
  theme_map() +
  scale_fill_gradient2(low = "white", mid = "orange",high = "darkred",
                       midpoint = 2.5,
                      limits = c(0,5), breaks = c(0,5),
                      oob = scales::squish) +
  # guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom")



df.all.wide.sum.tot <- df.all.wide.sum %>%
  ungroup() %>%
  mutate(continent = factor(case_when(lon <= - 35 ~ "Americas",
                                      lon <= 55 ~ "Africa",
                                      TRUE ~ "Australasia"),
                            levels = c("Americas","Africa","Australasia"))) %>%
  group_by(continent) %>%
  left_join(df.ecoregions.tropics,
            by = c("lon","lat")) %>%
  group_by(continent,global_ecoregions) %>%
  summarise(tot = sum(all.forest,na.rm = TRUE)/1000,
            .groups = "keep") %>%
  mutate(type = factor(case_when(global_ecoregions %in% c(101,102,103) ~ "A",
                          global_ecoregions %in% c(104,105,106) ~ "B",
                          global_ecoregions %in% c(107,108,109) ~ "C"),
                       levels = rev(c("A","B","C")))) %>%
  filter(!is.na(type))

ggplot(data = df.all.wide.sum.tot) +
  geom_bar(aes(x = continent, fill = as.factor(type),
               y = tot),
           stat = "identity") +
  labs(x = "", y = "",fill = "") +
  guides(fill = "none") +
  scale_fill_manual(values = rev(c(rep("#253b10",1),
                                   rep("#448704",1),
                                   rep("#c49402",1)))) +
  theme_minimal() +
  theme(text = element_text(size = 20))


df2plot %>%
  left_join(df.all.wide.sum.tot,
            by = c("continent","type")) %>%
  mutate(Nyear = tot.x/tot.y) %>%
  filter(Nyear<1000) %>%
  filter(type == "B")

