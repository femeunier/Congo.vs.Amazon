rm(list = ls())

library(ggplot2)
library(ggrepel)
library(raster)
library(dplyr)
library(ggforce)
library(ggthemes)
library(sf)
library(rnaturalearth)
library(dplyr)

e <- extent(-15,60,-15,10)

Gridarea <- readRDS("./outputs/GridArea.RDS") %>%
  mutate(lon = round(lon, digits = 2),
         lat = round(lat, digits = 2))

str_name<-'./data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tif'

imported_raster=crop(raster(str_name),e)
names(imported_raster) <- "ESA_tropical_africa_LC"
imported_raster.na <- imported_raster
NAvalue(imported_raster.na) <- 210
plot(imported_raster.na)


df.LC.ESA <- as.data.frame(imported_raster.na,
                           xy = TRUE) %>%
  filter(!is.na(ESA_tropical_africa_LC)) %>%
  rename(lon = x,
         lat = y,
         num = ESA_tropical_africa_LC)

# imported_raster.na.cropped <- crop(imported_raster.na,e)
# imported_raster.mod <- imported_raster.na.cropped

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

trans.mat <- t(matrix(c(10,4,

                        11,1,
                        12,1,

                        20,4,
                        30,4,
                        40,4,

                        50,2,

                        60,3,
                        61,3,
                        62,3,

                        70,2,

                        80,3,

                        90,4,

                        100,1,
                        110,1,

                        120,1,

                        122,1,
                        130,1,

                        140,4,

                        150,1,
                        151,1,
                        152,1,
                        153,1,

                        160,2,
                        170,2,

                        180,1,

                        190,4,
                        200,4,
                        201,4,
                        202,4,
                        210,4,
                        220,4),
                      nrow = 2))

# mod_raster <- reclassify(imported_raster.mod,
#                          trans.mat)
#
# writeRaster(mod_raster,
#             '/data/gent/vo/000/gvo00074/felicien/ELIE/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_Congo.tif',
#             options=c('TFW=YES'),overwrite = TRUE)

# system2("rsync",
#         c("-avz",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/ELIE/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_Congo.tif",
#           "./data/"))

mod_raster <- raster("./data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_Congo.tif")


# # https://maps.elie.ucl.ac.be/CCI/viewer/download/CCI-LC_Maps_Legend.pdf
# 10 = cropland,
# 11 = Herbaceous cover
# 12 = Tree or shrub cover
# 20 = Cropland, irrigated or post flooding
# 30 = Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# 40 = Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)
# 50 = Tree cover, broadleaved, evergreen, closed to open (>15%)
# 60 = Tree cover, broadleaved, deciduous, closed to open (>15%)
# 61 = Tree cover, broadleaved, deciduous, closed (>40%)
# 62 = Tree cover, broadleaved, deciduous, open (15‐40%)
# 70 = Tree cover, needleleaved, evergreen, closed to open (>15%)
# 80 = Tree cover, needleleaved, deciduous, closed to open (>15%)
# 90 = Tree cover, mixed leaf type (broadleaved and needleleaved)
# 100 = Mosaic tree and shrub (>50%) / herbaceous cover (<50%)
# 110 = Mosaic herbaceous cover (>50%) / tree and shrub (<50%)
# 120 = Shrubland
# 122 = Deciduous shrubland
# 130 = Grassland
# 140 = Lichens and mosses
# 150 = Sparse vegetation (tree, shrub, herbaceous cover) (<15%)
# 151 = Sparse tree (<15%)
# 152 = Sparse shrub (<15%)
# 153 = Sparse herbaceous cover (<15%)
# 160 = Tree cover, flooded, fresh or brakish water
# 170 = Tree cover, flooded, saline water
# 180 = Shrub or herbaceous cover, flooded, fresh/saline/brakish wate
# 190 = Urban areas
# 200 = Bare areas
# 201 = Consolidated bare areas
# 202 = Unconsolidated bare areas
# 210 = Water bodies
# 220 = Permanent snow and ice

init.class <- data.frame(
  num = c(10:12,seq(20,60,10),61:62,
           seq(70,120,10),
           122,
           seq(130,150,10),
           151,152,153,
           seq(160,200,10),
           201,202,210,220),
  veg.type = c("cropland",
               "Herbaceous cover",
               "Tree or shrub cover",
               "Cropland, irrigated or post flooding",
               "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)",
               "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)",
               "Tree cover, broadleaved, evergreen, closed to open (>15%)",
               "Tree cover, broadleaved, deciduous, closed to open (>15%)",
               "Tree cover, broadleaved, deciduous, closed (>40%)",
               "Tree cover, broadleaved, deciduous, open (15‐40%)",
               "Tree cover, needleleaved, evergreen, closed to open (>15%)",
               "Tree cover, needleleaved, deciduous, closed to open (>15%)",
               "Tree cover, mixed leaf type (broadleaved and needleleaved)",
               "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
               "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)",
               "Shrubland",
               "Deciduous shrubland",
               "Grassland",
               "Lichens and mosses",
               "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)",
               "Sparse tree (<15%)",
               "Sparse shrub (<15%)",
               "Sparse herbaceous cover (<15%)",
               "Tree cover, flooded, fresh or brakish water",
               "Tree cover, flooded, saline water",
               "Shrub or herbaceous cover, flooded, fresh/saline/brakish water",
               "Urban areas",
               "Bare areas",
               "Consolidated bare areas",
               "Unconsolidated bare areas",
               "Water bodies",
               "Permanent snow and ice"))

# Reclassify
# 1 = Grass/Savannah, 2 = Evergreen, 3 = Deciduous forest, 4 = Others

df.trans <- as.data.frame(trans.mat) %>% rename(num = V1,
                                                new = V2)
init.end.class <- init.class %>%
  left_join(df.trans,
            by = "num")

df.LC.ESA.class <- df.LC.ESA %>%
  filter(num != 210) %>%
  ungroup() %>%
  left_join(init.end.class,
            by = "num")

df.hemisp <- df.LC.ESA.class %>%
  filter(new == 1) %>%
  mutate(hemisph = case_when(lat < 0 ~ "S",
                             TRUE ~ "N")) %>%
  mutate(N = n()) %>%
  group_by(hemisph,veg.type) %>%
  summarise(Nc = n(),
            area = Nc*300*300/1000/1000,
            frac = n()/unique(N),
            .groups = "keep") %>%
  group_by(hemisph) %>%
  arrange(desc(frac))


saveRDS(df.LC.ESA.class,
        "./outputs/df.LC.ESA.class.RDS")




df.Afr <- data.frame(num = as.vector(imported_raster)) %>%
  left_join(init.end.class,
            by = "num")


df.Afr.sum <- df.Afr %>%
  filter(num != 210) %>%
  mutate(num = as.factor(num)) %>%
  group_by(veg.type,new) %>%
  summarise(N = length(new),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(prop = N/sum(N)) %>%
  arrange(desc(prop))


write.csv(df.Afr.sum %>%
  mutate(new.cat = case_when(new == 1 ~ "Savannas",
                             new == 2 ~ "Evergreen forests",
                             new == 3 ~ "Deciduous forests",
                             new == 4 ~ "Others")) %>%
  arrange(new) %>%
    mutate(extent = N*300*300/1000/1000/1e6) %>%
    dplyr::select(veg.type,new.cat,extent,prop) %>%
    mutate(extent = signif(extent,digits = 2),
           prop = signif(prop*100,digits = 2)),

  "./outputs/reclassification.csv")

Afr.count.data <- df.Afr.sum %>%
  mutate(new = as.factor(new),
         veg.type = as.character(veg.type)) %>%
  mutate(new.cat = case_when(new == 1 ~ "Savannas",
                             new == 2 ~ "Evergreen forests",
                             new == 3 ~ "Deciduous forests",
                             new == 4 ~ "Others")) %>%
  arrange(desc(new.cat)) %>%
  mutate(prop = prop*100) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
  mutate(labels = case_when(prop < 1 ~ NA_character_,
                            TRUE ~ veg.type)) %>%
  mutate(labels = case_when(labels == "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)" ~
                              "Mosaic cropland",
                            labels == "Tree cover, flooded, fresh or brakish water" ~ "Flooded forests",
                            labels == "Tree cover, broadleaved, deciduous, open (15‐40%)" ~ "Broadleaved deciduous open",
                            labels == "Tree cover, broadleaved, deciduous, closed to open (>15%)" ~
                              "Broadleaved deciduous, closed to open",
                            labels == "Tree cover, broadleaved, deciduous, closed (>40%)" ~
                              "Broadleaved deciduous, closed",
                            labels == "Tree cover, broadleaved, evergreen, closed to open (>15%)" ~
                              "Broadleaved evergreen",
                            labels == "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)" ~
                              "Mosaic herbs/shrubs",
                            labels == "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)" ~
                              "Mosaic cropland",
                            labels == "Shrub or herbaceous cover, flooded, fresh/saline/brakish water" ~
                              "Flooded shrub or herbs",
                            labels == "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)" ~
                              "Mosaic herbs/shrubs",
                            TRUE ~ labels)) %>%
  mutate(end = 2 * pi * cumsum(N)/sum(N),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

Afr.count.data.mod <- Afr.count.data %>%
  group_by(new.cat,labels) %>%
  summarise(start = min(start),
            end = max(end),
            labels = unique(labels),
            prop = sum(prop),
            .groups = "keep") %>%
  mutate(middle = (start + end)/2) %>%
  mutate(hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1)) %>%
  mutate(prop.character =
           case_when(prop > 2 ~ paste0(as.character(round(prop,1)),"%"),
                     TRUE ~ ""))

ggplot(Afr.count.data.mod) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = new.cat)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = labels,
                hjust = hjust, vjust = vjust)) +

  coord_fixed() +
  scale_x_continuous(limits = c(-2.5, 2),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-2, 1.1),    # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  scale_fill_manual(values = c("#448704","#005401","#c49402","grey"),
                    breaks = c("Deciduous forests","Evergreen forests",
                               "Savannas","Others")) +
  labs(fill = "") +
  theme_void()

Afr.count.data.mod %>%
  dplyr::select(new.cat,prop)


cuts=c(0,1,2,3,4) #set breaks
pal <- c("#c49402","#005401","#448704","grey")
plot(mod_raster,
     breaks=cuts, col = pal)
as.data.frame(X, xy = TRUE) %>%
  rename(lon = x,
         lat = y)
mod_raster_upscaled <- aggregate(mod_raster,
                                 fact = 0.5/res(mod_raster),
                                 fun = modal,
                                 na.rm = TRUE)
names(mod_raster_upscaled) <- "ESA_tropical_africa_LC"

df.LC.coarse <- as.data.frame(mod_raster_upscaled, xy = TRUE) %>%
  filter(!is.na(ESA_tropical_africa_LC)) %>%
  rename(lon = x,
         lat = y,
         LC = ESA_tropical_africa_LC)
X.fine <- terra::cellSize(terra::rast(mod_raster))
X.df <- as.data.frame(X.fine, xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  mutate(lon = round(lon, digits = 2),
         lat = round(lat, digits = 2)) %>%
  filter(lat >= -15, lat <= 10)


X <- terra::cellSize(terra::rast(mod_raster_upscaled))
X.df <- as.data.frame(X, xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  mutate(lon = round(lon, digits = 2),
         lat = round(lat, digits = 2)) %>%
  filter(lat >= -15, lat <= 10) %>%
  left_join(df.LC.coarse %>%
              mutate(lon = round(lon, digits = 2),
                     lat = round(lat, digits = 2)),
            by = c("lon","lat")) %>%
  filter(!is.na(LC)) %>%
  left_join(Gridarea,
            by = c("lon","lat"))
XXX <- X.df %>%
  group_by(LC) %>%
  summarise(S1 = sum(area.x/1e12*land.frac),
            S2 = sum(area.y/1e12*land.frac))

sum(XXX$S1)
sum(XXX$S2)

df.LC.fine <- as.data.frame(mod_raster,
                            xy = TRUE) %>%
  filter(!is.na(C3S.LC.L4.LCCS.Map.300m.P1Y.2020.v2.1.1_Congo)) %>%
  rename(lon = x,
         lat = y,
         LC = C3S.LC.L4.LCCS.Map.300m.P1Y.2020.v2.1.1_Congo)

saveRDS(df.LC.fine %>%
          filter(lat >= -15, lat <= 10,
                 lon >= -15, lon <= 60),
        file = "./data/LC_Congo_fine.RDS")

ggplot() +
  geom_tile(data = df.LC.coarse %>%
              filter(!is.na(LC)),
            aes(x = lon, y = lat,
                fill = as.factor(LC)),
            alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey"),
                    breaks = c(1,2,3,4)) +
  scale_x_continuous(limits = c(-15,60)) +
  scale_y_continuous(limits = c(-20,10)) +
  theme_map() +
  labs(x = "",y="",fill = "LC") +
  theme(legend.position = c(0.2,0.2)) +
  guides(fill = "none")


saveRDS(df.LC.coarse %>%
          filter(lat >= -15, lat <= 10,
                 lon >= -15, lon <= 60),
        file = "./data/LC_Congo.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/reclassify.ESA.LC.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


