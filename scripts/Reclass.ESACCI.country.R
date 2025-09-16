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
library(terra)

# e <- extent(-15,60,-15,10)
#
# str_name<-'./data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tif'
#
# imported_raster=crop(raster(str_name),e)
# names(imported_raster) <- "ESA_tropical_africa_LC"
# imported_raster.na <- imported_raster
# NAvalue(imported_raster.na) <- 210
# plot(imported_raster.na)
#
# # imported_raster.na.cropped <- crop(imported_raster.na,e)
# # imported_raster.mod <- imported_raster.na.cropped
#
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# trans.mat <- t(matrix(c(10,4,
#
#                         11,1,
#                         12,1,
#
#                         20,4,
#                         30,4,
#                         40,4,
#
#                         50,2,
#
#                         60,3,
#                         61,3,
#                         62,3,
#
#                         70,2,
#
#                         80,3,
#
#                         90,4,
#
#                         100,1,
#                         110,1,
#
#                         120,1,
#
#                         122,1,
#                         130,1,
#
#                         140,4,
#
#                         150,1,
#                         151,1,
#                         152,1,
#                         153,1,
#
#                         160,2,
#                         170,2,
#
#                         180,1,
#
#                         190,4,
#                         200,4,
#                         201,4,
#                         202,4,
#                         210,4,
#                         220,4),
#                       nrow = 2))
#
# # mod_raster <- reclassify(imported_raster.mod,
# #                          trans.mat)
# #
# # writeRaster(mod_raster,
# #             '/data/gent/vo/000/gvo00074/felicien/ELIE/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_Congo.tif',
# #             options=c('TFW=YES'),overwrite = TRUE)
#
# # system2("rsync",
# #         c("-avz",
# #           "hpc:/data/gent/vo/000/gvo00074/felicien/ELIE/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_Congo.tif",
# #           "./data/"))
#
# # # https://maps.elie.ucl.ac.be/CCI/viewer/download/CCI-LC_Maps_Legend.pdf
# # 10 = cropland,
# # 11 = Herbaceous cover
# # 12 = Tree or shrub cover
# # 20 = Cropland, irrigated or post flooding
# # 30 = Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)
# # 40 = Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)
# # 50 = Tree cover, broadleaved, evergreen, closed to open (>15%)
# # 60 = Tree cover, broadleaved, deciduous, closed to open (>15%)
# # 61 = Tree cover, broadleaved, deciduous, closed (>40%)
# # 62 = Tree cover, broadleaved, deciduous, open (15‐40%)
# # 70 = Tree cover, needleleaved, evergreen, closed to open (>15%)
# # 80 = Tree cover, needleleaved, deciduous, closed to open (>15%)
# # 90 = Tree cover, mixed leaf type (broadleaved and needleleaved)
# # 100 = Mosaic tree and shrub (>50%) / herbaceous cover (<50%)
# # 110 = Mosaic herbaceous cover (>50%) / tree and shrub (<50%)
# # 120 = Shrubland
# # 122 = Deciduous shrubland
# # 130 = Grassland
# # 140 = Lichens and mosses
# # 150 = Sparse vegetation (tree, shrub, herbaceous cover) (<15%)
# # 151 = Sparse tree (<15%)
# # 152 = Sparse shrub (<15%)
# # 153 = Sparse herbaceous cover (<15%)
# # 160 = Tree cover, flooded, fresh or brakish water
# # 170 = Tree cover, flooded, saline water
# # 180 = Shrub or herbaceous cover, flooded, fresh/saline/brakish wate
# # 190 = Urban areas
# # 200 = Bare areas
# # 201 = Consolidated bare areas
# # 202 = Unconsolidated bare areas
# # 210 = Water bodies
# # 220 = Permanent snow and ice
#
# init.class <- data.frame(
#   num = c(10:12,seq(20,60,10),61:62,
#           seq(70,120,10),
#           122,
#           seq(130,150,10),
#           151,152,153,
#           seq(160,200,10),
#           201,202,210,220),
#   veg.type = c("cropland",
#                "Herbaceous cover",
#                "Tree or shrub cover",
#                "Cropland, irrigated or post flooding",
#                "Mosaic cropland (>50%) / natural vegetation (tree, shrub, herbaceous cover) (<50%)",
#                "Mosaic natural vegetation (tree, shrub, herbaceous cover) (>50%) / cropland (<50%)",
#                "Tree cover, broadleaved, evergreen, closed to open (>15%)",
#                "Tree cover, broadleaved, deciduous, closed to open (>15%)",
#                "Tree cover, broadleaved, deciduous, closed (>40%)",
#                "Tree cover, broadleaved, deciduous, open (15‐40%)",
#                "Tree cover, needleleaved, evergreen, closed to open (>15%)",
#                "Tree cover, needleleaved, deciduous, closed to open (>15%)",
#                "Tree cover, mixed leaf type (broadleaved and needleleaved)",
#                "Mosaic tree and shrub (>50%) / herbaceous cover (<50%)",
#                "Mosaic herbaceous cover (>50%) / tree and shrub (<50%)",
#                "Shrubland",
#                "Deciduous shrubland",
#                "Grassland",
#                "Lichens and mosses",
#                "Sparse vegetation (tree, shrub, herbaceous cover) (<15%)",
#                "Sparse tree (<15%)",
#                "Sparse shrub (<15%)",
#                "Sparse herbaceous cover (<15%)",
#                "Tree cover, flooded, fresh or brakish water",
#                "Tree cover, flooded, saline water",
#                "Shrub or herbaceous cover, flooded, fresh/saline/brakish water",
#                "Urban areas",
#                "Bare areas",
#                "Consolidated bare areas",
#                "Unconsolidated bare areas",
#                "Water bodies",
#                "Permanent snow and ice"))
#
# # Reclassify
# # 1 = Grass/Savannah, 2 = Evergreen, 3 = Deciduous forest, 4 = Others
#
# df.trans <- as.data.frame(trans.mat) %>% rename(num = V1,
#                                                 new = V2)
# init.end.class <- init.class %>%
#   left_join(df.trans,
#             by = "num")
#
#
# df.Afr.coord <- as.data.frame(imported_raster.na,
#                               xy = TRUE) %>%
#   rename(lon = x,
#          lat = y,
#          num = ESA_tropical_africa_LC)
#
# points_sf <- st_as_sf(df.Afr.coord, coords = c("lon", "lat"), crs = 4326)
#
# # Load country polygons
# world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
#   dplyr::select(admin)  # keep only needed column
#
#
# st_agr(points_sf) <- "constant"  # prevent warnings
# world <- st_make_valid(world)    # ensure geometries are valid
#
# sf::sf_use_s2(FALSE)
#
# points_with_country <- st_join(points_sf,
#                                world,
#                                left = TRUE,
#                                join = st_intersects)
#
# as.data.frame(points_with_country)
#
# # 4. Drop geometry and keep only needed columns
# result_df <- points_with_country %>%
#   st_drop_geometry() %>%
#   as.data.frame() %>%
#   left_join(init.end.class,
#             by = "num")
#
# saveRDS(result_df,
#         "./outputs/Reclass.df.country.RDS")

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Reclass.ESACCI.country.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Reclass.df.country.RDS",
          "./outputs/"))

e <- extent(-15,60,-15,10)
str_name<-'./data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tif'
imported_raster=rast(crop(raster(str_name),e))
imported_raster.na <- imported_raster
imported_raster.na[imported_raster.na == 210] <- NA
plot(imported_raster.na)

gridsize <- terra::cellSize(imported_raster.na)
df.grid.size <- as.data.frame(gridsize,
                              xy = TRUE) %>%
  rename(lon = x,
         lat = y)


Reclass.df.country <- readRDS("./outputs/Reclass.df.country.RDS")
Reclass.df.country$area <- df.grid.size$area

sum(Reclass.df.country %>%
      filter(!is.na(new)) %>%
      pull(area))/1000/1000/1e6 # million km²

Reclass.df.country %>%
  ungroup() %>%
  filter(new == 2) %>%
  mutate(area.tot = sum(area,na.rm = TRUE)) %>%
  group_by(admin) %>%
  summarise(area = sum(area,na.rm = TRUE),
            area.tot = unique(area.tot),
            .groups = "keep") %>%
  mutate(frac = area/area.tot) %>%
  arrange(desc(frac))

Reclass.df.country %>%
  ungroup() %>%
  filter(new == 3) %>%
  mutate(area.tot = sum(area,na.rm = TRUE)) %>%
  group_by(admin) %>%
  summarise(area = sum(area,na.rm = TRUE),
            area.tot = unique(area.tot),
            .groups = "keep") %>%
  mutate(frac = area/area.tot) %>%
  arrange(desc(frac))


Reclass.df.country %>%
  ungroup() %>%
  filter(new == 4) %>%
  mutate(area.tot = sum(area,na.rm = TRUE)) %>%
  group_by(admin) %>%
  summarise(area = sum(area,na.rm = TRUE),
            area.tot = unique(area.tot),
            .groups = "keep") %>%
  mutate(frac = area/area.tot) %>%
  arrange(desc(frac))

Reclass.df.country %>%
  ungroup() %>%
  filter(new == 1) %>%
  mutate(area.tot = sum(area,na.rm = TRUE)) %>%
  group_by(admin) %>%
  summarise(area = sum(area,na.rm = TRUE),
            area.tot = unique(area.tot),
            .groups = "keep") %>%
  mutate(frac = area/area.tot) %>%
  arrange(desc(frac))
