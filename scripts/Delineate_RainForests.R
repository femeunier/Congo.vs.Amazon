# rm(list = ls())

library(dplyr)
library(ggplot2)
library(YGB)
library(raster)
library(terra)
library(tidyr)
library(sf)

LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

# Load your raster (replace with your file path)
r <- rast(rasterFromXYZ(LC))

# Step 1: Reclassify to isolate category 2
r_cat2 <- r == 2

# Step 2: Aggregate to reduce resolution (e.g., factor = 5 or more for rougher contour)
r_agg <- aggregate(r_cat2,
                   fact = 1,
                   fun = max)  # or 'mean' if you want a smoother envelope
r_agg <- round(r_agg)
plot(r_agg)

# Step 3: Convert to polygons
polys <- as.polygons(r_agg, dissolve = TRUE)

# Step 4: Simplify geometry for rough boundary
polys_simplified <- simplifyGeom(polys,
                                 tolerance = 0.01)  # adjust tolerance as needed

# Step 5: Convert to sf and save as shapefile
polys_sf <- st_as_sf(polys_simplified)
names(polys_sf)

# Suppose the column is named "layer" (common with terra::as.polygons)
polys_1 <- polys_sf %>% filter(LC == 1)
st_crs(polys_1) <- 4326  # EPSG code for WGS84
st_write(polys_1, "./data/Rainforests.shp",append = FALSE)
plot(polys_1)


################################################################################
# Same for CMIP6 ESM

LC.pred.MEM.CMIP6 <- readRDS("./outputs/LC.pred.MEM.CMIP6.RDS") %>%
  filter(model == "MEM",
         scenario == "ssp245") %>%
  dplyr::select(lon,lat,LC.pred) %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  mutate(LC.pred = factor(LC.pred,
                          levels = c(1,2,3))) %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(LC %in% c(1,2,3))

# Load your raster (replace with your file path)
r <- rast(rasterFromXYZ(LC.pred.MEM.CMIP6[,c("lon","lat","LC.pred")]))

plot(r)

# Step 1: Reclassify to isolate category 2
r_cat2 <- r == 2

# Step 2: Aggregate to reduce resolution (e.g., factor = 5 or more for rougher contour)
r_agg <- aggregate(r_cat2,
                   fact = 1,
                   fun = max)  # or 'mean' if you want a smoother envelope
r_agg <- round(r_agg)
plot(r_agg)

# Step 3: Convert to polygons
polys <- as.polygons(r_agg, dissolve = TRUE)

# Step 4: Simplify geometry for rough boundary
polys_simplified <- simplifyGeom(polys,
                                 tolerance = 0.01)  # adjust tolerance as needed

# Step 5: Convert to sf and save as shapefile
polys_sf <- st_as_sf(polys_simplified)
names(polys_sf)

# Suppose the column is named "layer" (common with terra::as.polygons)
polys_1 <- polys_sf %>%
  filter(LC.pred == 1)
st_crs(polys_1) <- 4326  # EPSG code for WGS84
st_write(polys_1, "./data/Rainforests_CMIP6.shp",append = FALSE)
plot(polys_1)



