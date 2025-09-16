rm(list = ls())

library(rgee)
library(reshape2)
library(raster)
library(terra)

reticulate::py_run_file('auth.py')

# Load SoilGrids OCS asset (mean organic carbon stock in Mg/ha)
ocs <- ee$Image("projects/soilgrids-isric/ocs_mean")

# Select a specific depth layer — e.g., 0-5 cm
ocs_0_5 <- ocs$select("ocs_0-30cm_mean")

# Reproject and upscale to ~0.5° resolution (~55 km at equator)
ocs_0_5_upscaled <- ocs_0_5$
  reproject(
    crs = "EPSG:4326",
    scale = 1000  # approx 0.5° resolution
  )

# Define export region (global extent or custom bounding box)
region <- ee$Geometry$Rectangle(
  coords = c(-180, -30, 180, 30), proj = "EPSG:4326", geodesic = FALSE
)

# Export upscaled raster to Google Drive
task <- ee_image_to_drive(
  image = ocs_0_5_upscaled,
  description = "SOC_0_5cm_0_5deg",
  folder = "GEE_exports",
  fileNamePrefix = "SOC_0_5cm_0_5deg",
  region = region,
  scale = 1000,
  maxPixels = 1e13
)

task$start()


soc <- rast("~/Downloads/SOC_0_5cm_0_5deg_2025_05_26_21_36_35.tif")
plot(soc)


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

soc.rspld <- resample(raster(soc),rasterFromXYZ(coord %>%
                                   mutate(value = 1) %>%
                                   dplyr::select(lon,lat,value)))

soc.rspld.df <- as.data.frame(soc.rspld,xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  left_join(Gridarea,
            by = c("lon","lat")) %>%
  filter(value > 0,
         !is.na(ocs_0.30cm_mean))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = soc.rspld.df,
              aes(x = lon, y = lat,
                  fill = ocs_0.30cm_mean)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  labs(x = "",y = "",
       fill = "Aridity index") +
  theme(legend.position = "none",
        text = element_text(size = 20))

Biomes <- c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
            "Semiarid","Arid","Hyperarid")
data  <- readRDS("./outputs/Data.used.RDS") %>%
  left_join(Gridarea,
            by = c("lat","lon")) %>%
  ungroup() %>%
  mutate(biome = Biomes[biome]) %>%
  mutate(biome = factor(biome,
                        levels = Biomes)) %>%
  dplyr::select(lon,lat,biome,area,value)

data.vs.soc <- data %>%
  left_join(soc.rspld.df,
            by = c("lon","lat"))

ggplot(data = data.vs.soc) +
  geom_boxplot(aes(x = biome, y = ocs_0.30cm_mean/10,  # t/ha --> kg/m2
                   fill = biome),
               alpha = 0.8) +
  theme_bw() +
  scale_fill_manual(values = (c("#253b10","#005401","#448704","#86a540",
                                    "#c49402","#d0ce9a","#e5e4cb"))) +
  guides(linetype = "none") +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 20)) +
  guides(fill = "none")

LM <- (lm(data = data.vs.soc,
          formula = (ocs_0.30cm_mean/10) ~ biome))
summary(LM)


soilc_aov <- aov((ocs_0.30cm_mean/10) ~ biome, data = data.vs.soc)
summary(soilc_aov)
library(multcompView)
Tukey.soilc <- TukeyHSD(soilc_aov)
letters_soilc <- multcompLetters4(soilc_aov, Tukey.soilc)
print(letters_soilc)

