rm(list = ls())

library(raster)

biomes <- readRDS("./outputs/biome.ERA5.1940.2023_global.RDS") %>%
  filter(model == "ORCHIDEE") %>%
  filter(abs(lat) <= 30)

grid <- rasterFromXYZ((biomes %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","MAT")))[,c("lon","lat","MAT")])

dir <- "/home/femeunier/Documents/projects/Congo.vs.Amazon/data/biomass/"
files <- list.files(dir,pattern = "*.tif",
                    full.names = TRUE)
files <- files[!grepl("rspld",files)]

for (file in files){
  print(file)

  r <- raster(file)
  r.rspld <- resample(r,grid)

  new.file <- paste0(tools::file_path_sans_ext(file),"_rspld.tif")

  writeRaster(r.rspld,new.file,
              options=c('TFW=YES'))
}

files <- list.files(dir,pattern = "*.tif",
                    full.names = TRUE)
files <- files[grepl("rspld",files)]
# names <- c("ESA","whrc","Xu")
names <- c("Xu")
df.biomass <- data.frame()
for (ifile in seq(1,length(files))){
  file <- files[ifile] ; cname <- names[ifile]
  print(file)

  r <- raster(file)
  names(r) <- "AGB"

  df.biomass <- bind_rows(df.biomass,
                          as.data.frame(r,xy = TRUE) %>%
                            rename(lon = x,
                                   lat = y) %>%
                            mutate(source = cname))
}

# saveRDS(df.biomass,
#         "./outputs/Map.biomass.RDS")
