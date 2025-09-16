rm(list = ls())

library(raster)
library(stringr)
library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

dir <- "/home/femeunier/Downloads/MOD/"
dir <- "/data/gent/vo/000/gvo00074/felicien/R/MOD"

files <- list.files(dir,full.names = TRUE)

plots <- read.csv("~/Downloads/plot_coordinates_MCWD.csv") %>%
  distinct() %>%
  na.omit() %>%
  mutate(biome = case_when(lon <= -35 ~ "Amazon",
                           lon <= 60 ~ "Congo")) %>%
  group_by(lon,lat,biome) %>%
  summarise(Plot_ID_Veg = paste(Plot_ID_Veg,collapse = "|"),
            .groups = "keep") %>%
  dplyr::select(lon,lat,biome,Plot_ID_Veg) %>%
  ungroup() %>%
  mutate(site = 1:n())

plots.sp <- SpatialPoints(plots[,c("lon","lat")])

buffer <- 1e-6
plots.sp.buffer <- st_buffer(st_as_sf(plots.sp),buffer)

all.df <- data.frame()

for (cfile in files){

  print(cfile)

  cEVI <- terra::rast(cfile,subds = 2)*0.0001
  cQA <- terra::rast(cfile,subds = 13)

  cname <- strsplit(basename(cfile),"\\.")[[1]][2]
  cyear <- as.numeric(str_sub(gsub(".*?([0-9]+).*", "\\1",cname),1,4))
  cday <- as.numeric(str_sub(gsub(".*?([0-9]+).*", "\\1",cname),5,8))
  cdate <- as.Date(cday - 1, origin = paste0(cyear,"-01-01"))
  cmonth <- month(cdate)

  cpoints <- terra::extract(cEVI, plots.sp.buffer)
  cpoints.QA <- terra::extract(cQA,plots.sp.buffer)
  names(cpoints) <- 1:length(cpoints) ;  names(cpoints.QA) <- 1:length(cpoints.QA)

  df.cpoints <- data.frame(site = cpoints[,1],
                           value = cpoints[,2],
                           QA = cpoints.QA[,2]) %>%
    left_join(plots,
              by = "site")

  all.df <- bind_rows(all.df,
                      df.cpoints %>%
                        mutate(year = cyear,
                               month = cmonth,
                               var = "EVI"))
  rownames(all.df) <-  NULL

}

all.df.site <- all.df %>%
  group_by(biome,site,month) %>%
  summarise(EVI.m = mean(value,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(site) %>%
  mutate(Delta_EVI =
           max(EVI.m,na.rm = TRUE) -
           min(EVI.m,na.rm = TRUE)) %>%
  ungroup()

ggplot() +
  geom_line(data = all.df.site,
            aes(x = month, y = EVI.m,
                color = biome, group = site),
            size = 0.1) +
  theme_bw()


saveRDS(all.df.site,
        "./outputs/EVI.var.sites.RDS")

# scp hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/EVI.var.sites.RDS /home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/

all.df.site <- readRDS("./outputs/EVI.var.sites.RDS")
all.df.biome <- all.df.site %>%
  group_by(biome,month) %>%
  summarise(EVI.m.m = mean(EVI.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = all.df.site) +
  geom_line(aes(x = month, y = EVI.m, color = biome, group = site),
            size = 0.1) +
  theme_bw()

all.df.site.Delta <- all.df.site %>%
  ungroup() %>%
  group_by(biome,site,Delta_EVI) %>%
  summarise(EVI.M = max(EVI.m,na.rm = TRUE),
            EVI.av = mean(EVI.m,na.rm = TRUE),
            EVI.m = min(EVI.m,na.rm = TRUE),
            .groups = "keep") %>%
  distinct() %>%
  left_join(plots %>%
              dplyr::select(-biome),
            by = "site")

saveRDS(all.df.site.Delta,
        "./outputs/EVI.var.RDS")

# scp hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/EVI.var.RDS /home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/

all.df.site.Delta <- readRDS("./outputs/EVI.var.RDS")
all.df.site.Delta.long <- all.df.site.Delta %>%
  pivot_longer(cols = -c(biome,site,Plot_ID_Veg,lon,lat),
               names_to = "variable",
               values_to = "value")

ggplot(data = all.df.site.Delta.long) +
  geom_density(aes(x = value, fill = biome),
               color = NA, alpha = 0.5) +
  facet_wrap(~ variable) +
  theme_bw()

ggplot() +
  geom_line(data = all.df.site,
            aes(x = month, y = EVI.m, color= biome, group = site), size = 0.1) +
  geom_line(data = all.df.biome,
            aes(x = month, y = EVI.m.m, color = biome), size = 2) +
  theme_bw()


# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/extract.EVI.MOD.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
stop()
################################################################################

# Solution 2: ask Johanna
rm(list = ls())

raw.plots <- read.csv("~/Downloads/plot_coordinates_MCWD.csv") %>%
  mutate(biome = case_when(lon <= -35 ~ "Amazon",
                           lon <= 60 ~ "Congo"))
plots <- raw.plots  %>%
  distinct() %>%
  na.omit() %>%
  group_by(lon,lat,biome) %>%
  summarise(Plot_ID_Veg = paste(Plot_ID_Veg,collapse = "|"),
            .groups = "keep") %>%
  dplyr::select(lon,lat,biome,Plot_ID_Veg) %>%
  ungroup() %>%
  mutate(site = 1:n())


df <- read.csv("~/Downloads/monthlymeanevi_felicien_modis_masked.csv") %>%
  pivot_longer(cols = -c(X)) %>%
  mutate(site = extract_numeric(name)) %>%
  rename(month = X) %>%
  dplyr::select(-name)

df.merged <- df %>%
  left_join(raw.plots %>%
              rename(site = X),
            by = "site")

df.merged.sum <- df.merged %>%
  group_by(biome,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.merged) +
  geom_line(aes(x = month, y = value,
                color = biome, group = site), size = 0.1) +
  geom_line(data = df.merged.sum,
            aes(x = month, y = value.m,
                color = biome), size = 1) +
  theme_bw()

df.merged.calc <- df.merged %>%
  group_by(site,Plot_ID_Veg,biome,lat,lon) %>%
  summarise(EVI.M = max(value,na.rm = TRUE),
            EVI.av = mean(value,na.rm = TRUE),
            EVI.m = min(value,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(site) %>%
  mutate(Delta_EVI =
           EVI.M - EVI.m)

saveRDS(df.merged.calc,
        "./outputs/EVI.var.Johanna.RDS")
