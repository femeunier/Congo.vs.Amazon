rm(list = ls())

library(dplyr)
library(tidyr)
library(raster)
library(ggplot2)

Dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Precip.Tropics"

files <- list.files(Dir,
                    pattern = ".Tropics.climate.rspld.RDS",
                    full.names = TRUE)

df.all <- data.frame()

variables <- c("tas",'tasmin','tasmax',"pre")

for (cfile in files){

  cmodel <- strsplit(basename(cfile),"\\.")[[1]][2]

  print(cmodel)

  A <- readRDS(cfile)

  colnames(A)[colnames(A) %in% c("pr","MAP","Pmm")] <- "pre"
  colnames(A)[colnames(A) %in% c("tmn","tmin","T2MMIN")] <- "tasmin"
  colnames(A)[colnames(A) %in% c("tmx","tmax","T2MMAX")] <- "tasmax"
  colnames(A)[colnames(A) %in% c("tmp","T2MMEAN")] <- "tas"

  CN <- colnames(A)

  missing.CN <- which(!(variables %in% CN))

  for (cmissing.CN in missing.CN){
    A[[variables[cmissing.CN]]] <- NA
  }

  cdf <- A %>%
    filter(!is.na(pre) | !is.na(tasmin) | !is.na(tasmax) | !is.na(tas)) %>%
    mutate(model = cmodel)

  if (!all(is.na(cdf$tas))){
    if (max(cdf$tas,na.rm = TRUE) < 100){
      cdf <- cdf %>%
        mutate(tas = tas + 273.15,
               tasmin = tasmin + 273.15,
               tasmax = tasmax + 273.15)
    }
  }




  df.all <- bind_rows(df.all,
                      cdf)

  print(c(nrow(cdf),nrow(df.all)))

}

load("./data/IPCC-WGI-reference-regions-v4_R.rda")

selected.df <- IPCC_WGI_reference_regions_v4[
  IPCC_WGI_reference_regions_v4@data$Acronym %in% c("WAF","CAF","NEAF","SEAF",
                                                    "WSAF","ESAF","MDG",
                                                    "SAH"),]

Search <- df.all %>%
  ungroup() %>%
  filter(model == model[1],
         year == year[1],
         month == month[1]) %>%
  dplyr::select(lon,lat) %>%
  distinct()

A <- Search
coordinates(A) <- ~lon + lat
proj4string(A) <- CRS("+proj=longlat +datum=WGS84")
matches <- over(A, selected.df)
Search$region <- matches$Acronym


reanalyses.region <- df.all %>%
  ungroup() %>%
  left_join(Search %>%
              dplyr::select(lon,lat,region) %>%
              distinct(),
            by = c("lon","lat"))

saveRDS(reanalyses.region %>%
          filter(!is.na(region)),
        "./outputs/Africa.climate.rspld.RDS")

print("OK")

# saveRDS(df.all,
#         file.path("./outputs/",
#                   "Pantropical.climate.rspld.RDS"))


# A <- readRDS(file.path("./outputs/",
#                        "Pantropical.climate.rspld.RDS"))

df.all %>%
  group_by(model) %>%
  summarise(Nyear = length(unique(year)),
            Ntot = n())

A.recent <- df.all %>%
  filter(year >= 1970)

A.seasonal <- A.recent %>%
  filter(year %in% c(1971:2000)) %>%
  group_by(model,lon,lat,month) %>%
  summarise(tasmin = mean(tasmin,na.rm = TRUE),
            tas = mean(tas,na.rm = TRUE),
            tasmax = mean(tasmax,na.rm = TRUE),
            pre = mean(pre,na.rm = TRUE),
            .groups = "keep")

print(c(nrow(df.all),nrow(A.recent),nrow(A.seasonal)))

saveRDS(A.recent,
        file.path("./outputs/",
                  "Pantropical.climate.recent.rspld.RDS"))

saveRDS(A.seasonal,
        file.path("./outputs/",
                  "Pantropical.climate.recent.seasonal.rspld.RDS"))

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/combine.all.Tropical.climate.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

