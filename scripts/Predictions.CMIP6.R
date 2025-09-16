rm(list = ls())

library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(tidyr)
library(stringr)
library(sf)

# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
#                       layer = "amazon_sensulatissimo_gmm_v1")
# Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
#                      layer = "CongoBasin")

Prefix <- "Basin.Comp.PreTmp"
continents <- c("Amazon","Congo")
vars <- c("gpp","npp","nep","nbp")
models <- get.model.names.TRENDY()

CMIP6models <- c("ACCESS-CM2","ACCESS-ESM1-5","AWI-CM-1-1-MR","CanESM5-1","CanESM5",
                 "CAS-ESM2-0","CESM2-WACCM","CMCC-CM2-SR5","CMCC-ESM2","FGOALS-g3","INM-CM4-8","INM-CM5-0",
                 "IPSL-CM6A-LR","KACE-1-0-G","MIROC6","MPI-ESM1-2-HR","MPI-ESM1-2-LR","MRI-ESM2-0",
                 "NorESM2-LM","NorESM2-MM","TaiESM1")


CO2 <- read.table("./data/CO2_1700_2019_TRENDYv2020.txt") %>%
  mutate(year = as.numeric(str_sub(V1,7,10)),
         CO2 = as.numeric(str_sub(V1,12,17))) %>%
  dplyr::select(year,CO2) %>%
  filter(year <= 2014)

#https://gmd.copernicus.org/articles/13/3571/2020/
CO2.scenarios <- read.csv("./data/CO2.global.csv") %>%
  pivot_longer(-year,
               names_to = "scenario",
               values_to = "CO2")

CO2.all <- bind_rows(CO2 %>%
                       mutate(scenario = "historical"),
                     CO2.scenarios)

all.df <- data.frame()

for (cCMIP6model in CMIP6models){

  CMIP6.file <- paste0("./outputs/","CMIP6.",cCMIP6model,".RDS")
  if (!file.exists(CMIP6.file)) next()

  CMIP6.model <- readRDS(CMIP6.file) %>%
    left_join(CO2.all,
              by = c("year","scenario"))

  for (cContinent in continents){

    CMIP6.model.continent <- CMIP6.model %>%
      filter(basin == cContinent)

    for (cmodel in models){
      for (cvar in vars){

        print(paste0(cCMIP6model," - ",cContinent," - ",cmodel," - ",cvar))

        AI.file <- paste0("./outputs/",Prefix,".",cContinent,".",cmodel,".S2.",cvar,".RDS")
        if (!file.exists(AI.file)){next()}

        AI.model <- readRDS(AI.file)

        CMIP6.model.continent[[cvar]] <- predict(AI.model,
                                       CMIP6.model.continent[,AI.model$finalModel$feature_names])
        CMIP6.model.continent.sum <- CMIP6.model.continent %>%
          group_by(year,scenario,basin) %>%
          summarise(value.m = mean(get(cvar)),
                    .groups = "keep")

        all.df <- bind_rows(all.df,
                            CMIP6.model.continent.sum %>%
                              mutate(var = cvar,
                                     model = cmodel,
                                     CMIP6model = cCMIP6model))

      }
    }
  }
}

saveRDS(all.df,
        paste0("./outputs/all.CMIP6.predictions.",Prefix,".RDS"))


# coord <- CMIP6.model %>%
#   ungroup() %>%
#   dplyr::select(lon,lat) %>%
#   distinct()
#
# ggplot(data = coord) +
#   geom_tile(aes(x=lon,y = lat,
#                 fill = 1),alpha = 1) +
#   geom_sf(data = world,fill = NA, color = "grey") +
#   geom_sf(data = Amazon.shp,fill = NA, color = "black") +
#   geom_sf(data = Congo.shp,fill = NA, color = "black") +
#
#   coord_sf(xlim = c(-85, 45), ylim = c(-25, 10), expand = FALSE) +
#   labs(x = "",y = "") +
#   theme_map() +
#   guides(fill = "none") +
#   theme(text = element_text(size = 20),
#         strip.background = element_blank(),
#         strip.text = element_blank())
#
# ggplot(data = CMIP6.model.sum %>%
#          filter(scenario != "historical")) +
#   geom_line(aes(x = year,
#                 y = gpp.m, color = scenario)) +
#   facet_wrap(~ basin) +
#   # scale_x_continuous(limits = c(1984,2100)) +
#   theme_bw()

# scp ./data/CO2.global.csv hpc:/data/gent/vo/000/gvo00074/felicien/R/data
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Predictions.CMIP6.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
