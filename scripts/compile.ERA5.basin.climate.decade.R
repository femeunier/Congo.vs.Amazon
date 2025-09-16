rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)
library(Congo.ED2)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggthemes)

convert.ERA5.year.select <- function(years2change,
                                       dir_prefix = "/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/ERA5_Tropics",
                                       OPpath_prefix = "./outputs/monthly.climate.pantropical.ERA5",
                                       coord = NULL){

  # years2change <- 1940:1949 + (decade - 1)*10

  all.df.years <- data.frame()

  vars <- c("t2m","tp","d2m")
  for (cyear in years2change){

    ncfile <- paste0(dir_prefix,"_",cyear,".nc")

    if (!file.exists(ncfile)) next()

    nc <- nc_open(ncfile)
    lons <- ncvar_get(nc,"longitude")
    lats <- ncvar_get(nc,"latitude")
    times <- as.Date(ncvar_get(nc,"time")/24,
                     origin = "1900-01-01")
    months <- month(times)

    all.df <- data.frame()

    for (cmonth in sort(unique(months))){

      print(paste0(cyear," - ", cmonth))

      pos.month <- which(months == cmonth)
      for (ivar in seq(1,length(vars))){

        cvar = vars[ivar]
        ctimes <- times[pos.month]

        temp <- length(dim(ncvar_get(nc,cvar)))

        if (temp == 4){

          data <- ncvar_get(nc,cvar,
                            start = c(1,1,1,min(pos.month)),
                            count = c(-1,-1,-1,length(pos.month)))

          expvers <- ncvar_get(nc,"expver")

          melt.data <- melt(data) %>%
            mutate(lon = lons[Var1],
                   lat = lats[Var2],
                   expver = expvers[Var3],
                   time = ctimes[Var4])

          if (!is.null(coord)){
            melt.data <- melt.data %>%
              mutate(lon.lat = paste0(lon,".",lat)) %>%
              filter(lon.lat %in% coord[["lon.lat"]])
          }

          df <- bind_rows(melt.data %>%
                            filter(expver == 1) %>%
                            filter(!is.na(value)) %>%
                            rename(!!cvar := "value") %>%
                            dplyr::select(-c("expver",starts_with("Var"))),
                          melt.data %>%
                            filter(expver == 5) %>%
                            filter(!is.na(value)) %>%
                            rename(!!cvar := "value") %>%
                            dplyr::select(-c("expver",starts_with("Var"))))

        } else{

          data <- ncvar_get(nc,cvar,
                            start = c(1,1,min(pos.month)),
                            count = c(-1,-1,length(pos.month)))

          df <- melt(data) %>%
            mutate(lon = lons[Var1],
                   lat = lats[Var2],
                   time = ctimes[Var3]) %>%
            rename(!!cvar := "value") %>%
            dplyr::select(-starts_with("Var"))

          if (!is.null(coord)){
            df <- df %>%
              mutate(lon.lat = paste0(lon,".",lat)) %>%
              filter(lon.lat %in% coord[["lon.lat"]])
          }
        }

        if (ivar > 1){
          all.df <- all.df %>%
            left_join(df,
                      by = c("lat","lon","time"))
        } else{
          all.df <- df
        }
      }

      cdf <- all.df %>%
        mutate(month = month(time),
               day = day(time)) %>%
        mutate(t = t2m - 273.15,
               dewpoint = d2m - 273.15) %>%
        mutate(beta = (112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t)),
               rh =   ((112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t))) ^ 8) %>%
        mutate(sh = PEcAn.data.atmosphere::rh2qair(rh,
                                                   t2m,
                                                   101325)) %>%
        mutate(VPD = PEcAn.data.atmosphere::get.vpd(rh*100,
                                                    t)) %>%
        group_by(month,day,lat,lon) %>%
        mutate(tmin = min(t2m),
               tmax = max(t2m)) %>%
        group_by(lat,lon,month,day) %>%
        summarise(tmp = mean(t2m),
                  tmin = mean(tmin),
                  tmax = mean(tmax),
                  pre = mean(tp)*1000*3,
                  VPD = mean(VPD),
                  .groups = "keep")

      all.df.years <- bind_rows(
        all.df.years,
        cdf %>%
          mutate(year = cyear))

    }

    nc_close(nc)

  }

  saveRDS(all.df.years,
          paste0(OPpath_prefix,"_",min(years2change),".RDS"))

}
################################################################################

all.years <- 1940:2024

overwrite = FALSE

years2change <- all.years[sample(1:length(all.years),length(all.years),replace = FALSE)]

for (cyear in years2change){

  OP.file <- paste0("./outputs/monthly.climate.basins.ERA5_",cyear,".RDS")

  if (file.exists(OP.file) & !overwrite){
    next()
  }

  basins <- bind_rows(readRDS("./outputs/Amazon.coord.ERA5.RDS"),
                      readRDS("./outputs/Congo.coord.ERA5.RDS")) %>%
    mutate(lon.lat = paste0(lon,".",lat))

  # world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  # Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
  #                       layer = "amazon_sensulatissimo_gmm_v1")
  # Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
  #                      layer = "CongoBasin")
  #
  #
  # ggplot(data = basins) +
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

  convert.ERA5.year.select(cyear,
                           dir_prefix = "/data/gent/vo/000/gvo00074/ED_common_data/met/global/ERA5_global",
                           OPpath_prefix = "./outputs/monthly.climate.basins.ERA5",
                           coord = basins)

}

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/compile.ERA5.basin.climate.decade.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
