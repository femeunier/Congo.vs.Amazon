rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)
library(plotbiomes)
library(raster)
library(ggplot2)
library(sf)
library(PEcAn.data.atmosphere)


input.file <-  "./outputs/monthly.climate.pantropical.2020.RDS"
df <- readRDS(input.file)

df.clean <- df %>%
  filter(year != 2020)
years <- 2020


outputfile <- "./outputs/monthly.climate.pantropical.2020_mod.RDS"


vars <- c("pre","tmp","tmin","tmax",
          "dlwrf","dswrf","spfh")
# vars <- c("pre")

# dir <- "/home/femeunier/Documents/projects/TrENDY.analyses/data"
dir <- "/data/gent/vo/000/gvo00074/felicien/TrENDY/inputs"

days2months <- c(31,28,31,30,31,30,
                 31,31,30,31,30,31)

all.days <- rep(1:12,days2months)
all.days.hours <- sort(rep(all.days,4))

df.all <- df.all.monthly <- data.frame()

prefix <- rev(c("2.1","2.2","2.3","2.3.1","2.4","2.5"))

for (cyear in years){

  print(paste0(cyear))
  cyr.df <- data.frame()

  temp.array <- array(data = NA,
                      dim = c(720,360,1460,length(vars)))

  ivar = 1
  for (cvar in vars){

    print(paste0("- ",cvar))

    i = 1 ; zip.file.exist = FALSE
    while (i <= length(prefix) & !zip.file.exist){

      zip.file <- file.path(dir,
                            paste0("crujra.v",prefix[i],".5d.",cvar,".",cyear,".365d.noc.nc.gz"))

      zip.file.exist <- file.exists(zip.file)

      i = i + 1
    }

    if (!zip.file.exist){
      warning(paste0("Zip file not found:", cvar," - ",cyear))
      next()
    }

    system2("gunzip",
            paste("-k",zip.file))

    nc.file <- file.path(dir,
                         paste0("crujra.v",prefix[i-1],".5d.",cvar,".",cyear,".365d.noc.nc"))

    nc <- nc_open(nc.file)
    lats <- ncvar_get(nc,"lat")
    lons <- ncvar_get(nc,"lon")

    data <- ncvar_get(nc,cvar)

    temp.array[,,,ivar] <- data
    ivar <- ivar + 1

    values <- apply(data,c(1,2),mean,na.rm = TRUE)
    montly <-
      montly.min <- montly.max <-
      array(NA,dim = c(dim(values),12))

    for (imonth in (1:12)){
      montly[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),
                                mean,
                                na.rm = TRUE)

      # if (cvar == "tmp"){
      #   montly.min[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),min,na.rm = TRUE)
      #   montly.max[,,imonth] <- apply(data[,,which(all.days.hours == imonth)],c(1,2),max,na.rm = TRUE)
      #
      #   montly.min[!is.finite(montly.min)] <- NA
      #   montly.max[!is.finite(montly.max)] <- NA
      #
      # }

    }

    cdf <- reshape2::melt(values) %>%
      rename(lon = Var1,
             lat = Var2) %>%
      mutate(lon = lons[lon],
             lat = lats[lat]) %>% filter(!is.na(value)) %>%
      rename(!!cvar := value)

    cdf.monthly <- reshape2::melt(montly) %>%
      rename(lon = Var1,
             lat = Var2,
             month = Var3) %>%
      mutate(lon = lons[lon],
             lat = lats[lat]) %>% filter(!is.na(value)) %>%
      rename(!!cvar := value) %>%
      filter(abs(lat) < 25)

    # if (cvar == "tmp"){
    #   cdf.monthly.min <- reshape2::melt(montly.min) %>%
    #     rename(lon = Var1,
    #            lat = Var2,
    #            month = Var3) %>%
    #     mutate(lon = lons[lon],
    #            lat = lats[lat]) %>% filter(!is.na(value)) %>%
    #     rename(tmp = value)
    #
    #   cdf.monthly.max <- reshape2::melt(montly.max) %>%
    #     rename(lon = Var1,
    #            lat = Var2,
    #            month = Var3) %>%
    #     mutate(lon = lons[lon],
    #            lat = lats[lat]) %>% filter(!is.na(value)) %>%
    #     rename(tmp := value)
    #
    #   cdf.monthly[["tmp.min"]] <- cdf.monthly.min[["tmp"]]
    #   cdf.monthly[["tmp.max"]] <- cdf.monthly.max[["tmp"]]
    # }

    if (cvar == vars[1]){
      # cyr.df <- cdf
      cmonth.df <- cdf.monthly
    } else {
      # cyr.df <- cyr.df %>%
      #   left_join(cdf,
      #             by = c("lat","lon"))

      cmonth.df <- cmonth.df %>%
        left_join(cdf.monthly,
                  by = c("lat","lon","month"))
    }



    system2("rm",nc.file)

  }

  print(paste0("- VPD"))

  RH <- qair2rh(temp.array[,,,which(vars == "spfh")],
                temp.array[,,,which(vars == "tmp")] - 273.15)
  VPD <- get.vpd(RH*100,
                 temp.array[,,,which(vars == "tmp")] - 273.15)

  montly <-
    montly.min <- montly.max <-
    array(NA,dim = c(dim(values),12))

  for (imonth in (1:12)){
    montly[,,imonth] <- apply(VPD[,,which(all.days.hours == imonth)],c(1,2),
                              mean,
                              na.rm = TRUE)

    # if (cvar == "tmp"){
    #   montly.min[,,imonth] <- apply(VPD[,,which(all.days.hours == imonth)],c(1,2),min,na.rm = TRUE)
    #   montly.max[,,imonth] <- apply(VPD[,,which(all.days.hours == imonth)],c(1,2),max,na.rm = TRUE)
    #
    #   montly.min[!is.finite(montly.min)] <- NA
    #   montly.max[!is.finite(montly.max)] <- NA
    #
    # }

  }

  cdf.monthly <- reshape2::melt(montly) %>%
    rename(lon = Var1,
           lat = Var2,
           month = Var3) %>%
    mutate(lon = lons[lon],
           lat = lats[lat]) %>% filter(!is.na(value)) %>%
    rename(VPD = value) %>%
    filter(abs(lat) < 25)

  cmonth.df <- cmonth.df %>%
    left_join(cdf.monthly,
              by = c("lat","lon","month"))



  # df.all <- bind_rows(list(df.all,
  #                          cyr.df %>% mutate(year = cyear)))

  df.all.monthly <- bind_rows(list(df.all.monthly,
                                   cmonth.df %>%
                                     mutate(year = cyear)))

}

final.df <- bind_rows(df.clean,
                      df.all.monthly)

saveRDS(final.df,outputfile)

# scp /home/femeunier/Documents/projects/TrENDY.analyses/scripts/correct.weird.CRUJRA.dswrf.year.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
