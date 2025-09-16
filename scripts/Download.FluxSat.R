rm(list = ls())

library(ncdf4)
library(dplyr)
library(ggplot2)
library(reshape2)

all.files <- read.table("allFluxSatLinks.txt",
                        header = FALSE)[[1]]

dest.dir <- "/data/gent/vo/000/gvo00074/felicien/FLuxSat/"

# IFL <- readRDS("./outputs/ILF2020.small.df") %>%
#   filter(is.undisturbed.factor == 1) %>%
#   mutate(lon.lat = paste0(lon,'.',lat))

IFL <- bind_rows(readRDS("./outputs/Amazon.IFL.coord.small.RDS") %>%
                   mutate(basin = "Amazon"),
                 readRDS("./outputs/Congo.IFL.coord.small.RDS") %>%
                   mutate(basin = "Congo"))%>%
    mutate(lon.lat = paste0(lon,'.',lat))

all.GPP <-
  data.frame()
ts.GPP.IFL <-
  data.frame()

for (ifile in seq(1,length(all.files))){
  clink <- all.files[ifile]


  ncfile <- file.path(dest.dir,basename(clink))

  cDate <- strsplit(basename(ncfile),"_")[[1]][6]
  cyear <- as.numeric(stringr::str_sub(cDate,1,4))
  cmonth <- as.numeric(stringr::str_sub(cDate,5,6))

  print(paste0(basename(ncfile)," - ", cyear,"/",cmonth))

  op.file <- paste0(dest.dir,"/GPP.IFL.",cyear,".",cmonth,".RDS")

  if (file.exists(op.file)){

    GPP.month <- readRDS(op.file)

    GPP.IFL <- GPP.month %>%
      left_join(IFL,
                by = c("lon","lat","lon.lat"))


  } else {
    system2("wget",
            c(clink,"-P",dest.dir))


    if (!file.exists(ncfile)){
      next()
    }
    nc <- nc_open(ncfile)

    all.lats <- round(ncvar_get(nc,"lat"),digits = 3)
    pos.lats <- which(abs(all.lats) <= 20)
    lats <- all.lats[pos.lats]

    all.lons <- round(ncvar_get(nc,"lon"),digits = 3)
    pos.lons <- which(all.lons >= -90 & all.lons <= 35)
    lons <- all.lons[pos.lons]

    times <- as.Date(ncvar_get(nc,"time"),"2000/01/01")

    GPP <- ncvar_get(nc,"GPP",
                     start = c(min(pos.lons),min(pos.lats),1),
                     count = c(length(pos.lons),length(pos.lats),-1))
    nc_close(nc)

    GPP.month <- melt(apply(GPP,c(1,2),mean),na.rm = TRUE) %>%
      rename(lon = Var1,
             lat = Var2) %>%
      mutate(lon = lons[lon],
             lat = lats[lat]) %>%
      filter(!is.na(value)) %>%
      mutate(year = cyear,
             month = cmonth) %>%
      mutate(lon.lat = paste0(lon,".",lat)) %>%
      dplyr::filter(lon.lat %in% c(IFL[["lon.lat"]]))

    GPP.IFL <- GPP.month %>%
      left_join(IFL,
                by = c("lon","lat","lon.lat"))

    # all.GPP <- bind_rows(all.GPP,
    #                      GPP.month)

    saveRDS(GPP.month,
            op.file)

    system2("rm",c(ncfile))
  }

  ts.GPP.IFL <- bind_rows(ts.GPP.IFL,
                          GPP.IFL %>%
                            group_by(basin,year,month) %>%
                            summarise(value.m = mean(value,na.rm = TRUE),
                                      .groups = "keep"))


}

# saveRDS(all.GPP,
#         "./outputs/FluxSat.GPP.basins.RDS")
saveRDS(ts.GPP.IFL,
        "./outputs/ts.GPP.IFL.basin.RDS")


# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/ILF2020.small.df hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Download.FluxSat.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

