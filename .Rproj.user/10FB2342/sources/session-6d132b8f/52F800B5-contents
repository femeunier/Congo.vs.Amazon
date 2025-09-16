rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(tidyr)

all.vars <- c("TPRAT_GDS4_SFC_S130",
              "DSWRF_GDS4_NTAT_S130",
              "DLWRF_GDS4_SFC_S130")
var.names <- c("pre","dswrf","dlwrf")

first = TRUE ; ratios <- c()

correction <- c(1/4,16550*0.9857,1)

all.years <- data.frame()
df.rspld.all <- bind_rows()
for (cyear in seq(2020,2023)){

  for (cmonth in seq(1,12)){

    print(paste0(cyear," - ", cmonth))
    cyearmonth <- paste0(cyear,sprintf("%02d",cmonth))

    df.all <- data.frame()

    ncfile <- paste0("./data/FCST/fcst_phy2m.",
                     cyearmonth,".nc")
    if (!file.exists(ncfile)){ next()}
    nc <- nc_open(ncfile)

    lats <- ncvar_get(nc,"g4_lat_0")
    lons <- ncvar_get(nc,"g4_lon_1")


    for (ivar in seq(1,length(all.vars))){

      var <- ncvar_get(nc,all.vars[ivar])
      df <- melt(var) %>%
        rename(lon = Var1,
               lat = Var2) %>%
        mutate(lon = lons[lon],
               lat = lats[lat],
               value = value*correction[ivar]) %>%
        filter(abs(lat) <= 25) %>%
        mutate(lon = case_when(lon > 180 ~ lon-360,
                               TRUE ~ lon))
      df.all <- bind_rows(df.all,
                          df %>%
                            mutate(var = var.names[ivar]))


    }

    df.wide <- df.all %>%
      pivot_wider(names_from = var,
                  values_from = value)

    nc_close(nc)


    df2 <- readRDS("./outputs/selected.RDS") %>%
      filter(year == cyear,
             month == cmonth) %>%
      dplyr::select(lon,lat,dlwrf,dswrf,pre)

    if (first){
      grid <- rasterFromXYZ((df2 %>%
         ungroup() %>%
         dplyr::select(c("lat","lon","dswrf")))[,c("lon","lat","dswrf")])
      first = FALSE
    }

    df.rspld <- resample.df.all.col(bigdf = df.wide %>%
                                      mutate(source = "JRA"),

                                    raster2resample = grid,
                                    var.names = c("pre","dswrf","dlwrf"),
                                    0.00092311) %>%
      filter(!is.na(pre))


    combined <- bind_rows(df.rspld,
                          df2 %>%
                            mutate(source = "CRUJRA"))

    # ratios <- c(ratios,combined %>%
    #   dplyr::select(-c(pre,dlwrf)) %>%
    #   pivot_wider(names_from = "source",
    #               values_from = "dswrf") %>%
    #   mutate(ratio = CRUJRA/JRA) %>%
    #   pull(ratio) %>% mean(na.rm = TRUE))

    combined.long <- combined %>%
      pivot_longer(cols = -c(lat,lon,source),
                   names_to = "var")

    all.years <- bind_rows(all.years,
                           combined.long %>%
                             mutate(year = cyear,
                                    month = cmonth))

    df.rspld.all <- bind_rows(df.rspld.all,
                              df.rspld %>% mutate(year = cyear,
                                                  month = cmonth))

  }
}

saveRDS(df.rspld.all,
        "./outputs/JRA.rspld.monthly.RDS")


combined <- all.years %>%
  filter(month == 5, year == 2022) %>%
  pivot_wider(names_from = var,
              values_from = value) %>%
  pivot_wider(names_from = source,
              values_from = c(pre,dswrf,dlwrf)) %>%
  mutate(diff = pre_JRA - pre_CRUJRA)

hist(combined$diff/combined$pre_JRA)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = combined) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff/pre_JRA),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-150, 180), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1)*0.5, oob = scales::squish) +
  # facet_wrap(~ source) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = all.years) +
  geom_density(aes(x = value, fill = source), alpha = 0.5, color = NA) +
  facet_wrap( ~ var,scales = "free") +
  theme_bw()


ggplot(data = all.years %>%
  filter(lon == -72.25,
         lat == -1.25)) +
  geom_line(aes(year + (month -1/2)/12, y = value, color = source)) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()


cmap <- all.years %>%
  # filter(lon >= -90 & lon <= -35) %>%
  filter(source == "JRA",
         var == "pre",
         month <= 11) %>%
  group_by(lat,lon,year) %>%
  summarise(MAP = mean(value),
            .groups = "keep") %>%
  pivot_wider(names_from = "year",
              values_from = "MAP") %>%
  mutate(ratio = `2023`/mean(c(`2022`)))


ggplot(data = cmap  ) +
  geom_raster(aes(x=lon,y = lat,
                  fill = ratio),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient2(limits = c(0.5,1.5),oob = scales::squish,midpoint = 1,
                       low = "darkred",mid = "white",high = "darkblue") +
  # facet_grid(source ~ var) +
  labs(x = "",y = "") +
  theme_bw()


ggplot(data = cmap %>%
         filter(lon <= -35,lat >= 20)) +
  geom_density(aes(x = ratio)) +
  theme_bw()

# library(dplyr)
# Tropics.sum <- readRDS("./outputs/monthly.climate.pantropical.all.RDS")
# S <- Tropics.sum %>%
#   filter(year >= 2020)
# saveRDS(S,"./outputs/selected.RDS")
#
# system2("scp",
#         c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/selected.RDS",
#           "./outputs/"))
