system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.monthly.pr.pantropical.AWI-CM-1-1-MR_rspld.RDS",
              "./outputs/"))

A <- readRDS("./outputs/df.monthly.climate.pr.ssp585.AWI-CM-1-1-MR.RDS")

A.regional <- A %>%
  group_by(lon,lat,year) %>%
  summarise(MAP = sum(pr*86400*365/12),
            .groups = "keep") %>%
  group_by(lon,lat) %>%
  summarise(MAP = mean(MAP),
            .groups = "keep")

ggplot() +
  geom_raster(data = A.regional ,
              aes(x = lon, y = lat,
                  fill = MAP)) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +
  scale_fill_gradient(low = "white",high = "darkblue",
                      limits = c(0,1500),
                      oob = scales::squish) +
  coord_sf(xlim = c(-90, 55), ylim = c(-30, 20), expand = FALSE) +
  theme_bw()


A.local <- A %>%
  ungroup() %>%
  filter(sqrt((lon+60.0000)**2) < 4 &
         sqrt((lat - 5.142840)**2) < 4) %>%
  filter(lat == lat[1],
         lon == lon[1])

ggplot(data = A.local %>%
         group_by(year) %>%
         summarise(MAP = sum(pr*86400*365/12))) +
  geom_line(aes(x = year,
                y = MAP)) +
  theme_bw()


###############################################################################

CMIP6 <- readRDS("./outputs/data2test.RDS")

cdata.all.mean <- CMIP6 %>%
  ungroup() %>%
  pivot_longer(cols = any_of(c("tas","tasmin","tasmax","pr")),
               names_to = "variable") %>%
  group_by(variable,scenario,model,lon,lat,month) %>%
  summarise(value.m = mean(value,
                           na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(names_from = "variable",
              values_from = "value.m")

cdata.all.conv <- cdata.all.mean %>%
  group_by(scenario,model,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         # E1 = SPEI::thornthwaite(tas,
         #                         unique(lat),
         #                         na.rm = TRUE,
         #                         verbose = FALSE)/Ndays,
         # E = SPEI::hargreaves(tasmin,tasmax,
         #                      unique(lat),Ra = NULL,
         #                      na.rm = TRUE,
         #                      verbose = FALSE)/Ndays,
         E = 3.33,
         Pmm = Ndays*pr*86400,
         Etot = E*Ndays,
         verbose = FALSE) %>%
  dplyr::select(-pr) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD),
         MAP = sum(Pmm),
         MAT = mean(tas))

View(cdata.all.conv %>%
       filter(lon == -60.25,lat == 5.25))

cdf.class <- cdata.all.conv %>%
  ungroup() %>%
  filter(month == 1) %>%
  mutate(basin = case_when(lon > -90 & lon < -30 ~ "Amazon",
                           lon > -30 & lon <= 55 ~ "Congo",
                           TRUE ~ "Else")) %>%
  filter(basin != "Else") %>%
  ungroup() %>%
  mutate(MCWD.threshold = thresholds$MCWD[match(basin,thresholds$basin)],
         MAP.threshold = thresholds$MAP[match(basin,thresholds$basin)]) %>%
  mutate(type = case_when(MCWD >= MCWD.threshold ~ 2,
                          MAP <= MAP.threshold ~ 1,
                          TRUE ~ 3)) %>%
  dplyr::select(scenario,model,lon,lat,
                MAP,MAT,MCWD,Etot,type)


cdf.class %>%
  filter(lat == 5.25,
         lon == -60.25)


ggplot() +
  geom_raster(data = cdf.class ,
              aes(x = lon, y = lat,
                  fill = as.factor(type))) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black",linewidth = 1) +
  geom_sf(data = Congo.shp,fill = NA, color = "black",linewidth = 1) +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  coord_sf(xlim = c(-90, 55), ylim = c(-30, 20), expand = FALSE) +
  theme_bw() +
  facet_grid(model ~ scenario)

