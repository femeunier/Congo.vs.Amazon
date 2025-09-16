rm(list = ls())

all.CMIP6.MCWD <- readRDS("./outputs/All.CMIP6.states.timing.MEM.RDS") %>%
  ungroup() %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  dplyr::select(-LC) %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(!(model %in% c("NESM3","CIESM")))


all.CMIP6.MCWD <- all.CMIP6.MCWD %>%
  ungroup() %>%
  filter(lat <= 10, lat >= -15,
         lon >= -15, lon <= 60) %>%
  mutate(hemisp = case_when(lat >= 0 ~ "N",
                            TRUE ~ "S"))
all.CMIP6.MCWD.selected <- all.CMIP6.MCWD %>%
  dplyr::select(period,model,lon,lat,month,pre,scenario,LC,hemisp)

all.CMIP6.MCWD.ref <- all.CMIP6.MCWD.selected %>%
  filter(period != "historical") %>%
  left_join(all.CMIP6.MCWD.selected %>%
              filter(period == "historical") %>%
              rename(pre.ref = pre) %>%
              dplyr::select(-period),
            by = c("scenario","model",
                   "lon","lat","month","LC","hemisp")) %>%
  mutate(diff = pre - pre.ref)

all.CMIP6.MCWD.ref.sum <- all.CMIP6.MCWD.ref %>%
  mutate(month.cat = case_when(month %in% c(3,4,5) ~ "MAM",
                               month %in% c(6,7,8) ~ "JJA",
                               month %in% c(9,10,11) ~ "SON",
                               month %in% c(12,1,2) ~ "DJF")) %>%
  group_by(month.cat, model,period, lon, lat, scenario, LC, hemisp) %>%
  summarise(diff.m = mean(diff,na.rm = TRUE),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(month.cat = factor(month.cat,
                            levels = c("DJF","MAM","JJA","SON")))


all.lats <- sort(unique(all.CMIP6.MCWD.ref.sum$lat))
all.lons <- sort(unique(all.CMIP6.MCWD.ref.sum$lon))

threshold <- 0.7
df.agreement <- all.CMIP6.MCWD.ref.sum %>%
  ungroup() %>%
  filter(model != "MEM") %>%
  mutate(sign.Delta = case_when(diff.m < 0 ~ -1,
                                diff.m >= 0 ~ 1)) %>%
  group_by(month.cat,period,lon,lat,
           scenario, LC) %>%
  summarise(mode = modal(sign.Delta),
            N = length(sign.Delta),
            agreement = (sum(sign.Delta == mode) >=
                           threshold*N),
            .groups = "keep") %>%
  ungroup()

df.agreement2plot <- df.agreement %>%
  ungroup() %>%
  filter(lon %in% all.lons[seq(1,length(all.lons),2)],
         lat %in% all.lats[seq(1,length(all.lats),2)])

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = all.CMIP6.MCWD.ref.sum %>%
         filter(scenario != "ssp534-over",
                model == "MEM",
                period == "Long_future")) +
  geom_raster(aes(x = lon, y = lat,
                  fill = diff.m)) +
  geom_point(data = df.agreement2plot %>%
               filter(scenario != "ssp534-over",
                      agreement,
                      period == "Long_future"),
             aes(x = lon, y = lat),
             shape = 20,
             size = 0.01) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  facet_grid(month.cat ~ scenario) +
  scale_x_continuous(limits = c(-15,60),
                     breaks = c(-15,0,15,30,45,60),
                     labels = c("15°W","0°","15°E","30°E","45°E","")) +
  scale_y_continuous(limits = c(-15,10),
                     breaks = c(-10,0,10),
                     labels = c("10°S","0°","10°N")) +
  scale_fill_gradient2(limits = c(-40,40),
                       oob = scales::squish) +
  labs(x = "", y = "", fill = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center")


ggplot(data = all.CMIP6.MCWD.ref.sum %>%
         filter(scenario == "ssp245",
                month.cat == "JJA",
                period == "Long_future")) +
  geom_raster(aes(x = lon, y = lat,
                  fill = diff.m)) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  facet_wrap( ~ model) +
  scale_x_continuous(limits = c(-15,60)) +
  scale_y_continuous(limits = c(-15,10)) +
  scale_fill_gradient2(limits = c(-40,40),
                       oob = scales::squish) +
  theme_bw()


