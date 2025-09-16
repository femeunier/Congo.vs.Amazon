rm = ls()

library(sp)
library(sf)
library(ggplot2)

df.all <- readRDS("./outputs/Pantropical.climate.recent.seasonal.rspld.RDS")


load("./data/IPCC-WGI-reference-regions-v4_R.rda")

selected.df <- IPCC_WGI_reference_regions_v4[
  IPCC_WGI_reference_regions_v4@data$Acronym %in% c("WAF","CAF","NEAF","SEAF",
                                                    "WSAF","ESAF","MDG",
                                                    "SAH"),]



world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

sf_object <- st_as_sf(selected.df)

ggplot() +
  geom_sf(data = sf_object,
          aes(group = Name), alpha = 0.4,
          color = "black",fill = NA) +  #
  # geom_raster(data = df.change.LC.long.sum.long %>%
  #               filter(variable == "Delta.MAT"),
  #             aes(x = lon, y = lat,
  #                 fill = value)) +
  geom_sf(data = world,fill = NA, color = "darkgrey") +
  coord_sf(xlim = c(-25, 60), ylim = c(-1.7, 1.6)*23.25, expand = FALSE) +
  theme_minimal() +
  labs(fill = "MAT") +
  guides(fill = "none")


Search <- df.all %>%
  ungroup() %>%
  filter(model == model[1],
         month == month[1])

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
            by = c("lon","lat")) %>%
  filter(!is.na(region))

reanalyses.region.sum <- reanalyses.region %>%
  dplyr::select(model,region,pre,tas,month) %>%
  group_by(model,region,month) %>%
  summarise(pre.m = mean(pre,na.rm = TRUE),
            tas.m = mean(tas) - 273.15,
            .groups = "keep")

regions <- reanalyses.region.sum %>%
  pull(region) %>% unique()

ggplot(data = reanalyses.region.sum %>%
         filter(region == "CAF")) +
  geom_line(aes(x = month, y = pre.m, color = model)) +
  facet_grid(~ region) +
  labs(x = "",y = "Monthly Precip. (mm)",
       color = "Product") +
  scale_x_continuous(breaks = c(1:12),
                     labels = c('J',"F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  theme_bw()

ggplot(data = reanalyses.region.sum %>%
         filter(region == "CAF")) +
  geom_line(aes(x = month, y = tas.m, color = model)) +
  facet_wrap(~ region) +
  theme_bw()


CMIP6 <- readRDS("./outputs/All.CMIP6.ET.RDS") %>%
  rename(pre = Pmm)

CMIP6.region <- CMIP6 %>%
  ungroup() %>%
  left_join(Search %>%
              dplyr::select(lon,lat,region),
            by = c("lon","lat"))

CMIP6.region.sum <- CMIP6.region %>%
  filter(!is.na(region)) %>%
  dplyr::select(lon,lat,month,model,region,tas,tasmin,tasmax,pre) %>%
  # pivot_longer(cols = c(tas,tasmin,tasmax,pre),
  #              names_to = "variable",
  #              values_to = "value") %>%
  group_by(model,region,month) %>%
  summarise(tas.m = mean(tas,na.rm = TRUE),
            pre.m = mean(pre,na.rm = TRUE),
            .groups = "keep")

Comp <- bind_rows(CMIP6.region.sum %>%
                    mutate(source = "CMIP6"),
                  reanalyses.region.sum %>%
                    mutate(source = "Reanalyses"))

Comp.sum <- Comp %>%
  group_by(region,month,source) %>%
  summarise(av = mean(pre.m),
            m = min(pre.m),
            M = max(pre.m),
            .groups = "keep")

ggplot(data =  Comp.sum %>%
         filter(region == "CAF")) +
  geom_ribbon(aes(x = month, y = av,  fill = source,
                  ymin = m, ymax = M,
                group = source), alpha = 0.5,
              color = NA) +
  geom_line(aes(x = month, y = av,  color = source,
                group = source)) +
  facet_grid(~ region) +
  labs(x = "",y = "Monthly Precip. (mm)",
       color = "Source") +
  scale_x_continuous(breaks = c(1:12),
                     labels = c('J',"F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  theme_bw() +
  guides(fill = "none")

