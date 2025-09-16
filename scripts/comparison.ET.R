rm(list = ls())

all <- bind_rows(
  readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.ERA5_coord.RDS") %>%
    mutate(source = "ERA5"),
  readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.CRU_coord.RDS") %>%
    mutate(source = "CRU"),
  readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.ERA5.penman_coord.RDS") %>%
    mutate(source = "ERA5.penman"),
  readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/biome.pantropical.CRU.penman_coord.RDS") %>%
    mutate(source = "CRU.penman"),
  readRDS("/home/femeunier/Documents/data/GLEAM/GLEAM.RDS") %>%
    mutate(source = "GLEAM") %>%
    rename(Etot = E.m)) %>%
  mutate(lat = round(lat,digits = 2),
         lon = round(lon,digits = 2)) %>%
  dplyr::select(source,lon,lat,tmp,tmin,tmax,MCWD,MAP,MAT,Etot)

all.wide <- all %>%
  pivot_longer(cols = -c(source,lon,lat))

ggplot(data = all.wide) +
  geom_density(aes(x = value,
                   fill = source),
               alpha = 0.5) +
  facet_wrap(~ name,
             scales = "free") +
  theme_bw()


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = all.wide %>%
                filter(name == "Etot"),
              aes(x = lon, y = lat,
                  fill = value)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(limits = c(1000,2000),
                       low = 'darkblue',high = "darkred",midpoint = 1500,
                       oob = scales::squish) +
  facet_wrap(~ source) +
  theme_bw() +
  theme(legend.position = "bottom")

df.Etot <- all.wide %>%
  filter(name == "Etot") %>%
  dplyr::select(-name) %>%
  pivot_wider(names_from = source,
              values_from = value) %>%
  pivot_longer(cols = -c(lon,lat,GLEAM),
               names_to = "model",
               values_to = "Etot") %>%
  filter(!is.na(GLEAM))

ET.max = 2100 ; ET.min = 0

ggplot(data = df.Etot %>%
         filter(GLEAM < ET.max,
                GLEAM > ET.min),
       aes(x = GLEAM, y = Etot)) +
  geom_hex() +
  stat_smooth(method = "lm", color = "black") +
  facet_wrap(~ model) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_fill_gradient(low = "white") +
  theme_bw()

df.Etot %>%
  filter(GLEAM < ET.max,
         GLEAM > ET.min) %>%
  group_by(model) %>%
  summarise(r2 = summary(lm(formula = Etot~GLEAM))[["r.squared"]],
            slope = coef(lm(formula = Etot~GLEAM))[2],
            bias = mean(abs(Etot - GLEAM),na.rm = TRUE),
            RMSE = sqrt(1/sum(!is.na(Etot))*sum((Etot-GLEAM)**2,na.rm = TRUE)),
            .groups = "keep")


ggplot() +
  geom_raster(data = df.Etot %>%
                filter(GLEAM < ET.max,
                       GLEAM > ET.min),
              aes(x = lon, y = lat,
                  fill = Etot - GLEAM)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = 'darkblue',high = "darkred",midpoint = 0,
                       limits = c(-500,500),
                       oob = scales::squish) +
  facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot() +
  geom_density(data = df.Etot %>%
                 filter(GLEAM < ET.max,
                        GLEAM > ET.min),
              aes(x = Etot - GLEAM,
                  fill = model),
              alpha = 0.5) +
  theme_bw()
