rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(YGB)
library(ggridges)
library(ggExtra)
library(sf)

################################################################################
# Plots

plots <- read.csv("~/Downloads/plot_coordinates_MCWD.csv") %>%
  distinct() %>%
  na.omit() %>%
  mutate(biome = case_when(lon <= -35 ~ "Amazon",
                           lon <= 60 ~ "Congo")) %>%
  group_by(lon,lat,biome) %>%
  summarise(Plot_ID_Veg = paste(Plot_ID_Veg,collapse = "|"),
            .groups = "keep") %>%
  dplyr::select(lon,lat,biome,Plot_ID_Veg) %>%
  ungroup() %>%
  mutate(site = 1:n())

plots.sp <- SpatialPoints(plots[,c("lon","lat")])

buffer <- 0.1
plots.sp.buffer <- st_buffer(st_as_sf(plots.sp),buffer)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world,fill = NA,color = "grey") +
  geom_point(data = plots,
             aes(x = lon, y = lat,
                 col = biome), size = 1) +
  theme_void() +
  labs(x = "",y="",fill = "Forest type") +
  theme(panel.grid.major = element_blank(),
        text = element_text(size = 24)) +
  scale_x_continuous(limits = c(-120,60),expand = c(0,0)) +
  scale_y_continuous(limits = c(-25,25),expand = c(0,0)) +
  scale_shape_manual(values = c(16,1)) +
  guides(size = "none", shape = "none")


################################################################################
# GLEAM variables

var.names <- c("SMrz","Ep","E","S")
years <- 1991:2020
months <- 1:12

all.df <- data.frame()

for (cvar in var.names){
  for (cyear in years){

    ncfile <- paste0("/home/femeunier/Documents/data/GLEAM/",
                     cvar,"_2022_GLEAM_v4.1a_MO.nc")
    nc <- nc_open(ncfile)

    lons <- ncvar_get(nc,"lon")
    lats <- ncvar_get(nc,"lat")
    times <- ncvar_get(nc,"time")
    values <- ncvar_get(nc,cvar)

    r <- stack()
    for (cmonth in months) {

      print(paste0(cvar," - ",cyear," - ", cmonth))

      ENV_col <- raster(t(values[,,cmonth]))
      names(ENV_col) <- cmonth
      r <- stack(r , ENV_col)
    }
    extent(r) <- c(-180,180,-90,90)
    r.crop <- crop(r,extent(c(-120,60,-30,30)))

    cpoints <- raster::extract(r.crop, plots.sp.buffer)
    names(cpoints) <- 1:length(cpoints)

    df.cpoints <- data.frame(site = as.numeric(rep(names(cpoints), sapply(cpoints, length))),
                             month = rep(1:12, sum(sapply(cpoints, nrow))),
                             Obs = unlist(sapply(cpoints,t))) %>%
      left_join(plots,
                by = "site") %>%
      group_by(site,Plot_ID_Veg,biome,lon,lat,month) %>%
      summarise(value = mean(Obs,na.rm = TRUE),
                .groups = "keep")

    # df.cpoints <- as.data.frame(cpoints) %>%
    #   pivot_longer(cols = -c("lon","lat"),
    #                names_to = "month",
    #                values_to = "value") %>%
    #   mutate(month = as.numeric(substr(month,2,5)))

    all.df <- bind_rows(all.df,
                        df.cpoints %>%
                          mutate(year = cyear,
                                 var = cvar))
    rownames(all.df) <-  NULL

    nc_close(nc)

  }
}

all.df <- all.df %>%
  filter(value != 0)

all.df.biome <- all.df %>%
  mutate(biome = case_when(lon <= -35 ~ "Amazon",
                           lon <= 60 ~ "Congo")) %>%
  left_join(plots %>%
              ungroup() %>%
              distinct() %>%
              dplyr::select(-c(biome)),
            by = c("lon","lat","Plot_ID_Veg","site"))

all.df.site <- all.df.biome %>%
  group_by(Plot_ID_Veg,lon,lat,biome,month,var) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

all.df.sum <- all.df.biome %>%
  group_by(var,month,biome) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = all.df.sum) +
  geom_line(data = all.df.site,
            aes(x = month, y = value.m,
                color = biome, group = Plot_ID_Veg),
            size = 0.1) +
  geom_line(aes(x = month, y = value.m,
                color = biome),size = 2) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

S <- all.df.site %>%
  filter(var == "S") %>%
  group_by(biome,Plot_ID_Veg,lon,lat) %>%
  summarise(m = min(value.m,
                    na.rm = TRUE),
            av = mean(value.m,
                      na.rm = TRUE),
            M = max(value.m,
                    na.rm = TRUE),
            .groups = "keep")

ggplot(data = S) +
  stat_density_ridges(aes(x = av ,y = biome,fill = biome),
                      scale = 0.9,
                      alpha = 0.5,
                      quantile_lines = TRUE,
                      jittered_points = TRUE) +
  theme_bw() +
  theme(legend.position = "none")

all.df.sum %>%
  filter(var %in% c("E","Ep")) %>%
  group_by(biome,var) %>%
  summarise(tot = sum(value.m))

all.df.sum %>%
  filter(var %in% c("SMrz","S")) %>%
  group_by(biome,var) %>%
  summarise(m = min(value.m),
            av = mean(value.m),
            M = max(value.m),
            .groups = "keep") %>%
  arrange(var)

E.Ep.site <- all.df.site %>%
  filter(var %in% c("E","Ep")) %>%
  pivot_wider(names_from = "var",
              values_from = "value.m") %>%
  mutate(E.Ep = E/Ep)

E.Ep <- all.df.sum %>%
  filter(var %in% c("E","Ep")) %>%
  pivot_wider(names_from = "var",
              values_from = "value.m") %>%
  mutate(E.Ep = E/Ep)

ggplot(data = E.Ep) +
  geom_line(aes(x = month, y = E.Ep,
                color = biome), size = 1) +
  theme_bw()

################################################################################
# Precip

all.P <- data.frame()

for (cyear in years){
  for (cmonth in months){

    print(paste0(cyear," - ",cmonth))

    cfile <- paste0("/home/femeunier/Documents/projects/YGB/data/Precip/MSWEP/",
                    cyear,sprintf("%02d",cmonth),".nc")

    if (!file.exists(cfile)){
      warning(paste(cfile,"does not exist"))
      next()
    }

    nc <- nc_open(cfile)
    precip <- ncvar_get(nc,"precipitation")
    cr <- raster(t(precip))
    nc_close(nc)

    extent(cr) <- c(-180,180,-90,90)
    cr.crop <- crop(cr,extent(c(-120,60,-30,30)))

    cp.points <- raster::extract(cr.crop, plots.sp.buffer)
    names(cp.points) <- 1:length(cp.points)

    df.cp.points <- data.frame(site = as.numeric(rep(names(cp.points),
                                                     sapply(cp.points, length))),
                               Obs = unlist(sapply(cp.points,t))) %>%
      left_join(plots,
                by = "site") %>%
      group_by(site,Plot_ID_Veg,biome,lon,lat) %>%
      summarise(value = mean(Obs,
                             na.rm = TRUE),
                .groups = "keep")


    all.P <- bind_rows(all.P,
                       df.cp.points %>%
                         mutate(year = cyear,
                                month = cmonth,
                                var = "Precip"))

  }
}

all.P <- all.P %>%
  filter(value != 0)

all.P.biome <- all.P %>%
  mutate(biome = case_when(lon <= -35 ~ "Amazon",
                           lon <= 60 ~ "Congo")) %>%
  left_join(plots %>%
              ungroup() %>%
              distinct() %>%
              dplyr::select(-c(biome)),
            by = c("lon","lat","Plot_ID_Veg","site"))

all.P.site <- all.P.biome %>%
  group_by(Plot_ID_Veg,lon,lat,biome,month,var) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

all.P.biome.sum <- all.P.biome %>%
  group_by(var,month,biome) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = all.P.biome.sum) +
  geom_line(data = all.P.site,
            aes(x = month, y = value.m,
                color = biome, group = Plot_ID_Veg),
            linewidth = 0.05) +
  geom_line(aes(x = month, y = value.m,
                color = biome),size = 2) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

all.P.biome.sum %>%
  group_by(biome,var) %>%
  summarise(MAP = sum(value.m))

################################################################################
# MCWD

E.P <- all.df.site %>%
  filter(var == "E") %>%
  ungroup() %>%
  dplyr::rename(E = value.m) %>%
  dplyr::select(-var) %>%
  left_join(all.P.site %>%
              ungroup() %>%
              rename(P = value.m) %>%
              dplyr::select(Plot_ID_Veg,month,P),
            by = c("Plot_ID_Veg","month"))

MCWD <- E.P %>%
  group_by(Plot_ID_Veg) %>%
  mutate(MAP = sum(P),
         E.tot = sum(E)) %>%
  mutate(diff = P - E) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD)) %>%
  dplyr::select(Plot_ID_Veg,lat,lon,MAP,E.tot,MCWD) %>%
  ungroup() %>%
  mutate(biome = case_when(lon <= -35 ~ "Amazon",
                           lon <= 60 ~ "Congo")) %>%
  distinct()

MCWD.long <- MCWD %>%
  pivot_longer(cols = c(MAP,E.tot,MCWD),
               names_to = "var",
               values_to = "value")

ggplot(data = MCWD.long) +
  stat_density_ridges(aes(x = value ,y = biome,fill = biome),
                      scale = 0.75,
                      alpha = 0.5,
                      quantile_lines = TRUE,
                      jittered_points = TRUE) +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

MCWD.long %>%
  group_by(var,biome) %>%
  summarise(m = min(value,na.rm = TRUE),
            av = mean(value,na.rm = TRUE),
            M = max(value,na.rm = TRUE),
            .groups = "keep")

p <- ggplot(data = MCWD,
       aes(x = MAP, y = MCWD,
           color = biome, fill = biome)) +
  geom_point() +
  stat_smooth(method = "lm",se = TRUE) +
  theme_bw() +
  theme(legend.position = c(0.9,0.2))

ggMarginal(p, type = "density", groupColour = FALSE, groupFill = TRUE)

MCWD %>%
  group_by(biome) %>%
  summarise(r2 = summary(lm(MCWD ~ MAP))[["r.squared"]],
            pval = summary(lm(MCWD ~ MAP))[["coefficients"]][2,4],
            .groups = "keep")


df.all.vars.GLEAM <- all.df.site %>%
  group_by(var,biome,Plot_ID_Veg,lon,lat) %>%
  summarise(m = min(value.m,
                    na.rm = TRUE),
            av = mean(value.m,
                      na.rm = TRUE),
            M = max(value.m,
                    na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(names_from = "var",
              values_from = c("m","av","M"),
              names_glue = "{var}_{.value}") %>%
  left_join(MCWD %>%
              dplyr::select(Plot_ID_Veg,MAP,MCWD,E.tot),
            by = c("Plot_ID_Veg"))

saveRDS(df.all.vars.GLEAM,
        "./outputs/All.vars.GLEAM.RDS")
