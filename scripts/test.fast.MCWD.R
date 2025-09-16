rm(list = ls())

library(dplyr)
library(YGB)
library(Rcpp)
library(ggplot2)
library(zoo)
library(roll)
library(data.table)

A <- readRDS("~/Downloads/Timeseries.MCWD.TaiESM1.ssp126.RDS")
# A <- readRDS("./outputs/Timeseries.MCWD.TaiESM1.ssp126.RDS")
A.s <- A %>%
  # filter(scenario %in% c("historical")) %>%
  filter(lon == 25.25,
         lat == 0.25)

cppFunction('
double computeMCWD(NumericVector diff) {
  int n = diff.size();
  if (n != 12) stop("Input must be length 12.");

  // Step 1: Find the wettest month (max diff)
  int wettest_month = 0;
  double max_val = diff[0];
  for (int i = 1; i < n; ++i) {
    if (diff[i] > max_val) {
      max_val = diff[i];
      wettest_month = i;
    }
  }

  // Step 2: Reorder months so that wettest month is first
  NumericVector diff_ord(n);
  for (int i = 0; i < n; ++i) {
    int m = (i - wettest_month + n) % n;  // shifted index
    diff_ord[i] = diff[m];
  }

  // Step 3: Compute CWD from reordered diff
  NumericVector CWD(n);
  CWD[0] = std::min(0.0, diff_ord[0]);
  double prev = CWD[0];
  for (int i = 1; i < n; ++i) {
    CWD[i] = std::min(0.0, diff_ord[i] + prev);
    prev = CWD[i];
  }

  // Step 4: Return MCWD (minimum of CWD)
  double mcwd = CWD[0];
  for (int i = 1; i < n; ++i) {
    if (CWD[i] < mcwd) {
      mcwd = CWD[i];
    }
  }
  return mcwd;
}
')

A.MCWD <- A.s %>%
  # filter(lon >= -25,lon <= 60) %>%
  group_by(year,scenario,model,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31)) %>%
  group_by(scenario,model,lon,lat) %>%
  mutate(E = SPEI::thornthwaite(tas,
                                lat = unique(lat),
                                na.rm = TRUE,
                                verbose = FALSE)/Ndays,
         Pmm = Ndays*pr*86400,
         Etot = E*Ndays) %>%
  dplyr::select(-pr) %>%
  mutate(diff = Pmm - Etot) %>%
  group_by(scenario,model,lon,lat) %>%
  mutate(MCWD = rollapply(diff, 12,
                          function(x) compute_MCWD(x),
                          fill = NA,
                          align = c("right")),
         MCWD2 = rollapply(diff, 12,
                          function(x) calc.MCWD(x),
                          fill = NA,
                          align = c("right")),
         MCWD3 = rollapply(diff, 12,
                          function(x) computeMCWD(x),
                          fill = NA,
                          align = c("right")))  %>%
  mutate(MAP = rollapply(Pmm, 12, sum,align = "right", fill = NA),
         Etot = rollapply(Etot, 12, sum,align = "right", fill = NA),
         MAT = rollapply(tas, 12, mean,align = "right", fill = NA))


plot(A.MCWD$MCWD,A.MCWD$MCWD3)

plot(A.MCWD$MAP,type = "l")
plot(A.MCWD$MCWD,type = "l")
plot(A.MCWD$MAT,type = "l")
plot(A.MCWD$Etot,type = "l")


A.MCWD <- A %>%
  filter(lon >= -25,lon <= 60) %>%
  group_by(year,scenario,model,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31)) %>%
  group_by(lat) %>%
  mutate(E = SPEI::thornthwaite(tas,
                                lat = unique(lat),
                                na.rm = TRUE,
                                verbose = FALSE)/Ndays) %>%
  ungroup() %>%
  mutate(Pmm = Ndays*pr*86400,
         Etot = E*Ndays) %>%
  dplyr::select(-pr) %>%
  mutate(diff = Pmm - Etot) %>%
  group_by(scenario,model,lon,lat) %>%
  mutate(MCWD = rollapply(diff, 12,
                           function(x) computeMCWD(x),
                           fill = NA,
                           align = c("right")))  %>%
  mutate(MAP = rollapply(Pmm, 12, sum,align = "right", fill = NA),
         Etot = rollapply(Etot, 12, sum,align = "right", fill = NA),
         MAT = rollapply(tas, 12, mean,align = "right", fill = NA))

A.MCWD.sum <- A.MCWD %>%
  group_by(lon,lat) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            Etot.m = mean(Etot,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),
            .groups = "keep")




world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = A.MCWD.sum,
              aes(x = lon, y = lat,
                  fill = MAP.m - Etot.m)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  scale_fill_gradient2(limits = c(-1500,1500),
                        oob = scales::squish) +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  theme_bw() +
  labs(x = "",y = "",
       fill = "Aridity index") +
  theme(text = element_text(size = 20))

################################################################################

files <- c("all.changes2.RDS",
           "transitions2.RDS")

for (ifile in seq(1,length(files))){
  system2("scp",
          c(paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/",files[ifile]),
            "./outputs/"))
}

all.changes <- readRDS("./outputs/all.changes2.RDS")


models <- sort(unique(all.changes %>%
                        filter(scenario == "ssp534-over") %>%
                        pull(model)))
cmodel <- models[2]
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = all.changes %>%
                filter(type == "nochanges",
                       model == cmodel),
              aes(x = lon, y = lat,
                  fill = as.factor(init))) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-20, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_bw() +
  facet_wrap(~ scenario,ncol = 1) +
  labs(x = "",y = "",
       fill = "Aridity index") +
  theme(text = element_text(size = 20))

ggplot() +
  geom_raster(data = all.changes %>%
                filter(type == "changes",
                       model == cmodel),
              aes(x = lon, y = lat,
                  fill = init - end)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-20, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_bw() +
  facet_wrap(~ scenario,ncol = 1) +
  scale_fill_gradient2() +
  labs(x = "",y = "",
       fill = "Aridity index") +
  theme(text = element_text(size = 20))


all.changes.sum <- all.changes %>%
  mutate(Delta_cat = end - init) %>%
  group_by(scenario,lon,lat) %>%
  summarise(no.changes = sum(init == end),
            changes = sum(end != init),
            more.arid = sum(end > init),
            more.wet = sum(end < init),

            Delta_cat = mean(Delta_cat,na.rm = TRUE),

            N = length(init),
            .groups = "keep")

Nmodel <- length(unique(all.changes$model))

map.all.changes.sum <- all.changes.sum %>%
  ungroup() %>%
  filter(N >= 0.8*N) %>%
  mutate(type = case_when(no.changes >= round(0.5*N) ~ "No changes",
                          more.arid >= round(0.5*N) ~ "More arid",
                          more.wet >= round(0.5*N) ~ "More wet",
                          TRUE ~ "Mixed"),
         change = case_when(type %in% c("No changes","Mixed") ~ 0,
                            type %in% c("More arid","More wet") ~ Delta_cat))


ggplot() +
  geom_raster(data = map.all.changes.sum %>%
                filter(type != "No changes"),
              aes(x = lon, y = lat,
                  fill = type)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-20, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_bw() +
  facet_wrap(~ scenario,ncol = 1) +
  labs(x = "",y = "") +
  theme(text = element_text(size = 20))

ggplot() +
  geom_raster(data = map.all.changes.sum,
              aes(x = lon, y = lat,
                  fill = Delta_cat)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-20, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_bw() +
  facet_wrap(~ scenario,ncol = 1) +
  scale_fill_gradient2(transform = "reverse") +
  labs(x = "",y = "") +
  theme(text = element_text(size = 20))


ggplot() +
  geom_raster(data = map.all.changes.sum,
              aes(x = lon, y = lat,
                  fill = change)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-20, 60),
           ylim = c(-15, 10), expand = FALSE) +
  theme_bw() +
  facet_wrap(~ scenario,ncol = 1) +
  scale_fill_gradient2(transform = "reverse") +
  labs(x = "",y = "") +
  theme(text = element_text(size = 20))

transitions <- readRDS("./outputs/transitions2.RDS") %>%
  filter(!is.na(LC)) %>%
  mutate(diff.biome = end - LC)

test <- transitions %>%
  ungroup() %>%
  filter(end == 2,
         scenario == "ssp585",
         model == model[1])

ggplot(test) +
  geom_line(aes(x = year + (month - 1/2)/12,
            y = MCWD.m,
            color = as.factor(LC))) +
  facet_wrap(~ LC) +
  theme_bw()


transitions %>%
  filter(scenario == "ssp534-over") %>%
  group_by(model) %>%
  summarise(N = length(unique(year)),
            ymin = min(year),
            missing = paste(c(1900:2100)[(which(!(1900:2100 %in% unique(year))))],collapse = "-"),
            ymax = max(year))

transitions.sum <- transitions %>%
  group_by(year,month,end,LC,scenario) %>%
  summarise(value.m.MEM = mean(value.m,na.rm = TRUE),
            MCWD.m.MEM = mean(MCWD.m,na.rm = TRUE),
            MAP.m.MEM = mean(MAP.m,na.rm = TRUE),
            diff.biome = unique(diff.biome),
            .groups = "keep")

ggplot(data = transitions.sum %>%
         filter(year > 1930)) +
    geom_line(aes(x = year + (month - 1/2)/12,
                  y = value.m.MEM,
                  group = interaction(end,diff.biome),
                  color = as.factor(diff.biome))) +
  scale_color_manual(values = c("darkred","red",
                                "black",
                                "blue","darkblue")) +
    # scale_color_brewer(palette = "RdBu") +
    # scale_color_brewer(direction = -1,) +  # or use "Dark2", "Paired", etc.
    facet_grid(scenario ~ end) +
    theme_bw()

ggplot(data = transitions.sum  %>%
         filter(end %in% c(1:3))) +
  geom_line(aes(x = year + (month - 1/2)/12,
                group = interaction(end),
                y = MAP.m.MEM, color = as.factor(end))) +
  facet_wrap(~ scenario) +
  scale_color_brewer(direction = -1) +
  theme_bw()

ggplot(data = transitions.sum %>%
         filter(end %in% c(1:5))) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = MCWD.m.MEM,
                group = interaction(end),
                color = as.factor(end))) +
  facet_wrap(~ scenario) +
  scale_color_brewer(direction = -1) +  # or use "Dark2", "Paired", etc.
  theme_bw()

