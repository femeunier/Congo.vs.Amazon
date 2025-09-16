rm(list = ls())

library(dplyr)
library(reshape2)
library(ggplot2)

Nboot <- 250

coord <- expand.grid(lat = seq(-30.25,30.25,0.5),
                     lon = seq(-179.75,179.75,0.5)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                  lat = as.vector(unique(coord$lat))) %>%
  melt() %>%
  mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
         Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
  rename(lon = Var1,
         lat = Var2,
         area = value) %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100) %>%
  left_join(LandFrac,
            by = c("lat","lon")) %>%
  mutate(area = area*value)

df.temp <- readRDS("./outputs/df.all.climate.change.Tropics.RDS") %>%
  ungroup() %>%
  left_join(Gridarea,
            by = c("lon","lat")) %>%
  # filter(period >= 2000) %>%
  filter((period > 2014) | (period <= 2014 & scenario == "ssp126")) %>%
  mutate(scenario = case_when(period <= 2014 ~ "historical",
                              TRUE ~ scenario))
Biomes <- levels(df.temp$biome)

df.biomass <- readRDS("./outputs/Biomass.per.biome.all.RDS")

df.weights <- readRDS("./outputs/CMIP6.weights.Tropics.RDS") %>%
  group_by(model) %>%
  summarise(w = mean(w))

df.GW <- readRDS("./outputs/df.Global.warming.RDS") %>%
  mutate(period = year) %>%
  filter(model %in% df.weights$model)

df.GW.w <- df.GW %>%
  filter(!is.na(tas)) %>%
  left_join(df.weights,
            by = "model")

df.all <- df.temp %>%
  mutate(biome = factor(as.character(biome),
                        levels = rev(Biomes))) %>%
  left_join(df.weights,
            by = "model") %>%
  filter(!is.na(w))

test <- df.all %>%
  dplyr::select(period,scenario,model,biome,lon,lat,area,w,MAP,MCWD,Etot) %>%
  distinct()

models <- sort(unique(df.GW$model))
scenarios <- levels(df.all$biome)
sources <- unique(df.biomass$source)

df.Biomass.tot <- df.Biomass.cat <-
  df.Biomass.map <- df.transitions.sum.all <-
  data.frame()

for (iboot in seq(1,Nboot)){

  print(paste0("Iteration: ",iboot,"/",Nboot))

  print("- Selecting Biomass")

  csource <- sample(sources,1)
  cdf.biomass <- df.biomass

  print("- Sampling climate")
  print("-- m")
  cdf.all <- test %>%
    group_by(period,scenario,lat,lon) %>%
    slice_sample(n = 1) %>%
    ungroup()

  print("-- w")
  cdf.all.w <- test %>%
    group_by(period,scenario,lat,lon) %>%
    slice_sample(n = 1,weight_by = w) %>%
    ungroup()

  print("- Linking climate and biomass")
  newAGB <- newAGB.w <- rep(NA,nrow(cdf.all.w))
  for (cBiome in Biomes){
    # print(cBiome)
    cvec <- cdf.biomass %>%
      filter(biome == cBiome)

    cDF <- cdf.all %>%
      filter(biome == cBiome)
    cDF.w <- cdf.all.w %>%
      filter(biome == cBiome)

    N = nrow(cDF); pos <- which(cdf.all$biome == cBiome)
    N.w = nrow(cDF.w); pos.w <- which(cdf.all.w$biome == cBiome)

    ccAGB <- sample(cvec$AGB,size = N, replace = TRUE)
    ccAGB.w <- sample(cvec$AGB,size = N.w, replace = TRUE)

    newAGB[pos] <- ccAGB
    newAGB.w[pos.w] <- ccAGB.w
  }

  cdf.all$AGB <- newAGB
  cdf.all.w$AGB <- newAGB.w

  print("- Summing Biomass")
  cdf.AGB.model.sum <- cdf.all %>%
    group_by(period,scenario) %>%
    summarise(AGB = sum(AGB*area,
                        na.rm = TRUE)/1e12,
              .groups = "keep") %>%
    group_by(scenario) %>%
    arrange(period) %>%
    mutate(AGB.change = AGB - AGB[1])

  cdf.AGB.model.sum.cat <-  cdf.all %>%
    mutate(continent = case_when(lon <= -30 ~ "America",
                                 lon <= 55 ~ "Africa",
                                 TRUE ~ "Australasia")) %>%
    group_by(period,scenario,biome,continent) %>%
    summarise(Area.tot = sum(area)/1e12,
              AGB = sum(AGB*area,
                        na.rm = TRUE)/1e12,
              .groups = "keep") %>%
    group_by(scenario,biome,continent) %>%
    arrange(period) %>%
    mutate(AGB.change = AGB - AGB[1])

  cdf.AGB.model.w.sum <- cdf.all.w %>%
    group_by(period,scenario) %>%
    summarise(AGB = sum(AGB*area,
                        na.rm = TRUE)/1e12,
              .groups = "keep") %>%
    group_by(scenario) %>%
    arrange(period) %>%
    mutate(AGB.change = AGB - AGB[1])

  cdf.AGB.model.w.sum.cat <-  cdf.all.w %>%
    mutate(continent = case_when(lon <= -30 ~ "America",
                                 lon <= 55 ~ "Africa",
                                 TRUE ~ "Australasia")) %>%
    group_by(period,scenario,biome,continent) %>%
    summarise(Area.tot = sum(area)/1e12,
              AGB = sum(AGB*area,
                        na.rm = TRUE)/1e12,
              .groups = "keep") %>%
    group_by(scenario,biome,continent) %>%
    arrange(period) %>%
    mutate(AGB.change = AGB - AGB[1])

  # Summary

  print("- Compiling")
  df.Biomass.tot <- bind_rows(df.Biomass.tot,
                              bind_rows(cdf.AGB.model.sum %>%
                                          mutate(weighting = "m"),
                                        cdf.AGB.model.w.sum %>%
                                          mutate(weighting = "w")) %>%
                                mutate(iter = iboot,
                                       source = csource))

  df.Biomass.cat <- bind_rows(df.Biomass.cat,
                              bind_rows(cdf.AGB.model.sum.cat %>%
                                          mutate(weighting = "m"),
                                        cdf.AGB.model.w.sum.cat %>%
                                          mutate(weighting = "w")) %>%
                                mutate(iter = iboot,
                                       source = csource))

  # spatial map of Carbon loss for specific scenarios

  print("- Data spatial map")

  df.Biomass.map <- bind_rows(df.Biomass.map,

                              bind_rows(cdf.all %>%
    filter(period %in% c(2000,
                         2090),
           scenario %in% c("historical",
                           "ssp245",
                           "ssp585")) %>%
    mutate(weighting = "m"),

  cdf.all.w %>%
    filter(period %in% c(2000,
                         2090),
           scenario %in% c("historical",
                           "ssp245",
                           "ssp585")) %>%
    mutate(weighting = "w")) %>%

    mutate(iter = iboot))

  ##################################
  # Changes of bioclimatic zones

  print("- Compute transitions")

  scenarios <- unique(cdf.all$scenario)
  scenarios <- scenarios[scenarios != "historical"]

  df.all.new <- df.all.new.w <- data.frame()
  for (cscenario in scenarios){
    df.all.new <- bind_rows(df.all.new,
                            cdf.all %>%
                              filter(scenario == "historical") %>%
                              mutate(scenario = cscenario),
                            cdf.all %>%
                             filter(scenario == cscenario))
    df.all.new.w <- bind_rows(df.all.new.w,
                              cdf.all.w %>%
                                filter(scenario == "historical") %>%
                                mutate(scenario = cscenario),
                              cdf.all.w %>%
                                filter(scenario == cscenario))

  }


  df.transitions <- df.all.new %>%
    mutate(biome.num = as.numeric(factor(biome,
                                         levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                                    "Semiarid","Arid","Hyperarid")))) %>%
    # filter(period >= 2000) %>%
    dplyr::select(period,model,scenario,lat,lon,biome,biome.num) %>%
    group_by(scenario,lat,lon) %>%
    arrange(lon,lat,scenario,period) %>%
    mutate(biome.num = case_when(model == model[1] ~ biome.num,
                                 TRUE ~ NA_integer_)) %>%
    mutate(biome.last.change = c(NA,diff(biome.num)),
           biome.change = biome.num - biome.num[1])

  df.transitions.w <- df.all.new.w %>%
    mutate(biome.num = as.numeric(factor(biome,
                                         levels = c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid",
                                                    "Semiarid","Arid","Hyperarid")))) %>%
    # filter(period >= 2000) %>%
    dplyr::select(period,model,scenario,lat,lon,biome,biome.num) %>%
    group_by(scenario,lat,lon) %>%
    arrange(lon,lat,scenario,period) %>%
    mutate(biome.num = case_when(model == model[1] ~ biome.num,
                                 TRUE ~ NA_integer_)) %>%
    mutate(biome.last.change = c(NA,diff(biome.num)),
           biome.change = biome.num - biome.num[1])


  df.transitions.sum <- bind_rows(df.transitions %>%
                                    filter(!is.na(biome.change)) %>%
    group_by(scenario,period) %>%
    summarise(Npos = length(which(biome.change > 0)),
              Nneg = length(which(biome.change < 0)),
              Nchange = length(which(abs(biome.change) > 0)),
              Ntot = length(biome.change),
              .groups = "keep") %>%
    mutate(r.pos = Npos/Ntot,
           r.neg = Nneg/Ntot,
           r.change = Nchange/Ntot) %>%
      mutate(weighting = "m"),
    df.transitions.w %>%
      filter(!is.na(biome.change)) %>%
      group_by(scenario,period) %>%
      summarise(Npos = length(which(biome.change > 0)),
                Nneg = length(which(biome.change < 0)),
                Nchange = length(which(abs(biome.change) > 0)),
                Ntot = length(biome.change),
                .groups = "keep") %>%
      mutate(r.pos = Npos/Ntot,
             r.neg = Nneg/Ntot,
             r.change = Nchange/Ntot) %>%
      mutate(weighting = "w"))

  df.transitions.sum.all <- bind_rows(df.transitions.sum.all,
                                      df.transitions.sum %>%
                                        mutate(iter = iboot))

}

Num <- 4

saveRDS(df.Biomass.tot,
        paste0("./outputs/df.Biomass.tot0",Num,".RDS"))

saveRDS(df.Biomass.cat,
        paste0("./outputs/df.Biomass.cat0",Num,".RDS"))

saveRDS(df.Biomass.map,
        paste0("./outputs/df.Biomass.map0",Num,".RDS"))

saveRDS(df.transitions.sum.all,
        paste0("./outputs/df.transitions0",Num,".RDS"))

# scp ./outputs/df.all.climate.change.Tropics.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp ./outputs/Biomass.per.biome.all.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp ./outputs/CMIP6.weights.Tropics.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp ./outputs/df.Global.warming.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp ./outputs/landFrac.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/

# scp /home/femeunier/Documents/projects/Congo.vs.Amazon/scripts/Bootstrapping.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

