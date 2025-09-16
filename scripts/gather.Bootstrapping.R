rm(list = ls())

library(dplyr)

system2("rsync",
        paste("-avz",
              "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.Biomass.tot0*",
              "./outputs/"))

system2("rsync",
        paste("-avz",
              "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.Biomass.cat0*",
              "./outputs/"))

system2("rsync",
        paste("-avz",
              "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.Biomass.map0*",
              "./outputs/"))

system2("rsync",
        paste("-avz",
              "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.transitions0*",
              "./outputs/"))

df.Biomass.tot.files <- list.files("./outputs/",
                                   "df.Biomass.tot0..RDS",
                                   full.names = TRUE)

df.Biomass.cat.files <- list.files("./outputs/",
                                   "df.Biomass.cat0..RDS",
                                   full.names = TRUE)

df.Biomass.map.files <- list.files("./outputs/",
                                   "df.Biomass.map0..RDS",
                                   full.names = TRUE)

df.transitions.files <- list.files("./outputs/",
                                   "df.transitions0..RDS",
                                   full.names = TRUE)

df.Biomass.tot <- df.Biomass.cat <-
  df.Biomass.map <- df.transitions <-
  data.frame()

Niter = 0
for (ifile in seq(2,length(df.transitions.files))){

  temp <- readRDS(df.Biomass.tot.files[ifile]) %>%
    mutate(iter = iter + Niter)
  temp2 <- readRDS(df.Biomass.cat.files[ifile]) %>%
    mutate(iter = iter + Niter)
  temp3 <- readRDS(df.Biomass.map.files[ifile]) %>%
    mutate(iter = iter + Niter)
  temp4 <- readRDS(df.transitions.files[ifile]) %>%
    mutate(iter = iter + Niter)

  df.Biomass.tot <- bind_rows(df.Biomass.tot,
                              temp)
  df.Biomass.cat <- bind_rows(df.Biomass.cat,
                              temp2)
  df.Biomass.map <- bind_rows(df.Biomass.map,
                              temp3)
  df.transitions <- bind_rows(df.transitions,
                              temp4)
  Niter <- max(temp$iter)

  print(Niter)
}


df.Biomass.tot.sum <- df.Biomass.tot %>%
  group_by(period,scenario,weighting) %>%
  summarise(AGB.m = mean(AGB,na.rm = TRUE),
            AGB.low = quantile(AGB,probs = 0.025,na.rm = TRUE),
            AGB.high = quantile(AGB,probs = 0.975,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.Biomass.tot.sum,
       aes(x = period,
           y = AGB.m,
           color = scenario,fill = scenario,
           linetype = weighting)) +
  geom_ribbon(aes(ymin = AGB.low,
                  ymax = AGB.high),
              alpha = 0.5,
              color = NA) +

  geom_line() +
  theme_bw()

saveRDS(df.Biomass.tot,
        "./outputs/df.Biomass.tot.RDS")
saveRDS(df.Biomass.cat,
        "./outputs/df.Biomass.cat.RDS")
saveRDS(df.Biomass.map,
        "./outputs/df.Biomass.map.RDS")
saveRDS(df.transitions,
        "./outputs/df.transitions.RDS")
