rm(list = ls())

Clim.Mask.MCWD.sum <- readRDS("./Summary.climate.RDS")
Clim.Mask.MCWD.sum <- bind_rows(Clim.Mask.MCWD.sum,
                                Clim.Mask.MCWD.sum %>%
                                  group_by(lon,lat) %>%
                                  summarise(MAP = mean(MAP,na.rm = TRUE),
                                            MCWD = mean(MCWD,na.rm = TRUE),
                                            LC = unique(LC),
                                            .groups = "keep") %>%
                                  mutate(model = "MEM")) %>%
  filter(!is.na(LC))

MCWD.thresholds <- seq(-600,-300,50)
MAP.thresholds <- seq(900,1200,50)

df.CM <- data.frame()

for (MAP.threshold in MAP.thresholds){

  print(MAP.threshold)

  for (MCWD.threshold in MCWD.thresholds){

    ConfusionM <- Clim.Mask.MCWD.sum %>%
      filter(LC %in% c(1:3)) %>%
      ungroup() %>%
      mutate(LC =as.factor(LC),
             LC.pred = as.factor(case_when(MCWD > MCWD.threshold ~ 2,
                                           MAP < MAP.threshold ~ 1,
                                           TRUE ~ 3))) %>%
      group_by(model) %>%
      summarise(Acc = (confusionMatrix(LC.pred,LC))[["overall"]][1],
                precision = mean(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                 na.rm = TRUE),
                precision.min = min(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                    na.rm = TRUE))

    df.CM <- bind_rows(df.CM,
                       ConfusionM %>%
                         mutate(MCWD.threshold,
                                MAP.threshold))

  }
}

ggplot(data = df.CM %>%
         filter(model == "MEM")) +
  geom_tile(aes(x = MAP.threshold, y = MCWD.threshold,
                fill = Acc)) +
  scale_fill_gradient() +
  theme_bw()

df.CM %>%
  filter(Acc == max(Acc))
