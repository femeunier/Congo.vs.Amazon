rm(list = ls())

library(dplyr)
library(ggplot2)
library(metR)

Clim.Mask.MCWD.sum <- readRDS("./outputs/Summary.climate.RDS")
MCWD.thresholds <- seq(-800,-200,25)
MAP.thresholds <- seq(500,1500,25)

MCWD.thresholds <- seq(-600,-300,25)
MAP.thresholds <- seq(800,1200,25)

df.CM <- data.frame()

ggplot(data = Clim.Mask.MCWD.sum) +
  geom_density(aes(x = tasmax - tasmin, fill = model),
               alpha = 0.5) +
  theme_bw()

for (cMAP.threshold in MAP.thresholds){

  print(cMAP.threshold)

  for (cMCWD.threshold in MCWD.thresholds){

    ConfusionM <- Clim.Mask.MCWD.sum %>%
      filter(LC %in% c(1:3)) %>%
      ungroup() %>%
      mutate(LC =as.factor(LC),
             LC.pred = as.factor(case_when(MCWD > cMCWD.threshold ~ 2,
                                           MAP < cMAP.threshold ~ 1,
                                           TRUE ~ 3))) %>%
      group_by(model) %>%
      summarise(Acc = (confusionMatrix(LC.pred,LC))[["overall"]][1],
                precision = mean(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                 na.rm = TRUE),
                precision.min = min(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                    na.rm = TRUE))

    df.CM <- bind_rows(df.CM,
                       ConfusionM %>%
                         mutate(MCWD.threshold = cMCWD.threshold,
                                MAP.threshold = cMAP.threshold))

  }
}

ggplot(df.CM %>%
         filter(model == "MEM"),
       aes(x = MAP.threshold, y = MCWD.threshold,
           fill = Acc, z = Acc)) +
  geom_raster() +
  geom_contour(color = "black",
               linewidth = 0.3) +  # Contour lines
  geom_text_contour(stroke = 0.2, size = 3) +  # adds labels
  scale_fill_viridis_c(option = "D",  # options: "A", "B", "C", "D", "E", "magma", "inferno", etc.
                       direction = 1) +   # legend title
  labs(fill = "Accuracy") +
  labs(x = "", y = "", fill = "") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggplot(data = df.CM %>%
         filter(model == "MEM"),
       aes(x = MAP.threshold, y = MCWD.threshold,
           z = Acc)) +
  # geom_tile(aes(x = MAP.threshold, y = MCWD.threshold,
  #               fill = Acc)) +
  geom_contour_filled() +
  geom_contour(color = "black",
               linewidth = 0.3) +  # Contour lines
  geom_text_contour(stroke = 0.2, size = 3) +  # adds labels
  scale_fill_viridis_d() +
  theme_bw() +
  labs(x = "",y = "", fill = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 20))

ggplot(data = df.CM) +
  geom_density(aes(x = Acc, fill = model),
               alpha = 0.5) +
  theme_bw()

df.best <- df.CM %>%
  group_by(model) %>%
  filter(Acc >= max(Acc)) %>%
  filter(precision >= max(precision)) %>%
  summarise(MCWD.threshold = mean(MCWD.threshold),
            MAP.threshold = mean(MAP.threshold),
            precision = unique(precision),
            precision.min = unique(precision.min),

            Acc = mean(Acc),
            .groups = "keep")

ggplot(data = df.best) +
  geom_hline(aes(yintercept = MAP.threshold,
                 color = model)) +
  geom_vline(aes(xintercept = MCWD.threshold,
                 color = model)) +
  scale_x_continuous(limits = c(-800,0)) +
  scale_y_continuous(limits = c(0,3000)) +
  theme_bw()

df.best %>%
  pull(Acc) %>%
  summary()

df.best %>%
  ungroup() %>%
  filter(precision %in% c(min(precision),max(precision)))

df.best %>%
  ungroup() %>%
  filter(model == "MEM")


df.best %>%
  ungroup() %>%
  filter(Acc == max(Acc))

df.best %>%
  filter(model == "MEM")

saveRDS(df.best,
        "./outputs/Sensitivity.thresholds.RDS")
summary(df.best$MCWD.threshold)
summary(df.best$MAP.threshold)

Sensitivity.thresholds <- df.best
threshold.sum <- Sensitivity.thresholds %>%
  ungroup() %>%
  summarise(MCWD.threshold.mean = MCWD.threshold[model == "MEM"],
            MCWD.threshold.min = min(MCWD.threshold),
            MCWD.threshold.max = max(MCWD.threshold),

            MAP.threshold.mean = MAP.threshold[model == "MEM"],
            MAP.threshold.min = min(MAP.threshold),
            MAP.threshold.max = max(MAP.threshold),

            .groups = "keep") %>%
  pivot_longer(cols = -c(),
               values_to = "value",
               names_to = "var") %>%
  rowwise() %>%
  mutate(variable = strsplit(var,"\\.")[[1]][1],
         metric = strsplit(var,"\\.")[[1]][3]) %>%
  dplyr::select(-var) %>%
  pivot_wider(names_from = metric,
              values_from = value)

saveRDS(threshold.sum,
        "./outputs/Sensitivity.thresholds.sum.RDS")
