rm(list = ls())

library(dplyr)
library(ggplot2)
library(metR)

Clim.Mask.MCWD.sum <- readRDS("./outputs/Summary.climate.Etot.RDS")

Clim.Mask.MCWD.sum %>%
  ungroup() %>%
  dplyr::select(starts_with("Etot")) %>%
  colnames()


ggplot(data = Clim.Mask.MCWD.sum %>%
         filter(model == "MEM")) +
  geom_point(aes(x = Etot_quarter_hot,y = MAP, color = as.factor(LC)),
             size = 0.25, alpha = 0.5) +
  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme_bw()


ConfusionM <- Clim.Mask.MCWD.sum %>%
  filter(LC %in% c(1:3)) %>%
  ungroup() %>%
  mutate(LC =as.factor(LC),
         LC.pred = as.factor(case_when(Etot_quarter_hot < 450 ~ 2,
                                       MAP < 1000 ~ 1,
                                       TRUE ~ 3))) %>%
  group_by(model) %>%
  summarise(Acc = confusionMatrix(factor(LC.pred,
                                          levels = c(1,2,3)),
                                   factor(LC,
                                          levels = c(1,2,3)))[["overall"]][1],
            precision = mean(as.matrix(confusionMatrix(factor(LC.pred,
                                                              levels = c(1,2,3)),
                                                       factor(LC,
                                                              levels = c(1,2,3)))[["byClass"]])[,"Precision"],
                             na.rm = TRUE),
            precision.min = min(as.matrix(confusionMatrix(factor(LC.pred,
                                                                 levels = c(1,2,3)),
                                                          factor(LC,
                                                                 levels = c(1,2,3)))[["byClass"]])[,"Precision"],
                                na.rm = TRUE))


ConfusionM

Etot.thresholds <- seq(300,600,25)
MAP.thresholds <- seq(500,1500,50)
MAP.thresholds <- seq(800,1200,25)

df.CM <- data.frame()

for (cMAP.threshold in MAP.thresholds){

  print(cMAP.threshold)

  for (cEtot.threshold in Etot.thresholds){

    ConfusionM <- Clim.Mask.MCWD.sum %>%
      filter(LC %in% c(1:3)) %>%
      ungroup() %>%
      mutate(LC =factor(LC,levels = c(1,2,3)),
             LC.pred = factor(case_when(Etot_quarter_hot < cEtot.threshold ~ 2,
                                        MAP < cMAP.threshold ~ 1,
                                           TRUE ~ 3),
                              levels = c(1,2,3))) %>%
      group_by(model) %>%
      summarise(Acc = (confusionMatrix(LC.pred,LC))[["overall"]][1],
                precision = mean(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                 na.rm = TRUE),
                precision.min = min(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                    na.rm = TRUE))

    df.CM <- bind_rows(df.CM,
                       ConfusionM %>%
                         mutate(Etot.threshold = cEtot.threshold,
                                MAP.threshold = cMAP.threshold))

  }
}

ggplot(df.CM %>%
         filter(model == "MEM"),
       aes(x = MAP.threshold, y = Etot.threshold,
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

ggplot(data = df.CM) +
  geom_density(aes(x = Acc, fill = model),
               alpha = 0.5) +
  theme_bw()

df.best <- df.CM %>%
  group_by(model) %>%
  filter(Acc >= max(Acc)) %>%
  filter(precision >= max(precision)) %>%
  summarise(Etot.threshold = mean(Etot.threshold),
            MAP.threshold = mean(MAP.threshold),
            precision = unique(precision),
            precision.min = unique(precision.min),

            Acc = mean(Acc),
            .groups = "keep")


df.best.sum <- df.best %>%
  ungroup() %>%
  summarise(Etot.threshold.min = min(Etot.threshold),
            MAP.threshold.min= min(MAP.threshold),
            Etot.threshold.max = max(Etot.threshold),
            MAP.threshold.max= max(MAP.threshold),
            Etot.threshold.MEM = (Etot.threshold)[model == "MEM"],
            MAP.threshold.MEM= (MAP.threshold)[model == "MEM"],

            .groups = "keep")

ggplot() +
  # geom_rect(data = df.best.sum,
  #           aes(xmin = Etot.threshold.min,
  #               xmax = Etot.threshold.max,
  #               ymin = -Inf,ymax = Inf),
  #           fill = "grey", color = NA, alpha = 0.5) +
  #
  # geom_rect(data = df.best.sum,
  #           aes(ymin = MAP.threshold.min,
  #               ymax = MAP.threshold.max,
  #               xmin = -Inf,xmax = Inf),
  #           fill = "grey", color = NA, alpha = 0.5) +

  geom_hline(data = df.best.sum,
             aes(yintercept = MAP.threshold.MEM),
             color = "grey17") +
  geom_vline(data = df.best.sum,
             aes(xintercept = Etot.threshold.MEM),
             color = 'grey17') +
  geom_point(data = Clim.Mask.MCWD.sum %>%
               filter(model == "MEM"),
             aes(x = Etot_quarter_hot,y = MAP, color = as.factor(LC)),
             size = 0.25, alpha = 0.5) +
  scale_color_manual(values = c("#c49402","#005401","#448704","grey")) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = "none") +
  labs(x = "", y = "")



ToGridArea <- readRDS("./outputs/GridArea.RDS")

Etot.best <- df.best.sum$Etot.threshold.MEM
MAP.best <- df.best.sum$MAP.threshold.MEM

df2test <- ConfusionM <- Clim.Mask.MCWD.sum %>%
  filter(model == "MEM") %>%
  filter(LC %in% c(1:3)) %>%
  ungroup() %>%
  mutate(LC =factor(LC,levels = c(1,2,3)),
         LC.pred = factor(case_when(Etot_quarter_hot < Etot.best ~ 2,
                                       MAP < MAP.best ~ 1,
                                       TRUE ~ 3),
                          levels = c(1,2,3))) %>%

  left_join(GridArea,
            by = c("lon","lat"))

df2test.Mat <- df2test %>%
  group_by(LC,LC.pred) %>%
  summarise(N = n(),
            area = sum(area*land.frac)/1e12,
            .groups = "keep") %>%
  ungroup() %>%
  complete(LC = factor(1:3,levels = c(1,2,3)),
           LC.pred = factor(1:3,levels = c(1,2,3)),
           fill = list(N = 0, area = 0)) %>%
  mutate(lab = paste("N =",
                     N),
         lab2 = paste(signif(area,2)))


ggplot(data = df2test.Mat %>%
         mutate(LC2plot = case_when(LC == 1 ~ 1,
                                    LC == 2 ~ 3,
                                    LC == 3 ~ 2),
                LC.pred2plot = case_when(LC.pred == 1 ~ 3,
                                         LC.pred == 2 ~ 1,
                                         LC.pred == 3 ~ 2)),
       aes(x = LC.pred2plot,
           y = LC2plot))  +
  geom_tile(aes(fill = area),
            color = "black",
            size = 0.5) +

  scale_fill_gradient(low = "white",
                      high = "grey32") +
  geom_label(aes(label = lab2), fill = NA,
             label.size = NA) +
  theme_minimal() +

  scale_x_discrete(breaks = c()) +
  scale_y_discrete(breaks = c()) +
  guides(fill = "none") +
  coord_equal() +
  labs(x="",y = "") +
  theme(text = element_text(size = 40),
        axis.text.x = element_text(angle = 360-45, vjust = 0, hjust=1),
        panel.grid = element_blank())



stop()

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
        "./outputs/Sensitivity.thresholds.sum.Etot.RDS")
