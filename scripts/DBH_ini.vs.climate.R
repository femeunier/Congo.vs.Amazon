rm(list = ls())

library(dplyr)
library(corrplot)
library(ggplot2)
library(stringr)
library(tidyr)
library(FactoMineR)
library(factoextra)

df.all.vars.GLEAM <- readRDS("./outputs/All.vars.GLEAM.RDS")
df.all.vars.EVI <- readRDS("./outputs/EVI.var.Johanna.RDS") %>%
  dplyr::select(-site)

df.all.all.vars <- df.all.vars.GLEAM %>%
  left_join(df.all.vars.EVI,
            by = join_by(biome, Plot_ID_Veg, lon, lat))
df.mod <- data.frame()

for(i in seq(1,nrow(df.all.all.vars))){
  cPlot_ID_Veg <- df.all.all.vars$Plot_ID_Veg[i]
  if (grepl("\\|",cPlot_ID_Veg)){
    ntimes <- 1+str_count(cPlot_ID_Veg,"\\|")
    cdf <- df.all.all.vars[i,][rep(1, ntimes), ]
    cdf$Plot_ID_Veg <- unlist(str_split(cPlot_ID_Veg,pattern = "\\|"))

  } else {
   cdf <- df.all.all.vars[i,]
  }

  df.mod <- bind_rows(df.mod,
                      cdf)
}

df.mod <- df.mod %>%
  mutate(Delta_SMrz = SMrz_M - SMrz_m)

inventories <- read.csv("~/Downloads/upper_perc_finaal_fiveBiggestTrees.csv") %>%
  # dplyr::select(continent,Plot_ID_Veg,lat,lon,DBH_ini)
  dplyr::select(-c(X.1,ClusterCode.x,X,MCWD)) %>%
  filter(PlotArea.y >= 1)

inventories.vs.climate <- inventories %>%
  left_join(df.mod %>%
              dplyr::select(-c(lat,lon,biome)),
            by = "Plot_ID_Veg")

inventories.vs.climate.long <- inventories.vs.climate %>%
  pivot_longer(cols = -c(Plot_ID_Veg,lon,lat,continent),
               names_to = "variable",
               values_to = "value")

ggplot(data = inventories.vs.climate.long) +
  geom_density(aes(x = value, fill = continent), color = NA,
               alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

ggplot(data = inventories.vs.climate.long %>%
         filter(variable %in% c("Pdep","DBH_ini","Prec_driest_month","SMrz_m","EVI.av"))) +
  geom_density(aes(x = value, fill = continent), color = NA,
               alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

ggplot(data = inventories.vs.climate) +
  geom_point(aes(y = Delta_EVI, x = Delta_SMrz, color = continent)) +
  stat_smooth(aes(y = Delta_EVI, x = Delta_SMrz),
              method = "lm", color = "black", se =FALSE) +
  stat_smooth(aes(y = Delta_EVI, x = Delta_SMrz, color = continent, fill = continent),
              method = "lm") +
  theme_bw()

bind_rows(inventories.vs.climate %>%
            group_by(continent) %>%
            summarise(r2 = summary(lm(formula = Delta_EVI ~ Delta_SMrz))[["r.squared"]],
                      pval = summary(lm(formula = Delta_EVI ~ Delta_SMrz))[["coefficients"]][2,4],
                      .groups = "keep"),
          inventories.vs.climate %>%
            mutate(continent = "all") %>%
            group_by(continent) %>%
            summarise(r2 = summary(lm(formula = Delta_EVI ~ Delta_SMrz))[["r.squared"]],
                      pval = summary(lm(formula = Delta_EVI ~ Delta_SMrz))[["coefficients"]][2,4],
                      .groups = "keep"))


ggplot(data = inventories.vs.climate) +
  geom_point(aes(y = EVI.M, x = SMrz_m, color = continent)) +
  stat_smooth(aes(y = EVI.M, x = SMrz_m),
              method = "lm", color = "black", se =FALSE) +
  stat_smooth(aes(y = EVI.M, x = SMrz_m, color = continent),
              method = "lm") +
  theme_bw()


ggplot(data = inventories.vs.climate) +
  geom_point(aes(x = Prec_driest_month, y = DBH_ini, color = continent)) +
  stat_smooth(aes(x = Prec_driest_month, y = DBH_ini),
              method = "lm", color = "black") +
  stat_smooth(aes(x = Prec_driest_month, y = DBH_ini, color = continent, fill = continent),
              method = "lm") +
  theme_bw() +
  theme(legend.position = c(0.9,0.8))

A <- summary(lm(data = inventories.vs.climate,
           formula = DBH_ini ~ Prec_driest_month + PlotArea.y))
A

bind_rows(inventories.vs.climate %>%
  group_by(continent) %>%
  summarise(r2 = summary(lm(formula = DBH_ini ~ Prec_driest_month))[["r.squared"]],
            pval = summary(lm(formula = DBH_ini ~ Prec_driest_month))[["coefficients"]][2,4],
            .groups = "keep"),
  inventories.vs.climate %>%
    mutate(continent = "all") %>%
    group_by(continent) %>%
    summarise(r2 = summary(lm(formula = DBH_ini ~ Prec_driest_month))[["adj.r.squared"]],
              pval = summary(lm(formula = DBH_ini ~ Prec_driest_month))[["coefficients"]][2,4],
              .groups = "keep"))

final.df <- inventories.vs.climate %>%
  mutate(continent = as.factor(continent)) %>%
  relocate(DBH_ini) %>%
  dplyr::select(-c(Plot_ID_Veg,lat,lon,site,Scale,Shape,ratio)) %>%
  na.omit()

corDBH <- as.data.frame(t(cor(x = final.df$DBH_ini, y =final.df[,3:ncol(final.df)]))) %>%
  tibble::rownames_to_column() %>%
  arrange(V1)
corDBH.final <- matrix(corDBH$V1,nrow = 1)
colnames(corDBH.final) <- corDBH$rowname
rownames(corDBH.final)<-'DBH_ini'
CN <- colnames(corDBH.final)

corrplot(corDBH.final, method = 'ellipse', type = 'upper',cl.pos = "n",tl.col = "black")

res.pca <- PCA(final.df %>%
                 dplyr::select(c("DBH_ini",CN[abs(corDBH.final) > 0.2])), graph = FALSE)

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

fviz_pca_biplot(res.pca,
                col.ind = inventories.vs.climate %>%
                  na.omit() %>% pull(continent),col.var = "black",  geom.ind = "point",
                addEllipses = TRUE,repel = TRUE)

# ggplot() +
#   geom_sf(data = world,fill = NA,color = "grey") +
#   geom_point(data = inventories.vs.climate,
#              aes(x = lon, y = lat,
#                  col = EVI.av), size = 1) +
#   theme_void() +
#   labs(x = "",y="",fill = "Forest type") +
#   theme(panel.grid.major = element_blank(),
#         text = element_text(size = 24)) +
#   scale_x_continuous(limits = c(-120,60),expand = c(0,0)) +
#   scale_y_continuous(limits = c(-25,25),expand = c(0,0)) +
#   scale_shape_manual(values = c(16,1)) +
#   guides(size = "none", shape = "none")
