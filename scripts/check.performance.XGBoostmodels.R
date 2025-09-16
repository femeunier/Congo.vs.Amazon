rm(list = ls())

library(dplyr)
library(ggplot2)
library(pals)
library(cowplot)
library(pals)
library(sf)
library(tidyr)
library(zoo)
library(segmented)

Prefix <- "Basin.Comp.PreTmp."

files2transfer <- paste0(Prefix,
                         c("mean.test.RDS",
                           "mean.sum.RDS"))

# for (cfile in files2transfer){
#   system2("rsync",
#           c("-avz",
#             paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }

Trendy.data <- readRDS(paste0("./outputs/",
                              Prefix,
                              "mean.test.RDS"))

Trendy.data.sum <- Trendy.data %>%
  group_by(model,basin,var,year,month) %>%
  summarise(pred.m = mean(pred),
            obs.m = mean(obs),
            .groups = "keep")


palette <- kelly(n=17)[2:17]

ggscater <- ggplot(Trendy.data %>%
                     filter(basin == "Congo",
                            var == "nbp"),
                   aes(x = pred,
                       y = obs,
                       color = model)) +
  geom_point(shape = NA) +
  geom_hex(color = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = 1, color = "black") +
  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(100,100000), oob = scales::squish,
    trans = "log10") +
  scale_color_manual(values = palette) +
  coord_equal() +
  # facet_wrap(~ basin) +
  theme_bw() +
  labs(x = "", y = "", color = "", fill = "") +
  theme(legend.position = c(0.2,0.75)) +
  guides(color = "none") +
  theme(text = element_text(size = 20))

ggExtra::ggMarginal(ggscater,
                    type = "density",
                    groupColour = TRUE)

df.r2 <- Trendy.data %>%
  group_by(model,var,basin) %>%
  summarise(r2 = summary(lm(formula = obs ~ pred))[["r.squared"]],
            obs.m = mean(obs,na.rm = TRUE),
            rmse = sqrt(1/length(obs)*(sum((obs-pred)**2,na.rm = TRUE))),
            .groups = "keep") %>%
  mutate(rel.RMSE = rmse/obs.m)


ggplot(data = df.r2) +
  geom_density(aes(x = r2,
                   fill = basin),
               alpha = 0.5, color = NA) +
  facet_wrap(~ var) +
  theme_bw()


df.r2.m <- Trendy.data.sum %>%
  group_by(model,var,basin) %>%
  summarise(r2 = summary(lm(formula = obs.m ~ pred.m))[["r.squared"]],
            obs.m = mean(obs.m,na.rm = TRUE),
            rmse = sqrt(1/length(obs.m)*(sum((obs.m-pred.m)**2,na.rm = TRUE))),
            .groups = "keep") %>%
  mutate(rel.RMSE = rmse/obs.m)


ggplot(data = df.r2.m) +
  geom_density(aes(x = r2,
                   fill = basin),
               alpha = 0.5, color = NA) +
  facet_wrap(~ var) +
  theme_bw()

################################################################################

Coord <- bind_rows(readRDS("./data/Amazon.coord.ILF.RDS") %>%
                     mutate(basin = "Amazon"),
                   readRDS("./data/Congo.coord.ILF.RDS") %>%
                     mutate(basin = "Congo")) %>%
  filter(model == "DLEM")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")

ggplot(data = Coord) +
  geom_tile(aes(x=lon,y = lat, fill = basin),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +
  geom_sf(data = Congo.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, 45), ylim = c(-25, 10), expand = FALSE) +
  labs(x = "",y = "") +
  scale_fill_manual(values = c("darkgreen","#72a83d")) +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

################################################################################

Basin.Comp.mean.sum <- readRDS(paste0("./outputs/",
                                      Prefix,
                                      "mean.sum.RDS"))
MEM <- Basin.Comp.mean.sum %>%
  group_by(year,month,var,basin) %>%
  summarise(pred.m = mean(pred),
            obs.m = mean(obs),
            .groups = "keep") %>%
  mutate(time = year + (month - 1/2)/12)

ggplot(data = MEM) +
  # geom_line(aes(x = time,
  #               y = obs.m,
  #               group = basin),
  #           color = "grey") +
  geom_line(aes(x = time,
                y = pred.m,
                color = basin),
            linetype = 1) +
  stat_smooth(aes(x = time,
                  y = pred.m,
                  color = basin),
              method = "lm", se = FALSE) +
  facet_wrap(~ var,scales = "free") +
  scale_x_continuous(limits = c(1994,2023)) +
  theme_bw()

Window = 12
Diff <- MEM %>%
  filter(year >= 1958) %>%
  ungroup() %>%
  dplyr::select(-obs.m) %>%
  pivot_wider(names_from = basin,
              values_from = c(pred.m)) %>%
  mutate(diff = Amazon - Congo,
         diff.rel = 100*(Amazon - Congo)/Congo,
         time = year + (month - 1/2)/12) %>%
  group_by(var) %>%
  mutate(diff.rm = rollapply(diff, width=Window,
                             FUN=function(x) mean(x, na.rm=TRUE),
                             partial=TRUE, fill=NA, align="center"),
         diff.rel.rm = rollapply(diff.rel, width=Window,
                             FUN=function(x) mean(x, na.rm=TRUE),
                             partial=TRUE, fill=NA, align="center"))

times <- data.frame(time = seq(1980,
                               2024,
                               length.out = 1000))
df.LM.br <- data.frame()
for (cvar in unique(MEM$var)){

  lmBr.Amazon <- segmented(lm(formula = pred.m ~ time,
                              data = MEM %>%
                                filter(var == cvar) %>%
                                filter(year %in% (1980:2023)) %>%
                                filter(basin == "Amazon")),
                           psi = 2010)

  lmBr.Congo <- segmented(lm(formula = pred.m ~ time,
                             data = MEM %>%
                               filter(var == cvar) %>%
                               filter(year %in% (1980:2023)) %>%
                               filter(basin == "Congo")),
                          psi = 2010)

  df.LM.br <- bind_rows(
    df.LM.br,
    data.frame(time = times$time,
               pred.m = predict(lmBr.Amazon,
                                times),
               basin = "Amazon",
               var = cvar),
    data.frame(time = times$time,
               pred.m = predict(lmBr.Congo,
                                times),
               basin = "Congo",
               var = cvar))
}

df.LM.br.wide <- df.LM.br %>%
  pivot_wider(values_from = pred.m,
              names_from = basin) %>%
  mutate(diff.rm = Amazon - Congo)


ggplot(data = MEM ,
       aes(x = time,
           y = pred.m,
           color = basin)) +
  geom_line(size = 0.25, alpha = 0.5) +
  geom_line(data = df.LM.br) +
  scale_x_continuous(limits = c(1994,2023)) +
  facet_wrap(~ var, scales = "free") +
  # scale_y_continuous(limits = c(2.5,4)) +
  guides(color = "none") +
  scale_color_manual(values = c("darkgreen","#72a83d")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(text = element_text(size = 20))

ggplot(data = Diff) +
  geom_line(aes(x = time,
                y = diff),
            color = "lightgrey") +
  geom_line(aes(x = time,
                y = diff.rm),
            color = "black") +
  # stat_smooth(aes(x = time,
  #                 y = diff.rm),
  #             method = "lm", color = "black",linetype = 1,
  #             se = FALSE) +
  geom_line(data = df.LM.br.wide,
            aes(x = time,
                y = diff.rm),
            color = "black",linetype = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_continuous(limits = c(1980,2023)) +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(x = "", y = "") +
  theme(text = element_text(size = 20))

summary(lm(data = Diff %>%
             filter(year %in% (1980:2023)),
           formula = diff.rm ~ year))

