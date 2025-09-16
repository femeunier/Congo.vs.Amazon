rm(list = ls())

library(ggplot2)
library(ggrepel)
library(raster)
library(ggthemes)
library(dplyr)
library(ggforce)

mod_raster <- raster("/data/gent/vo/000/gvo00074/felicien/ELIE/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_Congo.tif")
df.mod_raster <-
  as.data.frame(mod_raster,
                xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  na.omit()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world %>%
            filter(region_un == "Africa"),fill = "grey17", color = "grey17") +

  annotate("rect", xmin = -15, xmax = 60,
           ymin = -15, ymax = 10,size = 2,
           alpha = 0, color= "red") +
  scale_x_continuous(limits = c(-15,60)) +
  scale_y_continuous(limits = c(-35,35)) +
  theme_map() +
  labs(x = "", y="",fill = "") +
  theme(text = element_text(size = 20)) +
  guides(fill = "none")


ggpl <- ggplot() +
  geom_tile(data = df.mod_raster,
            aes(x = lon, y = lat,
                fill = as.factor(C3S.LC.L4.LCCS.Map.300m.P1Y.2020.v2.1.1_Congo)),
            alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey17") +
  geom_segment(data = data.frame(x = 5, xend = 47,
                          y = 0, yend = 0),
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1, linetype = 2, color = "black")+
  scale_fill_manual(values = c("#c49402","#005401","#448704","grey")) +
  scale_x_continuous(limits = c(-15,60)) +
  scale_y_continuous(limits = c(-15,10)) +
  theme_map() +
  labs(x = "", y="",fill = "") +
  theme(text = element_text(size = 20)) +
  guides(fill = "none")

ggsave("./Figures/LC_centraAfrica.png",
       ggpl,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 300)

system2("scp",
        c("hpc:/data/gent/vo/000/gvo00074/felicien/R/Figures/LC_centraAfrica.png",
          "./Figures"))
