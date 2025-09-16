rm(list = ls())

library(macroBiome)
library(dplyr)
library(pbapply)
library(terra)
library(geodata)
library(ggplot2)
library(pbapply)
library(Congo.vs.Amazon)

tmean <- worldclim_global('tavg',res = 10,path = "./data/")
prec <- worldclim_global('prec',res = 10,path = "./data/")

temp_mat <- as.matrix(tmean) %>%
  na.omit()
prec_mat <- as.matrix(prec) %>%
  na.omit()
xy <- as.data.frame(tmean[[1]],
                    xy = TRUE,na.rm = TRUE) %>%
  # dplyr::select(x,y) %>%
  rename(lon = x, lat = y)

# Use cliBioCliIdxPoints on subset (e.g. 1000 points for testing)
idx <- cliBioCliIdxPoints(
  temp = temp_mat,
  prec = prec_mat,
  lat = xy$lat)

zones <- classify.HLZ(
  abt = as.vector(idx[,"abt"]),
  tap = as.vector(idx[,"tap"]),
  per = as.vector(idx[,"per"]))


df <- xy %>%
  mutate(zone = zones) %>%
  filter(abs(lat) <= 23.25)

conv.df <- macroBiome::vegClsNumCodes %>% dplyr::select(Name.HLZ,Code.HLZ)
df2plot <- df %>%
  na.omit() %>%
  mutate(Code.HLZ = zone) %>%
  left_join(conv.df,
            by = "Code.HLZ")

cat.threshold <- 2000
cats <- sort(table(df2plot$Name.HLZ))

df2plot.recat <- df2plot %>%
  mutate(Name.HLZ.recat = case_when(Name.HLZ %in% names(cats[cats > cat.threshold]) ~ Name.HLZ,
                                    TRUE ~ "Other"))

ggplot(data = df2plot.recat) +
  geom_tile(aes(x = lon,y = lat,
                fill = Name.HLZ.recat)) +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = df2plot.recat) +
  geom_histogram(aes(y = Name.HLZ.recat,
                     fill = Name.HLZ.recat),
                 stat = "count") +
  theme_bw() +
  theme(legend.position = "bottom")



