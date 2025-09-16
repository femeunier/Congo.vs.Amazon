rm(list = ls())

df1 <- readRDS("./outputs/EVI.var.RDS") %>%
  ungroup() %>%
  dplyr::select(-site)
df2 <- readRDS("./outputs/EVI.var.Johanna.RDS")

merged <- df1 %>%
  left_join(df2 %>%
              ungroup() %>%
              dplyr::select(-c(site,biome,lon,lat)),
            by = c("Plot_ID_Veg"))

merged.long <- merged %>%
  pivot_longer(cols = -c(biome,Plot_ID_Veg,lon,lat)) %>%
  mutate(variable = substr(name,1,nchar(name)-2)) %>%
  mutate(origin = substr(name,nchar(name),nchar(name))) %>%
  dplyr::select(-name) %>%
  pivot_wider(names_from = 'origin',
              values_from = "value")

ggplot(data = merged.long,
       aes(x = x/10000, y = y, color = biome)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, color = "black") +
  facet_wrap(~ variable) +
  theme_bw()
