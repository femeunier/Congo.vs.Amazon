rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)
library(sf)
library(SPEI)
library(caret)

system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.classifications.*",
              "./outputs/"))

CMIP6.files <- list.files("./outputs","*CMIP6.classifications*",
                          full.names = TRUE)

CMIP6.files <- CMIP6.files[grepl("hargreaves",CMIP6.files)]

df.all <- data.frame()
for (file in CMIP6.files){

  print(file)

  CMIP6 <- readRDS(file)

  df.all <- bind_rows(df.all,
                      CMIP6 %>%
                        mutate(file))
}

models.selected <- df.all %>%
  dplyr::select(model,scenario) %>%
  distinct() %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = scenario,
              values_from = present) %>%
  filter(historical & ssp245  & ssp585) %>%
  pull(model)

models.selected.hargreaves <- df.all %>%
  # ungroup() %>%
  # filter(grepl("hargreaves",file)) %>%
  filter(ETeq %in% c("hargreaves")) %>%
  dplyr::select(model,scenario) %>%
  distinct() %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = scenario,
              values_from = present) %>%
  filter(historical & ssp245  & ssp585) %>%
  pull(model)

# models.selected.penman <- df.all %>%
#   # filter(!grepl("hargreaves",file)) %>%
#   filter(ETeq %in% c("penman")) %>%
#   dplyr::select(model,scenario) %>%
#   distinct() %>%
#   mutate(present = TRUE) %>%
#   pivot_wider(names_from = scenario,
#               values_from = present) %>%
#   filter(historical & ssp245  & ssp585) %>%
#   pull(model)

models.all <- df.all %>%
  pull(model) %>%
  unique()

saveRDS(models.selected.hargreaves,
        "./outputs/models.selected.hargreaves.RDS")
# saveRDS(models.selected.penman,
#         "./outputs/models.selected.penman.RDS")
saveRDS(models.selected,
        "./outputs/models.selected.RDS")
saveRDS(models.all,
        "./outputs/models.all.RDS")
