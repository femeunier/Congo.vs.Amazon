rm(list = ls())

library(dplyr)

data <- readRDS("./data/LC_Congo_fine.RDS")

data %>%
  ungroup() %>%
  filter(LC == 2) %>%
  mutate(N = n()) %>%
  filter(abs(lat) < 5) %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")

data %>%
  ungroup() %>%
  filter(LC == 2) %>%
  mutate(N = n()) %>%
  filter(lat > 0) %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")

data %>%
  ungroup() %>%
  filter(LC == 3) %>%
  mutate(N = n()) %>%
  filter(lat > 5) %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")

data %>%
  ungroup() %>%
  filter(LC == 3) %>%
  mutate(N = n()) %>%
  filter(lat < -5) %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")

data %>%
  ungroup() %>%
  filter(LC == 1) %>%
  mutate(N = n()) %>%
  filter(lon >= 25) %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")

data %>%
  ungroup() %>%
  filter(LC == 1) %>%
  mutate(N = n()) %>%
  filter(lat > 0) %>%
  summarise(N5 = n(),
            frac = N5/unique(N),
            .groups = "keep")

