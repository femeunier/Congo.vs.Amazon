rm(list = ls())

library(dplyr)
library(ggplot2)

df.SS <- readRDS("./outputs/SS.sigma.RDS") %>%
  filter((period == 2000 & scenario == "ssp245") |
           period != 2000) %>%
  group_by(period,scenario,weighting,sigma_D,sigma_S) %>%
  summarise(AGB = sum(AGB,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(ref = case_when(period == 2000 ~ TRUE,
                         TRUE ~ FALSE))

df.SS.change <- df.SS %>%
  filter(!ref) %>%
  left_join(df.SS %>%
              filter(ref) %>%
              rename(AGB.ref = AGB) %>%
              ungroup() %>%
              dplyr::select(-c(ref,period,scenario)),
            by = c("weighting","sigma_D","sigma_S")) %>%
  mutate(diff = AGB - AGB.ref)

# sigma_D_main = 0.8 # model performance
# sigma_S_main = 0.75 # model similarities

selected.D <- 0.8 ; Delta.D <- 0.1
selected.S <- 0.75 ; Delta.S <- 0.1

ggplot(data = df.SS.change %>%
         filter(weighting == "w")) +
  geom_tile(aes(x = sigma_D,
                y = sigma_S,
                fill = diff)) +
  geom_rect(aes(xmin = selected.D - Delta.D/2, xmax = selected.D + Delta.D/2,
                ymin = selected.S - Delta.S/2, ymax = selected.S + Delta.S/2),
            fill = NA,
            color = "black",linewidth = 0.5) +
  facet_wrap(~ scenario,
             nrow = 1) +
  labs(x = "", y = "", fill = "") +
  scale_fill_gradient(low = "darkred",high = "white",
                      breaks = c(-40,-20,0),
                      limits = c(-50,0),
                      oob = scales::squish) +
  theme_bw() +
  coord_equal() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        strip.text = element_blank(),
        legend.position = "bottom")


ggplot(data = df.SS.change %>%
         filter(weighting == "m")) +
  geom_tile(aes(x = sigma_D,
                y = sigma_S,
                fill = diff)) +
  facet_wrap(~ scenario,
             nrow = 1) +
  labs(x = "", y = "") +
  scale_fill_gradient(low = "darkred",high = "white",
                      limits = c(-50,0),
                      oob = scales::squish) +
  scale_x_continuous(breaks = c()) +
  scale_y_continuous(breaks = c()) +
  theme_bw() +
  coord_equal() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        strip.text = element_blank())


ggplot(data = df.SS.change) +
  geom_tile(aes(x = sigma_D,
                y = sigma_S,
                fill = diff)) +
  facet_grid(weighting ~ scenario) +
  labs(x = "", y = "") +
  scale_fill_gradient(low = "darkred",high = "white",
                      oob = scales::squish) +
  scale_x_continuous(breaks = c()) +
  scale_y_continuous(breaks = c()) +
  theme_bw() +
  coord_equal() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        strip.text = element_blank())

################################################################################

df.weights.all <- readRDS("./outputs/df.weights.all.RDS")

w.threshold <- 0.9
df.weights.all.sum <- df.weights.all %>%
  filter(model != "CRUJRA") %>%
  group_by(sigma_D,sigma_S) %>%
  arrange(desc(w)) %>%
  mutate(cum.w = cumsum(w),
         id = 1:n()) %>%
  filter(cum.w > w.threshold) %>%
  slice_head(n = 1)

ggplot(data = df.weights.all.sum) +
  geom_tile(aes(x = sigma_D,
                y = sigma_S,
                fill = id)) +
  geom_rect(aes(xmin = selected.D - Delta.D/2, xmax = selected.D + Delta.D/2,
                ymin = selected.S - Delta.S/2, ymax = selected.S + Delta.S/2),
            fill = NA,
            color = "black",linewidth = 0.5) +
  theme_bw() +
  scale_fill_gradient(low = "white",high = "black",
                      limits = c(1,30),
                      breaks = c(0,10,20,30)) +
  labs(x = "", y = "", fill = "") +
  coord_equal() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        strip.text = element_blank())
