rm(list = ls())

library(dplyr)
library(ggplot2)

df <- expand.grid(impact = seq(0.5,4.5,length.out = 5),
                  likehood = seq(0.5,4.5,length.out = 5)) %>%
  mutate(prod = impact*likehood)

ggplot(data = df) +
  geom_tile(aes(x = impact,
                y = likehood,
                fill = prod)) +
  scale_fill_gradient2(midpoint = 4,limits = c(0,14),
                       low = "darkgreen", high = "darkred") +
  geom_vline(xintercept = seq(0,4)) +
  geom_hline(yintercept = (0:4)) +
  scale_x_continuous(expand = c(0,0),limits = c(0,4),
                     breaks = (0:4)+0.5,
                     labels = c()) +
  labs(x = "", y = "", fill = "") +
  scale_y_continuous(expand = c(0,0),limits = c(0,4),
                     breaks = (0:4)+0.5,
                     labels = c()) +
  theme_minimal() +
  guides(fill = "none")
