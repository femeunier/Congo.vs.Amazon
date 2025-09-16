rm(library = ls())

library(ggplot2)
library(dplyr)

data <- read.csv("~/Documents/CO2_scenarios.csv")

ggplot(data = data %>%
         filter(Scenario != "SSP119",
                Year >= 1901)) +
  geom_line(aes(x = Year,
                y = CO2,
                color = Scenario)) +
  scale_color_manual(values = c("black","#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  labs(x="", y = "") +
  guides(color = "none") +
  scale_x_continuous(breaks = c()) +
  scale_y_continuous(breaks = c()) +
  theme()
