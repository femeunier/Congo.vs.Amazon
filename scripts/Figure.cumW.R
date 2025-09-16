rm(list = ls())

df.weights <- bind_rows(
  data.frame(id = 0,
             cum.w = 0),
  readRDS("./outputs/CMIP6.weights.Tropics.RDS") %>%
    filter(model != "CRUJRA") %>%
    arrange((desc(w))) %>%
  mutate(cum.w = cumsum(w),
         id = 1:n()))

ggplot(data = df.weights) +
  geom_line(aes(x = id,
                y = cum.w)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(text = element_text(size = 20))
