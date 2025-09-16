rm(list = ls())

library(feather)

files <- c("all.test.tasmin.feather",
           "all.test.tasmax.feather")

for (cfile in files){
  system2("rsync",
          c("-avz",
            paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
            "./outputs/"))
}

all.test.tasmin <- read_feather("./outputs/all.test.tasmin.feather")
all.test.tasmax <- read_feather("./outputs/all.test.tasmax.feather")

all <- bind_rows(all.test.tasmin %>%
                   mutate(type = "tasmin"),
                 all.test.tasmax %>%
                   mutate(type = "tasmax")) %>%
  mutate(type = factor(type,
                       levels = c("tasmin","tasmax")))
ggplot(data = all,
       aes(x = pred, y = obs)) +
  geom_hex() +
  stat_smooth(method = "lm", se = FALSE,
              color = "black") +
  geom_abline(slope = 1,intercept = 0, linetype=2) +
  facet_wrap(~ type) +
  scale_fill_gradient(low = "white",
                      high = "darkgrey",trans = "log10") +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

all %>%
  group_by(type) %>%
  summarise(slope = coef(lm(obs ~ pred))[2],
            int = coef(lm(obs ~ pred))[1],
            rsq = summary(lm(obs ~ pred))[["r.squared"]])
