rm(list = ls())

library(dplyr)
library(ggplot2)
library(pals)
library(cowplot)
library(pals)
library(zoo)

Prefixes <- c("Basin.Comp.all.",
              "Basin.Comp.min.",
              "Basin.Comp.min2.")


Window = 12

df.r2.all <- Diff.all <-
  data.frame()

for (Prefix in Prefixes){

  files2transfer <- paste0(Prefix,
                           c("mean.test.RDS",
                             "mean.sum.RDS"))

  for (cfile in files2transfer){
    system2("rsync",
            c("-avz",
              paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
              "./outputs/"))
  }

  Trendy.data <- readRDS(paste0("./outputs/",
                                Prefix,
                                "mean.test.RDS"))


  df.r2 <- Trendy.data %>%
    group_by(model,basin) %>%
    summarise(r2 = summary(lm(formula = obs ~ pred))[["r.squared"]],
              rmse = 1/length(obs)*sqrt(sum((obs-pred)**2,na.rm = TRUE)),
              .groups = "keep")


  df.r2.all <- bind_rows(df.r2.all,
                         df.r2 %>%
                           mutate(scenario = Prefix))

  Basin.Comp.mean.sum <- readRDS(paste0("./outputs/",
                                        Prefix,
                                        "mean.sum.RDS"))
  MEM <- Basin.Comp.mean.sum %>%
    group_by(year,month,var,basin) %>%
    summarise(pred.m = mean(pred),
              obs.m = mean(obs),
              .groups = "keep") %>%
    mutate(time = year + (month - 1/2)/12)

  Diff <- MEM %>%
    filter(year >= 1958) %>%
    ungroup() %>%
    dplyr::select(-obs.m) %>%
    pivot_wider(names_from = basin,
                values_from = c(pred.m)) %>%
    mutate(diff = Amazon - Congo,
           diff.rel = 100*(Amazon - Congo)/Congo,
           time = year + (month - 1/2)/12) %>%
    mutate(diff.rm = rollapply(diff, width=Window,
                               FUN=function(x) mean(x, na.rm=TRUE),
                               partial=TRUE, fill=NA, align="center"),
           diff.rel.rm = rollapply(diff.rel, width=Window,
                                   FUN=function(x) mean(x, na.rm=TRUE),
                                   partial=TRUE, fill=NA, align="center"))


  Diff.all <- bind_rows(Diff.all,
                        Diff %>%
                           mutate(scenario = Prefix))

}

ggplot(data = df.r2.all) +
  geom_density(aes(x = r2,
                   fill = scenario),
               alpha = 0.5, color = NA) +
  facet_wrap(~ basin) +
  theme_bw()

df.r2.all %>%
  group_by(scenario,basin) %>%
  summarise(r2.min = min(r2),
            r2.mean = mean(r2),
            r2.max = max(r2)) %>%
  arrange(basin)

################################################################################

ggplot(data = Diff.all) +
  geom_line(aes(x = time,
                y = diff.rel.rm,
                color = scenario)) +
  stat_smooth(aes(x = time,
                  y = diff.rel.rm,
                  color = scenario),
              method = "lm", se = FALSE) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_continuous(limits = c(1958,2023)) +
  theme_bw()

Absolute.GPP <- Diff.all %>%
  dplyr::select(time,year,month,var,scenario,
                Amazon,Congo) %>%
  pivot_longer(cols = c(Amazon,Congo),
               names_to = "basin")

ggplot(data = Absolute.GPP,
       aes(x = time,
           y = value,
           color = scenario)) +
  geom_line(size = 0.25) +
  facet_wrap(~ basin) +
  stat_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(1958,2023)) +
  theme_bw()



