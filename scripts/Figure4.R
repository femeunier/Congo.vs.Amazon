rm(list = ls())

library(modi)
library(dplyr)
library(tidyr)
library(ggplot2)

df.GW.sum.long <- readRDS("./outputs/Global.warming.weighting.RDS") %>%
  rename(DeltaT.m = m,
         DeltaT.low = low,
         DeltaT.high = high)

AGB <- readRDS("./outputs/df.Biomass.tot.RDS")
transitions <- readRDS("./outputs/df.transitions.RDS")
transitions.m <- transitions %>%
  group_by(scenario,period,weighting) %>%
  summarise(r.change.m = mean(r.change)*100,
            r.change.low = quantile(r.change,0.025)*100,
            r.change.high = quantile(r.change,0.975)*100,
            .groups = "keep")

scenarios <- unique(AGB$scenario)
scenarios <- scenarios[scenarios != "historical"]

df.GW.sum.long.new <- AGB.new <- data.frame()
for (cscenario in scenarios){
  df.GW.sum.long.new <- bind_rows(df.GW.sum.long.new,
                                  df.GW.sum.long %>%
                                    filter(scenario == "historical") %>%
                                    mutate(scenario = cscenario),
                                  df.GW.sum.long %>%
                                    filter(scenario == cscenario))
  AGB.new <- bind_rows(AGB.new,
                       AGB %>%
                         filter(scenario == "historical") %>%
                         mutate(scenario = cscenario),
                       AGB %>%
                         filter(scenario == cscenario))

}

AGB.new <- AGB.new %>%
  group_by(period,scenario,weighting,iter) %>%
  summarise(AGB = sum(AGB),
            .groups = "keep") %>%
  group_by(scenario,weighting,iter) %>%
  arrange(period) %>%
  mutate(AGB.change = AGB - AGB[1])


AGB.m <- AGB.new %>%
  group_by(scenario,period,weighting) %>%
  summarise(AGB.change.m = mean(AGB.change,na.rm = TRUE),
            AGB.change.low = quantile(AGB.change, probs = 0.025, na.rm = TRUE),
            AGB.change.high = quantile(AGB.change, probs = 0.975, na.rm = TRUE),
            .groups = "keep") %>%
  left_join(transitions.m,
            by = c("scenario","period","weighting")) %>%
  left_join(df.GW.sum.long.new,
            by = c("scenario","period","weighting"))

all <- (AGB.m)

all.long <- all %>%
  # dplyr::select(-c(AGB,tas.m)) %>%
  pivot_longer(cols = c(AGB.change.m,AGB.change.low,AGB.change.high,
                        r.change.m,r.change.low,r.change.high),
               names_to = "variable") %>%
  mutate(var = case_when(grepl("AGB.change",variable) ~ "AGB.change",
                         grepl("r.change",variable) ~ "r.change",
                         TRUE ~ "other")) %>%
  mutate(metric = sub("AGB.change.|r.change.","",variable)) %>%
  dplyr::select(-variable) %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  mutate(scenario = case_when(period <= 2014 ~ "historical",
                              TRUE ~ scenario))


ggplot(data = all.long,
       aes(x = DeltaT.m,
           y = m,
           shape = weighting,
           linetype = weighting)) +
  stat_smooth(method = lm,
              se = TRUE, color = "black",
              size = 0.5,
              formula = y ~ poly(x,2)) +

  # geom_ribbon(data = all.long,
  #                 aes(ymin = low,ymax = high),
  #               linetype = 1, alpha = 0.5,
  #               width = 0) +

  # geom_errorbar(data = all.long %>%
  #                 filter(period %in% c(1900,1950,2000,2050,2100)),
  #                 aes(ymin = low,
  #                   ymax = high),
  #               linetype = 1,
  #               width = 0) +
  geom_errorbarh(aes(xmin = DeltaT.low,
                     xmax = DeltaT.high),
                linetype = 1,
                height = 0) +

  geom_point(aes(color = scenario)) +

  labs(x = "", y = "") +
  facet_wrap(~ var, scales = "free") +
  scale_color_manual(values = c("darkgrey","#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  guides(linetype = "none",shape = "none") +
  # scale_x_continuous(limits = c(0,6)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(2, "lines"),
    text = element_text(size = 20)) +
  guides(color = "none")


all.long[["predicted"]] <- NA

for (cvar in c("AGB.change","r.change")){
  for (cweighting in c("w","m")){

    pos <- which(all.long$var == cvar & all.long$weighting == cweighting)
    cmodel <- lm(data = all.long %>%
         filter(var == cvar,
                weighting == cweighting) %>%
           mutate(DeltaT.m2 = DeltaT.m**2),
       formula = m ~ DeltaT.m + DeltaT.m2)


    all.long$predicted[pos] <- predict(cmodel,
                                       newdata = data.frame(DeltaT.m = all.long$DeltaT.m[pos],
                                                            DeltaT.m2 = (all.long$DeltaT.m[pos])**2))

    cx <- df.GW.sum.long %>%
      filter(weighting == cweighting) %>%
      filter(period == 2010) %>%
      pull(DeltaT.m) %>% mean()
    # cx = 0.
    # cx = 2
    Coefs <- coef(cmodel)
    cy <- 2*Coefs[3]*cx + Coefs[2]


    print(paste(cvar,cweighting,":",cx,cy,", r2: ",  summary(cmodel)[["adj.r.squared"]]))

  }
}


ggplot(data = all.long,
       aes(x = DeltaT.m,
           y = m,
           shape = weighting,
           linetype = weighting)) +

  geom_ribbon(data = all.long,
                  aes(y = predicted,ymin = (-m + predicted) + low,ymax = (-m + predicted) + high),
                linetype = 1, alpha = 0.5,
              fill = "lightgrey",
              width = 0) +

  stat_smooth(method = lm,
              se = FALSE, color = "black",
              size = 0.5,
              formula = y ~ poly(x,2)) +

  geom_point(aes(color = scenario)) +

  labs(x = "", y = "") +
  facet_wrap(~ var, scales = "free") +
  scale_color_manual(values = c("darkgrey","#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  guides(linetype = "none",shape = "none") +
  # scale_x_continuous(limits = c(0,6)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(2, "lines"),
    text = element_text(size = 20)) +
  guides(color = "none")


###################################################################


ggplot(data = all.long,
       aes(x = DeltaT.m,
           y = m,
           shape = weighting,
           linetype = weighting)) +
  stat_smooth(method = lm,
              se = FALSE, color = "black",
              size = 0.5,
              formula = y ~ poly(x,2)) +

  geom_errorbar(data = all.long %>%
                  filter(period %in% c(1900,1950,2000,2050,2100)),
                aes(ymin = low,
                    ymax = high),
                linetype = 1,
                width = 0) +

  geom_point(aes(color = scenario)) +

  labs(x = "", y = "") +
  facet_wrap(~ var, scales = "free",
             ncol = 1) +
  scale_color_manual(values = c("darkgrey","#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw() +
  guides(linetype = "none",shape = "none") +
  # scale_x_continuous(limits = c(0,6)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(2, "lines"),
    text = element_text(size = 20)) +
  guides(color = "none")

