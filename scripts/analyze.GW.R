rm(list = ls())

library(zoo)

system2("rsync",
        paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Global.warming.RDS",
              "./outputs/"))

Global.warming <- readRDS("./outputs/Global.warming.RDS") %>%
  rename(scenario = cscenario)
df <- Global.warming %>%
  filter((scenario == "historical" & year <= 2014) |
           scenario != "historical")
df$tas[df$tas> 400] <- NA

scenarios <- unique(df$scenario)
scenarios <- scenarios[scenarios != "historical"]

df2augment <- data.frame()
for (cscenario in scenarios){

  df2augment <- bind_rows(df2augment,
                          df %>%
                            filter(scenario == "historical") %>%
                            mutate(scenario = cscenario),
                          df %>%
                            filter(scenario == cscenario))
}

Window <- 30
df.rm <-
  df2augment %>%
  group_by(model,variant,scenario) %>%
  mutate(tas.rm = rollapply(tas, width=Window,
                            FUN=function(x) mean(x, na.rm=TRUE),
                            partial=TRUE, fill=NA, align="center")) %>%
  filter( (year > 2014) | (year <= 2014 & scenario == "ssp245")) %>%
  mutate(scenario = case_when(year <= 2014 ~ "historical",
                              TRUE ~ scenario)) %>%
  ungroup() %>%
  arrange(year)

df.rm.sum <- df.rm %>%
  group_by(year,scenario) %>%
  summarise(tas.rm.m = mean(tas.rm,na.rm = TRUE),
            tas.rm.low = quantile(tas.rm,0.25,na.rm = TRUE),
            tas.rm.high = quantile(tas.rm,0.75,na.rm = TRUE),
            .groups = "keep")


ggplot() +
  geom_line(data = df.rm,
            aes(x = year, y = tas.rm  - 273.15, color = scenario,
                group = interaction(model,scenario)),
            linewidth = 0.1) +
  geom_ribbon(data = df.rm.sum,
              aes(x = year, y = tas.rm.m - 273.15,
                  ymin = tas.rm.low - 273.15, ymax = tas.rm.high - 273.15,
                  fill = scenario), color = NA, alpha = 0.5) +
  geom_line(data = df.rm.sum,
            aes(x = year, y = tas.rm.m - 273.15, color = scenario)) +
  geom_vline(xintercept = 2014.5) +
  scale_color_manual(values = c("black","#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  scale_fill_manual(values = c("black","#263b5d","#8b9bac","#b48a40","#6a2d31")) +
  theme_bw()

saveRDS(df.rm,
        "./outputs/df.Global.warming.RDS")


df.sum <- df.rm %>%
  group_by(year,scenario) %>%
  summarise(tas.m = mean(tas,na.rm = TRUE),
            tas.low = quantile(tas,0.25,na.rm = TRUE),
            tas.high = quantile(tas,0.75,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_line(data = df,
            aes(x = year, y = tas  - 273.15, color = scenario,
                group = interaction(model,scenario)),
            linewidth = 0.1) +
  geom_ribbon(data = df.sum,
              aes(x = year, y = tas.m - 273.15,
                  ymin = tas.low - 273.15, ymax = tas.high - 273.15,
                  fill = scenario), color = NA, alpha = 0.5) +
  geom_line(data = df.sum,
            aes(x = year, y = tas.m - 273.15, color = scenario)) +
  geom_vline(xintercept = 2014.5) +
  theme_bw()

# saveRDS(df,
#         "./outputs/df.Global.warming.RDS")


df.weights <- readRDS("./outputs/CMIP6.weights.Tropics.RDS") %>%
  group_by(model) %>%
  summarise(w = mean(w))

df.GW <- df.rm %>%
  mutate(period = year) %>%
  filter(model %in% df.weights$model)


df.GW.w <- df.GW %>%
  filter(!is.na(tas)) %>%
  left_join(df.weights,
            by = "model")

df.GW.sum <- df.GW.w %>%
  group_by(scenario, period) %>%
  summarise(tas.w.m = weighted.mean(tas.rm,w,na.rm = TRUE),
            tas.w.low = weighted.quantile(tas.rm,w,prob = 0.025),
            tas.w.high = weighted.quantile(tas.rm,w,prob = 0.975),

            tas.m.m = mean(tas.rm,na.rm = TRUE),
            tas.m.low = quantile(tas.rm,prob = 0.025),
            tas.m.high = quantile(tas.rm,prob = 0.975),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(DeltaT.w.m = tas.w.m - mean(tas.w.m[period %in% c(1900:1929)],
                                     na.rm = TRUE),
         DeltaT.w.low = tas.w.low - mean(tas.w.m[period %in% c(1900:1929)],
                                         na.rm = TRUE),
         DeltaT.w.high = tas.w.high - mean(tas.w.m[period %in% c(1900:1929)],
                                           na.rm = TRUE),

         DeltaT.m.m = tas.m.m - mean(tas.m.m[period %in% c(1900:1929)],
                                     na.rm = TRUE),
         DeltaT.m.low = tas.m.low - mean(tas.m.m[period %in% c(1900:1929)],
                                         na.rm = TRUE),
         DeltaT.m.high = tas.m.high - mean(tas.m.m[period %in% c(1900:1929)],
                                           na.rm = TRUE))

df.GW.sum.long <- df.GW.sum %>%
  dplyr::select(-c(starts_with("tas"))) %>%
  pivot_longer(cols = -c(scenario,period),
               names_to = "variable",
               values_to = "DeltaT") %>%
  mutate(variable = sub("DeltaT.","",variable)) %>%
  mutate(weighting = sub("\\..*", "", variable),
         metric = sub(".*\\.", "", variable)) %>%
  dplyr::select(-variable) %>%
  pivot_wider(names_from = metric,
              values_from = DeltaT)


ggplot(data = df.GW.sum.long,
       aes(x = period,
           y = m,
           ymin = low,
           ymax = high,
           fill = scenario,
           color = scenario)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line() +
  theme_bw() +
  facet_wrap(~weighting)

saveRDS(df.GW.sum.long,
        "./outputs/Global.warming.weighting.RDS")

