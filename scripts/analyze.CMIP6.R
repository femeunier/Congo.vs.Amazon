rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(lubridate)

files2transfer <- c("df.monthly.climate*.RDS")

for (cfile in files2transfer){
  system2("rsync",
          c("-avz",
            paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
            "/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/"))
}


l.files <- list.files("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs",pattern = "df.monthly.climate*")
OP.files.no.ext <- tools::file_path_sans_ext((l.files))
all.attributes <- strsplit(OP.files.no.ext,split = "\\.")

vars <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                        function(i){
                                                          data.frame(var = all.attributes[[i]][4])}))))
scenars <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                        function(i){
                                                          data.frame(var = all.attributes[[i]][5])}))))
models <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                       function(i){
                                                         data.frame(var = all.attributes[[i]][6])}))))
data.sum <- data.frame()
for (ifile in seq(1,length(l.files))){

  print(ifile/length(l.files))
  cscenario <- scenars[ifile]
  cmodel <- models[ifile]
  cvar <- vars[ifile]

  cdata <- readRDS(paste0("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/",l.files[ifile])) %>%
    mutate(model = cmodel,
           var = cvar) %>%
    mutate(basin = case_when(lon <= 0 ~ "Amazon",
                             TRUE ~ "Congo"))

  data.sum <- bind_rows(list(data.sum,
                             cdata %>%
                               group_by(basin,year,month,scenario,model,var) %>%
                               summarise(value.m = mean(get(cvar),na.rm = TRUE),
                                         .groups = "keep")))

}

df.models <- data.sum %>%
  dplyr::select(scenario,model,var) %>%
  distinct() %>%
  mutate(exist = TRUE) %>%
  pivot_wider(names_from = var,
              values_from = exist) %>%
  mutate(all.present = pr & tas) %>%
  dplyr::select(-c(pr,tas)) %>%
  pivot_wider(names_from = scenario,
              values_from = all.present) %>%
  filter(historical & ssp126 & ssp245 & ssp370 &ssp585)

models <- unique(df.models$model)
scenarios <- colnames(df.models %>%
                        dplyr::select(-model))
vars <- unique(vars)

overwrite = FALSE
for (cmodel in models){
  print(cmodel)

  op.file <- paste0("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/","CMIP6.",cmodel,".RDS")
  if (file.exists(op.file) & !overwrite){
    next()
  }

  data.all <- data.frame()
  for (cscenario in scenarios){
    for (cvar in vars){

      cfile <- paste0("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/",
                      "df.monthly.climate.",
                      cvar,".",
                      cscenario,".",
                      cmodel,".RDS")

      if (!file.exists(cfile)){next()}


      cdata <- readRDS(cfile) %>%
        mutate(basin = case_when(lon <= 0 ~ "Amazon",
                                 TRUE ~ "Congo")) %>%
        rename(value = !!cvar) %>%
        mutate(var = cvar) %>%
        dplyr::select(lat,lon,var,value,year,month,scenario,basin)

      data.all <- bind_rows(data.all,
                            cdata)

    }
  }

  data2save <- data.all %>%
    group_by(var,lat,lon,scenario,year,month,basin) %>%
    summarise(value = mean(value),
              .groups = "keep") %>%
    pivot_wider(names_from = "var",
                values_from = "value") %>%
    mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
    mutate(pr = pr*86400*N) %>%
    rename(pre = pr,
           tmp = tas) %>%
    dplyr::select(-N)

  saveRDS(data2save,
          op.file)
}

system2("rsync",
        c("-avz",
          "/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/CMIP6*",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))


data.mod <- data.frame()
for (cmodel in models){
  for (cscenario in scenarios){
    for (cvar in vars){
      if (cscenario == "historical"){

        cdf <- data.sum %>%
          filter(model == cmodel,
                 scenario == cscenario,
                 var == cvar)

      } else {

        cdf <- data.sum %>%
          filter(model == cmodel,
                 scenario %in% c("historical",cscenario),
                 var == cvar) %>%
          mutate(scenario = cscenario)

      }

      data.mod <- bind_rows(data.mod,
                            cdf)

    }
  }
}

data.mod.wide <- data.mod %>%
  pivot_wider(names_from = var,
              values_from = value.m) %>%
  mutate(tas = case_when(tas > 400 ~ (tasmax + tasmin)/2,
                         TRUE ~ tas)) %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  mutate(pr = pr*86400*N)

Window = 6

data.mod.long <- data.mod.wide %>%
  pivot_longer(cols = c(pr,tas,tasmin,tasmax),
               values_to =  "value",
               names_to = "var")

data.mod.anomalies <- data.mod.long %>%
  filter((year >= 1994 & scenario != "historical") |
           ((year %in% 1985:2014 & scenario == "historical")))%>%
  group_by(basin,model,scenario,var) %>%
  mutate(global.mean = mean(value[year <= 2023],
                            na.rm = TRUE)) %>%
  group_by(basin,model,scenario,var,month) %>%
  mutate(mean.month = mean(value[year <= 2023] - global.mean[year <= 2023],
                           na.rm = TRUE)) %>%
  group_by(basin,model,var,scenario) %>%
  mutate(anomaly.month = value - mean.month - global.mean) %>%
  mutate(anomaly.month.rm = rollapply(anomaly.month, width=Window,
                                      FUN=function(x) mean(x, na.rm=TRUE),
                                      partial=TRUE, fill=NA, align="center"))

data.mod.anomalies.sum <- data.mod.anomalies %>%
  group_by(basin,var,year,scenario) %>%
  summarise(value.m = mean(value,
                           na.rm = TRUE),
            anomaly.month.rm.m = mean(anomaly.month.rm,
                                      na.rm = TRUE),
            .groups = "keep")

ggplot(data = data.mod.anomalies.sum %>%
         filter(var == "pr")) +
  geom_line(aes(x = year,
                y = value.m,
                color = scenario)) +
  facet_grid(basin ~ var,scales = "free") +
  theme_bw()

ggplot(data = data.mod.anomalies.sum %>%
         filter(var != "pr")) +
  geom_line(aes(x = year,
                y = value.m,
                color = scenario)) +
  facet_grid(basin ~ var,scales = "free") +
  theme_bw()


ggplot(data = data.mod.anomalies.sum %>%
         filter(var == "pr")) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  geom_line(aes(x = year,
                y = anomaly.month.rm.m,
                color = scenario)) +
  facet_grid(basin ~ var,scales = "free") +
  theme_bw()

ggplot(data = data.mod.anomalies.sum %>%
         filter(var != "pr")) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  geom_line(aes(x = year,
                y = anomaly.month.rm.m,
                color = scenario)) +
  facet_grid(basin ~ var,scales = "free") +
  theme_bw()


################################################################################

coord <- bind_rows(readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Amazon.coord.ILF.RDS"),
                   readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Congo.coord.ILF.RDS")) %>%
  filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))

climate <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/monthly.climate.pantropical.JRA.historical.RDS") %>%
  filter(year >= 1985)

climate.select <- climate %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  mutate(basin = case_when(lon <= 0 ~ "Amazon",
                           TRUE ~ "Congo")) %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  mutate(pre = pre*N*4)

data <- climate.select %>%
  dplyr::select(basin,lon,lat,tmp,tmin,tmax,pre,year,month) %>%
  rename(tas = tmp,
         tasmin = tmin,
         tasmax = tmax,
         pr = pre) %>%
  pivot_longer(cols = c(tas,tasmin,tasmax,pr),
               names_to = "var",
               values_to = "value") %>%
  group_by(basin,var,year,month) %>%
  summarise(value.m = mean(value),
            .groups = "keep") %>%
  group_by() %>%
  mutate(global.mean = mean(value.m[year >= 1994 &year <= 2023],na.rm = TRUE)) %>%
  group_by(basin,var,month) %>%
  mutate(mean.month = mean(value.m[year >= 1994 & year <= 2023] - global.mean[year >= 1994 & year <= 2023],
                           na.rm = TRUE)) %>%
  group_by(basin,var) %>%
  mutate(anomaly.month = value.m - mean.month - global.mean) %>%
  mutate(anomaly.month.rm = rollapply(anomaly.month, width=Window,
                                      FUN=function(x) mean(x, na.rm=TRUE),
                                      partial=TRUE, fill=NA, align="center"))


CMIP.vs.JRA <- data.mod.anomalies %>%
  filter(scenario == "historical") %>%
  ungroup() %>%
  dplyr::select(basin,year,month,var,model,value) %>%
  left_join(data %>%
              ungroup() %>%
              dplyr::select(basin,var,year,month,value.m),
            by = c("basin","year","month","var")) %>%
  mutate(value = case_when(var == "pr" ~ value,
                           TRUE ~ value - 273.15),
         value.m = case_when(var == "pr" ~ value.m,
                             TRUE ~ value.m - 273.15))

weights <- CMIP.vs.JRA %>%
  group_by(model) %>%
  summarise(RMSE = sqrt(sum(((value.m-value)/value)**2,na.rm = TRUE)),
            bias = mean(abs((value.m - value)/value),na.rm = TRUE),
            .groups = "keep") %>%
  arrange(RMSE) %>%
  ungroup() %>%
  mutate(w = 1/bias**2) %>%
  mutate(w = w/sum(w)) %>%
  arrange(model)

model2plot <- 'MRI-ESM2-0'

ggplot() +
  geom_line(data = data.mod.anomalies %>%
              filter(scenario == "historical",
                     model == model2plot,
                     var == "pr"),
            aes(x = year + (month - 1/2)/12,
                y = value),
            color = "red") +
  geom_line(data = data %>%
              filter(var == "pr"),
            aes(x = year + (month - 1/2)/12,
                y = value.m)) +
  facet_grid(basin ~ var, scales = "free") +
  theme_bw()


ggplot() +
  geom_line(data = data.mod.anomalies %>%
              filter(scenario == "historical",
                     model == model2plot,
                     var != "pr"),
            aes(x = year + (month - 1/2)/12,
                y = value),
            color = "red") +
  geom_line(data = data %>%
              filter(var != "pr"),
            aes(x = year + (month - 1/2)/12,
                y = value.m)) +
  facet_grid(basin ~ var, scales = "free") +
  theme_bw()

ggplot() +
  geom_line(data = data.mod.anomalies %>%
              filter(scenario == "historical",
                     var != "pr") %>%
              group_by(basin,year,var) %>%
              summarise(value.m = mean(value,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = year,
                y = value.m),
            color = "red") +
  geom_line(data = data %>%
              filter(var != "pr") %>%
              group_by(basin,year,var) %>%
              summarise(value.m = mean(value.m,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = year,
                y = value.m)) +
  facet_grid(basin ~ var, scales = "free") +
  theme_bw()

data.mod.anomalies.sum <- data.mod.anomalies %>%
  group_by(basin,var,scenario,year,month) %>%
  summarise(value.m = mean(value,
                           na.rm = TRUE),
            anomaly.month.rm = mean(anomaly.month.rm,
                                    na.rm = TRUE),
            .groups = "keep")
  # ungroup() %>%
  # left_join(weights,
  #           by = "model") %>%
  # group_by(scenario,year,month) %>%
  # filter(!is.na(RMSE)) %>%
  # summarise(anomaly.month.rm = weighted.mean(anomaly.month.rm,
  #                                            w = 1/bias**2,
  #                                            na.rm = TRUE),
  #           tas.m.m = weighted.mean(tas.m,
  #                                   w = 1/bias**2,
  #                                   na.rm = TRUE),
  #           .groups = "keep")


data.mod.anomalies.sum <- data.mod.anomalies.sum %>%
  group_by(basin,var) %>%
  mutate(global.mean = mean(value.m[year <= 2023],na.rm = TRUE)) %>%
  group_by(basin,var,scenario,month) %>%
  mutate(mean.month = mean(value.m[year <= 2023] - global.mean[year <= 2023],
                           na.rm = TRUE)) %>%
  group_by(basin,var,scenario) %>%
  mutate(anomaly.month2 = value.m - mean.month - global.mean) %>%
  mutate(anomaly.month.rm2 = rollapply(anomaly.month2, width=Window,
                                       FUN=function(x) mean(x, na.rm=TRUE),
                                       partial=TRUE, fill=NA, align="center"))

ggplot(data = data.mod.anomalies.sum %>%
         filter(var != "pr") %>%
         group_by(var,basin,year,month) %>%
         mutate(N = n()) %>%
         filter(N == 4 | (N == 5 & scenario == "historical"))) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m -273.15,
                color = scenario)) +
  geom_line(data = data %>%
              filter(var != "pr"),
            aes(x = year + (month - 1/2)/12,
                y = value.m - 273.15),
            color = "black",linetype = 2) +
  scale_color_manual(values = c("grey","#263b5d","#8b9bac","#b48a40","#6a2d31"))+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = "none") +
  labs(x = "",y = "") +
  facet_grid(basin ~ var) +
  scale_x_continuous(limits = c(1985,2100))


ggplot(data = data.mod.anomalies.sum %>%
         filter(var == "pr") %>%
         group_by(var,basin,year,month) %>%
         mutate(N = n()) %>%
         filter(N == 4 | (N == 5 & scenario == "historical"))) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = scenario)) +
  geom_line(data = data %>%
              filter(var == "pr"),
            aes(x = year + (month - 1/2)/12,
                y = value.m),
            color = "black",linetype = 2) +
  scale_color_manual(values = c("grey","#263b5d","#8b9bac","#b48a40","#6a2d31"))+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = "none") +
  labs(x = "",y = "") +
  facet_grid(basin ~ var) +
  scale_x_continuous(limits = c(1994,2100))

ggplot(data = data.mod.anomalies.sum %>%
         filter(var != "pr") %>%
         group_by(basin,var,year,month) %>%
         mutate(N = n()) %>%
         filter(N == 4 | (N == 5 & scenario == "historical")),) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.month.rm2,
                color = scenario)) +
  geom_line(data = data %>%
              filter(var != "pr") ,
            aes(x = year + (month - 1/2)/12,
                y = anomaly.month.rm)) +
  scale_color_manual(values = c("grey","#263b5d","#8b9bac","#b48a40","#6a2d31"))+
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  guides(color = "none") +
  labs(x = "",y = "") +
  facet_grid(basin ~ var) +
  scale_x_continuous(limits = c(1985,2100)) +
  theme_bw() +
  theme(text = element_text(size = 20))


ggplot(data = data.mod.anomalies.sum %>%
         filter(var == "pr") %>%
         group_by(basin,var,year,month) %>%
         mutate(N = n()) %>%
         filter(N == 4 | (N == 5 & scenario == "historical")),) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.month.rm2,
                color = scenario)) +
  geom_line(data = data %>%
              filter(var == "pr") ,
            aes(x = year + (month - 1/2)/12,
                y = anomaly.month.rm)) +
  scale_color_manual(values = c("grey","#263b5d","#8b9bac","#b48a40","#6a2d31"))+
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  guides(color = "none") +
  labs(x = "",y = "") +
  facet_grid(basin ~ var) +
  scale_x_continuous(limits = c(1985,2100)) +
  theme_bw() +
  theme(text = element_text(size = 20))

