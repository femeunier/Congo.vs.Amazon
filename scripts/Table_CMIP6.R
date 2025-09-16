rm(list = ls())

LC <- readRDS("./data/LC_Congo.RDS") %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2))

all.CMIP6.MCWD <- readRDS("./outputs/All.CMIP6.states.timing.MEM.RDS") %>%
  filter(!(model %in% c("NESM3","CIESM"))) %>%
  ungroup() %>%
  mutate(lon = round(lon,2),
         lat = round(lat,2)) %>%
  dplyr::select(-LC) %>%
  left_join(LC,
            by = c("lon","lat")) %>%
  filter(!(model %in% c("NESM3","CIESM")))


all.CMIP6.MCWD %>%
  filter(scenario != "ssp534-over",
         model != "MEM") %>%
  group_by(scenario) %>%
  summarise(N = length(unique(model)))

df.scenario <- all.CMIP6.MCWD %>%
  ungroup() %>%
  filter(scenario != "ssp534-over") %>%
  mutate(scenario = paste0("SSP",substr(scenario,4,4),
                           "-",substr(scenario,5,5),".",
                           substr(scenario,6,6))) %>%
  group_by(model,Emodel) %>%
  summarise(scenarios = paste0(unique(scenario),collapse = "/"),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(extr.included = case_when(Emodel == "Hargreaves" ~ "Yes",
                                   TRUE ~ "No")) %>%
  dplyr::select(model,extr.included,scenarios)

transitions <- all.CMIP6.MCWD %>%
  group_by(scenario,period,model,lon,lat,LC) %>%
  summarise(MAP = sum(pre,na.rm = TRUE),
            MCWD = unique(MCWD),
            .groups = "keep") %>%
  ungroup() %>%
  dplyr::select(scenario,period,model,lon,lat,MCWD,MAP,LC) %>%
  mutate(LC.pred = as.factor(case_when(MCWD > -475 ~ 2,
                                       MAP < 975 ~ 1,
                                       TRUE ~ 3)))

df.ConfusionMat <- transitions %>%
  group_by(model) %>%
  na.omit() %>%
  filter(LC %in% c(1,2,3)) %>%
  mutate(LC = factor(LC,
                     levels = c(1,2,3)),
         LC.pred = factor(LC.pred,
                          levels = c(1,2,3)),) %>%
  summarise(Acc = (confusionMatrix(LC.pred,LC))[["overall"]][1],
            precision = mean(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                             na.rm = TRUE),
            precision.min = min(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                na.rm = TRUE),
            precision.max = max(as.matrix(confusionMatrix(LC.pred,LC)[["byClass"]])[,"Precision"],
                                na.rm = TRUE)) %>%
  left_join(df.scenario,
            by = "model") %>%
  mutate(Acc = signif(Acc,digits = 2),
         Precision = paste0(signif(precision,digits = 2)," \r\n",
                            "(",signif(precision.min,digits = 2),
                            "-",signif(precision.max,digits = 2),
                            ")"))

write.csv(df.ConfusionMat %>%
            dplyr::select(model,
                          extr.included,scenarios,
                          Acc,Precision),
          "./outputs/CMIP6.table.csv")
