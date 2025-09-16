rm(list = ls())

XGBmodel <- readRDS("./outputs/XGB.fit.JRA.historical.IFLAmazon.JSBACH.S2.gpp.RDS")

all.data <- as.data.frame(XGBmodel$training.data)

data2test <- all.data %>%
  ungroup() %>%
  mutate(ID = 1:n()) %>%
  filter(year == 2020,
         month == 1,
         lon == lon[1],
         lat == lat[1])

data2test.climate <- all.data %>%
  filter(lon == data2test[["lon"]],
         lat == data2test[["lat"]],
         month == data2test[["month"]]) %>%
  summarise(spfh = mean(spfh,na.rm = TRUE),
            VPD = mean(VPD,na.rm = TRUE),
            dlwrf = mean(dlwrf,na.rm = TRUE),
            dswrf = mean(dswrf,na.rm = TRUE),
            .groups = "keep")


data2test.NA <- data2test
data2test.NA[,c("spfh","VPD","dlwrf")] <- NA

data2test.fill <- data2test
data2test.fill[,c("spfh","VPD","dswrf","dlwrf")] <- c(data2test.climate %>% pull(spfh),
                                                      data2test.climate %>% pull(VPD),
                                                      data2test.climate %>% pull(dswrf),
                                                      data2test.climate %>% pull(dlwrf))

data.frame(label = XGBmodel$labels[data2test$ID],
           prediction = predict(XGBmodel,
                                data2test[,XGBmodel$finalModel$feature_names]),
           prediction.NA = predict(XGBmodel,
                                   data2test.NA[,XGBmodel$finalModel$feature_names]),
           prediction.fill = predict(XGBmodel,
                                     data2test.fill[,XGBmodel$finalModel$feature_names]))
