rm(list = ls())

library(dplyr)

LandFrac <- readRDS("./outputs/landFrac.RDS") %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

all.lons <- seq(-179.75,179.75,0.5)
all.lats <- seq(-23.25,23.25,0.5)
GridArea <- RCMIP5:::calcGridArea(lon = all.lons,
                                  lat = all.lats) %>%
  melt() %>%
  mutate(Var1 = all.lons[Var1],
         Var2 = all.lats[Var2]) %>%
  rename(lon = Var1,
         lat = Var2,
         area = value) %>%
  left_join(LandFrac %>%
              rename(land.frac = value),
            by = c("lon","lat")) %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

saveRDS(GridArea,
        "./outputs/GridArea.RDS")
