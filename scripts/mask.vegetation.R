rm(list = ls())


r <- raster("/home/femeunier/Desktop/FWO/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_aggr.tif")
sort(unique(r))

# croplands, herbaceous cover, mosaics where
# cropland or herbaceous cover was >50%, grasslands, and urban and
# bare areas

mod_raster <- reclassify(r,t(matrix(c(10,0,
                                      11,0,
                                      12,0,
                                      20,0,
                                      30,0,
                                      40,1,
                                      50,1,
                                      60,1,
                                      61,1,
                                      62,1,
                                      70,1,
                                      80,1,
                                      90,1,
                                      100,1,
                                      110,1,
                                      120,1,
                                      121,1,
                                      122,1,
                                      130,0,
                                      150,1,
                                      152,1,
                                      153,1,
                                      160,1,
                                      170,1,
                                      180,1,
                                      190,0,
                                      200,0,
                                      201,0,
                                      202,0,
                                      210,0,
                                      220,0),
                                    nrow = 2)))

r.coarse <- aggregate(mod_raster,10)
r.coarse[r.coarse < 0.5] <- 0
r.coarse[r.coarse >= 0.5] <- 1
r.coarse[is.na(r.coarse)] <- 0

plot(r.coarse)

df.mask <- as.data.frame(r.coarse,
              xy = TRUE) %>%
  rename(lon = x,
         lat = y) %>%
  mutate(lon = round(lon,digits = 2),
         lat = round(lat,digits = 2))


saveRDS(df.mask,
        "./outputs/mask.vegetation.RDS")
