rm(list = ls())

library(ggplot2)
library(dplyr)
library(leaflet)
library(raster)
library(stringr)
library(sf)
library(htmlwidgets)
library(webshot)
library(leaflet)

nutrient <- read.csv("~/Downloads/ERC_coordinates.csv") %>%
  dplyr::filter(lat >= 0.5, lat <= 1,
                lon <= 25, lon >= 24) %>%
  filter(grepl("addition",as.character(Description))) %>%
  mutate(label_short = str_sub(Label, end = -2)) %>%
  group_by(label_short) %>%
  summarise(lat = mean(lat),
            lon = mean(lon))

coords <- read.csv("~/Downloads/Congo_coordinates.csv")
coords.YGB <- coords %>%
  dplyr::filter(lat >= 0.5, lat <= 1,
                lon <= 25, lon >= 24)
plots <- coords.YGB %>%
  mutate(label_short = sub("\\_.*", "", Label)) %>%
  filter(Description == "Yangambi_COBIMFO") %>%
    group_by(label_short) %>%
    summarise(lat = mean(lat),
              lon = mean(lon)) %>%
  filter(!(label_short %in% c("MIX2","MIX5")))

coords.FT <- coords %>%
  dplyr::filter(Label == "CongoFlux tower")
coords.plot <- coords %>%
  dplyr::filter((Description == "Congoflux_site"),
                !(Label %in% c("Forest_flume_west","Forest_flume_East"))) %>%
  group_by(Label) %>%
  summarise(Xmin = min(lon),Xmax = max(lon),
            Ymin = min(lat),Ymax = max(lat))

coords.ForestFeo <- coords %>%
  dplyr::filter(Description == "Yangambi_ForestGEO",Label != "VILA")
Xmin <- min(coords.ForestFeo$lon) ; Xmax <- max(coords.ForestFeo$lon)
Ymin <- min(coords.ForestFeo$lat) ; Ymax <- max(coords.ForestFeo$lat)


library(rgdal)
catchements <- st_transform(
  st_as_sf(
    readOGR(dsn = "~/Downloads/Forest catchments/",layer = "right_basin")),
  "+proj=longlat +datum=WGS84")
catchement2 <- st_transform(
  st_as_sf(
    readOGR(dsn = "~/Downloads/Forest catchments/",layer = "left_basin")),
  "+proj=longlat +datum=WGS84")


YGB <- readOGR(dsn = "~/Downloads/",layer = "Yangambi-MAB_WWF")
C <- st_as_sf(YGB)
D <- st_transform(C,
             "+proj=longlat +datum=WGS84")

my_sf <- read_sf("~/Downloads/chunks_polygones.shp")
A <- sf::st_union(st_transform(my_sf, "+proj=longlat +datum=WGS84"))
plot(A)


Points <- st_coordinates(A)
Chull <- Points[chull(Points[,c(1,2)]),]
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(Chull[,c(1,2)])), ID=1)))
# sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))


DAbis <- sf::st_union((
  D %>%
  filter(OBJECTID %in% c(2,3,4))))

Dbis <-   D %>%
  filter(OBJECTID %in% c(4))

Dqat <- sf::st_union((
  D %>%
    filter(OBJECTID %in% c(2,3,4))))

plot(Dqat)

# get a second polygon to substract from (this would be your "urban" + "rural")
buff <- st_buffer(Dqat, 1000000)

# "inverse clipping/cropping" (this would be your "rural")
Dtris <- st_difference(buff, Dqat)

plot(Dtris)

# ggplot(my_sf) +
#   geom_sf(fill = "#69b3a2", color = "#69b3a2")


xys = st_as_sf(plots, coords=c("lon","lat"))

polys = xys %>%
  dplyr::group_by(Label) %>%
  dplyr::summarise() %>%
  st_cast("POLYGON")

plot(polys)

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = 24.5, lat = 0.8, zoom = 11,options = list(opacity = 0.1)) %>%
  addProviderTiles("Esri.WorldImagery",) %>%
  addPolygons(data=sp_poly,weight=2,fill = "pink",
              col = 'pink',opacity = 0.5) %>%
  addPolygons(data=Dtris,
              weight=0.1,col = "white",opacity = 0.1,
              fill = "white") %>%
  # addPolygons(data=Dbis,
  #             weight=2,col = "white",opacity = 1,dashArray = c(5,5),
  #             fill = FALSE) %>%
  addPolygons(data=catchements,
              weight=2,col = "black",opacity = 1,
              fill = FALSE) %>%
  addPolygons(data=catchement2,
              weight=2,col = "black",opacity = 1,
              fill = FALSE) %>%
  addPolygons(data=DAbis,
              weight=2,col = "white",opacity = 1,
              fill = FALSE) %>%
  addPolygons(data=Dqat,
              weight=0.1,col = "black",opacity = 1,
              fill = FALSE) %>%
  # addPolylines(lng = c(24.5,25), lat = c(0.8,0.8), color = "blue") %>%
  addRectangles(lng1 = Xmin,
                lng2 = Xmax,
                lat1 = Ymin,
                lat2 = Ymax,
                fill = FALSE, opacity = 1,weight = 2,
                color = "white") %>%
  addRectangles(lng1 = coords.plot$Xmin,
              lng2 = coords.plot$Xmax,
              lat1 = coords.plot$Ymin,
              lat2 = coords.plot$Ymax,group = coords.plot$Label,
              fill = TRUE, opacity = 1,weight = 2,
              color = "red") %>%

  addCircleMarkers(lng = plots$lon,
             lat = plots$lat,
             fill = TRUE, opacity = 1,weight = 2,radius = 5,fillOpacity = 1,
                color = "yellow") %>%

  addCircleMarkers(lng = nutrient$lon,
                   lat = nutrient$lat,
                   fill = TRUE, opacity = 1,weight = 2,radius = 3,fillOpacity = 1,
                   color = "blue") %>%

  addCircleMarkers(lng = coords.FT$lon, lat = coords.FT$lat,fill = TRUE,
                   fillColor = "white",
                   fillOpacity = 1,
                   color = "black",weight = 2,opacity = 1,
             radius = 4) %>%
  addScaleBar(
    position = c("bottomright"),
    options = scaleBarOptions()
  ) %>%
  addCircleMarkers(lng = coords.FT$lon, lat = coords.FT$lat,fill = TRUE,
                   fillColor = "white",
                   fillOpacity = 1,
                   color = "black",weight = 2,opacity = 1,
                   radius = 7)

m

saveWidget(m, "~/Documents/temp.html", selfcontained = FALSE)



m2 <- leaflet() %>%
  addTiles() %>%
  setView(lng = 24.5, lat = 0.8, zoom = 11,options = list(opacity = 0.1)) %>%
  addProviderTiles("Esri.WorldImagery",) %>%

  addPolygons(data=Dtris,
              weight=0.1,col = "white",opacity = 0.25,
              fill = "white") %>%
  # addPolygons(data=Dbis,
  #             weight=2,col = "white",opacity = 1,dashArray = c(5,5),
  #             fill = FALSE) %>%
  addPolygons(data=DAbis,
              weight=2,col = "white",opacity = 1,
              fill = FALSE) %>%
  addScaleBar(
    position = c("bottomright"),
    options = scaleBarOptions()
  ) %>%
  addCircleMarkers(lng = coords.FT$lon, lat = coords.FT$lat,fill = TRUE,
                   fillColor = "white",
                   fillOpacity = 1,
                   color = "black",weight = 3,opacity = 1,
                   radius = 5)

  # addPolylines(lng = c(24.5,25), lat = c(0.8,0.8), color = "blue") %>%



m2

saveWidget(m2, "~/Documents/temp2.html", selfcontained = FALSE)


library(ggthemes)
library(rnaturalearth)

world <- ne_countries(scale = "small", returnclass = "sf", continent = "Africa") %>%
  filter(name != "Madagascar")

ggplot() +
  geom_sf(data = world,color = "lightgrey",
          fill = "black",linewidth = 0.8) +
  geom_point(color = "white",aes(x = 24.5,y = 0.8),
             shape = "+",size = 1) +
  theme_map()

