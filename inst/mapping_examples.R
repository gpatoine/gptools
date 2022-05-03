
library(sf)
library(leaflet)
library(mapview)
library(dplyr)

nc <- st_read(system.file("shape/nc.shp", package="sf"))

plot(nc[1])

iris2 <- iris %>% mutate(x = runif(nrow(.))*10,
                         y = runif(nrow(.))*10)

iris_sf <- iris2 %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

plot(iris_sf[1])

leaflet(nc) %>% addTiles() %>% addPolygons()
leaflet(iris_sf) %>% addTiles() %>% addMarkers()

mapview(nc)
mapview(iris_sf)
mapview(iris_sf, zcol = "Species")

gp_pointmap(iris2, "Species")
gp_pointmap(iris_sf, "Species")
gp_pointmap(iris_sf, type = "leaf")
gp_pointmap(iris_sf, type = "gg")

qwmap(iris_sf)
