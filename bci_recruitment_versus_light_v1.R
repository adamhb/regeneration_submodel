library(tidyverse)
attach(canopyht5x5)
library(tmap)
library(raster)

#exploring canopy light data
attach(canopyht5x5)

bci_polygon <- raster::shapefile("C:/Users/ahanb/OneDrive/Documents/data/Boundaries_and_AOIs/BCI_50Ha/BCI_50Ha.shp")

utm_pan <- CRS("+init=epsg:32617")

bci_polygon <- spTransform(bci_polygon, CRSobj =  utm_pan)

light_map_data <- canopyht5x5 %>%
  mutate(utm_e = x + 625774, utm_n = y + 1011743)


light_map <- rasterFromXYZ(as.data.frame(light_map_data)[, c("utm_e", "utm_n", "s1990")], crs = utm_pan)

coordinates(light_map) <- ~ utm_e + utm_n
# coerce to SpatialPixelsDataFrame
gridded(light_map) <- TRUE
# coerce to raster
light_map <- raster(light_map)

plot(light_map)


#plotting this with the tmap package
tm_shape(light_map)+
  tm_raster()


#next steps: add recruitment rates by pft to each pixel, and normalize by reproductive basal area in the nearest ha (because the models are each a 1 ha run)

