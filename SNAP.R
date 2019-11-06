library(tidyverse)
library(foreign)
library(leaflet)
library(htmltools)

t <- read.dbf("c397Polygon.dbf")
write.csv2(file = "snap.csv", x=t, row.names=F)
t <-  read.csv2("snap.csv")
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = ifelse(t$est_avance=="En proceso de ingreso al SNAP", "orange", "green")
)

pal <- c("orange", "green")
leaflet(t) %>%
  addTiles() %>%
  addProviderTiles(provider =
                     providers$OpenTopoMap) %>%
  addAwesomeMarkers(lat=t$latitud,
             lng=t$longitud,
             icon=icons,
             label=~htmlEscape(paste(t$popup, "\n| Estado:", t$est_avance)))

