library(leaflet)
library(rgdal)
library(htmlwidgets)
library(webshot)

wall <- "https://opendata.arcgis.com/datasets/ef3c99e6dfcf4f84a90ae91820b08d7a_2.geojson"
res <- readOGR(dsn = wall)

m <- leaflet() %>% 
  addProviderTiles(provider =
                    providers$OpenStreetMap.BlackAndWhite) %>%
  setView(lat=52.52, lng=13.4, zoom=10.5) %>% 
  addPolylines(data = res, color="black")

saveWidget(m, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "berlin wall.png",
        cliprect = "viewport")
