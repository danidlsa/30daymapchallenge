library(tidyverse)
library(rayshader)
library(raster)
library(magick)
library(gganimate)

ras = raster("URY_alt.grd")
elmat= matrix(raster::extract(ras, raster::extent(ras), buffer = 500),
              nrow = ncol(ras), ncol = nrow(ras))
raymat <- ray_shade(elmat)
ambmat <- ambient_shade(elmat)

elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_water(detect_water(elmat), color = "white") %>% 
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  plot_3d(elmat, zscale = 10, fov = 60, theta = 0, zoom = .9, phi = 90, windowsize = c(1000, 800))
render_snapshot("imagen fija.png", clear=FALSE)


##### cargar funciones de transition_values, save_3dgif y plot_3d_tidy desde github (mismo repo)
## Fuente de las funciones: https://wcmbishop.github.io/rayshader-demo/ 


# gif transition variables
n_frames <- 180
theta <- transition_values(from = 340, to = 250, steps = n_frames, #chequear que la resta entre to y from sea 180, 90, etc.
                           one_way = TRUE, type = "lin")
phi <- transition_values(from = 90, to = 10, steps = n_frames, 
                         one_way = TRUE, type = "cos")
zoom <- transition_values(from = 0.9, to = 0.3, steps = n_frames, 
                          one_way = TRUE, type = "cos")
watermap <- detect_water(elmat)

# GIF it!
zscale <- 10
elmat %>% 
  sphere_shade(texture = "imhof1") %>% 
  add_water(watermap, color = "imhof3") %>%
  add_shadow(raymat, 0.4) %>%
  add_shadow(ambmat, 0.4) %>%
  save_3d_gif(elmat, file = "elmat2.gif", duration = 6,
            solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
            watercolor = "imhof3", wateralpha = 0.8, 
            waterlinecolor = "#ffffff", waterlinealpha = 0.5,
            waterdepth = waterdepths/zscale, 
            theta = thetas, phi = phi, zoom=zoom, fov=60)

