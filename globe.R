
#install.packages("mapproj")
library(mapproj)
library(maps)


#All projections available in the mapproject package
map("world",proj='aitoff')

map("world",proj='albers', par=c(30,40))

map("world",proj='azequalarea')

map("world",proj='azequidist')

map("world",proj='bicentric', par=0)

map("world",proj='bonne', par=45)

map("world",proj='conic', par=45)

map("world",proj='cylequalarea', par=45)

map("world",proj='cylindrical')

map("world",proj='eisenlohr')

map("world",proj='elliptic', par=45)

map("world",proj='fisheye', par=45)

map("world",proj='gall', par=30)

map("world",proj='gilbert')

map("world",proj='guyou')

map("world",proj='harrison', par=c(40, 30))

map("world",proj='hex')

map("world",proj='homing', par=45)

map("world",proj='lagrange')

map("world",proj='lambert', par=c(40, 30))

map("world",proj='homing', par=45)

map("world",proj='laue')

map("world",proj='lune', par=c(40, 30))

map("world",proj='mercator')

map("world",proj='mollweide')

map("world",proj='newyorker', par=20)

map("world",proj='orthographic')

map("world",proj='perspective', par=30)

map("world",proj='polyconic')

map("world",proj='rectangular', par=20)

map("world",proj='simpleconic', par=c(20, 30))

map("world",proj='sinusoidal')

map("world",proj='tetra')

map("world",proj='trapezoidal', par=c(20, 40))

##A giant sinkhole opens up in Greenland!

map("world",proj='newyorker', par=10)

sinkhole <- function(m) {
  parameter=10
  for (i in 1:9) {
    nombre <- paste0("mapa", i, ".png")
    png(nombre)
    map(m, proj="newyorker", par=parameter)
    dev.off()
    parameter=parameter+10
  }
}

sinkhole("world")

library(gifski)

# Convert png files to gif
png("mapa%01d.png")
par(ask = FALSE)
png_files <- sprintf("mapa%01d.png", 1:9)
gif_file <- tempfile(fileext = ".gif")
gifski(png_files, gif_file)
utils::browseURL(gif_file)
