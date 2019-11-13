#Average count of trucks by road - Uruguay 2004/2017

library(tidyverse)
library(sf)
library(foreign)
library(cowplot)
library(extrafont)

#Download files from: https://geoportal.mtop.gub.uy/geoservicios 

#Modif dbf files

rutas04_dbf <- read.dbf("tpda_tramos_2004.dbf") %>%
  mutate(camion_c= ifelse(cam_total<=200, "0 a 200",
                              ifelse(cam_total<=500, "201 a 500",
                                     ifelse(cam_total<=1000, "501 a 1000",
                                            ifelse(cam_total<=1500, "1001 a 1500",
                                                   "Mas de 1500")))))

rutas17_dbf <- read.dbf("tpda_tramos_2017.dbf") %>%
  mutate(camion_c= ifelse(cam_total<=200, "0 a 200",
                          ifelse(cam_total<=500, "201 a 500",
                                 ifelse(cam_total<=1000, "501 a 1000",
                                        ifelse(cam_total<=1500, "1001 a 1500",
                                               "Mas de 1500")))))


write.dbf(rutas04_dbf, "tpda_tramos_2004.dbf")
write.dbf(rutas17_dbf, "tpda_tramos_2017.dbf")

#Load shapefiles

mapa04 <- st_read("tpda_tramos_2004.shp")
mapa17 <- st_read("tpda_tramos_2017.shp")

mapa04$camion_c = factor(mapa04$camion_c,levels(mapa04$camion_c)[c(1,3,4,2,5)])
mapa17$camion_c = factor(mapa17$camion_c,levels(mapa17$camion_c)[c(1,3,4,2,5)])

#Map function

mapa_rutas <- function (mapa){
  ggplot(mapa) +
  geom_sf(data=mapa) +
  geom_sf(aes(col=camion_c), size=1.5) +
  theme_void() +
  scale_color_manual(name="Tránsito promedio diario", values=c(
    "0 a 200"="#f7f308",
    "201 a 500"= "#e3b200",
    "501 a 1000"="#cf9134",
    "1001 a 1500"="#6b5833",
    "Mas de 1500"="#332712")) +
    theme(legend.title=element_text(face="bold", size=12),
          text=element_text(family="Segoe UI"),
          legend.text=element_text(size=12),
          plot.title=element_text(hjust=.5, face="bold"),
          plot.margin=unit(c(0, 0, 0, 0), "cm"))
}

#Arranging the dataviz

title_gg <- ggplot() + 
  labs(title = "Tránsito diario de camiones en rutas uruguayas", subtitle = "Conteo promedio") + 
  theme(text=element_text(family="Segoe UI", face="bold"),
        plot.title=element_text(size=16),
        plot.subtitle=element_text(size=14, face="italic", hjust=.5))

m1 <- mapa_rutas(mapa04) +
  theme(legend.position="") +
  labs(title="2004") 
m2 <- mapa_rutas(mapa17) +
  labs(title="2017") +
  theme(legend.position="right")

both <- plot_grid(m1, m2,
                  rel_widths = c(.5, 1), align="hv", hjust=.5)

g <- add_sub(both, "Fuente de datos: MTOP | Visualización: @danidlsa",
        hjust=.5, vjust=1, fontfamily="Segoe UI", fontface="italic")

g2 <- ggdraw(g)

completo <- plot_grid(title_gg, g2, ncol = 1, rel_heights = c(0.15, 1))

ggsave("mapa.png", completo , height=14, width=30, units="cm")


