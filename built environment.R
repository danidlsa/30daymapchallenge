library(foreign)
library(tidyverse)

#Census data and shapefiles available for download at http://www.ine.gub.uy 

viv <- read.dbf("Viviendas.dbf")
vtable::vtable(viv)

viv <-  viv %>% mutate(techos=ifelse(VIVDV02>1, 1, 0)) %>%
  mutate(pisos=ifelse(VIVDV03>1, 1, 0)) %>%
  mutate(agua=ifelse(VIVDV05>1, 1, 0)) %>% 
  mutate(count=1) %>%
  mutate(CODSEG=paste0("1",SECC,SEGM))

segm_techos <- aggregate(techos~CODSEG, viv, sum) 
segm_pisos <- aggregate(pisos~CODSEG, viv, sum) 
segm_agua <- aggregate(agua~CODSEG, viv, sum) 
segm_total <- aggregate(count~CODSEG, viv, sum)

segm_total <- segm_total %>% left_join(segm_techos, by="CODSEG") %>%
  left_join(segm_pisos, by="CODSEG") %>%
  left_join(segm_agua, by="CODSEG")

segm_total <- segm_total %>% 
  mutate(por_techos=techos/count*100) %>%
  mutate(por_pisos=pisos/count*100) %>%
  mutate(por_agua=agua/count*100)

segm_total$CODSEG <- as.numeric(segm_total$CODSEG)

dbf.segm <- read.dbf("ine_seg_11.dbf") %>%
  left_join(segm_total, by="CODSEG")

write.dbf(dbf.segm, "ine_seg_11.dbf")

library(sf)
#library(maps)
mapa <- st_read("ine_seg_11.shp") %>% filter(DEPTO==1)

library(viridis)
library(extrafont)
g1 <- ggplot() + geom_sf(data=mapa, aes(fill=por_techos)) +
  theme_void() +
  scale_fill_viridis(name="% roofs made of tin \n(and other light materials)") +
  theme(text=element_text(family="Segoe UI"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=14, face="italic", hjust=.5),
        plot.caption=element_text(size=12, face="italic"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        legend.position="bottom") +
  labs(title="Precarious constructions in Montevideo",
       subtitle="Roofs",
       caption="Data Source: 2011 Census | Visualization: @danidlsa")

g2 <- ggplot() + geom_sf(data=mapa, aes(fill=por_pisos)) +
  theme_void() +
  scale_fill_viridis(name="% households with unbuilt floors") +
  theme(text=element_text(family="Segoe UI"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=14, face="italic", hjust=.5),
        plot.caption=element_text(size=12, face="italic"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        legend.position="bottom") +
  labs(title="Precarious constructions in Montevideo",
       subtitle="Floors",
       caption="Data Source: 2011 Census | Visualization: @danidlsa")
  

g3 <- ggplot() + geom_sf(data=mapa, aes(fill=por_agua)) +
  theme_void() +
  scale_fill_viridis(name="% households not connected to \nwater supply system") +
  theme(text=element_text(family="Segoe UI"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=14, face="italic", hjust=.5),
        plot.caption=element_text(size=12, face="italic"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        legend.position="bottom") +
  labs(title="Precarious constructions in Montevideo",
       subtitle="Running water",
       caption="Data Source: 2011 Census | Visualization: @danidlsa")


ggsave("roofs.png", g1, height=14, width=18, units="cm")
ggsave("floors.png", g2, height=14, width=18, units="cm")
ggsave("water.png", g3, height=14, width=18, units="cm")
