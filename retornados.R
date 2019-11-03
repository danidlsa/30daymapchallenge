#Mapa inmigrantes y retornados

library(tidyverse)
library(foreign)

base <- read.spss("HyP_2018_TERCEROS.sav")

base <- as.data.frame(base)

table(base$e37)
base <- base %>% mutate(oriental=ifelse(e37=="En otro país", 0, 1)) %>%
  mutate(uruguayo_retornado=ifelse(oriental==1 & e38_1<10 & e39=="En otro país", 1, 0)) %>%
  mutate(inmigrante=ifelse(oriental==0 & e38_1<10 & e39=="En otro país", 1, 0)) %>%
  mutate(e235_2=ifelse(e235_2==0, NA, e235_2))


tabla_paises_emisores <- aggregate(pesoano~e235_2+uruguayo_retornado+inmigrante, base, sum) %>%
  mutate(uruguayo_retornado=uruguayo_retornado*pesoano) %>% 
  mutate(inmigrante=inmigrante*pesoano)


#Códigos países

codes <- readr::read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/7255fdfbf292c592b75cf5f7a19c16ea59735f74/countries_codes_and_coordinates.csv")

tabla_paises_emisores <- tabla_paises_emisores %>%
  left_join(codes %>% mutate(e235_2=`Numeric code`, by="e235_2"))

paises_emisores <- subset(tabla_paises_emisores, inmigrante>0 | uruguayo_retornado>0)

colnames(paises_emisores)[9:10] <- c("lat", "long")

uru <- subset(codes, Country=="Uruguay")

paises_emisores <- paises_emisores %>% 
  mutate(long_uru=uru$`Longitude (average)`) %>%
  mutate(lat_uru=uru$`Latitude (average)`)

inmigrantes <- subset(paises_emisores, inmigrante>0 & Country!="Venezuela, Bolivarian Republic of")

uruguayos_retornados <- subset(paises_emisores, uruguayo_retornado>0 & Country!="Venezuela, Bolivarian Republic of")

#Mapa

library(maps)
library(ggmap)
library(ggrepel)
library(extrafont)

world <- map_data("world")

mapa_retornados <- ggplot() + 
  geom_polygon(data=world, aes(long, lat, group=group), fill="grey", col="black") +
  geom_segment(data=uruguayos_retornados, aes(y=lat, x=long, yend=lat_uru, xend=long_uru,
                                     size=uruguayo_retornado), 
               col="yellow",
               alpha=.3) +
  theme_void() +
  geom_label_repel(data=uruguayos_retornados, aes(x=long, y=lat, 
                                            label=paste0(`Alpha-2 code`, "\n", uruguayo_retornado)), 
             position="identity", show.legend = F, size=2, alpha=.8) +
  labs(title="Uruguayos retornados a Uruguay entre 2008 y 2018",
       subtitle="Estimación a partir de Encuesta Continua de Hogares",
       caption="Fuente: Elaboración propia en base a ECH-INE 2018 | @danidlsa") +
  theme(text=element_text(family="Segoe UI", color="white"),
        legend.position = "",
        plot.title=element_text(size=16, face="bold"),
        plot.subtitle=element_text(size=14, face="italic"),
        plot.caption=element_text(size=12),
        plot.background = element_rect(fill="#333332")) 

mapa_retornados


mapa_inmigrantes <- ggplot() + 
  geom_polygon(data=world, aes(long, lat, group=group), fill="grey", col="black") +
  geom_segment(data=inmigrantes, aes(y=lat, x=long, yend=lat_uru, xend=long_uru,
                                              size=inmigrante), 
               col="lightblue",
               alpha=.5) +
  theme_void() +
  geom_label_repel(data=inmigrantes, aes(x=long, y=lat, 
                                                  label=paste0(`Alpha-2 code`, "\n", inmigrante)), 
                   position="identity", show.legend = F, size=2, alpha=.8) +
  labs(title="Inmigrantes llegados a Uruguay entre 2008 y 2018",
       subtitle="Estimación a partir de Encuesta Continua de Hogares",
       caption="Fuente: Elaboración propia en base a ECH-INE 2018 | @danidlsa") +
  theme(text=element_text(family="Segoe UI", color="white"),
        legend.position = "",
        plot.title=element_text(size=16, face="bold"),
        plot.subtitle=element_text(size=14, face="italic"),
        plot.caption=element_text(size=12),
        plot.background = element_rect(fill="#333332")) 

mapa_inmigrantes

ggsave("uruguayos retornados.png", mapa_retornados, height=12, width=20, units="cm")

ggsave("inmigrantes.png", mapa_inmigrantes, height=12, width=20, units="cm")
