library(tidyverse)
library(foreign)

#Both census data and shapefiles can be downloaded from http://ine.gub.uy/
censo <- as.data.frame(read.spss("Poblacion.sav"))

censo$count <- 1

vtable::vtable(censo)

table(censo$LOCNOMBRE)

rurales <- subset(censo, grepl("Rural", LOCNOMBRE)==TRUE)

censo$rural <- ifelse(grepl("Rural", censo$LOCNOMBRE)==T, 1, 0)
agreg <- aggregate(count~DPTO+PERPH02, rurales, sum)

agreg_porcentaje <- aggregate(count~DPTO+rural, censo, sum)
tt <- aggregate(count~DPTO, agreg_porcentaje, sum)
agreg_porcentaje <- merge(agreg_porcentaje, tt, by="DPTO", all.x=TRUE) %>%
  filter(rural==1) %>%
  mutate(porcentaje=count.x/count.y*100)

mujeres_rurales <- agreg %>% filter(PERPH02=="Mujer")

agreg_porcentaje <- agreg_porcentaje %>% left_join(mujeres_rurales, by="DPTO")
colnames(agreg_porcentaje)[7] <- "count.mujeres"

agreg_porcentaje <- agreg_porcentaje %>% mutate(porcentaje_mujeres=count.mujeres/count.x*100)

rm(censo)
rm(rurales)
rm(tt)

#Cargo dbf del shape

dbf.dptos <- read.dbf("ine_depto.dbf")

agreg_porcentaje <- agreg_porcentaje %>% 
  mutate(DPTO=as.character(DPTO)) %>% 
  mutate(NOMBRE= case_when (DPTO == "PAYSANDÚ" ~ "PAYSANDU",
                            DPTO == "RÍO NEGRO" ~ "RIO NEGRO",
                            DPTO == "TACUAREMBÓ" ~ "TACUAREMBO",
                            TRUE ~ DPTO)) %>%
  select(-rural, -PERPH02, -DPTO)

dbf.dptos <- dbf.dptos %>% left_join(agreg_porcentaje, by="NOMBRE")

write.dbf(dbf.dptos, "ine_depto.dbf")                                                  

library(sf)

pob_rural <- st_read("ine_depto.shp")

library(extrafont)

ggplot() +
  geom_sf(data=pob_rural, aes(fill=porcentaje)) +
  theme_void() +
  scale_fill_gradient(name="Percentage of rural population", low="lightgreen", high="darkgreen",
                      breaks=c(0, 5, 10, 15)) +
  labs(title="Rural population in Uruguay, \nby department",
       subtitle = "2011 Census Data",
       caption="Visualization: @danidlsa") +
  theme(text=element_text(family="Segoe UI", color="white"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=14, face="italic", hjust=.5),
        plot.caption=element_text(size=12, face="italic"),
        plot.background = element_rect(fill="#333332"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        legend.position="bottom",
        plot.margin = unit(c(0.5, 0.5, .5, .5), "lines")
    )

ggsave("rural population.png", height=15, width=22, units="cm")


