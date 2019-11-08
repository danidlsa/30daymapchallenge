library(tidyverse)
install.packages("tradestatistics")
library(tradestatistics)


ots_countries

View(ots_tables)
View(ots_communities)
View(ots_products)

d <- ots_create_tidy_data(years = 2018, include_communities = TRUE)

head(d)

veggies <- d %>% filter(community_name=="Vegetable Products")
exports_by_reporter <- aggregate(export_value_usd~reporter_iso, veggies, sum) 

rm(d)

library(maps)
library(ggmap)
library(extrafont)
library(countrycode)
library(colorRamps)

world <- map_data("world") %>% 
  mutate(reporter_iso=countrycode(region, "country.name", "iso3c"))

exports_by_reporter <- exports_by_reporter %>%
  mutate(reporter_iso=toupper(reporter_iso))

world <- world %>% left_join(exports_by_reporter, by="reporter_iso")
ggplot() + 
  geom_polygon(data=world, aes(long, lat, group=group, fill=export_value_usd)) +
  theme_void() +
  scale_fill_gradient(name="Exports value (USD)", low="lightgreen", high="darkgreen",
                        breaks=c(1e+10,6e+10)) +
  labs(title="Vegetable Products Exports in 2018",
       caption="Data Source: Open Trade Statistics | Visualization: @danidlsa") +
  theme(text=element_text(family="Segoe UI", color="white"),
        plot.title=element_text(size=16, face="bold", hjust=.5),
        plot.subtitle=element_text(size=14, face="italic"),
        plot.caption=element_text(size=12, face="italic"),
        plot.background = element_rect(fill="#333332"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        legend.position="bottom",
        plot.margin = unit(c(0.5, 0.5, .5, .5), "lines")) 

ggsave("greens.png", height=15,width=25,units="cm")
