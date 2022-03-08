library(osmdata)
library(sf)
library(tidyverse)
library(ggplot2)
library(leaflet)


data = st_read("C:/Users/barguzin/Documents/Github/church_data_prep/prepped_data.shp")

plot(st_geometry(data))

no_cap <- data %>%
  filter(capacity == 473.292) %>% # this was simply average of the capacity
  st_transform(3857)

ggplot(data) + 
  geom_sf(aes(color = capacity)) + 
  geom_sf(data = no_cap, color='red', size=1) 


# generate buffer around   
buff = no_cap %>%
  #st_drop_geometry() %>%
  select(church_id, lng, lat) %>%
  mutate(buff_geom = st_buffer(geometry, 100))

# make buff geometry main and drop old point geometry
st_geometry(buff) <- "buff_geom"
buff = buff %>% select(-geometry) 
buff = buff %>% st_transform(4326)

# check if this worked via Leaflet
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = head(buff)
  )


