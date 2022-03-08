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

x <- opq(bbox = buff$buff_geom[1]) %>% 
  add_osm_feature(key='building') %>% 
  osmdata_sf()

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = x$osm_polygons
  ) %>% 
  addPolygons(data = buff$buff_geom[1]) # for some reason there are inconsistencies in coordinates 


###############################################################
#-------------------------------------------------------------#
###############################################################
# read csv with temples 200 and geocode 
church_csv = read.csv('C:/Users/barguzin/Documents/Github/church_data_prep/church_test.csv')

church_csv = st_as_sf(church_csv, coords = c("lng", "lat"), crs = 4326)

plot(church_csv)

st_write(church_csv, 'C:/Users/barguzin/Documents/Github/church_cover/data/church_200.geojson')


###############################################################
#-------------------------------------------------------------#
###############################################################
# creating bounding box for Lagos
moscow_bb <- getbb("Moscow, Russia")

# retrieving data of streets in Lagos
moscow_churches <- moscow_bb %>%
  opq() %>%
  add_osm_feature("building", c("church")) %>%
  osmdata_sf()

church_points = moscow_churches$osm_points
church_poly = moscow_churches$osm_polygons

leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = moscow_churches$osm_polygons
  ) 

iconv(church_poly$name, to="UTF-8") # changes encoding for cyrillic 

church_poly %>% 
  select(osm_id) %>% 
  st_write('C:/Users/barguzin/Documents/Github/church_cover/data/church_osm_poly.geojson')

church_points %>% 
  st_write('C:/Users/barguzin/Downloads/church_points.geojson')

# non of the two files above are complete 
# loading another file 
churches_complete <- st_read('C:/Users/barguzin/Downloads/temples_of_russia41.kml')

# generate unqiue id 
churches_complete <- churches_complete %>% 
  rowid_to_column("geo_id") %>% 
  select(-Description) %>% 
  st_write('C:/Users/barguzin/Documents/Github/church_cover/data/church_mos_temples_all.geojson')

