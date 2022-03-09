library(osmdata)
library(tidyverse)
library(sf)
library(leaflet)
#library(data.table)

setwd("D:/gits/church_cover/")

all_churches = st_read('data/church_mos_temples_all.geojson')
plot(st_geometry(all_churches))

# generate buffer around   
buff = all_churches %>%
  st_transform(3857) %>%
  mutate(buff_geom = st_buffer(geometry, 100))

# make buff geometry main and drop old point geometry
st_geometry(buff) <- "buff_geom"
buff = buff %>% select(-geometry) 
buff = buff %>% st_transform(4326)

plot(st_geometry(buff)[1])
plot(st_geometry(all_churches)[1], add=T)

# check if this worked via Leaflet
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = buff[1,]
  ) %>% 
  addMarkers(
    data=all_churches[1,]
  )

x <- opq(bbox = buff$buff_geom[1411]) %>% 
  add_osm_feature(key='building') %>% 
  osmdata_sf()

# unname so that it plots fine in leaflet
x <- unname_osmdata_sf(x)

plot(x$osm_polygons)


leaflet() %>%
  addTiles() %>%
  addPolygons(data = x$osm_polygons) %>%
  addMarkers(data = st_centroid(buff$buff_geom[1411])) %>% # for some reason there are inconsistencies in coordinates 
  addMarkers(data = x$osm_points)
  
poly = x$osm_multipolygons

poly %>% 
  filter(st_contains(., st_centroid(buff$buff_geom[1]), sparse=FALSE)[,1])


#####################################################
#---------------------------------------------------#
#####################################################

# for each buffer 
# 1. pull OSM building polygons 
# 2. select multipolygon which intesects with a centroid of a buffer 
# 3. save feature to list 

for (row in 1:nrow(buff)) {
  
  tryCatch({
    
    print('checking >>> working: ')
    print(row)
    
    geom = buff[row, 'buff_geom']
    geom_centr = st_centroid(geom)
    
    buildings <- opq(bbox = geom) %>% 
      add_osm_feature(key='building') %>% 
      osmdata_sf()
    
    cat("# of points: ", dim(buildings$osm_points, '\n'))
    cat("# of polygons: ", dim(buildings$osm_polygons, '\n'))
    cat("# of multipolygons: ", dim(buildings$osm_multipolygons, '\n'))
    
    # ------------------------------------
    # things to add in the future 
    # 1. the if-else statement below 
    # 2. check if the st_contains is empty, use nearest to locate the nearest poly
    
    if (!is.null(x$osm_multipolygons)) {
      cat('multi')
      buildings_poly = buildings$osm_multipolygons
    } else {
      buildings_poly = buildings$osm_polygons
    }
    
    subset_poly = buildings_poly %>% 
      filter(st_contains(., geom_centr, sparse=FALSE)[,1])
    
    subset_poly %>% 
      select(osm_id, building, denomination) %>% 
      st_write('data/subset_poly.geojson', append=T)
    
    # sleep for n seconds
    Sys.sleep(1)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}


# 
poly_file = st_read('data/subset_poly.geojson')
print(dim(poly_file))

