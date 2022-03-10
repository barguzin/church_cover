library(sf)
library(tidyverse)
library(leaflet)
library(units)

uvao_centr = st_read('data/uvao_centr.geojson')
uvao_jj = st_read('data/uvao_churches.geojson')
uvao = st_read('data/uvao_adm.geojson')

# 25-30k people / church 
# within 1km 
# prepare buffers around churches 
uvao_jj_buff = uvao_jj %>%
  st_transform(3857) %>%
  mutate(buff_geom = st_buffer(geometry, 1000))

# make buff geometry main and drop old point geometry
st_geometry(uvao_jj_buff) <- "buff_geom"
uvao_jj_buff = uvao_jj_buff %>% select(-geometry) 
uvao_jj_buff = uvao_jj_buff %>% st_transform(4326)
st_geometry(uvao_jj_buff) = "geometry"

plot(st_geometry(uvao_jj_buff))
  
leaflet() %>%
  addTiles() %>% 
  addPolygons(data=uvao_jj_buff)

# count covered demand for 
# 1. all churches - This is a total covered demand 
# 2. churches in prog 200 
# 3. churches not in prog 200 

###########################################
#-----------------------------------------#
#             calculate stats             #
#-----------------------------------------#
###########################################
# total demand 
sum(uvao_centr$flat_cnt, na.rm=T) # 420,398

# total covered demand: 173,534 (41.28%) - including the 200 prog
uvao_centr %>% 
  st_filter(uvao_jj_buff) %>% 
  st_drop_geometry() %>%
  select(flat_cnt) %>%
  summarise_all(sum, na.rm=T)

# total covered by temples not included in program 200: 104,629 (24.89%)
uvao_centr %>% 
  st_filter(uvao_jj_buff %>% filter(prog_200==0)) %>% 
  st_drop_geometry() %>%
  select(flat_cnt) %>%
  summarise_all(sum, na.rm=T)


###########################################
#-----------------------------------------#
#             sampling grid               #
#-----------------------------------------#
###########################################
# generate sampling grid 
grid = st_make_grid(uvao, what='centers', n=50, square=F) %>%
  st_as_sf() %>%
  st_filter(uvao)

st_write(grid, 'data/grid_all.geojson', delete_dsn = T)

# cut out covered demand from 
inter_for_sites = st_difference(uvao, st_union(uvao_jj_buff %>% filter(prog_200!=1)))

# get rid of dangling geometries
split = st_cast(inter_for_sites,"POLYGON")
uvao_for_sites = split[4,]

grid_for_sites = st_filter(grid, uvao_for_sites)
grid_for_sites = grid_for_sites %>% rowid_to_column("geo_id")

st_write(grid_for_sites, 'data/grid_for_sites.geojson', delete_dsn=T)


leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    data=grid_for_sites, 
    radius = 1, 
    stroke=FALSE, 
    fillOpacity = .5) %>%
  addMarkers(data=uvao_jj %>% filter(prog_200==1))



###########################################
#-----------------------------------------#
#           distance calculations         #
#-----------------------------------------#
###########################################

# distance between centr and grid 
# yields matrix where rows (i) are demand points, and columns (j) are potential sites 
dist = st_distance(grid_for_sites, uvao_centr) 
# the matrix is too large, make it smaller by roudning to ones before writing 
y = function(x) round(x,0)
dist[] <- vapply(dist, y, numeric(1))

write.table(dist, file='data/dist.txt', row.names=FALSE, col.names=FALSE)

# also save as pairwise distance 
dist = drop_units(dist)

# also save binary table with 1km buffer 
binar = ifelse(dist<=1000, 1, 0)
write.table(binar, file='data/binar.txt', row.names=FALSE, col.names=FALSE)



###########################################
#-----------------------------------------#
#           prep data for Python          #
#-----------------------------------------#
###########################################
# demand points
uvao_centr %>% 
  st_drop_geometry() %>% 
  rowid_to_column("geo_id") %>%
  mutate(geo_id = geo_id - 1) %>%
  select(geo_id, flat_cnt, -index) %>%
  write_csv('data/demand.csv')

# facilities points 
grid_for_sites %>% 
  st_drop_geometry() %>% 
  select(-geo_id) %>%
  rowid_to_column("geo_id") %>% 
  mutate(geo_id = geo_id - 1) %>%
  write_csv('data/facility.csv')
