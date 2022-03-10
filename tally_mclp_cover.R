library(sf) 
library(tidyverse)

setwd('C:/Users/barguzin/Documents/GitHub/church_cover/')

ff = st_read('data/sited_locs.geojson')
uvao_centr = st_read('data/uvao_centr.geojson')
uv_c = st_read('data/uvao_churches.geojson')

# prepare buffers around churches 
uvao_jj_buff = uv_c %>%
  st_transform(3857) %>%
  filter(prog_200==0) %>%
  mutate(buff_geom = st_buffer(geometry, 1000))

# make buff geometry main and drop old point geometry
st_geometry(uvao_jj_buff) <- "buff_geom"
uvao_jj_buff = uvao_jj_buff %>% select(-geometry) 
uvao_jj_buff = uvao_jj_buff %>% st_transform(4326) %>% st_as_sf()
uvao_jj_buff = uvao_jj_buff %>% select(geo_id)


# prepare buffers around churches 
ff_buff = ff %>%
  st_transform(3857) %>%
  mutate(buff_geom = st_buffer(geometry, 1000))

# make buff geometry main and drop old point geometry
st_geometry(ff_buff) <- "buff_geom"
ff_buff = ff_buff %>% select(-geometry) 
ff_buff = ff_buff %>% st_transform(4326) %>% st_as_sf()
ff_buff = ff_buff %>% select(geo_id)

rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}

uvao_jj_buff = rename_geometry(uvao_jj_buff, "geometry")
ff_buff = rename_geometry(ff_buff, "geometry")

joined = rbind(ff_buff, uvao_jj_buff)

# total demand: 420,398
# total covered demand: 173,534 (41.28%) - including the 200 prog
# total covered demand: 199,911 (41.28%) - including the 200 prog
uvao_centr %>% 
  st_filter(joined) %>% 
  st_drop_geometry() %>%
  select(flat_cnt) %>%
  summarise_all(sum, na.rm=T) 
