library(sf) 
library(tidyverse) 
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(leaflet)

setwd("D:/gits/church_cover/")

okrug = st_read('C:/Users/noibar/YandexDisk/mosmonitor/geo_data/RU-MOW/data/boundary-polygon.shp')
plot(st_geometry(okrug))

ao = okrug %>% filter(ADMIN_LVL==5)

# read prepped 
prepped = st_read('D:/gits/church_data_prep/prepped_data.shp')

# read all temples (from temples.ru)
geo = st_read("data/church_mos_temples_all.geojson")
print(geo$prog_200 %>% sum()) # make sure all of the 200 program is there! 

jj <- left_join(geo, st_drop_geometry(prepped), by=c("temples_id"="church_id"), keep=TRUE)


#################################################
#-----------------------------------------------#
#                     EDA                       #
#-----------------------------------------------#
#################################################
# plot building types versus active (functioning)
ggplot(jj) + 
  geom_bar(aes(x=type_build, fill=status)) + 
  #scale_fill_brewer(palette = "Purples") + 
  theme_bw() + 
  labs(title="Building types versus status", x="Type of Building", y="Count")
ggsave('imgs/build_type.png', dpi=150)

# plot cumulative sum of churches built in Moscow in 1990-2020
jj %>% 
  st_drop_geometry() %>%
  filter(as.numeric(founded2) > 1990) %>%
  count(founded2) %>% 
  mutate(csum = cumsum(n)) %>% 
  mutate(founded2 = as.numeric(founded2)) %>% 
  ggplot(aes(x=founded2, y=csum)) + 
    #geom_line(group=1, color='tomato', size=2) +
    geom_col(group=1, fill='seagreen4') + 
    theme_bw() + 
    labs(x='year', y='total churches built', title='Number of churches constructed')
ggsave('imgs/cumsum.png', dpi=150)
    

# plot churches in UVAO 
uvao = ao%>%filter(OSM_ID==-1278703)
# split geometry 
split = st_cast(uvao,"POLYGON")
uvao = split[1,]
uvao_jj = st_filter(jj, uvao)
# convert prog_200 to binary 
uvao_jj$prog_200 = as_factor(uvao_jj$prog_200)
# only keep active 
uvao_jj = uvao_jj %>% filter(status!="non_preserve")
# only keep where name does not have 'дерев' - those are temporary wooden buildings 

ggplot(uvao_jj) + geom_bar(aes(x=type_build))

# this is good for 3 or more colors 
#myColors <- brewer.pal(2, "Spectral")
#names(myColors) <- levels(uvao_jj$prog_200)

 
# plot geographic map with locations in UVAO 
ggplot() + 
  geom_sf(data=uvao) + 
  geom_sf(data=uvao_jj, aes(color=prog_200), size=2) + 
  #scale_colour_manual(name = "Species Names", values = myColors)
  scale_color_manual(labels = c('not in program', 'in program'), values = c("0" = "blue","1"="red")) + 
  labs(color='Program 200') + theme_bw()

# same in leaflet 
pal <- colorFactor(c("navy", "red"), domain = c(0, 1))

leaflet(uvao_jj) %>%
  addTiles() %>% 
  addCircleMarkers(
    label = ~church_id,
    radius = 5, 
    color=~pal(prog_200), 
    stroke=FALSE, 
    fillOpacity = .5
  )