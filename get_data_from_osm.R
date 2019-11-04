#getting data and clean in with grass

#packages
library(sf)
library(tidyverse)
library(CARTElette)
library(osmdata)
library(rgrass7)


#get the communes of Albret
albret <- loadMap(nivsupra="EPCI") %>% filter(EPCI=="200068948")

#bbox in WGS84 for extracting data from OSM
albret <- st_transform(albret, 4326)
bb <- st_bbox(albret)

#get the road network from OSM
albret_roads <- opq(bbox =  bb) %>% 
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf() %>% 
  osm_poly2line()

#Keep only the lines
albret_roads <- albret_roads$osm_lines %>% 
  select(highway)


#clip the road networks with territory boundaries
albret_roads <- st_intersection(albret_roads, albret) %>% 
  st_cast("LINESTRING")
albret_roads <- albret_roads[,-c(2:3)]

#plot the result of the intersection
ggplot()+
  geom_sf(data=albret)+
  geom_sf(data=albret_roads)



# Topology cleaning with GRASS GIS -------------

#add the data to a GRASS database
writeVECT(SDF = albret_roads, 
          vname = 'albret_roads', 
          v.in.ogr_flags = 'overwrite')

#clean topologies
execGRASS("g.proj", flags = c("c", "quiet"))
execGRASS(cmd = 'v.clean', 
          input = 'albret_roads', 
          output = 'albret_cleaned',        
          tool = 'break', 
          flags = c('overwrite', 'c'))