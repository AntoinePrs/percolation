
#packages
library(sf)
library(tidyverse)
library(CARTElette)
library(osmdata)
library(rgrass7)
library(tidygraph)
library(igraph)
library(units)
library(ggraph)

#functions
source("functions/sf_to_tidygraph.R")
source("functions/percolation_function.R")

#Data import ------------

#get the communes of Albret
albret <- loadMap(nivsupra="EPCI") %>% filter(EPCI=="200068948")


#import clean data to R
use_sf()
albret_roads <- readVECT('albret_cleaned') %>%
  rename(geometry = geom) %>%
  select(-cat)

# Transform the sf object in a network object -
g <- sf_to_tidygraph(albret_roads)



# Percolation analysis ----------
#compute network distance
g <- g %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

#keep only the biggest component of the network 
bc <- which(clusters(g)$csize==max(clusters(g)$csize))

g <- g %>% 
  activate(nodes) %>% 
  filter(nodeID %in% which(clusters(g)$membership==bc))

#size of the biggest cluster given distance threshold 
sequence <- seq(round(max(E(g)$length)/100, 1)*100,
                round(min(E(g)$length)/100, 1)*100,
                -10)

bc_size_df <- lapply(sequence, function(x) bc_size(g,x)) %>% 
  bind_rows()


#compute the drops in transition
bc_size_df$tr <- c(bc_size_df$number[1],
                   bc_size_df$number[1:nrow(bc_size_df)-1])-bc_size_df$number

#select the 10 biggest drops
phase <- bc_size_df[order(bc_size_df$tr, decreasing = T),]
phase <- phase[1:10,]
phase <- phase[order(phase$dist, decreasing = T),]

#plot the evolution of the largest cluster size
ggplot(bc_size_df[bc_size_df$dist<1000,], 
       aes(x=dist, 
           y=number/max(number))) +
  geom_line()+
  geom_point(size=1, pch=21, fill="white")+
  geom_text(data=phase, 
            aes(x=dist, y=number/max(number),label=dist), 
            color="red", size=2)+
  theme_bw()


#
thresholds <- c(230, 350, 430, 520, 1000)

#extract phase graphs and store them in a list
g_list <- lapply(thresholds, function(x) graph_phase(g, set_units(x, "m")))

#keep only the clusters with more than 100 nodes
g_list <- lapply(1:5, function(x) filter_gph(gph = g_list[[x]],
                                             min_size = 200))


#
see <- lapply(1:4, function(x) link_clusters(x)) %>% bind_rows()
graph_test <- as_tbl_graph(see)

#get the size of each cluster of nodes
cl_size <- lapply(1:5, function(x) g_list[[x]] %>% 
         activate(nodes) %>% 
         as_tibble() %>% 
         group_by(cluster) %>% 
         summarize(n=n()) %>% 
         mutate(level=thresholds[x])) %>% bind_rows()

#give them a name for the join with the graph
cl_size$name <- paste("g", cl_size$level, cl_size$cluster, sep="_")

#join
graph_nd_size <- graph_test %>% 
  activate(nodes) %>% 
  left_join(lala, by = c("name"="name"))

#plot the nested hierarchy of clusters
graph_nd_size %>% 
  ggraph(layout = 'tree') + 
  geom_edge_link() + 
  geom_node_point(aes(size=n), pch=21, fill="steelblue") + 
  scale_color_continuous(guide = 'legend')+
  theme_graph()


#create a list of concave hull for mapping
poly_list <- do.call(rbind,
                     lapply(1:5, 
                            function(x) nodes_to_poly(g_list[[x]], 50)))

#order the factor
poly_list$phase <- factor(poly_list$phase, levels = sort(unique(poly_list$phase), decreasing = T))


#plot the polygons for different levels of percolation 
ggplot()+
  geom_sf(data=albret)+
  geom_sf(data=poly_list, 
          aes(fill=as.factor(cluster)),
          show.legend = F)+
  facet_wrap(~phase)