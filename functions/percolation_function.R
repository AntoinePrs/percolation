#functions for percolation analysis

#extract the graph that corresponds to a distance threshold
graph_phase <- function(graph, phase){
  new_graph <- graph %>% 
    activate(edges) %>%
    filter(length<set_units(phase, "m"))
  
  new_graph <- new_graph %>% 
    activate(nodes) %>% 
    mutate(cluster=clusters(new_graph)$membership,
           phase=phase)
  
  return(new_graph)
}

#compute the size of the biggest cluster with a certain 
#distance threshold
bc_size <- function(graph, distance){
  new_graph <- graph %>%
    activate(edges) %>%
    filter(length<distance)
  
  return(tibble(dist=distance,
                number=max(clusters(new_graph)$csize)))
}

#filter the graph by keeping the nodes above a threshold
filter_gph <- function(gph, min_size){
  big_clusters <- table(V(gph)$cluster) %>% as.data.frame()
  big_clusters <- big_clusters[big_clusters$Freq>min_size,1]
  
  return(gph %>% activate(nodes) %>% filter(cluster %in% big_clusters))
}

#draw a concave hull around nodes
concav <- function(df, x){
  cl <- df[df$cluster==x,]
  cl <- concaveman::concaveman(cl, 1)
  cl$cluster <- x
  cl$phase <- as.numeric(unique(df$phase))
  return(cl)
}

#perform a concave hull on the nodes of different clusters
nodes_to_poly <- function(gph, buffer){
  new_graph <- gph %>% 
    activate(nodes) %>% 
    as_tibble() %>% 
    st_as_sf()
  
  new_graph$lon <- st_coordinates(new_graph)[,1]
  new_graph$lat <- st_coordinates(new_graph)[,2]
  
  #remaining clusters
  cls <- unique(new_graph$cluster)
  
  #get all concav hulls
  ch <- do.call(rbind,lapply(cls, function(x) concav(df = new_graph, x)))
  
  #add a buffer around
  ch <- st_transform(ch, st_crs(2154))
  ch <- st_buffer(ch, units::set_units(buffer, "m"))
  ch <- st_transform(ch, st_crs(4326))
  
  ch
}

#get concave hulls of clusters given a threshold
phase_poly <- function(graph, distance){
  gph <- graph_phase(graph, distance)
  ch <- nodes_to_poly(graph = gph, min_size = 100, buffer = 50)
  ch$phase <- x
  ch
}

#link clusters of two level
link_clusters <- function(n){
  cl <- g_list[[n]] %>% 
    activate(nodes) %>% 
    as_tibble() %>% .$cluster %>% unique()
  
  link_cl <- function(g1, g2, x){
    clx <- g1 %>% 
      activate(nodes) %>% 
      as_tibble() %>% 
      filter(cluster==x) %>% 
      .$nodeID
    
    tibble(V1=paste("g", thresholds[n+1], g2 %>% 
                      activate(nodes) %>% 
                      as_tibble() %>% 
                      filter(nodeID %in% clx) %>% .$cluster %>% unique(), sep = "_"),
           V2=paste("g", thresholds[n], x, sep="_"))
  }
  
  lapply(cl, function(x) link_cl(g_list[[n]], g_list[[n+1]], x)) %>% bind_rows()
}
####################################


#get the cluster of each nodes at each iteration of the
#percolation algorithm
#extract_clusters <- function(x){
#  gph <- graph_phase(phase = x)
#  gph_df <- gph %>% 
#    activate(nodes) %>% 
#    as_tibble() %>% 
#    select(nodeID, cluster)
#  gph_df$distance <- x
#  gph_df
#}
