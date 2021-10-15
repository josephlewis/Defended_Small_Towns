shuffle_towns <- function(network) { 
  
  small_town_nodes <- network %>%
    sfnetworks::activate("nodes") %>%
    sf::st_as_sf() %>%
    sf::st_drop_geometry()
  
  small_town_index_reshuffled <- sample(small_town_nodes[which(!is.na(small_town_nodes$Second_Century_Defence)),]$Second_Century_Defence)
  
  road_network <- network %>%
    sfnetworks::activate("nodes") %>%
    dplyr::mutate(Second_Century_Defence = replace(Second_Century_Defence, !is.na(Second_Century_Defence), small_town_index_reshuffled))
  
  return(road_network)
  
}

calculate_nodal_efficiency <- function(network, shuffle = TRUE) {
  
  efficiency <- brainGraph::efficiency(g = network, type = "nodal", use.parallel = FALSE)
  
  small_town_nodes <- network %>%
    sfnetworks::activate("nodes") %>%
    sf::st_as_sf() %>%
    sf::st_drop_geometry()
  
  town_efficiency <- mean(efficiency[which(small_town_nodes$Second_Century_Defence == "Yes")])
  
  df <- data.frame(metric = "Mean Nodal Efficiency", value = town_efficiency, shuffle = shuffle)
  
  return(df)
  
}

calculate_betweenness <- function(network, shuffle = TRUE) {
  
  betweenness <- igraph::edge_betweenness(graph = network, directed = FALSE)
  
  small_town_nodes <- network %>%
    sfnetworks::activate("nodes") %>%
    sf::st_as_sf() %>%
    filter(Second_Century_Defence == "Yes")
  
  network_edges <- network %>%
    activate("edges") %>%
    sf::st_as_sf()
  
  edges_with_towns <- sf::st_nearest_feature(small_town_nodes, network_edges)
  
  town_edge_betweenness <- mean(betweenness[edges_with_towns])
  
  df <- data.frame(metric = "Mean Edge Betweeness", value = town_edge_betweenness, shuffle = shuffle)
  
  return(df)
  
}

plot_network <- function(network, legend = TRUE, title = NA, small_towns = TRUE, point_size = 0.1, road_colour = "black") {
  
  road_network_sf <- network %>%
    sfnetworks::activate("edges") %>%
    st_as_sf()
  
  if (small_towns) { 
    
    nodes <- sf::st_as_sf(network) %>%
      dplyr::filter(!is.na(Second_Century_Defence))
    
    plot <-     
      tm_shape(outline) + 
      tm_polygons(col = "white", border.col = "white") +
      tm_shape(road_network_sf) + 
      tm_lines(col = road_colour) + 
      tm_shape(nodes[nodes$Second_Century_Defence == "No",]) +
      tm_dots(col = viridis::cividis(5)[3], size = point_size, title = "Second Century Defence", legend.show = FALSE, shape = 16) + 
      tm_shape(nodes[nodes$Second_Century_Defence == "Yes",]) +
      tm_dots(col = "black", size = point_size, title = "Second Century Defence", legend.show = FALSE, shape = 15)
    
  } else { 
    
    nodes <- sf::st_as_sf(network) %>%
      dplyr::filter(is.na(Second_Century_Defence))
    
    plot <-     
      tm_shape(outline) + 
      tm_polygons(col = "white", border.col = "white") +
      tm_shape(road_network_sf) + 
      tm_lines(col = road_colour) + 
      tm_shape(nodes) +
      tm_dots(col = "black", size = point_size, title = "Road Network Nodes", legend.show = FALSE, shape = 16)
    
  }
  
  if (legend) { 
    
    plot <- 
      plot + 
      tm_add_legend('symbol', border.col = "black",col = "black", size = 1, labels = c('Defended Small Town'), shape = 15) + 
      tm_add_legend('symbol', border.col = viridis::cividis(5)[3],col = viridis::cividis(5)[3], size = 1,labels = c('Non-Defended Small Town')) + 
      tm_add_legend('line', col = c(road_colour),lwd = c(1),labels = c('Roman Road')) + 
      tm_scale_bar()
    
  }
  
  plot + 
    tm_layout(title = title, bg.color = "#C8C8C8", legend.position = c("right","top"))
  
}

plot_nodal_network <- function(network) { 
  
  nodal_efficiency <- brainGraph::efficiency(g = network, type = "nodal")
  
  network <- network %>%
    activate("nodes") %>%
    mutate(nodal_efficiency = nodal_efficiency)
  
  road_edges_sf <- network %>%
    sfnetworks::activate("edges") %>%
    st_as_sf()
  
  road_defended_small_town_nodes_sf <- network %>%
    sfnetworks::activate("nodes") %>%
    filter(Second_Century_Defence == "Yes") %>%
    st_as_sf()
  
  road_nodes_sf <- network %>%
    sfnetworks::activate("nodes") %>%
    filter(!is.na(Second_Century_Defence)) %>%
    st_as_sf()
  
  plot <- 
    tm_shape(outline) + 
    tm_polygons(col = "white", border.col = "white") + 
    tm_shape(road_edges_sf) + 
    tm_lines() + 
    tm_shape(road_defended_small_town_nodes_sf["nodal_efficiency"]) + 
    tm_dots(col = "black", size = 0.8) + 
    tm_shape(road_nodes_sf["nodal_efficiency"])  +
    tm_dots(col = "nodal_efficiency", size = 0.3, palette = viridis::plasma(5), title = "Nodal Efficiency") + 
    tm_add_legend('symbol', 
                  border.col = c("black"),
                  border.lwd = 4,
                  col = NA,
                  size = 1,
                  labels = c('Defended Small Town')) + 
    tm_layout(frame = FALSE, bg.color = "#C8C8C8", legend.position = c("right","top")) + 
    tm_scale_bar(position = c("right", "bottom"))
  
  return(plot)
}

plot_betweenness_network <- function(network) { 
  
  betweenness <- igraph::edge_betweenness(graph = network, directed = FALSE)
  
  network_sf <- network %>%
    activate("edges") %>%
    mutate("Edge Betweenness" = betweenness) %>%
    sfnetworks::activate("edges") %>%
    st_as_sf()
  
  road_defended_small_town_nodes_sf <- network %>%
    sfnetworks::activate("nodes") %>%
    filter(Second_Century_Defence == "Yes") %>%
    st_as_sf()
  
  road_nodes_sf <- network %>%
    sfnetworks::activate("nodes") %>%
    filter(Second_Century_Defence == "No") %>%
    st_as_sf()
  
  plot <- tm_shape(outline) + 
    tm_polygons(col = "white", border.col = "white") + 
    tm_shape(network_sf) + 
    tm_lines(lwd = "Edge Betweenness", scale=10, col = "grey60") + 
    tm_shape(road_nodes_sf) + 
    tm_dots(col = "grey40", size = 0.5) + 
    tm_shape(road_defended_small_town_nodes_sf) + 
    tm_dots(col = "black", size = 0.5, shape = 15) + 
    tm_add_legend('symbol', border.col = "black",col = "black", size = 1,shape = 15, labels = c('Defended Small Town')) + 
    tm_add_legend('symbol', border.col = "grey40",col = "grey40", size = 1,labels = c('Non-Defended Small Town')) + 
    tm_layout(frame = FALSE, bg.color = "#C8C8C8", legend.position = c("right","top")) + 
    tm_scale_bar()
  
  return(plot)
}
