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

calculate_global_efficiency <- function(network, nodes_removed, shuffle = TRUE) { 
  
  df <- data.frame(nodes_removed = nodes_removed, metric = "Global Efficiency", value = brainGraph::efficiency(g = network, type = "global", use.parallel = FALSE), shuffle = shuffle)
  
  return(df)
  
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
  
  edges_with_towns <- sf::st_intersection(network_edges, small_town_nodes)
  
  town_edge_betweenness <- mean(betweenness[which(network_edges$Name %in% unique(edges_with_towns$Name))])
  
  df <- data.frame(metric = "Mean Edge Betweeness", value = town_edge_betweenness, shuffle = shuffle)
  
  return(df)
  
}

plot_network <- function(network, legend = TRUE, title = NA, small_towns = TRUE, col = "Second_Century_Defence", size = 0.2) {
  
  road_network_sf <- network %>%
    sfnetworks::activate("edges") %>%
    st_as_sf()
  
  if (small_towns) { 
    
    nodes <- sf::st_as_sf(network) %>%
      dplyr::filter(!is.na(Second_Century_Defence))
    
  } else { 
    
    nodes <- sf::st_as_sf(network) %>%
      dplyr::filter(is.na(Second_Century_Defence))
    
  }
  
  plot <-     
    tm_shape(outline) + 
    tm_polygons(col = "white", border.col = "white") +
    tm_shape(road_network_sf) + 
    tm_lines() + 
    tm_shape(nodes) + 
    tm_dots(col = col, size = size, palette = c("red", "black"), title = "Second Century Defence", legend.show = legend)
  
  if (legend) { 
    
    plot <- 
      plot + 
      tm_add_legend('line', 
                    col = c("black"),
                    lwd = c(1),
                    labels = c('Roman Road')) + 
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
  
  plot <-     
    tm_shape(outline) + 
    tm_polygons(col = "white", border.col = "white") + 
    tm_shape(network_sf) + 
    tm_lines(lwd = "Edge Betweenness", scale=10) + 
    tm_shape(road_defended_small_town_nodes_sf) + 
    tm_dots(col = "red", size = 0.5) + 
    tm_add_legend('symbol', 
                  border.col = "red",
                  col = "red",
                  size = 1,
                  labels = c('Defended Small Town')) + 
    tm_layout(frame = FALSE, bg.color = "#C8C8C8", legend.position = c("right","top")) + 
    tm_scale_bar()
  
  return(plot)
}
