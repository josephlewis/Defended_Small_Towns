library(brainGraph)
library(doParallel)
library(dplyr)
library(foreach)
library(ggdist)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(rgdal)
library(raster)
library(shp2graph)
library(sf)
library(sfnetworks)
library(tmap)
library(viridis)

options(scipen = 999)

#### LOAD USER-CREATED FUNCTIONS ####

source("./R/Functions.R")

#### VARIABLES ####

nodal_nsims <- 999

#### LOAD PREPROCESS DATA ####

road <- rgdal::readOGR("./Data/Roman Road Network/Connected_Roman_Road_Network.shp")

outline <- rgdal::readOGR("./Data/Outline/greatbritain.shp")
outline <- outline[1,]
outline <- raster::crop(outline, extent(97165, 666140, -5045, 700000))

small_towns <- read.csv("./Data/Small Towns/Small_Towns.csv", na.strings = c("", "NA"))
small_towns <- sp::SpatialPointsDataFrame(coords = data.frame(small_towns$Site.X, small_towns$Site.Y), proj4string = raster::crs(road), data = small_towns)
small_towns <- small_towns[rgeos::gBuffer(road, width = 1000),]
small_towns$Second_Century_Defence <- factor(small_towns$Second_Century_Defence, levels = c("Yes", "No"))

#### CREATE ROMAN ROAD NETWORK GRAPH ####
road_network <- sf::st_as_sf(road)
road_network <- sfnetworks::as_sfnetwork(road_network, directed = FALSE)
sf::st_crs(road_network) <- raster::crs(small_towns)

#### JOIN SMALL TOWNS TO ROMAN ROAD NETWORK GRAPH ####
road_network = sfnetworks::st_network_blend(road_network, sf::st_as_sf(small_towns))

#### CALCULATE MEAN NODAL EFFICIENCY FOR TRUE AND RANDOMISED DEFENDED SMALL TOWNS ####

true_nodal_efficiency <- calculate_nodal_efficiency(network = road_network, shuffle = FALSE)

sim_nodal_efficiency <- do.call(rbind, replicate(n = nodal_nsims, expr = shuffle_towns(network = road_network) %>%
                                                   calculate_nodal_efficiency(network = ., shuffle = TRUE), simplify = FALSE))

nodal_efficiency_df <- rbind(true_nodal_efficiency, sim_nodal_efficiency)

write.csv(x = nodal_efficiency_df, file = "./Outputs/mean_nodal_efficiency_999.csv")

#### CALCULATE MEAN NODAL BETWEENNESS FOR TRUE AND RANDOMISED DEFENDED SMALL TOWNS ####

true_nodal_betweenness <- calculate_betweenness(network = road_network, shuffle = FALSE)

sim_nodal_betweenness <- do.call(rbind, replicate(n = nodal_nsims, expr = shuffle_towns(network = road_network) %>%
                                                   calculate_betweenness(network = ., shuffle = TRUE), simplify = FALSE))

nodal_betweenness_df <- rbind(true_nodal_betweenness, sim_nodal_betweenness)

write.csv(x = nodal_betweenness_df, file = "./Outputs/mean_nodal_betweenness_999.csv")

#### CREATE FIGURES ####

#### FIGURE ONE ####

fig1a <-
  tm_shape(outline) + 
  tm_polygons(col = "white", border.col = "white") +
  tm_shape(road[road$Source == "Margary",]) +
  tm_lines(col = "grey40", lwd= 1) +
  tm_shape(road[road$Source == "Lewis",]) +
  tm_lines(col = "black", lwd=2) +
  tm_layout(frame = TRUE, title = "A", bg.color = "#C8C8C8")

fig1b <- plot_network(road_network, legend = FALSE, small_towns = FALSE, point_size  = 0.05, title = "B", road_colour = "grey20")

fig1 <- tmap::tmap_arrange(list(fig1a, fig1b))

tmap::tmap_save(tm = fig1,  "./Outputs/Figures/Figure 1.png", dpi = 300, height = 5, width = 7)

#### FIGURE TWO ####

road_network_nodes <- sf::st_as_sf(road_network) %>%
  dplyr::filter(is.na(Second_Century_Defence))

fig2 <- tm_shape(outline) + 
  tm_polygons(col = "white", border.col = "white") +
  tm_shape(road) +
  tm_lines(col = "black", lwd=1) +
  tm_shape(road_network_nodes) + 
  tm_dots(size = 0.05) + 
  tm_shape(small_towns[small_towns$Second_Century_Defence == "No",]) +
  tm_dots(col = viridis::cividis(5)[3], size = 0.5, title = "Second Century Defence", legend.show = FALSE, shape = 16) + 
  tm_shape(small_towns[small_towns$Second_Century_Defence == "Yes",]) +
  tm_dots(col = "black", size = 0.5, title = "Second Century Defence", legend.show = FALSE, shape = 15) + 
  tm_add_legend('symbol', border.col = "black",col = "black", size = 1, labels = c('Defended Small Town'), shape = 15) + 
  tm_add_legend('symbol', border.col = viridis::cividis(5)[3],col = viridis::cividis(5)[3], size = 1,labels = c('Non-Defended Small Town')) + 
  tm_add_legend('symbol', border.col = "black",col = "black", size = 0.5,labels = c('Roman Road Nodes')) + 
  tm_layout(frame = FALSE, bg.color = "#C8C8C8", legend.position = c("right","top")) + 
  tm_scale_bar()

tmap::tmap_save(tm = fig2,  "./Outputs/Figures/Figure 2.png", dpi = 300)

#### FIGURE THREE ####

fig3 <- plot_nodal_network(network = road_network)

tmap::tmap_save(tm = fig3,  "./Outputs/Figures/Figure 3.png", dpi = 300)

#### FIGURE FOUR ####

fig4 <- plot_betweenness_network(road_network)

tmap::tmap_save(tm = fig4,  "./Outputs/Figures/Figure 4.png", dpi = 300)

#### FIGURE FIVE ####

fig5 <- tmap_arrange(list(
  plot_network(road_network, legend = TRUE, title = "A", road_colour = "grey40"),
  plot_network(shuffle_towns(road_network), legend = FALSE, title = "B", road_colour = "grey40"),
  plot_network(shuffle_towns(road_network), legend = FALSE, title = "C", road_colour = "grey40"),
  plot_network(shuffle_towns(road_network), legend = FALSE, title = "D", road_colour = "grey40")
), ncol = 2, nrow = 2)

tmap::tmap_save(tm = fig5, filename = "./Outputs/Figures/Figure 5.png",dpi = 300, width = 2000, height = 3000)

#### FIGURE SIX ####

nodal_eff <- brainGraph::efficiency(g = road_network, type = "nodal", use.parallel = FALSE)

small_town_nodes <- road_network %>%
  sfnetworks::activate("nodes") %>%
  sf::st_as_sf() %>%
  sf::st_drop_geometry()

efficiency_by_status <- rbind(data.frame(value = nodal_eff[which(small_town_nodes$Second_Century_Defence == "No")], type = "No"),
            data.frame(value = nodal_eff[which(small_town_nodes$Second_Century_Defence == "Yes")], type = "Yes"))

fig6a <- ggplot(efficiency_by_status, aes(x = type, y = value)) + 
  geom_boxplot(width = .12, outlier.color = NA) +
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
  ggdist::stat_histinterval(width = .6, justification = -.5, .width = 0, point_colour = NA) +
  theme_classic() + 
  coord_flip() + 
  labs(x = "Small Town Defended in the Second Century", y = "Nodal Efficiency", title = "A")

edge_between <- igraph::edge_betweenness(graph = road_network, directed = FALSE)

def_small_town_nodes <- road_network %>%
  sfnetworks::activate("nodes") %>%
  sf::st_as_sf() %>%
  filter(Second_Century_Defence == "Yes")

no_small_town_nodes <- road_network %>%
  sfnetworks::activate("nodes") %>%
  sf::st_as_sf() %>%
  filter(Second_Century_Defence == "No")

network_edges <- road_network %>%
  activate("edges") %>%
  sf::st_as_sf()

edges_with_towns <- sf::st_nearest_feature(def_small_town_nodes, network_edges)
edges_with_no_towns <- sf::st_nearest_feature(no_small_town_nodes, network_edges)

betweenness_by_status <- rbind(data.frame(value = edge_between[edges_with_no_towns], type = "No"),
                               data.frame(value = edge_between[edges_with_towns], type = "Yes"))

fig6b <- ggplot(betweenness_by_status, aes(x = type, y = value)) + 
  geom_boxplot(width = .12, outlier.color = NA) +
  geom_point(size = 2, alpha = .3, position = position_jitter(seed = 1, width = .2)) +
  ggdist::stat_histinterval(width = .6, justification = -.5, .width = 0, point_colour = NA) +
  theme_classic() + 
  coord_flip() + 
  labs(x = "Small Town Defended in the Second Century", y = "Edge Betweenness", title = "B")

fig6 <- arrangeGrob(fig6a, fig6b, ncol = 2)

ggsave(file="./Outputs/Figures/Figure 6.png", fig6, width = 2500, height = 1200, units = "px", dpi = 300)

#### FIGURE SEVEN ####

N.greater_efficiency <- sum(nodal_efficiency_df$value[nodal_efficiency_df$shuffle == TRUE] > nodal_efficiency_df$value[nodal_efficiency_df$shuffle == FALSE])

p_val_efficiency <- (N.greater_efficiency + 1) / (nodal_nsims + 1)

fig7a <- ggplot() +
  geom_histogram(data = nodal_efficiency_df[nodal_efficiency_df$shuffle == TRUE,], mapping = aes(value), col = "grey", fill = "grey") +
  geom_vline(xintercept = nodal_efficiency_df$value[nodal_efficiency_df$shuffle == FALSE], linetype= "dotted",  color = "black", size=1.5) +
  annotate("text", x = 0.060, y = 100, label = paste0("p = ", p_val_efficiency)) +
  ylim(c(0, 150)) + 
  labs(x = "Mean Nodal Efficiency", y = "Density", title = "A") +
  theme_classic()

N.greater_betweenness <- sum(nodal_betweenness_df$value[nodal_betweenness_df$shuffle == TRUE] > nodal_betweenness_df$value[nodal_betweenness_df$shuffle == FALSE])

p_val_betweenness <- (N.greater_betweenness + 1) / (nodal_nsims + 1)

fig7b <- ggplot() +
  geom_histogram(data = nodal_betweenness_df[nodal_betweenness_df$shuffle == TRUE,], mapping = aes(value), col = "grey", fill = "grey") +
  geom_vline(xintercept = nodal_betweenness_df$value[nodal_betweenness_df$shuffle == FALSE], linetype= "dotted",  color = "black", size=1.5) +
  annotate("text", x = 20000, y = 100, label = paste0("p = ", p_val_betweenness)) +
  ylim(c(0, 150)) + 
  labs(x = "Mean Edge Betweenness", y = "Density", title = "B") +
  theme_classic()

fig7 <- arrangeGrob(fig7a, fig7b, ncol = 2)

ggsave(file="./Outputs/Figures/Figure 7.png", fig7, width = 2500, height = 1200, units = "px", dpi = 300)

