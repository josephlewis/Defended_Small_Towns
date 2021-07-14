library(brainGraph)
library(doParallel)
library(dplyr)
library(foreach)
library(ggpubr)
library(rgdal)
library(raster)
library(shp2graph)
library(sf)
library(sfnetworks)
library(tmap)
library(viridis)

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

#write.csv(x = nodal_efficiency_df, file = "./Outputs/mean_nodal_efficiency.csv")

#### CALCULATE MEAN NODAL BETWEENNESS FOR TRUE AND RANDOMISED DEFENDED SMALL TOWNS ####

true_nodal_betweenness <- calculate_betweenness(network = road_network, shuffle = FALSE)

sim_nodal_betweenness <- do.call(rbind, replicate(n = nodal_nsims, expr = shuffle_towns(network = road_network) %>%
                                                   calculate_betweenness(network = ., shuffle = TRUE), simplify = FALSE))

nodal_betweenness_df <- rbind(true_nodal_betweenness, sim_nodal_betweenness)

#write.csv(x = nodal_betweenness_df, file = "./Outputs/mean_nodal_betweenness.csv")

#### CREATE FIGURES ####

#### FIGURE ONE ####

fig1 <- plot_nodal_network(network = road_network)

#tmap::tmap_save(tm = fig1,  "./Outputs/Figures/Figure 1.png", dpi = 300)

#### FIGURE TWO ####

fig2 <- plot_betweenness_network(road_network)

#tmap::tmap_save(tm = fig2,  "./Outputs/Figures/Figure 2.png", dpi = 300)

#### FIGURE THREE ####

fig3 <- tmap_arrange(list(
  plot_network(road_network, legend = TRUE, title = "A"),
  plot_network(shuffle_towns(road_network), legend = FALSE, title = "B"),
  plot_network(shuffle_towns(road_network), legend = FALSE, title = "C"),
  plot_network(shuffle_towns(road_network), legend = FALSE, title = "D")
), ncol = 2, nrow = 2)

#tmap::tmap_save(tm = fig3, filename = "./Outputs/Figures/Figure 3.png",dpi = 300, width = 2000, height = 3000)

#### FIGURE FOUR ####

fig4a <- 
  tm_shape(outline) + 
  tm_polygons(col = "white", border.col = "white") +
  tm_shape(road[road$Source == "Margary",]) +
  tm_lines(col = "black", lwd= 1) +
  tm_shape(road[road$Source == "Lewis",]) +
  tm_lines(col = "red", lwd=2) +
  tm_layout(frame = TRUE, title = "A", bg.color = "#C8C8C8")

fig4b <- plot_network(road_network, legend = FALSE, small_towns = FALSE, size = 0.05, col = "black", title = "B")

fig4 <- tmap::tmap_arrange(list(fig4a, fig4b))

#tmap::tmap_save(tm = fig4,  "./Outputs/Figures/Figure 4.png", dpi = 300, height = 5, width = 7)

#### FIGURE FIVE ####

fig5 <-   tm_shape(outline) + 
  tm_polygons(col = "white", border.col = "white") +
  tm_shape(road) +
  tm_lines(col = "black", lwd=1) +
  tm_shape(small_towns) +
  tm_dots(col = "Second_Century_Defence", size = 0.5, palette = c("red", "black"), title = "Second Century Defence", legend.show = FALSE) +
  tm_layout(frame = FALSE, bg.color = "#C8C8C8")

#tmap::tmap_save(tm = fig5,  "./Outputs/Figures/Figure 5.png", dpi = 300)

#### FIGURE SIX ####

N.greater_efficiency <- sum(nodal_efficiency_df$value[nodal_efficiency_df$shuffle == TRUE] > nodal_efficiency_df$value[nodal_efficiency_df$shuffle == FALSE])

p_val_efficiency <- (N.greater_efficiency + 1) / (nodal_nsims + 1)

fig6 <- ggplot() +
  geom_density(data = nodal_efficiency_df[nodal_efficiency_df$shuffle == TRUE,], mapping = aes(value), col = "grey", fill = "grey") +
  geom_vline(xintercept = nodal_efficiency_df$value[nodal_efficiency_df$shuffle == FALSE], linetype= "dotted",  color = "black", size=1.5) +
  annotate("text", x = 0.0602, y = 200, label = paste0("p = ", p_val_efficiency)) +
  labs(x = "Mean Nodal Efficiency", y = "Density") +
  theme_classic()

#ggexport(plotlist = list(fig6), filename = "./Outputs/Figures/Figure 6.png", res = 300, width = 3000, height = 2000)

##########

N.greater_betweenness <- sum(nodal_betweenness_df$value[nodal_betweenness_df$shuffle == TRUE] > nodal_betweenness_df$value[nodal_betweenness_df$shuffle == FALSE])

p_val_betweenness <- (N.greater_betweenness + 1) / (nodal_nsims + 1)

fig7 <- ggplot() +
  geom_density(data = nodal_betweenness_df[nodal_betweenness_df$shuffle == TRUE,], mapping = aes(value), col = "grey", fill = "grey") +
  geom_vline(xintercept = nodal_betweenness_df$value[nodal_betweenness_df$shuffle == FALSE], linetype= "dotted",  color = "black", size=1.5) +
  annotate("text", x = 26200, y = 0.000075, label = paste0("p = ", p_val_betweenness)) +
  labs(x = "Mean Edge Betweenness", y = "Density") +
  theme_classic()

#ggexport(plotlist = list(fig7), filename = "./Outputs/Figures/Figure 7.png", res = 300, width = 3000, height = 2000)