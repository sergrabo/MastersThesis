#     graph_world_network
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Linked world plot
#' @description Generates a network plot in which the nodes correspond to their geographical location
#' @param graph 
#' @param coords
#' @param weighted
#' @return 
#' @author Sergio Gracia
#' @references https://www.r-bloggers.com/2018/05/three-ways-of-visualizing-a-graph-on-a-map/
#' 
#' 
#' 


graph_world_network <- function(graphObj, mute = FALSE){
  graph <- graphObj$graph
  coords <- graphObj$VertexCoords
  weighted = attr(graphObj, "weighted")
  th = attr(graphObj, "threshold")
  
  alpha <- 0.25
  # Abrimos ventana para el plot
  if(mute == FALSE){x11()}
  # Guardamos todos los links de la red en formato "from-to" indicando de un id a otro
  edges <- get.edgelist(graph) %>% data.frame() %>% setNames(c("from", "to"))
  if(weighted == TRUE){edges$weight <- E(graph)$weight}
  
  # Signed adjacency matrix
  signed.adj <- graphObj$signed_adjacency
  edges$sign <- mapply(FUN = function(x,y) signed.adj[x,y], edges$from, edges$to)
  
  # Edge distance
  dist.matrix <- graphObj$geodist
  edges$dist <- mapply(FUN = function(x,y) dist.matrix[x,y], edges$from, edges$to)
  
  # Añadimos a los links la informacion de las coordenadas de cada id
  edges_for_plot <- edges %>%
    inner_join(coords %>% select(id, lon, lat), by = c('from' = 'id')) %>%
    rename(x = lon, y = lat) %>%
    inner_join(coords %>% select(id, lon, lat), by = c('to' = 'id')) %>%
    rename(xend = lon, yend = lat)
  
  # Tema del mapa 
  maptheme <- theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) + 
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    theme(legend.position = "bottom") +
    theme(legend.box.background  = element_rect(colour = "black", size = 1)) +
    theme(panel.grid = element_blank()) +
    theme(panel.background = element_rect(fill = "#d2edfa")) +
    theme(plot.margin = unit(c(1, 0, 0.5, 0), 'cm'))
  
  # Background del mapamundi
  country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                                 data = map_data('world'),
                                 fill = "#c4a174", color = "#515151",
                                 size = 0.15)
  mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
  
  title <- ggtitle(bquote("Spatial Network for " ~ tau[c] ~ "=" ~ .(th)))
  title1 <- ggtitle(bquote("Positive Spatial Network for " ~ tau[c] ~ "=" ~ .(th)))
  title2 <- ggtitle(bquote("Negative Spatial Network for " ~ tau[c] ~ "=" ~ .(th)))
  legend <- labs(color = "Correlation sign")
  
  # PLOT: ¡¡¡IMPORTANTE!!! Siempre poner el plot lo último para que ggplot pueda plotearlo
  if(weighted == FALSE){
    if(th >= 0.8){alpha <- 0.5}
    palette <- scale_color_manual(values = c("red", "blue"))
    if(range(edges$sign)[1] == 1){palette <- scale_color_manual(values = c("blue"))}
    ggplot(coords) + country_shapes +
      geom_curve(aes(x = x, y = y, xend = xend, yend = yend, color = as.factor(sign)),
                 data = edges_for_plot, curvature = 0.33, alpha = alpha) +
      palette + mapcoords + maptheme + title + legend
  }else{
    if(th >= 0.8){alpha <- 0.9}
    if(th >= 0.6 & th < 0.8){alpha <- 0.5}
    colors.discrete <- brewer.pal(9, "Blues")[-(1:2)]
    colors.continuous <- colorRampPalette(colors.discrete)
    palette1 <- scale_color_gradientn(colours = colors.continuous(20), guide = guide_colorbar(title = "Correlation coefficient"))
    colors.discrete <- rev(brewer.pal(9, "Reds")[-(1:2)])
    colors.continuous <- colorRampPalette(colors.discrete)
    palette2 <- scale_color_gradientn(colours = colors.continuous(20), guide = guide_colorbar(title = "Correlation coefficient"))
    
    edges_for_plot <- edges_for_plot %>% mutate(signed_weight = weight*sign)
    
    p1 <- ggplot(coords) + country_shapes +
      geom_curve(aes(x = x, y = y, xend = xend, yend = yend, color = signed_weight),
                 data = edges_for_plot[edges_for_plot$sign == 1,], curvature = 0.33, alpha = alpha) +
      palette1 + mapcoords + maptheme +
      title1
    
    p2 <- ggplot(coords) + country_shapes +
      geom_curve(aes(x = x, y = y, xend = xend, yend = yend, color = signed_weight),
                 data = edges_for_plot[edges_for_plot$sign == -1,], curvature = 0.33, alpha = alpha) +
      palette2 + mapcoords + maptheme+
      title2
    grid.arrange(p1,p2, ncol = 1)
  }
}
