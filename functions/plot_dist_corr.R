#     plot_dist_corr()
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

#' @title 
#' @description 
#' @param graphObj
#' @return 
#' @author Sergio Gracia
#' @references 
#' 
#' 

plot_dist_corr <- function(graphObj, mute = FALSE){
  
  graph <- graphObj$graph
  weighted = attr(graphObj, "weighted")
  if(!weighted){stop("Cannot plot for unweighted graph. Please introduce a weighted graph", call. = FALSE)}
  
  edges <- get.edgelist(graph) %>% data.frame() %>% setNames(c("from", "to"))
  
  # Weights
  edges$weight <- E(graph)$weight
  # Signed adjacency matrix
  signed.adj <- graphObj$signed_adjacency
  edges$sign <- mapply(FUN = function(x,y) signed.adj[x,y], edges$from, edges$to)
  # Edge distance
  dist.matrix <- graphObj$geodist
  edges$dist <- mapply(FUN = function(x,y) dist.matrix[x,y], edges$from, edges$to)
  
  # Plot colors
  palette <- scale_color_gradient(low = "red", high = "blue")
  palette <- scale_color_manual(values = c("red", "blue"))
  
  # Plot
  if(mute == FALSE){x11()}
  ggplot() + 
    geom_point(aes(weight, dist, col = as.factor(sign)), data = edges) +
    palette +
    xlim(0,1) + ylim(0, 20000)
}