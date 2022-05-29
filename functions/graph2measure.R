#     graph2measure
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

#' @title Calculate measures from graph
#' @description Computes diferent measures for a given graphObj, which should be an output of previous function Graph_from_Grid
#' @param graphObj 
#' @return List with the computed measures
#' @author Sergio Gracia
#' @references 
#' 
#' 

graph2measure <- function(graphObj) {
  
  weighted = attr(graphObj, "weighted")
  
  #Degree
  K <- igraph::degree(graphObj$graph, loops = FALSE)
  #Betweenness
  B <- igraph::betweenness(graphObj$graph, directed = FALSE)
  
  # area weighted connectivity
  # Calculacion area total:
  sumArea <- sum(cos(graphObj$VertexCoords$lon/(180)*pi))
  # Calculacion Area weighted connectivity per gridbox: 
  awconnectivity <- as.vector(cos(graphObj$VertexCoords$lat/(180)*pi)%*%graphObj$adjacency) / sumArea
  
  # Distance-based strength
  dists.net <- graph_from_adjacency_matrix(graphObj$geodist, weighted = TRUE, mode = "undirected", diag = FALSE) 
  strength.dist <- igraph::strength(dists.net)
  
  # Mean distance per node
  MDN <- strength.dist/K
  MDN[which(is.nan(MDN))] <- 0
  
  # Correlation-based strength
  if(weighted == TRUE){
    strength.cor <- igraph::strength(graphObj$graph)
  }else{
    strength.cor <- NA
  }
  
  out <- list("degree" = K,
              "betweenness" = B,
              "awconnectivity" = awconnectivity,
              "dist_strength" = strength.dist,
              "mean_dist_per_node" = MDN)
  
  if(weighted == TRUE){out$cor_strength = strength.cor}
  
  attr(out, "Xcoords") <- attr(graphObj, "Xcoords")
  attr(out, "Ycoords") <- attr(graphObj, "Ycoords")
  attr(out, "ref.dates") <- attr(graphObj, "ref.dates")
  attr(out, "weighted") <- attr(graphObj, "weighted")
  return(out)
}