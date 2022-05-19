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
  
  # strength
  if(!is.null(edge_attr(graphObj$graph))){
    strength <- igraph::strength(graphObj$graph)
  } else {strength <- NA}
  
  # area weighted connectivity
  # Calculacion area total:
  sumArea <- sum(cos(graphObj$VertexCoords$y/(180)*pi))
  # Calculacion Area weighted connectivity per gridbox: 
  awconnectivity <- as.vector(cos(graphObj$VertexCoords$y/(180)*pi)%*%graphObj$adjacency) / sumArea
  
  out <- list("strength" = close, 
              "awconnectivity" = awconnectivity)
  attr(out, "Xcoords") <- attr(graphObj, "Xcoords")
  attr(out, "Ycoords") <- attr(graphObj, "Ycoords")
  attr(out, "ref.dates") <- attr(graphObj, "ref.dates")
  return(out)
}