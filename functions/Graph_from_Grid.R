#     Graph_from_Grid
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

#' @title Graph from C4R grid
#' @description Convert a climate4R grid to an igraph undirected complex network
#' @param grid Input C4R grid
#' @param th (Absolute) correlation coefficient threshold to consider a link
#' @return 
#' @author Sergio Gracia
#' @references 
#' 

Graph_from_Grid <- function(grid,
                            th = 0.8,
                            weighted = FALSE,
                            mask = NULL,
                            subind = NULL,
                            method = c("spearman")) {
  
  coords <- getCoordinates(grid)
  x <- coords$x
  y <- coords$y
  ref.coords <- expand.grid(y, x)[mask,2:1]
  ref.coords$id <- 1:nrow(ref.coords)
  names(ref.coords) <- c("lon", "lat", "id")
  ref.dates <- getRefDates(grid)

  aux <- grid
  grid <- NULL
  time.coords.matrix <- array3Dto2Dmat(aux$Data)
  
  
  if (!is.null(subind)) {
    time.coords.matrix <- time.coords.matrix[subind,]}
  
  # Mask
  if(!is.null(mask)){
    time.coords.matrix <- time.coords.matrix[,mask]
  }
  
  # Correlation matrix
  cor.matrix <- cor(time.coords.matrix, method = method) 
  
  # Adjacency matrix
  adj.matrix <- cor.matrix %>% abs()
  diag(adj.matrix) <- 0
  adj.matrix[adj.matrix <= th ] <- 0
  adj.matrix[is.na(adj.matrix)] <- 0
  adj.matrix[adj.matrix > th ] <- 1
  
  # Signed adjacency matrix
  signed.adj <- sign(cor.matrix) * adj.matrix
  signed.adj[is.na(signed.adj)] <- 0
  
  # Geographical distance matrix
  pts <- SpatialPoints(cbind(ref.coords$lon, ref.coords$lat), proj4string = CRS("+init=epsg:4326"))
  dists <- matrix(data = 0, nrow = length(pts), ncol = length(pts))
  dists[which(adj.matrix > 0)] <- spDists(pts, longlat = TRUE)[which(adj.matrix > 0)]
  
  # Graph
  graph <- graph_from_adjacency_matrix(adj.matrix, mode = "undirected")
  
  # Correlation-weighted graph
  if(weighted == TRUE){
    # Adjacency matrix
    adj.matrix <- cor.matrix %>% abs()
    diag(adj.matrix) <- 0
    adj.matrix[adj.matrix <= th] <- 0
    adj.matrix[is.na(adj.matrix)] <- 0
    
    # Graph
    graph <- graph_from_adjacency_matrix(adj.matrix, weighted = TRUE, mode = "undirected")
  }
  
  
  
  graphObj <- list("graph" = graph,
                   "data_coords" = time.coords.matrix,
                   "correlation" = cor.matrix,
                   "VertexCoords" = ref.coords,
                   "adjacency" = adj.matrix,
                   "signed_adjacency" = signed.adj,
                   "geodist" = dists)
  
  attr(graphObj, "Xcoords") <- x
  attr(graphObj, "Ycoords") <- y
  attr(graphObj, "ref.dates") <- ref.dates
  attr(graphObj, "weightedGraph") <- weighted
  return(graphObj)
}
