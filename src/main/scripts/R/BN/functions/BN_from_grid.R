#     BN_from_grid
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

#' @title Bayesian Network from C4R grid with anomalies
#' @description Convert a climate4R grid with anomalies to an igraph Bayesian network using the hill-climbing algorithm
#' @param grid Input C4R grid
#' @param mask Logical mask to use only locations that are burnable
#' @param max.iter Maximum number of allowed hill-climbing iterations
#' @param start Reference Bayesian Network to start the process from
#' @param score Score to compute hill-climbing. Defaults to "bic-g"
#' @return BNobj
#' @author Sergio Gracia
#' @references 

# Load necesary libraries
library(bnlearn)
library(graph)
library(igraph)
library(transformeR)

BN_from_grid <- function(grid,
                         mask,
                         max.iter,
                         start = NULL,
                         score = "bic-g"){
  
  coords <- getCoordinates(grid)
  x <- coords$x
  y <- coords$y
  ref.coords <- expand.grid(y, x)[mask,2:1]
  ref.coords$id <- 1:nrow(ref.coords)
  names(ref.coords) <- c("lon", "lat", "id")
  
  # Data isn't forced through logarithm becase its anomalies
  time.coords.matrix <- array3Dto2Dmat(grid$Data)[,mask]
  
  data <- time.coords.matrix %>% data.frame() 
  
  BN <-  hc(data, start=start, score=score, max.iter=max.iter)
  
  # igraph Object
  igraph <- igraph.from.graphNEL(as.graphNEL(BN)) 
  
  BNobj <- list("BN" = BN,
                "BN.igraph" = igraph,
                "VertexCoords" = ref.coords,
                "data" = data)
  
  attr(BNobj, "Xcoords") <- x
  attr(BNobj, "Ycoords") <- y
  attr(BNobj, "data_origin") <- "anomalies_data"
  
  return(BNobj)
}