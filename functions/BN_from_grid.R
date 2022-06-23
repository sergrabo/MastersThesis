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

#' @title Bayesian Network from C4R grid
#' @description Convert a climate4R grid to an igraph Bayesian network
#' @param grid Input C4R grid
#' @param mask Logical mask to use only locations that are burnable
#' @return 
#' @author Sergio Gracia
#' @references 
#'

BN_from_grid <- function(grid, mask){
  
  coords <- getCoordinates(grid)
  x <- coords$x
  y <- coords$y
  ref.coords <- expand.grid(y, x)[mask,2:1]
  ref.coords$id <- 1:nrow(ref.coords)
  names(ref.coords) <- c("lon", "lat", "id")
  
  # Hacemos el logaritmo porque los datos no son anomalías
  time.coords.matrix <- array3Dto2Dmat(grid$Data)[,mask] %>% log1p()
  
  data = time.coords.matrix %>% data.frame() 
  
  start <- Sys.time()
  BN <-  hc(data, score = "bic-g")
  end <- Sys.time()
  print(paste("Time elapsed on training:", end-start))
  
  # igraph Object
  igraph <- igraph.from.graphNEL(as.graphNEL(BN)) 
  
  BNobj <- list("BN" = BN,
                "BN.igraph" = igraph,
                "VertexCoords" = ref.coords)
  
  attr(BNobj, "Xcoords") <- x
  attr(BNobj, "Ycoords") <- y
  attr(BNobj, "data_origin") <- "raw_data"
  attr(BNobj, "elapsed_time") <- end-start
  
  return(BNobj)
}