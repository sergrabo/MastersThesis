#     identify_node
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

#' @title identify_node
#' @description Function to single out any node on the grid
#' @param node Which node to identify
#' @param ref.grid Reference Grid
#' @param ref.mask Reference Mask
#' @param mute Whether to display the plot or not. Defaults to False 
#' @return p 
#' @author Sergio Gracia
#' @references  

library(visualizeR)
library(transformeR)

identify_node <- function(node, ref.grid, ref.mask, mute = FALSE){
  single.node <- rep(1,645)
  single.node[node] <- -1
  # Convert to climatology object
  quantity <- single.node
  if(!is.null(ref.mask)){
    L = length(ref.grid$xyCoords$x) * length(ref.grid$xyCoords$y)
    mat <- matrix(NA, nrow = 1, ncol = L)  
    mat[mask] <- quantity
  }else{
    mat <- matrix(quantity, nrow = 1)
  }
  ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
  out <- ref.grid
  if(mute == FALSE){
    x11()
    spatialPlot(out, backdrop.theme = "coastline", color.theme = "RdYlGn", ylim=c(-75,90))
  } else{
    p <- spatialPlot(out, backdrop.theme = "coastline", color.theme = "RdYlGn", ylim=c(-75,90))
    return(p)
  }
}
