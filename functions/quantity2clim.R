#     quantity2clim
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

#' @title C4R climatology from measure
#' @description Convert a vector (which can be a measure on the complex network) to a climate4R climatology
#' @param quantity Vector of integers or doubles, which length should match the number of nodes used in the complex network
#' @param ref.grid
#' @return 
#' @author Sergio Gracia
#' @references 
#' 
#' 
quantity2clim <- function(quantity, ref.grid, ref.mask = NULL, what, backperm = NULL) {
  if(!is.null(backperm)){quantity <- quantity[backperm]}
  if(!is.null(ref.mask)){
    L = length(ref.grid$xyCoords$x) * length(ref.grid$xyCoords$y)
    mat <- matrix(NA, nrow = 1, ncol = L)  
    mat[mask] <- quantity
  }else{
    mat <- matrix(quantity, nrow = 1)
  }
  ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
  attr(ref.grid$Data, "climatology:fun") <- what
  return(ref.grid)
}