#     BN_plot_inference
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

#' @title BN_plot_inference
#' @description Outputs a world-grid spatial-plot of the Bayesian Network propagation when performing inference
#' @param node Which node to perform inference with
#' @param sign Whether the propagation is with positive or negative sign
#' @param propagation_df Dataframe with the inference probability
#' @param ref.grid Reference grid for plotting
#' @param ref.mask Reference mask for plotting
#' @return plot
#' @author Sergio Gracia
#' @references

library(visualizeR)
library(transformeR)

BN_plot_inference <- function(node, sign, propagation_df, ref.grid=NULL, ref.mask=NULL, mute=FALSE){
  
  if(!is.null(ref.mask)){
    L = length(ref.grid$xyCoords$x) * length(ref.grid$xyCoords$y)
    mat <- matrix(NA, nrow = 1, ncol = L)  
    mat[mask] <- propagation_df$diff
  }else{
    mat <- matrix(propagation_df$diff, nrow = 1)
  }
  ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
  out <- ref.grid
  
  if(sign == "pos"){
    color <- "Reds"
  }else if (sign == "neg"){
    color <- "Blues"
  }else{
    print("Wrong sign")
  }
  
  if(mute==FALSE){
    x11()
    spatialPlot(out, backdrop.theme = "coastline", main = "Bayesian network inference", color.theme = color, ylim=c(-75,90), at = seq(0.05, 0.85, by = 0.05))
  }else{
    p <- spatialPlot(out, backdrop.theme = "coastline", main = "Bayesian network inference", color.theme = color, ylim=c(-75,90), at = seq(0.05, 0.85, by = 0.05))
    return(p)
  }
}
