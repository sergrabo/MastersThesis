#     plot_measures
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

#' @title Plot centrality measures 
#' @description Plots maps with centrality measures for all nodes in the network
#' @param graphObj GraphObj with network's info 
#' @param mute bool, whether to save the graph or show it on screen
#' @return 
#' @author Sergio Gracia
#' @references 
#' 

func_path = "src/main/scripts/R/CN/functions/"
source(paste(func_path, "graph2measure.R", sep="/"))
source(paste(func_path, "quantity2clim.R", sep="/"))

library(grid)
library(visualizeR)

plot_measures <- function(graphObj, mute = FALSE) {
  
  th = attr(graphObj, "threshold")
  
  # Visually remove Antarctica from the plot
  ylim = c(-75,90)
  
  # Compute complex network's centrality measures
  measures.5deg <- graph2measure(graphObj)
  
  # Transform centrality measures to climatology objects to plot
  clim <- quantity2clim(measures.5deg, ref.grid = ba.5deg.std.anom, ref.mask = mask)
  
  if(!attr(graphObj, "weighted")){
    stop("Cannot plot correlation-based strength since edges have no weight", call. = FALSE)
  }
  
  p1 = spatialPlot(clim$degree, backdrop.theme = "coastline", main = "Degree", color.theme = "YlOrRd", ylim=ylim)
  p2 = spatialPlot(clim$betweenness, backdrop.theme = "coastline", main = "Betweenness", color.theme = "YlGnBu",ylim=ylim)
  p3 = spatialPlot(clim$dist_strength, backdrop.theme = "coastline", main = "Distance-based strength", color.theme = "PuRd",ylim=ylim)
  p4 = spatialPlot(clim$mean_dist_per_node, backdrop.theme = "coastline", main = "Mean link distance per node", color.theme = "PuRd",ylim=ylim)
  p5 = spatialPlot(clim$cor_strength, backdrop.theme = "coastline", main = "Correlation-based strength", color.theme = "PuBu",ylim=ylim)
  p6 = spatialPlot(clim$awconnectivity, backdrop.theme = "coastline", main = "Area Weighted Connectivity", color.theme = "RdPu", ylim = ylim)

  if(mute == FALSE){x11()}
  grid.arrange(p1, p2, p3, p4, p5, p6,
               nrow = 3, ncol = 2,
               top=textGrob(bquote("Centrality measures for " ~ tau[c] ~ "=" ~ .(th)), gp=gpar(fontsize=20,font=1)))
               
  
}