#     plot_clust_coeff_diameter
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

#' @title Plots clust coeff and diam vs ths
#' @description Function that plots the clustering coefficient and the diameter of the network for several threhsolds. Will always open a new window.
#' @param atts List containing all desired attributes
#' @param rgl Random Generator List: Object containing the parameters of the random network generated with the function `generate_random_networks()`
#' @return A new window with the plot
#' @author Sergio Gracia

plot_clust_coeff_diameter <- function(atts, rgl){
  # Open new window
  x11()
  # Assign thresholds (just to shorten the variable's name)
  ths <- atts$thresholds
  ## add extra space to right margin of plot within frame
  par(mar=c(5, 4, 4, 6) + 0.1)
  ## Plot first set of data and draw its axis
  plot(ths, atts$clust_coeff, axes = FALSE, col = "darkolivegreen2", xlab = "", ylab = "", ylim = c(-0.05,1),
       pch = 16, type = "b", lwd = 2)
  points(ths, rgl$rand.clust_coeff, col = "#85B13B", pch = 16, type = "b", lwd = 2)
  text(ths, -0.05, atts$total_edges, col= "blue", cex = 0.8, lwd = 2)
  axis(2, at = seq(0, 1, by = 0.1), ylim = c(0,1), col="black", las = 1)  ## las=1 makes horizontal labels
  mtext("Clustering coefficient", side = 2, line = 2.5)
  box()
  
  ## Allow a second plot on the same graph
  par(new=TRUE)
  
  # Plot the second plot and put axis scale on right
  plot(ths, atts$diam, axes = FALSE, col = "darkorange1", xlab = "", ylab = "", ylim = c(-0.05,max(c(max(rgl$rand.diam), max(atts$diam)))),
       pch = 16, type = "b", lwd = 2)
  points(ths[rgl$complex.conn==Inf | rgl$complex.conn == 645], atts$diam[rgl$complex.conn==Inf | rgl$complex.conn == 645], col = "darkorange1", pch = 8, cex = 2, lwd = 2)
  points(ths, rgl$rand.diam, col = "darkorange3", pch = 16, type = "b", lwd = 2)
  points(ths[rgl$rand.conn==Inf| rgl$rand.conn == 645], rgl$rand.diam[rgl$rand.conn==Inf | rgl$rand.conn == 645], col = "darkorange3", pch = 8, cex = 2, lwd = 2)
  
  ## a little farther out (line=4) to make room for labels
  axis(4, at = 0:max(c(max(rgl$rand.diam), max(atts$diam))), ylim = c(0,max(c(max(rgl$rand.diam), max(atts$diam)))), col = "red", col.axis = "red", las = 1)
  mtext("Diameter", side = 4, col = "red", line = 4) 
  
  ## Draw the time axis
  axis(1, ths)
  mtext("Correlation threshold", side=1, col="black", line=2.5)  
  
  ## Add Legend
  legend("topright",
         legend = c("CN Clustering Coefficient",
                    "RN Clustering Coefficient",
                    "CN Diameter",
                    "RN Diameter"),
         text.col = c("darkolivegreen1",
                      "#85B13B",
                      "darkorange1",
                      "darkorange3"),
         pch = c(16, 16, 16, 16),
         lty = c(1, 1, 1, 1),
         col = c("darkolivegreen1",
                 "#85B13B",
                 "darkorange1",
                 "darkorange3"))
  
}