#     plot_mean_edge_dist
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

#' @title Plots mean edge dist vs ths
#' @description Function that plots the mean edge distance of all the network's edges for several threhsolds. Will always open a new window.
#' @param atts List containing all desired attributes
#' @return A new window with the plot
#' @author Sergio Gracia

plot_mean_edge_dist <- function(atts){
  # Open new window
  x11()
  # Assign thresholds (just to shorten the variable's name)
  ths <- atts$thresholds
  par(mar=c(5, 6, 4, 2) + 0.1)
  plot(ths, atts$total_mean_dist, col = "black", pch = 16, type = "b",
       ylim = c(0, 10000), xlab = "", ylab = "", axes = FALSE)
  points(ths, atts$pos_mean_dist, col = "blue", pch = 16, type = "b")
  points(ths, atts$neg_mean_dist, col = "red", pch = 16, type = "b")
  text(ths, -0.05, atts$total_edges, col= "blue", cex = 0.8, lwd = 2)
  box()
  axis(1, ths)
  mtext("Correlation threshold",side=1,col="black",line=2.5) 
  axis(2, ylim = c(0,10000), col="black", las = 1)
  mtext("Mean link distance", side = 2, line = 4)
  ## Add Legend
  legend(x = 0.15, y = 4000,
         legend = c("All links",
                    "Positively correlated links",
                    "Negatively correlated links"),
         text.col = c("black",
                      "blue",
                      "red"),
         pch = c(16, 16, 16),
         lty = c(1, 1, 1),
         col = c("black",
                 "blue",
                 "red"))
}