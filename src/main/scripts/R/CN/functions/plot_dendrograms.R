#     plot_dendrograms
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

#' @title Plot dendrograms
#' @description  Plots the dendrograms of the communities obtained from the script `main_networks_per_th.R`
#' @param comms List of community objects to plot the dendrograms from
#' @param results_path path where the results are stored. Should be './src/main/resources/results/CN_results/'
#' @param thresholds Array with the values of the thresholds
#' @return a List with the unweighted net, the weighted net, and a community object if th>0.4
#' @author Sergio Gracia


# Load create_path()
source("src/main/scripts/R/CN/functions/create_path.R")

plot_dendrograms <- function(comms, results_path, thresholds){
  # Dendrograms for communities where th>0.4
  lower_bound = which(thresholds==0.55)
  for (th in thresholds[lower_bound:length(ths)]) {
    c <- comms[[which.min(abs(thresholds-th))]]
    d <- as.hclust(c)
    th_path = create_path(th)
    dendro_file = paste0(results_path, th_path, "/", "dendrogram.pdf")
    pdf(dendro_file, width = 8.27, height = 11.69)
    plot(d, labels = FALSE, cex = .4, main = bquote("Dendrogram for " ~ tau[c] ~ "=" ~ .(th)), sub = "", xlab = "", lwd = 0.1)
    dev.off()
  }
}
