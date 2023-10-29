#     generate_random_networks
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

#' @title Generate random networks
#' @description Generates a list of Erdos-Renyi graphs for comparison
#' @param thresholds Array with the values of the thresholds
#' @param comms List of community objects to plot the dendrograms from
#' @param results_path path where the results are stored. Should be './src/main/resources/results/CN_results/'
#' @return a List with the unweighted net, the weighted net, and a community object if th>0.4
#' @author Sergio Gracia
#' 

library(igraph)

generate_random_networks <- function(thresholds, unwnet, atts, seed = 4){
  # Set seed
  set.seed(seed)
  # Initialize empty variables
  rnet <- list()
  rand.clust_coeff <- c()
  rand.diam <- c()
  rand.conn <- c()
  complex.conn <- c()
  
  for (i in 1:length(ths)){
    n_vertex <- vcount(unwnet[[i]]$graph)
    n_edges <- atts$total_edges[i]
    
    rand.graph <- erdos.renyi.game(n_vertex, n_edges, type = "gnm")
    rnet[[i]] <- rand.graph # save random net
    rand.clust_coeff <- c(rand.clust_coeff, transitivity(rand.graph))
    rand.diam <- c(rand.diam, diameter(rand.graph))
    
    # Connectivity of the graph
    rand.conn <- c(rand.conn, diameter(rand.graph, unconnected = FALSE))
    complex.conn <- c(complex.conn, diameter(unwnet[[i]]$graph, unconnected = FALSE))
  }
}
