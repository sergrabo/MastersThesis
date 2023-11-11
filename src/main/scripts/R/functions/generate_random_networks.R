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
#' @param unwnet List containing all the unweighted networks to extract the parameters from
#' @param atts List containing all the attributes to extract the parameters from
#' @param seed Int, the random seed to initialize the random graph creation process. Defaults to 4.
#' @return out, a list containing:
#'         rnet -> List containing graph objects of random graphs for each specified threshold
#'         rand.clust_coeff -> Clustering coefficient of the random network
#'         rand.diam -> Diameter of the random network
#'         rand.conn -> Connectivity of the random network -> Specifies wether the graph is fully connected or there are isolated nodes
#'         complex.conn -> Connectivity of the unweighted network -> Specifies wether the graph is fully connected or there are isolated nodes
#'         rnet.plot -> rnet list converted to graphObj to be plotted
#' @author Sergio Gracia
#' 

library(igraph)
source('src/main/scripts/R/functions/as_graphObj.R')

generate_random_networks <- function(thresholds, unwnet, atts, seed = 4){
  # Set seed
  set.seed(seed)
  # Initialize empty variables
  rnet <- list()
  rand.clust_coeff <- c()
  rand.diam <- c()
  rand.conn <- c()
  complex.conn <- c()
  
  for (i in 1:length(thresholds)){
    n_vertex <- vcount(unwnet[[i]]$graph)
    n_edges <- atts$total_edges[i]
    
    rand.graph <- erdos.renyi.game(n_vertex, n_edges, type = "gnm")
    rnet[[i]] <- rand.graph # save random net
    rand.clust_coeff <- c(rand.clust_coeff, transitivity(rand.graph))
    rand.diam <- c(rand.diam, diameter(rand.graph))
    
    # Connectivity of the graph
    rand.conn <- c(rand.conn, diameter(rand.graph, unconnected = FALSE))
    complex.conn <- c(complex.conn, diameter(unwnet[[i]]$graph, unconnected = FALSE))
    
    # Plot objects
    rnet.plot <- lapply(rnet, FUN = as.graphObj)
    
    out <- list(rnet=rnet,
                rand.clust_coeff=rand.clust_coeff,
                rand.diam=rand.diam,
                rand.conn=rand.conn,
                complex.conn=complex.conn,
                rnet.plot=rnet.plot)
  }
  return(out)
}