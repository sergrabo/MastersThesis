#     as.graphObj
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

#' @title Transform graph to graphObj
#' @description 
#' @param graph
#' @param ref.graphObj
#' @return graphObj
#' @author Sergio Gracia
#' @references https://www.r-bloggers.com/2018/05/three-ways-of-visualizing-a-graph-on-a-map/

as.graphObj <- function(graph, ref.graphObj){
  
  out <- ref.graphObj
  out$graph <- graph
  out$correlation <- NULL
  out$adj.matrix <- graph %>% as_adj() %>% as.matrix
  out$signed_adjacency <- out$adj.matrix
  
  attr(out, "weightedGraph") <- FALSE
  
  return(out)
}