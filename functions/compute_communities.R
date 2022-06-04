#     compute_communities
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

#' @title 
#' @description 
#' @param graphObj
#' @param th Population threshold
#' @return 
#' @author Sergio Gracia
#' @references 
#' 
#' 
#' 

compute_communities <- function(graphObj, ref.grid, ref.mask = NULL, th = 6){
  
  # Compute communities through cluster_edge_betweenness
  ceb <- cluster_edge_betweenness(graphObj$graph, directed = FALSE)
  
  cuts <- c(185)
  ceb2 <- cut_at(ceb, cuts[1])
  
  # com <- membership(ceb)  
  com <- ceb2
  
  # Choose communities with population bigger than the threshold
  com.mask <- as.integer(dimnames(sizes(ceb)[which(sizes(ceb)>=th)])$`Community sizes`)
  com <- ifelse(com %in% com.mask, com, NA)
  ncom <- length(levels(factor(com)))
  
  # Convert to climatology object
  quantity <- com
  if(!is.null(ref.mask)){
    L = length(ref.grid$xyCoords$x) * length(ref.grid$xyCoords$y)
    mat <- matrix(NA, nrow = 1, ncol = L)  
    mat[mask] <- quantity
  }else{
    mat <- matrix(quantity, nrow = 1)
  }
  ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
  attr(ref.grid$Data, "climatology:fun") <- "Communities"
  memClim <- ref.grid
  
  # Plot
  # ngroups <- length(com.mask)
  # cols <- colorRampPalette(colors = brewer.pal(11, "Spectral"))
  # x11()
  # spatialPlot(ref.grid, backdrop.theme = "coastline", main = "Communities", col.regions = sample(cols(ngroups+1), ngroups, replace = FALSE))
  
  ##############################################################################
  # visualizar communities: evitar problemas con colores
  
  if(ncom <= 9){
    colRainbow <- brewer.pal(ncom,"Set1")
  } else {
    colRainbow <- brewer.pal(9,"Set1")
    colRainbow<- colorRampPalette(colRainbow)(ncom)
  }
  if(ncom > 15){colRainbow <- sample(colRainbow,length(colRainbow))}
  x11()
  spatialPlot(grid = memClim, backdrop.theme = "coastline", 
                       set.min = NULL, set.max = NULL, 
                       lonCenter = 0, 
                       regions = TRUE,col.regions = colRainbow, at = 0:ncom, rev.colors = FALSE, 
                       main = paste0("Comunities"),
                       colorkey = list(col = colRainbow, width = 0.6, at = 0:ncom,
                                       lables = list(cex = 0.5, labels =as.character(0:ncom),at = 0:ncom))
  )
  ############################################################################
  }