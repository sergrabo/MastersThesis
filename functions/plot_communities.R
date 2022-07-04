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

plot_communities <- function(comObj, ref.grid, ref.mask = NULL, th = 2, cuts = 0 , mute = FALSE) {
  
  if(!is.null(comObj)){
    
    # Mínimo valor de cuts para que no salga warning
    min.cuts <- comObj$vcount - nrow(comObj$merges)
    com <- cut_at(comObj, min.cuts + cuts)
    # com <- membership(comObj)  
    com.sizes <- table(com)

    # Choose communities with population bigger than the threshold
    com.mask <- as.integer(names(com.sizes[which(com.sizes>=th)]))
    
    com <- match(com, com.mask)
    com[is.na(com)] <- 0 # Comunidades eliminadas pertenecen a una ficticia
    
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

    ##############################################################################
    # visualizar communities: evitar problemas con colores
    
    set.seed(44)
    if(ncom < 3){
      colRainbow <- c("", "#ff0000") # c(blank (fictional comm), red)
    }else if(ncom >= 3 & ncom <= 9){
      colRainbow <- brewer.pal(ncom, "Set1")
    }else {
      colRainbow <- brewer.pal(9,"Set1")
      colRainbow<- colorRampPalette(colRainbow)(ncom)
    }
    if(ncom > 15){colRainbow <- sample(colRainbow,length(colRainbow))}
    
    colRainbow[1] = "#F5E9E2" # Definimos el color de la comunidad ficticia
    print(colRainbow)
    
    if(mute == FALSE){
      x11()
      spatialPlot(grid = memClim, backdrop.theme = "coastline",
                  lonCenter = 0, 
                  regions = TRUE, col.regions = colRainbow, rev.colors = FALSE, 
                  main = paste0("Comunities"),
                  colorkey = list(col = colRainbow, width = 0.6, at = 0:(ncom-1),
                                  labels = list(cex = 0.5, labels = as.character(1:ncom), at = 0.5:(ncom-0.5))))
    }else{
      p <- spatialPlot(grid = memClim, backdrop.theme = "coastline",
                       lonCenter = 0, 
                       regions = TRUE, col.regions = colRainbow, rev.colors = FALSE, 
                       main = paste0("Comunities"),
                       colorkey = list(col = colRainbow, width = 0.6, at = 0:(ncom-1),
                                       labels = list(cex = 0.5, labels = as.character(1:ncom), at = 0.5:(ncom-0.5))))
      return(p)
    }
  }
  
  ############################################################################
}