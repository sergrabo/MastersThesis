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

plot_communities <- function(comObj, ref.grid, ref.mask = NULL, th = 2, cuts = 0, cor.th = NULL, mute = FALSE) {
  
  ### Debug ###
  # cor.th <- 0.6
  # comObj <- comms[[which.min(abs(ths-cor.th))]]
  # ref.grid <- ba.5deg.std.anom
  # ref.mask <- mask
  # th <- 2
  # cuts <- 25
  # mute  <- FALSE
  
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
    quantity <- com + 0.5 #Se suma 0.5 para que cuadre con el eje de colores (cerdada)
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
    
    set.seed(45412)
    # if(ncom < 3){
    #   colRainbow <- brewer.pal(3, "Set1")[1:ncom]
    # }else if(ncom >= 3 & ncom <= 9){
    #   colRainbow <- brewer.pal(ncom, "Set1")[1:ncom]
    # }else {
    #   colRainbow <- brewer.pal(9,"Set1")
    #   colRainbow<- colorRampPalette(colRainbow)(ncom)
    # }
    # if(ncom > 9){colRainbow <- sample(colRainbow,length(colRainbow))}
    
    #### Definición alternativa de los colores ####
    # Paleta de N_vertex colores
    N_vertex <- comObj$vcount
    colRainbow <- brewer.pal(9,"Set1")
    colRainbow<- colorRampPalette(colRainbow)(N_vertex)
    colRainbow <- sample(colRainbow,length(colRainbow))[1:ncom]
    # Definimos el color de la comunidad ficticia id = 0
    colRainbow[1] = "#F5E9E2"
    # Definimos el color de la comunidad más grande id = 1
    colRainbow[2] = "#E41A1C"
    
    if(mute == FALSE){
      x11()
      # spatialPlot(grid = memClim, backdrop.theme = "coastline",
      #             lonCenter = 0, 
      #             regions = TRUE, col.regions = colRainbow, rev.colors = FALSE, 
      #             main = paste0("Communities"),
      #             colorkey = list(col = colRainbow, width = 0.6, at = 0:(ncom),
      #                             labels = list(cex = 0.5, labels = as.character(0:ncom), at = 0.5:(ncom-0.5))))
      
      spatialPlot(grid = memClim, backdrop.theme = "coastline",
                  lonCenter = 0, 
                  regions = TRUE, col.regions = colRainbow, at = 0:ncom, rev.colors = FALSE,
                  main = bquote("Communities for " ~ tau[c] ~ "=" ~ .(cor.th)))
    }else{
      p <- spatialPlot(grid = memClim, backdrop.theme = "coastline",
                       lonCenter = 0, 
                       regions = TRUE, col.regions = colRainbow, at = 0:ncom, rev.colors = FALSE,
                       main = bquote("Communities for " ~ tau[c] ~ "=" ~ .(cor.th)))
      return(p)
    }
  }
  
  ############################################################################
}