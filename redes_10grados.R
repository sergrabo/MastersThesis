########## Inicializacion del script ##########

# Paquetes de climate4R <https://github.com/SantanderMetGroup/climate4R>
library(transformeR)
library(visualizeR)

# Paquetes redes y visualizacion
library(igraph)

# Otros paquetes
library(magrittr) # Carga el "pipe operator" %>% 
library(ggplot2)
library(dplyr)
library(tidyr)

# Eliminamos las posibles variables de una sesion anterior
rm(list = ls())

# Directorio de trabajo
setwd("C:/Users/sergr/Dropbox/TFM_Sergio_Gracia")
#setwd("/home/juaco/Dropbox/TFM_Sergio_Gracia")

########## Carga y preparacion de los datos ##########

# Parámetros del modelo #

th <- 0.5 # Threshold
res <- 10 / 0.25 # Resolucion


# Cargamos los datos pre-adaptados
load("./Rdata/MODIS_OLCI_ba_200101-202004.Rdata", verbose = TRUE)
load("./Rdata/MODIS_OLCI_fba_200101-202004.Rdata", verbose = TRUE)

# ba.merge es el dataset original a 0.25 grados de resolucion espacial
str(ba.merge)
#getShape(ba.merge)
#getRefDates(ba.merge)

# Proceso de Upscaling: pasamos los datos de area quemada a una rejilla regular de 10 grados usando un metodo conservativo
ba.10deg <- upscaleGrid(ba.merge, times = res, aggr.fun = list(FUN = "sum", na.rm = TRUE)) %>% redim(drop = TRUE)
fba.10deg <- upscaleGrid(fba.merge, times = res, aggr.fun = list(FUN = "mean", na.rm = TRUE)) %>% redim(drop = TRUE)

# Calculamos anomalias estandarizadas para que no influya la "magnitud" del area quemada en el caclulo de correlaciones
ba.10deg.std.anom <- scaleGrid(ba.10deg, time.frame = "monthly", spatial.frame = "gridbox")#, type = "standardize")

# Funciones para dibujar mapas
spatialPlot(climatology(ba.10deg), backdrop.theme = "coastline")
spatialPlot(climatology(ba.10deg.std.anom), backdrop.theme = "coastline")
# temporalPlot(ba.10deg.std.anom)


########## Calculo de la matriz de correlaciones ##########

# Aplicamos una capa para filtrar según el area que puede quemarse (fraction of burnable area)
fba.clim <- climatology(fba.10deg)
fba.vec <- array3Dto2Dmat(fba.clim$Data)[1,]
mask <- which(fba.vec > 0.1)

# Transformamos el array3D a matriz2D
mat2d <- array3Dto2Dmat(ba.10deg.std.anom$Data)
coords <- getCoordinates(ba.10deg.std.anom)
coords.2d <- expand.grid(coords$y,coords$x)[mask,2:1] %>% setNames(c("lon", "lat"))
coords.2d$id <- 1:nrow(coords.2d) # Añadimos id para posterior merge

# Matriz de correlaciones
?cor.test #Pendiente cambiar cor por cor.test
cormat <- cor(mat2d[,mask], method = "spearman") %>% abs()
x11()
fields::image.plot(cormat)

########## Construccion de redes complejas ##########


### Red compleja pesada ###
adj.mat <- cormat

adj.mat[which(cormat < th)] <- 0
adj.mat[is.na(cormat)] <- 0


weighted.net <- graph_from_adjacency_matrix(adj.mat, weighted = TRUE, mode = "undirected", diag = FALSE)

print_all(weighted.net)

### Red compleja no pesada ###

adj.mat <- matrix(data = NA, nrow = nrow(cormat), ncol = ncol(cormat))

adj.mat[which(cormat < th)] <- 0
adj.mat[which(cormat >= th)] <- 1

unweighted.net <- graph_from_adjacency_matrix(adj.mat, mode = "undirected", diag = FALSE)

print_all(unweighted.net)


########## Representacion de redes complejas ##########

#https://www.r-bloggers.com/2018/05/three-ways-of-visualizing-a-graph-on-a-map/

### Red compleja no pesada ###

# Guardamos todos los links de la red en formato "from-to" indicando de un id a otro
edges <- get.edgelist(unweighted.net) %>% data.frame() %>% setNames(c("from", "to"))

# Añadimos a los links la informacion de las coordenadas de cada id
edges_for_plot <- edges %>%
  inner_join(coords.2d %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(coords.2d %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

# Tema del mapa 
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

# Background del mapamundi
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

# Plot
ggplot(coords.2d) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.5, col = "blue") +
  mapcoords + maptheme

### Red compleja pesada ###

# Guardamos todos los links de la red en formato "from-to" indicando de un id a otro
edges <- get.edgelist(weighted.net) %>% data.frame() %>% setNames(c("from", "to"))
edges$weight <- E(weighted.net)$weight

# Añadimos a los links la informacion de las coordenadas de cada id
edges_for_plot <- edges %>%
  inner_join(coords.2d %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(coords.2d %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

# Tema del mapa 
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

# Background del mapamundi
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

# Plot
ggplot(coords.2d) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend, color = weight),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.5) +
  mapcoords + maptheme

########## Metricas de redes complejas ##########

# Funcion generica para pasar una cantidad a objeto Climatology
quantity2clim <- function(quantity, ref.grid, ref.mask = NULL, what, backperm = NULL) {
  if(!is.null(backperm)){quantity <- quantity[backperm]}
  if(!is.null(ref.mask)){
    L = length(ref.grid$xyCoords$x) * length(ref.grid$xyCoords$y)
    mat <- matrix(NA, nrow = 1, ncol = L)  
    mat[mask] <- quantity
  }else{
    mat <- matrix(quantity, nrow = 1)
  }
  ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
  attr(ref.grid$Data, "climatology:fun") <- what
  return(ref.grid)
}

### Red compleja no pesada ###

# Distribución de grado
K <- degree(unweighted.net, loops = FALSE)

climK <- quantity2clim(K, ref.grid = ba.10deg.std.anom, ref.mask = mask, what = "degree")

spatialPlot(climK, backdrop.theme = "coastline", main = "Distribución de grado")

# Closeness
C <- closeness(unweighted.net, normalized = FALSE)

climC <- quantity2clim(C, ref.grid = ba.10deg.std.anom, ref.mask = mask, what = "closeness")

spatialPlot(climC, backdrop.theme = "coastline", main = "Closeness")

# Betweenness
B <- betweenness(unweighted.net, directed = FALSE)

climB <- quantity2clim(B, ref.grid = ba.10deg.std.anom, ref.mask = mask, what = "betweenness")

spatialPlot(climB, backdrop.theme = "coastline", main = "Betweenness")

# Eigenvalues
E <- spectrum(unweighted.net, algorithm = c("arpack"))

climE <- quantity2clim(E$vectors, ref.grid = ba.10deg.std.anom, ref.mask = mask, what = "eigenvalues")

spatialPlot(climE, backdrop.theme = "coastline", main = "Eigenvalues")



########## Estudio de clustering en la red ##########
ceb <- cluster_edge_betweenness(unweighted.net, directed = FALSE)

com.mask <- as.integer(dimnames(sizes(ceb)[which(sizes(ceb)>5)])$`Community sizes`)
com <- membership(ceb)

com <- ifelse(com %in% com.mask, com, NA)

climcom <- quantity2clim(com, ref.grid = ba.10deg.std.anom, ref.mask = mask, what = "eigenvalues")

spatialPlot(climcom, backdrop.theme = "coastline", main = "Communities")

# ¿¿Calcular distancias??
library(sp)
?spDistsN1
?diag
spDistsN1(cbind(coords.2d$lon, coords.2d$lat), c(40, -35))
