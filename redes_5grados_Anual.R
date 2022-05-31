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
library(RColorBrewer)
library(sp)

# Eliminamos las posibles variables de una sesion anterior
rm(list = ls())

# Directorio de trabajo
setwd("C:/Users/sergr/Dropbox/TFM_Sergio_Gracia")
#setwd("/home/juaco/Dropbox/TFM_Sergio_Gracia")

########## Carga y preparacion de los datos ##########

# Parametros del modelo #

th <- 0.65 # Threshold
res <- 5 / 0.25 # Resolucion

# # Cargamos los datos pre-adaptados
# load("./Rdata/MODIS_OLCI_ba_200101-202010.Rdata", verbose = TRUE)
# load("./Rdata/MODIS_OLCI_fba_200101-202010.Rdata", verbose = TRUE)
# 
# # ba.merge es el dataset original a 0.25 grados de resolucion espacial
# str(ba.merge)
# #getShape(ba.merge)
# #getRefDates(ba.merge)
# 
# # Agregamos anualmente
# ba.merge <- subsetGrid(ba.merge, years = 2001:2019)
# ba.merge <- aggregateGrid(ba.merge, aggr.y = list(FUN = "sum"))
# 
# # Proceso de Upscaling: pasamos los datos de area quemada a una rejilla regular de 5 grados usando un metodo conservativo
# ba.5deg <- upscaleGrid(ba.merge, times = res, aggr.fun = list(FUN = "sum", na.rm = TRUE)) %>% redim(drop = TRUE)
# fba.5deg <- upscaleGrid(fba.merge, times = res, aggr.fun = list(FUN = "mean", na.rm = TRUE)) %>% redim(drop = TRUE)
# 
# 
# # Calculamos anomalias estandarizadas para que no influya la "magnitud" del area quemada en el caclulo de correlaciones
# ba.5deg.std.anom <- scaleGrid(ba.5deg, spatial.frame = "gridbox") %>% redim(drop = TRUE)
# 
# # Funciones para dibujar mapas
# spatialPlot(climatology(ba.5deg), backdrop.theme = "coastline")
# spatialPlot(climatology(ba.5deg.std.anom), backdrop.theme = "coastline")
# # temporalPlot(ba.10deg.std.anom)

# # Aplicamos una capa para filtrar segun el area que puede quemarse (fraction of burnable area)
# fba.clim <- climatology(fba.5deg) # %>% redim(drop = TRUE) -> No necesario en este caso
# fba.vec <- array3Dto2Dmat(fba.clim$Data)[1,]
# mask <- which(fba.vec > 0.1)

# Cargamos los datos ya calculados, para evitar problemas de memoria
load("./Rdata/ba5degAnom.Rdata", verbose = TRUE)
load("./Rdata/mask.Rdata", verbose = TRUE)

########## Calculo de las redes complejas ##########
source("scripts/MastersThesis/functions/Graph_from_Grid.R")

unweighted.net <- Graph_from_Grid(ba.5deg.std.anom, th = th, mask = mask)
weighted.net <- Graph_from_Grid(ba.5deg.std.anom, th = th, mask = mask, weighted = TRUE)

########## Representacion de redes complejas ##########


#https://www.r-bloggers.com/2018/05/three-ways-of-visualizing-a-graph-on-a-map/

source("scripts/MastersThesis/functions/graph_world_network.R")

### Red compleja no pesada ###
graph_world_network(unweighted.net)

### Red compleja pesada ###
graph_world_network(weighted.net)


### Mapa en polares ###

# # remotes::install_github("EarthSystemDiagnostics/grfxtools")
# library(grfxtools)
# library(rgeos)

# Plot
# curve_data <- geom_curve(aes(x = x, y = y, xend = xend, yend = yend),
#                          data = edges_for_plot, curvature = 0.33,
#                          alpha = 0.5, col = "blue")
# 
# ggpolar(pole = "N", max.lat = 90, min.lat = 0, data.layer = curve_data)
  


########## Metricas de redes complejas ##########

# Cargamos funcion graph2measure
source("scripts/MastersThesis/functions/graph2measure.R")
# measures.5deg <- graph2measure(unweighted.net)
measures.5deg <- graph2measure(weighted.net)

# Cargamos función quantity2clim
source("scripts/MastersThesis/functions/quantity2clim.R")
clim <- quantity2clim(measures.5deg, ref.grid = ba.5deg.std.anom, ref.mask = mask)

dev.new()
display.brewer.all()

spatialPlot(clim$degree, backdrop.theme = "coastline", main = "Distribución de grado", color.theme = "YlOrRd")
spatialPlot(clim$betweenness, backdrop.theme = "coastline", main = "Betweenness", color.theme = "YlGnBu", at = seq(0,10000,1000))
spatialPlot(clim$dist_strength, backdrop.theme = "coastline", main = "Distance-based strength", color.theme = "PuRd")
spatialPlot(clim$awconnectivity, backdrop.theme = "coastline", main = "Area Weighted Connectivity", color.theme = "RdPu")
spatialPlot(clim$cor_strength, backdrop.theme = "coastline", main = "Correlation-based strength", color.theme = "RdPu")#PuBu
spatialPlot(clim$mean_dist_per_node, backdrop.theme = "coastline", main = "Mean link distance per node", color.theme = "PuRd")

source("scripts/MastersThesis/functions/measure2plot.R")
p <- measure2plot()

########## Estudio de clustering en la red ##########
ceb <- cluster_edge_betweenness(unweighted.net$graph, directed = FALSE)

com.mask <- as.integer(dimnames(sizes(ceb)[which(sizes(ceb)>=4)])$`Community sizes`)
com <- membership(ceb)

com <- ifelse(com %in% com.mask, com, NA)

climcom <- quantity2clim(com, ref.grid = ba.5deg.std.anom, ref.mask = mask, what = "eigenvalues")

ngroups <- length(com.mask)
cols <- colorRampPalette(colors = brewer.pal(11, "Spectral"))
spatialPlot(climcom, backdrop.theme = "coastline", main = "Communities", col.regions = sample(cols(ngroups+1), ngroups, replace = FALSE))

