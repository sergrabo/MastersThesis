########## Inicializacion del script ##########

# Eliminamos las posibles variables de una sesion anterior
rm(list = ls())

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
library(gridExtra)

# Directorio de trabajo
setwd("C:/Users/sergr/Dropbox/TFM_Sergio_Gracia")
#setwd("/home/juaco/Dropbox/TFM_Sergio_Gracia")

# Cargar funciones definidas por mi
sapply(list.files("./scripts/MastersThesis/functions/", full.names = TRUE), "source", .GlobalEnv)

########## Carga y preparacion de los datos ##########

# Parametros del modelo #

cor.th <- 0.65 # Correlation Threshold
dist.th <- 1000 # Distance threshold (km)
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
# time.coords.matrix <- array3Dto2Dmat(ba.5deg.std.anom$Data)
# mask <- which(apply(time.coords.matrix, MARGIN = 2, FUN = mean) != 0 & fba.vec > 0.1)

# Cargamos los datos ya calculados, para evitar problemas de memoria
load("./Rdata/ba5degAnom.Rdata", verbose = TRUE)
load("./Rdata/mask.Rdata", verbose = TRUE)

########## Calculo de las redes complejas ##########
unweighted.net <- Graph_from_Grid(ba.5deg.std.anom, cor.th = cor.th, dist.th = dist.th, mask = mask)
weighted.net <- Graph_from_Grid(ba.5deg.std.anom, cor.th = cor.th, dist.th = dist.th, mask = mask, weighted = TRUE)

########## Representacion de redes complejas ##########

### Red compleja no pesada ###
graph_world_network(unweighted.net)

### Red compleja pesada ###
graph_world_network(weighted.net)


# Plot distance vs correlation
plot_dist_corr(weighted.net)


########## Metricas de redes complejas ##########


# Plot measures
plot_measures(weighted.net)

########## Estudio de clustering en la red ##########

compute_communities(unweighted.net, ref.grid = ba.5deg.std.anom, ref.mask = mask, th = 7)