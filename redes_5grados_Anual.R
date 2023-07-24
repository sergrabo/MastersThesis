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

cor.th <- 0.7 # Correlation Threshold
dist.th <- 1000 # Distance threshold (km)
res <- 5 / 0.25 # Resolucion

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
