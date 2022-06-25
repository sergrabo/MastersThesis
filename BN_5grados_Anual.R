########## Inicializacion del script ##########

# Eliminamos las posibles variables de una sesion anterior
rm(list = ls())

# Paquetes de climate4R <https://github.com/SantanderMetGroup/climate4R>
library(transformeR)
library(visualizeR)

# Paquetes redes y visualizacion
library(graph)
library(igraph)
library(bnlearn)

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

# Cargamos los datos ya calculados, para evitar problemas de memoria
load("./Rdata/ba5degAnom.Rdata", verbose = TRUE)
load("./Rdata/mask.Rdata", verbose = TRUE)

#### Area quemada original ####
# Construccion de la red
load("./Rdata/ba_5deg.Rdata", verbose = TRUE)
BNobj <- BN_from_grid(grid = ba.5deg, mask = mask)
save(BNobj, file = "./Rdata/BNobj.Rdata")

# Carga de la red preconstruida
load("./Rdata/BNobj.Rdata", verbose = TRUE)

#### Anomalias de area quemada ####
# Construccion de la red
BNobj_anom <- BN_from_grid_anom(grid = ba.5deg.std.anom, mask = mask)
save(BNobj_anom, file = "./Rdata/BNobj_anom.Rdata")

# Carga de la red preconstruida
load("./Rdata/BNobj_anom.Rdata", verbose = TRUE)

graph_world_BN(BNobj)
graph_world_BN(BNobj_anom)
