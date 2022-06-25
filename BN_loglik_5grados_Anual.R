##################################################################################
########## Inicializacion del script ##########

# Eliminamos las posibles variables de una sesion anterior
rm(list = ls())

# Paquetes de climate4R <https://github.com/SantanderMetGroup/climate4R>
library(transformeR)

# Paquetes redes y visualizacion
library(graph)
library(igraph)
library(bnlearn)

# Directorio de trabajo
setwd("C:/Users/sergr/Dropbox/TFM_Sergio_Gracia")
#setwd("/home/juaco/Dropbox/TFM_Sergio_Gracia")

# Cargamos los datos ya calculados, para evitar problemas de memoria
load("./Rdata/ba5degAnom.Rdata", verbose = TRUE)
load("./Rdata/ba_5deg.Rdata", verbose = TRUE)
load("./Rdata/mask.Rdata", verbose = TRUE)

# Seleccionar grid a estudiar
anom = TRUE

if(anom == FALSE){
  base_dir <- "./BN_phases_raw"
  grid <- ba.5deg
  time.coords.matrix <- array3Dto2Dmat(grid$Data)[,mask]
  burned.nodes <- which(apply(time.coords.matrix, MARGIN = 2, FUN = mean) != 0)
  time.coords.matrix <- time.coords.matrix[ , burned.nodes] %>% log1p()
} else{
  base_dir <- "./BN_phases_anom"
  grid <- ba.5deg.std.anom
  time.coords.matrix <- array3Dto2Dmat(grid$Data)[,mask]
  burned.nodes <- which(apply(time.coords.matrix, MARGIN = 2, FUN = mean) != 0)
  time.coords.matrix <- time.coords.matrix[ , burned.nodes]
}

data <- time.coords.matrix %>% data.frame()


##################################################################################
# load HC iteration data and make list
##################################################################################
hc_list <- list.files(base_dir, full.names = T)
hc_names <- list.files(base_dir)
hc_names <- gsub(".Rdata", "", hc_names)

hc_networks <- list()

for (i in 1:length(hc_list)){
  hc_networks[[i]] <- get(load(hc_list[i]))
}

# Remove loaded data
rm(list = hc_names)

names(hc_networks) <- hc_names
sizes <- sapply(hc_networks,narcs)
hc_networks <- hc_networks[order(sizes)]

####################################################################################
# hc log-likelihood per iteration analyse
####################################################################################

hc_fits <- lapply(hc_networks, bn.fit, data = data)

logliks_hc <- sapply(X = hc_fits, logLik,  data = data)
nedges_hc <- sapply(hc_networks, narcs)

plot(nedges_hc, logliks_hc, col = "blue")
###################################################################################