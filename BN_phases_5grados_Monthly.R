########## Inicializacion del script ##########

# Eliminamos las posibles variables de una sesion anterior
rm(list = ls())
   
# Paquetes de climate4R <https://github.com/SantanderMetGroup/climate4R>
library(transformeR)

# Paquetes redes y visualizacion
# library(graph)
library(igraph)
library(bnlearn)

# Directorio de trabajo
setwd("C:/Users/sergr/Dropbox/TFM_Sergio_Gracia")
#setwd("/home/juaco/Dropbox/TFM_Sergio_Gracia")

# Cargamos los datos ya calculados, para evitar problemas de memoria
load("./Rdata/ba_5deg_anom_monthly.Rdata", verbose = TRUE)
load("./Rdata/ba_5deg_monthly.Rdata", verbose = TRUE)
load("./Rdata/mask.Rdata", verbose = TRUE)

##################################################################################
# train BN through Hill-Climbing and save in files.
##################################################################################
# Seleccionar grid a estudiar
anom = TRUE

if(anom == FALSE){
  print("Training Bayesian Network for raw data")
  base_dir <- "./BN_phases_raw"
  grid <- ba.5deg.monthly
  time.coords.matrix <- array3Dto2Dmat(grid$Data)[,mask]
  burned.nodes <- which(apply(time.coords.matrix, MARGIN = 2, FUN = mean) != 0)
  time.coords.matrix <- time.coords.matrix[ , burned.nodes] %>% log1p()
} else{
  print("Training Bayesian Network for anomalies data")
  base_dir <- "./BN_phases_anom"
  grid <- ba.5deg.std.anom.monthly
  time.coords.matrix <- array3Dto2Dmat(grid$Data)[,mask]
  burned.nodes <- which(apply(time.coords.matrix, MARGIN = 2, FUN = mean) != 0)
  time.coords.matrix <- time.coords.matrix[ , burned.nodes]
}

data <- time.coords.matrix %>% data.frame()
# set.seed(4)
# ind <- sample(seq(1, nrow(data)), nrow(data), replace = FALSE)
# f <- 0.7
# ind.train <- ind[1:floor(length(ind)*f)] %>% sort()
# d.train <- data[ind.train, ]

start <- NULL
steps <- 100
last <- 10000

total.ini <- Sys.time()

for (m in 0:floor(last/steps)) {
  t.ini <- Sys.time()
  
  i <- m*steps
  j <- i + steps
  BN <- hc(data, max.iter = steps, score = "bic-g", start = start)
  var_name <- paste0("firedata_hc_",i,"_",j,"i")
  assign(var_name, BN)
  
  origin <- ifelse(anom == FALSE, "raw", "anom")
  
  save(list = var_name, 
       file = paste0("BN_phases_", origin, "/", var_name, ".Rdata"))
  rm(list = var_name)
  
  t.end <- Sys.time()
  print(paste0("Elapsed time: ", t.end-t.ini))
  
  if(m==0){
    start <- BN
  } else if(narcs(BN) == narcs(start)){ # esto lo hice porque queria que acabase cuando no anadiese mas links (i.e. no queria que el algoritmo cambiase direciones)
    break
  } else {start <- BN}
}

total.end <- Sys.time()
print(paste0("Total elapsed time: ", total.end-total.ini))
