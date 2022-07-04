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
# load("./Rdata/ba5degAnom.Rdata", verbose = TRUE)
load("./Rdata/ba5deg_anom_standarize.Rdata", verbose = TRUE)
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
# BNobj_anom <- BN_from_grid_anom(grid = ba.5deg.std.anom, mask = mask)
# save(BNobj_anom, file = "./Rdata/BNobj_anom.Rdata")

# Construccion de la red
BNobj_anom <- BN_from_grid_anom(grid = ba.5deg.std.anom.standarize, mask = mask)
save(BNobj_anom, file = "./Rdata/BNobj_anom_standarize.Rdata")

# Carga de la red preconstruida
load("./Rdata/BNobj_anom_standarize.Rdata", verbose = TRUE)

graph_world_BN(BNobj)
graph_world_BN(BNobj_anom)

############################################################
# Function propagation of evidence
############################################################
PropagationExactGeneralPerm <- function(baysnet, nodesEvents, valueEvent, nodesEvidence, valueEvidence, perm = NULL){
  
  # baysnet <- fitted
  # nodesEvents <- c(81,280)
  # valueEvent <- ">= 1"
  # nodesEvidence <- c(81,280)
  # valueEvidence <- c(2,2)
  # dataperm <- datapermutations[[1]]
  # perm <- permutations[[1]]
  
  # baysnet = fitted
  # nodesEvents = c(298,299)
  # valueEvent = ">=1"
  # nodesEvidence = c(81,280)
  # valueEvidence = c(2,2)
  # perm = permutations[[1]]
  
  
  if (is.null(perm)) {nodesEventsRef <- nodesEvents} 
  else {
    nodesEventsRef <- c()
    for (i in 1:length(nodesEvents)){
      nodesEventsRef[i] <- which(perm == nodesEvents[i])
    }
  }
  
  with <- numeric(length = length(nodesEvents))
  without <- numeric(length = length(nodesEvents))
  
  if (length(nodesEvidence) == 1) {
    # Cambiar V por X
    str2 <- paste0("list(V", nodesEvidence[1]," = ", valueEvidence[1], ")")
    probname <- paste0("P(V ", valueEvent,"|",nodesEvidence[1]," = ", valueEvidence[1], ")")
  }
  
  
  if (length(nodesEvidence) > 1){
    proves <- c()
    for (j in 1:length(nodesEvidence)){
      proves[j]<- paste0("V",nodesEvidence[j]," = ", valueEvidence[j])
    }
    
    text <- "list("
    for (j in 1:(length(nodesEvidence)-1)){
      text <- paste0(text,proves[j],",")
    }
    text <- paste0(text,proves[length(nodesEvidence)],")")
    str2 <- text
    
    
    probname <- paste0("P(V ", valueEvent,"|")
    for (j in 1:(length(nodesEvidence)-1)){
      probname <- paste0(probname,proves[j],",")
    }
    probname <- paste0(probname,proves[length(nodesEvidence)],")")
  }
  
  # str2
  # i <- 2
  
  for(i in 1:length(nodesEvents)) {
    # l <- nodesEvents[i]
    # l
    l <- nodesEventsRef[i]
    # str <- paste0("(", names(baysnet)[l], ">=", valueEvent, ")")
    str <- paste0("(", names(baysnet)[l], valueEvent, ")")
    str
    nparams(baysnet)
    cmd1 = paste0("cpquery(baysnet, ", str, ", ", str2, ", method = ","'lw'",")")
    cmd3 = paste0("cpquery(baysnet, ", str, ", ", "TRUE", ", method = ","'lw'",")")
    cmd1
    cmd3
    
    with[i] <- eval(parse(text = cmd1))
    with[i]
    
    without[i] <- eval(parse(text = cmd3))
    without[i]
    
    
    # with[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = eval(parse(text = str2)))
    # withcomplement[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = eval(parse(text = str3)))
    # without[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = TRUE)
    
  }
  
  attr(with, "probability") <- probname
  attr(without, "probability") <- paste0("P(V ", valueEvent,")")
  df <- data.frame(names = names(baysnet)[nodesEventsRef], with = with, without = without)
  return(df)
  
}



#####################################################################################
# Propagation of Evidence Runs Bayesian Networks
#####################################################################################
rm(list = ls())
library(transformeR)
library(magrittr)
library(bnlearn)


load("/oceano/gmeteo/WORK/lisette/Trabajo/R_practice/Data/interim/tas_interim_10dnew.rda")

####################################################################################
# load HC interim iteration data permutation 1 and make list
####################################################################################
whichperm <- 3
for(j in c(whichperm)){
  pattern <- paste0("int_hc",whichperm,"_")
  hc_interim_list <- list.files(paste0("/oceano/gmeteo/WORK/lisette/Trabajo/R_practice/Data/interim_struct/hciterations/perm",whichperm), full.names = T, pattern = pattern)
  hc_interim_names <- list.files(paste0("/oceano/gmeteo/WORK/lisette/Trabajo/R_practice/Data/interim_struct/hciterations/perm",whichperm), pattern = pattern)
  hc_interim_names <- gsub(".rda", "", hc_interim_names)
  
  hc_interim_networks <- list()
  
  for (i in 1:length(hc_interim_list)){
    object <- get(load(hc_interim_list[i]))
    hc_interim_networks[[i]] <- object
  }
}
names(hc_interim_networks) <- hc_interim_names
interimsizes <- sapply(hc_interim_networks,narcs)
hc_interims <- hc_interim_networks[order(interimsizes)]
nedges_int_hc <- sapply(hc_interims, narcs)
####################################################################################
# hc interim iteration analyse
####################################################################################3
gridused <- tas_interim_10dnew
# data_int_RMS <- as.data.frame(TimeCoordsAnom_from_Grid_rms(gridused, rms = TRUE))
hc_interim_fits <- lapply(hc_interims, bn.fit, data = data_int_RMS)


####################################################################################
# Propagation V81
####################################################################################
#################################################################################
# Single positive evidence. Probability on positive deviation (V81 (+ +) )
#################################################################################
# sizes <- c(18,length(hc_interims)-1,10,26) perm1
# sizes <- c(17,18,length(hc_interims)-1,10,26)

x <- seq(0,9000,100)
y <- seq(100,9100,100)

for (i in sizes){
  
  assign(paste0("prop_int_hc_",x[i],"_",y[i],"i_V81_equal2"),
         PropagationExactGeneralPerm(baysnet = hc_interim_fits[[i]],
                                     nodesEvents = 1:648,
                                     valueEvent = ">= 1",
                                     nodesEvidence = c(81),
                                     valueEvidence = c(2),
                                     perm = permutations[[whichperm]]))
  save(list = paste0("prop_int_hc_",x[i],"_",y[i],"i_V81_equal2"),
       file = paste0("/oceano/gmeteo/WORK/lisette/Trabajo/R_practice/Data/interim_propagation/hc",whichperm,"_iteration/posV81pos/prop_int_hc_",x[i],"_",y[i],"i_V81_equal2.rda"))
  
}

#################################################################################
# Single negative evidence. Probability on negative deviation (V81 (+ -))
#################################################################################
# sizes <- c(18,length(hc_interims)-1,10,26) perm1
sizes <- c(17,18,length(hc_interims)-1,10,26)
for (i in sizes){
  
  assign(paste0("propneg_int_hc_",x[i],"_",y[i],"i_V81_equal2"),
         PropagationExactGeneralPerm(baysnet = hc_interim_fits[[i]],
                                     nodesEvents = 1:648,
                                     valueEvent = "<= -1",
                                     nodesEvidence = c(81),
                                     valueEvidence = c(2),
                                     perm = permutations[[whichperm]]))
  save(list = paste0("propneg_int_hc_",x[i],"_",y[i],"i_V81_equal2"),
       file = paste0("/oceano/gmeteo/WORK/lisette/Trabajo/R_practice/Data/interim_propagation/hc",whichperm,"_iteration/posV81neg/propneg_int_hc_",x[i],"_",y[i],"i_V81_equal2.rda"))
  
}
