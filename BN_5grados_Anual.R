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
ba.5deg.std.anom <- ba.5deg.std.anom.standarize
rm(ba.5deg.std.anom.standarize)
load("./Rdata/mask.Rdata", verbose = TRUE)

#### Area quemada original ####
# Construccion de la red
# load("./Rdata/ba_5deg.Rdata", verbose = TRUE)
# BNobj <- BN_from_grid(grid = ba.5deg, mask = mask)
# save(BNobj, file = "./Rdata/BNobj.Rdata")

# Carga de la red preconstruida
# load("./Rdata/BNobj.Rdata", verbose = TRUE)

#### Anomalias de area quemada ####

# Construccion de la red
# BNobj_anom <- BN_from_grid_anom(grid = ba.5deg.std.anom.standarize, mask = mask)
# save(BNobj_anom, file = "./Rdata/BNobj_anom_standarize.Rdata")

############ Standarized data #############
# Carga de la red preconstruida

# load("./Rdata/BNobj_anom_standarize.Rdata", verbose = TRUE)


# graph_world_BN(BNobj)
# graph_world_BN(BNobj_anom)

# Parametric learning

# bn_fitted <- bn.fit(BNobj_anom$BN, data = BNobj_anom$data)
# BNobj_anom$fit <- bn_fitted

############ Size 2000 Anual #############
load("./Rdata/BN_anom_anual_2000.Rdata", verbose = TRUE)
BN_anom_anual_2k <- firedata_hc_1900_2000i
rm(firedata_hc_1900_2000i)
grid <- ba.5deg.std.anom
data <- array3Dto2Dmat(grid$Data)[,mask] %>% data.frame()
# Parametric learning
bn_fitted <- bn.fit(BN_anom_anual_2k, data = data)

############################################################
# Function propagation of evidence
############################################################
PropagationExactGeneralPerm <- function(baysnet, nodesEvents, valueEvent, nodesEvidence, valueEvidence, compute.without = FALSE, perm = NULL){
  
  # baysnet <- bn_fitted
  # nodesEvents <- 1:645
  # valueEvent <- ">= 1"
  # nodesEvidence <- c(231)
  # valueEvidence <- c(2)
  # perm <- NULL
  
  # baysnet = fitted
  # nodesEvents = c(298,299)
  # valueEvent = ">=1"
  # nodesEvidence = c(81,280)
  # valueEvidence = c(2,2)
  # perm = permutations[[1]]
  
  if(length(nodesEvents) > length(baysnet)){stop("Number of Event Nodes exceds Bayesian Network's size")}
  
  if (is.null(perm)) {nodesEventsRef <- nodesEvents}
  
  with <- numeric(length = length(nodesEvents))
  without <- numeric(length = length(nodesEvents))
  
  if (length(nodesEvidence) == 1) {
    str2 <- paste0("list(X", nodesEvidence[1]," = ", valueEvidence[1], ")")
    probname <- paste0("P(X ", valueEvent,"|",nodesEvidence[1]," = ", valueEvidence[1], ")")
  }
  
  
  if (length(nodesEvidence) > 1){
    proves <- c()
    for (j in 1:length(nodesEvidence)){
      proves[j]<- paste0("X",nodesEvidence[j]," = ", valueEvidence[j])
    }
    
    text <- "list("
    for (j in 1:(length(nodesEvidence)-1)){
      text <- paste0(text,proves[j],",")
    }
    text <- paste0(text,proves[length(nodesEvidence)],")")
    str2 <- text
    
    
    probname <- paste0("P(X ", valueEvent,"|")
    for (j in 1:(length(nodesEvidence)-1)){
      probname <- paste0(probname,proves[j],",")
    }
    probname <- paste0(probname,proves[length(nodesEvidence)],")")
  }
  
  # str2
  # i <- 2
  start.all <- Sys.time()
  print(paste0("Calculating inference for given evidence in node ", nodesEvidence))
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
    
    start.with <- Sys.time()
    with[i] <- eval(parse(text = cmd1))
    end.with <- Sys.time()
    print(paste0("Elapsed time for with: ", difftime(end.with, start.with, units = "secs") %>% round(2)," seconds", " on node ", l))
    with[i]
    
    if(compute.without == TRUE){
      start.without <- Sys.time()
      without[i] <- eval(parse(text = cmd3))
      end.without <- Sys.time()
      print(paste0("Elapsed time for without: ", difftime(end.without, start.without, units = "secs") %>% round(2)," seconds", " on node ", l))
      without[i]
    }else{without[i] <- 0.158}
    
    # with[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = eval(parse(text = str2)))
    # withcomplement[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = eval(parse(text = str3)))
    # without[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = TRUE)
    
  }
  end.all <- Sys.time()
  print(paste0("Elapsed time for all nodes: ", difftime(end.all, start.all, units = "hours") %>% round(2), " hours"))
  
  attr(with, "probability") <- probname
  attr(without, "probability") <- paste0("P(X ", valueEvent,")")
  df <- data.frame(names = names(baysnet)[nodesEventsRef], with = with, without = without)
  return(df)
  
}

####################################################################################
# Propagation V81
####################################################################################
node.evidence <- c(185)
#################################################################################
# Single positive evidence. Probability on positive deviation (V81 (+ +) )
#################################################################################
df.pos <- PropagationExactGeneralPerm(baysnet = bn_fitted,
                                     nodesEvents = 1:645,
                                     valueEvent = ">= 1",
                                     nodesEvidence = node.evidence,
                                     valueEvidence = c(2),
                                     compute.without = FALSE,
                                     perm = NULL)
  


#################################################################################
# Single negative evidence. Probability on negative deviation (V81 (+ -))
#################################################################################
df.neg <- PropagationExactGeneralPerm(baysnet = bn_fitted,
                                      nodesEvents = 1:645,
                                      valueEvent = "<= -1",
                                      nodesEvidence = node.evidence,
                                      valueEvidence = c(2),
                                      compute.without = FALSE,
                                      perm = NULL)

df.pos %<>% mutate(diff = (with-without))
df.neg %<>% mutate(diff = (with-without))

write.csv(df.pos, file = paste0("./inference/X", as.character(node.evidence), "_posInference_2k_anual.csv"))
write.csv(df.neg, file = paste0("./inference/X", as.character(node.evidence), "_negInference_2k_anual.csv"))

######### Plot Inferencia Positiva #################
node <- 185 #231, 541, 185, 572
sign <- "pos"
# sign <- "neg"
df <- read.csv(file = paste0("./inference/X", node, "_", sign, "Inference_2k_anual.csv"))

quantity <- df$diff
ref.grid <- ba.5deg.std.anom
ref.mask <- mask
if(!is.null(ref.mask)){
  L = length(ref.grid$xyCoords$x) * length(ref.grid$xyCoords$y)
  mat <- matrix(NA, nrow = 1, ncol = L)  
  mat[mask] <- quantity
}else{
  mat <- matrix(quantity, nrow = 1)
}
ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
out <- ref.grid
x11()
if(sign == "pos"){
  spatialPlot(out, backdrop.theme = "coastline", main = "Bayesian network inference", color.theme = "Reds", ylim=c(-75,90), at = seq(0.05, 0.85, by = 0.05))
}else if (sign == "neg"){
  spatialPlot(out, backdrop.theme = "coastline", main = "Bayesian network inference", color.theme = "Blues", ylim=c(-75,90), at = seq(0.05, 0.85, by = 0.05))
}else{
  print("Wrong sign")
}


####################################################



######### Fragmento de código para identificar un único nodo en el mapa #################
node.id <- 541
single.node <- rep(1,645)
single.node[node.id] <- -1
# Convert to climatology object
quantity <- single.node 
ref.grid <- ba.5deg.std.anom
ref.mask <- mask
if(!is.null(ref.mask)){
  L = length(ref.grid$xyCoords$x) * length(ref.grid$xyCoords$y)
  mat <- matrix(NA, nrow = 1, ncol = L)  
  mat[mask] <- quantity
}else{
  mat <- matrix(quantity, nrow = 1)
}
ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
out <- ref.grid
x11()
spatialPlot(out, backdrop.theme = "coastline", color.theme = "RdYlGn", ylim=c(-75,90))
#########################################################################################


