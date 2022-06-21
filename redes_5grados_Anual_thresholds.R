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

# Cargamos los datos ya calculados, para evitar problemas de memoria
load("./Rdata/ba5degAnom.Rdata", verbose = TRUE)
load("./Rdata/mask.Rdata", verbose = TRUE)

################# Estudio por thresholds ########################

# Define thresholds to load
ths <- seq(0, 0.95, by = 0.05)
# Define results path
res.path <- "./results5degAn"

all_data <- load_th_data(res.path, ths)

# Separate into net data and communities data
unwnet <- list()
wnet <- list()
comms <- list()

# Attributes list
atts <- list()
atts <- attributes(all_data[[1]][[1]])[-(1:5)]
for(i in 1:length(ths)){
  unwnet[[i]] <- all_data[[i]][[1]]
  wnet[[i]] <- all_data[[i]][[2]]
  comms[[i]] <- all_data[[i]][[3]]
  
  atts.unwnet <- attributes(unwnet[[i]])[-(1:5)] # Not necessary for weighted nets since only "weighted" attribute changes
  for(j in 1:length(atts.unwnet)){
    if(i == 1){ #Initialize
      atts[[j]] <- c( atts.unwnet[[j]])
    }else{ #add
      atts[[j]] <- c(atts[[j]], atts.unwnet[[j]])
    }
  }
}
atts$thresholds <- ths

rm(all_data)

### Communities ###
c <- comms[[which(ths == 0.8)]]
cuts <- 668# 667
plot_communities(c, ref.grid = ba.5deg.std.anom, ref.mask = mask, th = 2, cuts = cuts)

### Net Measures ###
n <- wnet[[which(ths == 0.8)]]
plot_measures(n)

K <- degree(n$graph)

rand.clust_coeff <- c()
rand.diam <- c()

# Comparison with random net
for(th in ths){
  n_vertex <- vcount(unwnet[[1]]$graph)
  n_edges <- atts$total_edges[which(atts$thresholds == th)]
  
  rand.graph <- erdos.renyi.game(n_vertex, n_edges, type = "gnm")
  rand.clust_coeff <- c(rand.clust_coeff, transitivity(rand.graph))
  rand.diam <- c(rand.diam, diameter(rand.graph))
}


# Plot clustering coefficient & diameter
plot(atts$thresholds, atts$clust_coeff, col = "green", pch = 16, ylim=c(-0.05,1), type = "b")
points(atts$thresholds, atts$diam/max(atts$diam), col = "orange", pch = 16, type = "b")
text(atts$thresholds, -0.05, atts$total_edges, col= "blue", cex = 0.7)
points(ths, rand.clust_coeff, col = "darkgreen", pch = 16, type = "b")
points(ths, rand.diam/max(rand.diam), col = "red", pch = 16, type = "b")


# Plot number of edges
plot(atts$thresholds, atts$total_edges, col = "black", pch = 16)
lines(atts$thresholds, atts$total_edges, col = "black")
points(atts$thresholds, atts$pos_edges, col = "blue", pch = 16)
lines(atts$thresholds, atts$pos_edges, col = "blue")
points(atts$thresholds, atts$neg_edges, col = "red", pch = 16)
lines(atts$thresholds, atts$neg_edges, col = "red")

# Plot mean edge distance
plot(atts$thresholds, atts$total_mean_dist, col = "black", pch = 16, ylim = c(0, 10000))
lines(atts$thresholds, atts$total_mean_dist, col = "black")
points(atts$thresholds, atts$pos_mean_dist, col = "blue", pch = 16)
lines(atts$thresholds, atts$pos_mean_dist, col = "blue")
points(atts$thresholds, atts$neg_mean_dist, col = "red", pch = 16)
lines(atts$thresholds, atts$neg_mean_dist, col = "red")
text(atts$thresholds, 0, atts$total_edges, col= "blue", cex = 0.7)



# Plot communities
l <- lapply(comms, FUN = plot_communities, ref.grid = ba.5deg.std.anom, ref.mask = mask, mute = TRUE)