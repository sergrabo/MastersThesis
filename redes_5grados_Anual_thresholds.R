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
      atts[[j]] <- c(atts.unwnet[[j]])
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
plot(atts$thresholds, atts$clust_coeff, col = "darkolivegreen2", pch = 16, ylim=c(-0.05,1), type = "b", lwd = 2)
points(atts$thresholds, atts$diam/max(atts$diam), col = "darkorange1", pch = 16, type = "b", lwd = 2)
text(atts$thresholds, -0.05, atts$total_edges, col= "blue", cex = 0.7, lwd = 2)
points(ths, rand.clust_coeff, col = "#85B13B", pch = 16, type = "b", lwd = 2)
points(ths, rand.diam/max(rand.diam), col = "darkorange3", pch = 16, type = "b", lwd = 2)


## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)
## Plot first set of data and draw its axis
plot(ths, atts$clust_coeff, axes = FALSE, col = "darkolivegreen2", xlab = "", ylab = "", ylim = c(-0.05,1),
     pch = 16, type = "b", lwd = 2)
points(ths, rand.clust_coeff, col = "#85B13B", pch = 16, type = "b", lwd = 2)
text(ths, -0.05, atts$total_edges, col= "blue", cex = 0.8, lwd = 2)
axis(2, at = seq(0, 1, by = 0.1), ylim = c(0,1), col="black", las = 1)  ## las=1 makes horizontal labels
mtext("Clustering coefficient", side = 2, line = 2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(ths, atts$diam, axes = FALSE, col = "darkorange1", xlab = "", ylab = "", ylim = c(-0.05,max(atts$diam)),
     pch = 16, type = "b", lwd = 2)
points(ths, rand.diam, col = "darkorange3", pch = 16, type = "b", lwd = 2)
## a little farther out (line=4) to make room for labels
mtext("Diameter", side = 4, col = "red", line = 4) 
axis(4, at = 0:max(atts$diam), ylim = c(0,max(atts$diam)), col = "red", col.axis = "red", las = 1)

## Draw the time axis
axis(1, ths)
mtext("Thresholds",side=1,col="black",line=2.5)  

## Add Legend
legend("topright",
       legend = c("CN Clustering Coefficient",
                              "RN Clustering Coefficient",
                              "CN Diameter",
                              "RN Diameter"),
       text.col = c("darkolivegreen1",
                    "#85B13B",
                    "darkorange1",
                    "darkorange3"),
       pch = c(16, 16, 16, 16),
       lty = c(1, 1, 1, 1),
       col = c("darkolivegreen1",
               "#85B13B",
               "darkorange1",
               "darkorange3"))


# Plot mean edge distance
par(mar=c(5, 6, 4, 2) + 0.1)
plot(ths, atts$total_mean_dist, col = "black", pch = 16, type = "b",
     ylim = c(0, 10000), xlab = "", ylab = "", axes = FALSE)
points(atts$thresholds, atts$pos_mean_dist, col = "blue", pch = 16, type = "b")
points(atts$thresholds, atts$neg_mean_dist, col = "red", pch = 16, type = "b")
text(ths, -0.05, atts$total_edges, col= "blue", cex = 0.8, lwd = 2)
box()
axis(1, ths)
mtext("Thresholds",side=1,col="black",line=2.5) 
axis(2, ylim = c(0,10000), col="black", las = 1)
mtext("Mean edge distance", side = 2, line = 4)
## Add Legend
legend(x = 0.15, y = 4000,
       legend = c("All edges",
                  "Positively correlated edges",
                  "Negatively correlated edges"),
       text.col = c("black",
                    "blue",
                    "red"),
       pch = c(16, 16, 16),
       lty = c(1, 1, 1),
       col = c("black",
               "blue",
               "red"))

# Plot communities
l <- lapply(comms, FUN = plot_communities, ref.grid = ba.5deg.std.anom, ref.mask = mask, mute = TRUE)
