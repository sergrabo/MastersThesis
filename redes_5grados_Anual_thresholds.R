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
library(grid)
library(gridExtra)
library(xtable)

# Directorio de trabajo
setwd("C:/Users/sergr/Dropbox/TFM_Sergio_Gracia")
#setwd("/home/juaco/Dropbox/TFM_Sergio_Gracia")

# Cargar funciones definidas por mi
sapply(list.files("./scripts/MastersThesis/functions/", full.names = TRUE), "source", .GlobalEnv)

# Cargamos los datos ya calculados, para evitar problemas de memoria
# load("./Rdata/ba5degAnom.Rdata", verbose = TRUE)
load("./Rdata/ba5deg_anom_standarize.Rdata", verbose = TRUE)
ba.5deg.std.anom <- ba.5deg.std.anom.standarize
rm(ba.5deg.std.anom.standarize)
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
  attr(unwnet[[i]], "threshold") <- ths[i]
  attr(wnet[[i]], "threshold") <- ths[i]
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

############## Communities ##################

# Plot and save dendrograms
for (i in ths[9:20]) {
  th <- i
  c <- comms[[which.min(abs(ths-th))]]
  d <- as.hclust(c)
  pdf(paste0("graphs/5deg_Anual_dist0/dendrograms/","dendrogram_CN_",th,".pdf"), width = 8.27, height = 11.69)
  plot(d, labels = FALSE, cex = .4, main = bquote("Dendrogram for " ~ tau[c] ~ "=" ~ .(th)), sub = "", xlab = "", lwd = 0.1)
  # abline(h=645-nrow(c$merges), col = "red", lwd = 1.5)
  # abline(h=nrow(c$merges)-45, col = "blue")
  dev.off()
}

th <- 0.7
c <- comms[[which.min(abs(ths-th))]]
# n <- wnet[[which.min(abs(ths-th))]]
# k <- degree(n$graph)
# k.null <- which(k==0)
plot_communities(c, ref.grid = ba.5deg.std.anom, ref.mask = mask, th = 10, cuts = 3, cor.th = th)


# Comparison with random net
set.seed(4)
rnet <- list()
rand.clust_coeff <- c()
rand.diam <- c()
rand.conn <- c()
complex.conn <- c()

for (i in 1:length(ths)){
  n_vertex <- vcount(unwnet[[i]]$graph)
  n_edges <- atts$total_edges[i]
  
  rand.graph <- erdos.renyi.game(n_vertex, n_edges, type = "gnm")
  rnet[[i]] <- rand.graph # save random net
  rand.clust_coeff <- c(rand.clust_coeff, transitivity(rand.graph))
  rand.diam <- c(rand.diam, diameter(rand.graph))
  
  # Connectivity of the graph
  rand.conn <- c(rand.conn, diameter(rand.graph, unconnected = FALSE))
  complex.conn <- c(complex.conn, diameter(unwnet[[i]]$graph, unconnected = FALSE))
}

rnet.plot <- lapply(rnet, FUN = as.graphObj, ref.graphObj = unwnet[[15]])

################# SPATIAL PLOT #################
# th = 0.5, 0.7, 0.8
th <- 0.8
i <- which.min(abs(ths-th))
# graphObj <- wnet[[i]]
attr(rnet.plot[[i]], "threshold") <- th

graph_world_network(rnet.plot[[i]])
graph_world_network(unwnet[[i]])
graph_world_network(wnet[[i]])

# Measures
plot.path <- paste0("./graphs/5deg_Anual_dist0/measures_th0", 10*th, ".pdf")
pdf(file = plot.path, width = 7, height = 7)
plot_measures(wnet[[i]], mute = TRUE)
dev.off()

# Tabla de atributos
# a <- attributes(unwnet[[i]])[-(1:6)]
a <- atts
max.id <- which(1:length(a$total_range_dist)%%2 == 0)
min.id <- which(1:length(a$total_range_dist)%%2 == 1)

a$total_max_dist <- a$total_range_dist[max.id]
a$pos_max_dist <- a$pos_range_dist[max.id]
a$neg_max_dist <- a$neg_range_dist[max.id]

a$total_min_dist <- a$total_range_dist[min.id]
a$pos_min_dist <- a$pos_range_dist[min.id]
a$neg_min_dist <- a$neg_range_dist[min.id]

a$total_range_dist <- NULL
a$pos_range_dist <- NULL
a$neg_range_dist <- NULL
a $ thresholds<- NULL

df <- a %>% data.frame()
print(xtable(df, type = "latex"), file = "tablaAtribs.tex")
################################################

################# CLUSTERING COEFFICIENT AND DIAMETER VS THREHSOLD #################
x11()
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

# Plot the second plot and put axis scale on right
plot(ths, atts$diam, axes = FALSE, col = "darkorange1", xlab = "", ylab = "", ylim = c(-0.05,max(c(max(rand.diam), max(atts$diam)))),
     pch = 16, type = "b", lwd = 2)
points(ths[complex.conn==Inf | complex.conn == 645], atts$diam[complex.conn==Inf | complex.conn == 645], col = "darkorange1", pch = 8, cex = 2, lwd = 2)
points(ths, rand.diam, col = "darkorange3", pch = 16, type = "b", lwd = 2)
points(ths[rand.conn==Inf| rand.conn == 645], rand.diam[rand.conn==Inf | rand.conn == 645], col = "darkorange3", pch = 8, cex = 2, lwd = 2)

## a little farther out (line=4) to make room for labels
axis(4, at = 0:max(c(max(rand.diam), max(atts$diam))), ylim = c(0,max(c(max(rand.diam), max(atts$diam)))), col = "red", col.axis = "red", las = 1)
mtext("Diameter", side = 4, col = "red", line = 4) 

## Draw the time axis
axis(1, ths)
mtext("Correlation threshold",side=1,col="black",line=2.5)  

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
####################################################################################

################# MEAN EDGE DISTANCE VS THREHSOLD #################
par(mar=c(5, 6, 4, 2) + 0.1)
plot(ths, atts$total_mean_dist, col = "black", pch = 16, type = "b",
     ylim = c(0, 10000), xlab = "", ylab = "", axes = FALSE)
points(atts$thresholds, atts$pos_mean_dist, col = "blue", pch = 16, type = "b")
points(atts$thresholds, atts$neg_mean_dist, col = "red", pch = 16, type = "b")
text(ths, -0.05, atts$total_edges, col= "blue", cex = 0.8, lwd = 2)
box()
axis(1, ths)
mtext("Correlation threshold",side=1,col="black",line=2.5) 
axis(2, ylim = c(0,10000), col="black", las = 1)
mtext("Mean link distance", side = 2, line = 4)
## Add Legend
legend(x = 0.15, y = 4000,
       legend = c("All links",
                  "Positively correlated links",
                  "Negatively correlated links"),
       text.col = c("black",
                    "blue",
                    "red"),
       pch = c(16, 16, 16),
       lty = c(1, 1, 1),
       col = c("black",
               "blue",
               "red"))
###################################################################