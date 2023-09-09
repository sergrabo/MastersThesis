# Remove variables, if there are any
rm(list = ls())

# Source functions
func_path = "src/main/scripts/R/functions/"
source(paste0(func_path, "create_path.R"))
source(paste0(func_path, "graph_from_grid.R"))
source(paste0(func_path, "graph_world_network.R"))
source(paste0(func_path, "plot_dist_corr.R"))



# Load pre-processed data
data_dir="src/main/resources/input_data"
load(paste0(data_dir, "/ba5degStdAnom.Rdata"), verbose = TRUE)
load(paste0(data_dir, "/mask.Rdata"), verbose = TRUE)

# Define results path
results_path = "src/main/resources/results/"
corrnet_results_path = paste0(results_path, "CN_results/")
if(!dir.exists(corrnet_results_path)){dir.create(corrnet_results_path); cat(paste("Created path", corrnet_path, "\n"))}

# Define correlation thresholds
by = 0.05
thresholds <- seq(0, 1-by, by = by)
# thresholds <- c(0.95, 0.9, 0.85)
  
for(cor.th in rev(thresholds)){
  
  i <- which(thresholds == cor.th)
  
  path <- paste0(corrnet_results_path, create_path(cor.th))
  if(!dir.exists(path)){dir.create(path); cat(paste("Created path", path, "\n"))}
  
  
  ########## Compute Correlation Networks ##########
  unweighted.net <- graph_from_grid(ba.5deg.std.anom, cor.th = cor.th, mask = mask)
  weighted.net <- graph_from_grid(ba.5deg.std.anom, cor.th = cor.th, mask = mask, weighted = TRUE)
  
  net.file <- paste0(path, "/networks.Rdata")
  save(unweighted.net, weighted.net, file = net.file)
  
  ########## Correlation Network Worldmap Representation ##########
  
  ### Unweighted Correlation Network ###
  graph_world_network(unweighted.net, mute = TRUE, save_path = path, filename="SpatialNetwork_unweighted.pdf")

  ### Weighted Correlation Network ###
  graph_world_network(weighted.net, mute = TRUE, save_path = path, filename="SpatialNetwork_weighted.pdf")

  # Plot distance vs correlation
  plot_dist_corr(weighted.net, mute = TRUE, save_path=path, filename="distVScorr.pdf")

  # ########## Metricas de redes complejas ##########
  # 
  # # Plot measures
  # plot.file <- paste0(path, "/CentralityMeasures.pdf")
  # pdf(file = plot.file)
  # 
  # plot_measures(weighted.net, mute = TRUE)
  # 
  # dev.off()
  
  
  
  ######### Estudio de clustering en la red ##########
  # # Compute communities through cluster_edge_betweenness
  # start <- Sys.time()
  # comObj <- cluster_edge_betweenness(unweighted.net$graph, directed = FALSE)
  # end <- Sys.time()
  # print(paste("Communities execution time: ", end-start))
  # 
  # com.file <- paste0(path, "/communities.Rdata")
  # save(comObj, file = com.file)
  # 
  # plot.file <- paste0(path, "/CentralityMeasures.pdf")
  # pdf(file = plot.file)
  # 
  # plot_communities(comObj, ref.grid = ba.5deg.std.anom, ref.mask = mask, th = 7, mute = TRUE)
  # dev.off()
  
  
  cat("Plotted for th = ", cor.th, "\n")
  
}
