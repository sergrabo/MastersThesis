# Remove variables, if there are any
rm(list = ls())

# Source functions
func_path = "src/main/scripts/R/BN/functions/"
source(paste0(func_path, "BN_from_grid.R"))

# Load pre-processed data
data_dir="src/main/resources/input_data"
load(paste0(data_dir, "/ba5degStdAnom.Rdata"), verbose = TRUE)
load(paste0(data_dir, "/mask.Rdata"), verbose = TRUE)

# Define results path
results_path = "src/main/resources/results/"
bn_results_path = paste0(results_path, "BN_results/")
if(!dir.exists(bn_results_path)){dir.create(bn_results_path); cat(paste("Created path", bn_results_path, "\n"))}

# Define loop parameters
start <- NULL
edges_added <- 100
edges_max <- 10000

total.ini <- Sys.time()
for (m in 0:floor(edges_max/edges_added)) {
  
  i <- m*edges_added
  j <- i + edges_added
  
  # Time BN Training
  t.ini <- Sys.time()
  # BN Training
  BNobj <- BN_from_grid(grid=ba.5deg.std.anom, mask=mask, max.iter=edges_added, score="bic-g", start=start)
  
  # Save on predefined path
  path <- paste0(bn_results_path, "bayesian_network_objects/")
  if(!dir.exists(path)){dir.create(path); cat(paste("Created path", path, "\n"))}
  var_name <- paste0("firedata_hc_",i,"_",j,"i")
  assign(var_name, BNobj)
  save(list = var_name, 
       file = paste0(path, var_name, ".Rdata"))
  
  t.end <- Sys.time()
  print(paste0("Elapsed time on BN training: ", difftime(t.end, t.ini, units = "mins") %>% round(2)," minutes", " on network ", var_name))
  
  attr(BNobj, "elapsed_time") <- t.end-t.ini
  
  # Update start
  if(m==0){
    start <- BNobj$BN
  } else if(narcs(BNobj$BN) == narcs(start)){ # If no more arcs can be added on training
    break
  } else {
    start <- BNobj$BN
  }
  
}

total.end <- Sys.time()
print(paste0("Total elapsed time: ", total.end-total.ini))

# Remove variables so nothing is loaded when the script ends
rm(list = ls())
