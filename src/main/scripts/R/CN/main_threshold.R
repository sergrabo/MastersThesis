# Remove variables, if there are any
rm(list = ls())

# Source functions
func_path = "src/main/scripts/R/CN/functions/"
source(paste0(func_path, "create_path.R"))
source(paste0(func_path, "load_th_data.R"))
source(paste0(func_path, "plot_dendrograms.R"))
source(paste0(func_path, "generate_random_networks.R"))
source(paste0(func_path, "generate_attributes_table.R"))
source(paste0(func_path, "plot_clust_coeff_diameter.R"))
source(paste0(func_path, "plot_mean_edge_dist.R"))

# Load pre-processed data
data_dir="src/main/resources/input_data"
load(paste0(data_dir, "/ba5degStdAnom.Rdata"), verbose = TRUE)
load(paste0(data_dir, "/mask.Rdata"), verbose = TRUE)

# Define results path
results_path = "src/main/resources/results/"

################# Estudio por thresholds ########################

# Define thresholds to load
ths <- seq(0, 0.95, by = 0.05)
# Define results path
results_cn_path <- paste0(results_path, "CN_results/")
# Load results
all_data <- load_th_data(results_cn_path, ths)

# Separate results into net data and communities data
unwnet <- list()
wnet <- list()
comms <- list()
atts <- list()
atts <- attributes(all_data[[1]][[1]])[-(1:5)]

for(i in 1:length(ths)){
  unwnet[[i]] <- all_data[[i]][[1]]
  wnet[[i]] <- all_data[[i]][[2]]
  comms[[i]] <- all_data[[i]][[3]]
  # Attributes
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
plot_dendrograms(comms, results_cn_path, ths)

# Generate a random net (Erdos-Renyi) for comparison on each threshold
random_generator_list <- generate_random_networks(ths, unwnet, atts)

# Save attributes in a table
generate_attributes_table(atts, save_path=results_cn_path)

################# CLUSTERING COEFFICIENT AND DIAMETER VS THREHSOLD #################
plot_clust_coeff_diameter(atts=atts, rgl=random_generator_list)

################# MEAN EDGE DISTANCE VS THREHSOLD #################
plot_mean_edge_dist(atts)
###################################################################