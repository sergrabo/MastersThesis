# Remove variables, if there are any
rm(list = ls())

# load libraries
library(bnlearn)

# Define results path
results_path = "src/main/resources/results/"
bn_results_path = paste0(results_path, "BN_results/")
plots_path <- paste0(bn_results_path, "loglikelihood/")
if(!dir.exists(plots_path)){dir.create(plots_path); cat(paste("Created path", plots_path, "\n"))}

# Load trained bayesian networks into a list
hc_list <- list.files(paste0(bn_results_path, "bayesian_network_objects"), full.names = T)
hc_names <- list.files(paste0(bn_results_path, "bayesian_network_objects"))
hc_names <- gsub(".Rdata", "", hc_names)

hc_bn_objects <- list()
hc_networks <- list()
for (i in 1:length(hc_list)){
  hc_bn_objects[[i]] <- get(load(hc_list[i]))
  # Keep only the bayesian network element
  hc_networks[[i]] <- hc_bn_objects[[i]]$BN
}

# Training data is the exact same for all networks, so we take the first one
hc_data <- hc_bn_objects[[1]]$data

# Remove loaded data
rm(list = hc_names)
rm(hc_bn_objects)

# Order list by number of arcs
names(hc_networks) <- hc_names
sizes <- sapply(hc_networks, narcs)
hc_networks <- hc_networks[order(sizes)]

# Parametric learning for each network
hc_fits <- lapply(hc_networks, bn.fit, data = hc_data)

# Compute log-likelihood for each BN
logliks_hc <- sapply(X = hc_fits, logLik, data = hc_data)
nedges_hc <- sapply(hc_networks, narcs)

# Save plot in the results folder
plot.file <- paste0(plots_path, "LogLikelihood_Year.pdf")
pdf(file = plot.file)
plot(nedges_hc, logliks_hc, col = "blue", pch= 16,
     xlab = "Number of links",
     ylab = "Log-likelihood")

dev.off()
###################################################################################