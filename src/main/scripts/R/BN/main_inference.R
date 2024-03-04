# Remove variables, if there are any
rm(list = ls())

# Paquetes redes y visualizacion
library(magrittr)
library(bnlearn)

# Source functions
func_path <- "src/main/scripts/R/BN/functions/"
source(paste0(func_path, "BN_propagation.R"))
source(paste0(func_path, "BN_plot_inference.R"))
source(paste0(func_path, "identify_node.R"))

# Load pre-processed data
data_dir <- "src/main/resources/input_data"
load(paste0(data_dir, "/ba5degStdAnom.Rdata"), verbose = TRUE)
load(paste0(data_dir, "/mask.Rdata"), verbose = TRUE)

# Define results path
results_path <- "src/main/resources/results/"
bn_results_path <- paste0(results_path, "BN_results/")
inference_path <- paste0(bn_results_path, "inference/")
if(!dir.exists(inference_path)){dir.create(inference_path); cat(paste("Created path", inference_path, "\n"))}

# Choose BN with 2000 edges
bn_obj_path <- "firedata_hc_1900_2000i.Rdata"
load(paste0(bn_results_path, "bayesian_network_objects/", bn_obj_path), verbose = TRUE)

# Parametric learning
bn_fitted <- bn.fit(firedata_hc_1900_2000i$BN, data = firedata_hc_1900_2000i$data)

####################################################################################
# Define node evidence
node.evidence <- c(185)

# Single positive evidence. Probability on positive deviation (+ +)
df.pos <- BN_propagation(baysnet = bn_fitted,
                         nodesEvents = 1:645,
                         valueEvent = ">= 1",
                         nodesEvidence = node.evidence,
                         valueEvidence = c(2),
                         compute.without = FALSE,
                         perm = NULL)

# Save results
df.pos %<>% mutate(diff = (with-without))
results_name <- paste0("X", as.character(node.evidence), "_posInference_2k_anual.csv")
write.csv(df.pos, file = paste0(inference_path, results_name))

# Single negative evidence. Probability on negative deviation (+ -)
df.neg <- BN_propagation(baysnet = bn_fitted,
                         nodesEvents = 1:645,
                         valueEvent = "<= -1",
                         nodesEvidence = node.evidence,
                         valueEvidence = c(2),
                         compute.without = FALSE,
                         perm = NULL)

# Save results
df.neg %<>% mutate(diff = (with-without))
results_name <- paste0("X", as.character(node.evidence), "_negInference_2k_anual.csv")
write.csv(df.neg, file = paste0(inference_path, results_name))

######### Plot Positive Inference #################
node <- 185 #231, 541, 185, 572
sign <- "pos"

read_csv_name <- paste0("X", node, "_", sign, "Inference_2k_anual.csv")
df <- read.csv(file = paste0(inference_path, read_csv_name))

plot.file <- paste0(inference_path, "/posinference_X", node %>% as.character(), ".pdf")
pdf(file = plot.file)

BN_plot_inference(node, sign, df, ref.grid = ba.5deg.std.anom, ref.mask = mask, mute = TRUE)

dev.off()
######### Plot Negative Inference #################
sign <- "neg"

read_csv_name <- paste0("X", node, "_", sign, "Inference_2k_anual.csv")
df <- read.csv(file = paste0(inference_path, read_csv_name))

plot.file <- paste0(inference_path, "/neginference_X", node %>% as.character(), ".pdf")
pdf(file = plot.file)

BN_plot_inference(node, sign, df, ref.grid = ba.5deg.std.anom, ref.mask = mask, mute = TRUE)

dev.off()
