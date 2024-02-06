# climate4R packages: <https://github.com/SantanderMetGroup/climate4R>
library(magrittr)
library(transformeR)
library(visualizeR)

# Remove variables, if there are any
rm(list = ls())

data_preprocessing <- function(annual_period = 2001:2019,
                               spatial_resolution = 5,
                               data_dir="src/main/resources/input_data"){
  # data_preprocessing
  
  #' @title Data preprocessing
  #' @description Perform time aggregation and spatial upscaling to downloaded MODIS data
  #' @param annual_period A vector containing the annual time interval
  #' @param spatial_resolution New desired grid spatial resolution in degrees
  #' @param data_dir Path to data
  #' @return Nothing. Saves variables in .Rdata files
  #' @author Sergio Gracia
  #' @references
  
  original_resolution = 0.25 #deg
  times = spatial_resolution/original_resolution
  
  # Load pre-adapted data
  load(paste0(data_dir, "/MODIS_OLCI_ba_200101-202010.Rdata"), verbose = TRUE)
  load(paste0(data_dir, "/MODIS_OLCI_fba_200101-202010.Rdata"), verbose = TRUE)
  
  # Annual time aggregation
  cat("--- Starting annual time aggregation ---\n")
  ba.merge <- subsetGrid(ba.merge, years = annual_period)
  ba.merge <- aggregateGrid(ba.merge, aggr.y = list(FUN = "sum"))
  
  # Spatial Upscaling: 5 deg resolution using conservative method
  cat("---Starting spatial upscaling ---\n")
  ba.5deg <- upscaleGrid(ba.merge, times = times, aggr.fun = list(FUN = "sum", na.rm = TRUE)) %>% redim(drop = TRUE)
  fba.5deg <- upscaleGrid(fba.merge, times = times, aggr.fun = list(FUN = "mean", na.rm = TRUE)) %>% redim(drop = TRUE)
  
  # Compute standarized anomalies so the magnitude of burned area doesn't influence correlation
  ba.5deg.std.anom <- scaleGrid(ba.5deg, spatial.frame = "gridbox") %>% redim(drop = TRUE)
  
  # Difference ???
  # ba.5deg.std.anom.standarize <- scaleGrid(ba.5deg, spatial.frame = "gridbox", type = "standardize") %>% redim(drop = TRUE)# Funciones para dibujar mapas
  
  # Representation
  # spatialPlot(climatology(ba.5deg), backdrop.theme = "coastline")
  # spatialPlot(climatology(ba.5deg.std.anom), backdrop.theme = "coastline")
  # spatialPlot(climatology(ba.5deg.std.anom.standarize), backdrop.theme = "coastline")
  
  # Filter mask based on:
  # 1. BurnedArea != 0
  # 2. FractionBurnableArea > 0.1
  fba.clim <- climatology(fba.5deg)
  fba.vec <- array3Dto2Dmat(fba.clim$Data)[1,]
  time.coords.matrix <- array3Dto2Dmat(ba.5deg.std.anom$Data)
  mask <- which(apply(time.coords.matrix, MARGIN = 2, FUN = mean) != 0 & fba.vec > 0.1)
  
  # Save data
  save(ba.5deg.std.anom, file = file.path(data_dir, "ba5degStdAnom.Rdata"))
  save(mask, file = file.path(data_dir, "mask.Rdata"))
}

# Execute the whole function
data_preprocessing(annual_period = 2001:2019,
                   spatial_resolution = 5,
                   data_dir="src/main/resources/input_data")
