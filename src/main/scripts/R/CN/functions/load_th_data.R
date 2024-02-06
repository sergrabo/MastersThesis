#     load_th_data
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Load threshold data
#' @description  Loads all the results obtained from the script `main_networks_per_th.R`
#' @param results_path path where the results are stored. Should be './src/main/resources/results/CN_results/'
#' @param thresholds Array with the values of the thresholds
#' @return a List with the unweighted net, the weighted net, and a community object if th>0.4
#' @author Sergio Gracia
#' 
#' 

# Load create_path()
source("src/main/scripts/R/CN/functions/create_path.R")

load_th_data <- function(results.path, thresholds){
  
  # Define lists
  out = list()
  
  for(cor.th in thresholds){
    # Define path
    path <- paste0(results.path, create_path(cor.th))
    # List index
    i <- which(thresholds == cor.th)
    
    # Load networks
    net.path <- paste0(path, "/networks.Rdata")
    load(net.path, verbose = TRUE)
    
    # Load communites
    if(cor.th >= 0.4){
      com.path <- paste0(path, "/communities.Rdata")
      load(com.path, verbose = TRUE)
    }else{
      comObj <- NULL
    }
    
    # Introduce variables in output list
    out[[i]] <- mget(c("unweighted.net", "weighted.net", "comObj"))
    rm(unweighted.net, weighted.net, comObj)
    
  }
  th.min = min(thresholds)
  th.max = max(thresholds)
  print(paste0("Loaded data for thresholds from th = ", th.min, " to th = ", th.max))
  
  return(out)
}