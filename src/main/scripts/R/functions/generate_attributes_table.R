#     generate_attributes_table
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

#' @title generates an attribute table
#' @description Generates an attribute table with graph information in latex format
#' @param atts List containing all desired attributes
#' @param save_path Where to save the .tex file
#' @return 
#' @author Sergio Gracia

library(xtable)

generate_attributes_table <- function(atts, save_path){
  max.id <- which(1:length(atts$total_range_dist)%%2 == 0)
  min.id <- which(1:length(atts$total_range_dist)%%2 == 1)
  
  atts$total_max_dist <- atts$total_range_dist[max.id]
  atts$pos_max_dist <- atts$pos_range_dist[max.id]
  atts$neg_max_dist <- atts$neg_range_dist[max.id]
  
  atts$total_min_dist <- atts$total_range_dist[min.id]
  atts$pos_min_dist <- atts$pos_range_dist[min.id]
  atts$neg_min_dist <- atts$neg_range_dist[min.id]
  
  atts$total_range_dist <- NULL
  atts$pos_range_dist <- NULL
  atts$neg_range_dist <- NULL
  atts$thresholds<- NULL
  
  df <- atts %>% data.frame()
  print(xtable(df, type = "latex"), file = paste0(save_path, "tablaAtribs.tex"))
}