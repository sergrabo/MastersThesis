#     create_path
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

#' @title 
#' @description 
#' @param 
#' @return 
#' @author Sergio Gracia
#' @references 
#' 
#' 

create_path <- function(th){
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
  th.form <- specify_decimal(th, 2)
  decimal <- substr(th.form, 3, 4)
  path <- paste0("th_0",decimal)
  return(path)
}