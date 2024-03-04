#     BN_propagation
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

#' @title BN_propagation
#' @description Perform bayesian inference on the trained Bayesian Network
#' @param baysnet Trained bayesian network (both structurally and parametrically) to perform inference with
#' @param nodesEvents Set of nodes where information can be propagated
#' @param valueEvent Deviation in sigmas over (>=) or under (<=) the mean value of the normal distribution on the nodesEvents.
#' @param nodesEvidence Reference nodes who's value changes and propagates through the network
#' @param valueEvidence Deviation in sigmas quantifying the extreme event in nodesEvidence 
#' @param compute.without Defaults to FALSE
#' @param perm Defaults to NULL
#' @return df Dataframe with the probabilities
#' @author Lissete Graafland
#' @references  


BN_propagation <- function(baysnet, nodesEvents, valueEvent, nodesEvidence, valueEvidence, compute.without = FALSE, perm = NULL){
  
  if(length(nodesEvents) > length(baysnet)){stop("Number of Event Nodes exceds Bayesian Network's size")}
  
  if (is.null(perm)) {nodesEventsRef <- nodesEvents}
  
  with <- numeric(length = length(nodesEvents))
  without <- numeric(length = length(nodesEvents))
  
  if (length(nodesEvidence) == 1) {
    str2 <- paste0("list(X", nodesEvidence[1]," = ", valueEvidence[1], ")")
    probname <- paste0("P(X ", valueEvent,"|",nodesEvidence[1]," = ", valueEvidence[1], ")")
  }
  
  
  if (length(nodesEvidence) > 1){
    proves <- c()
    for (j in 1:length(nodesEvidence)){
      proves[j]<- paste0("X",nodesEvidence[j]," = ", valueEvidence[j])
    }
    
    text <- "list("
    for (j in 1:(length(nodesEvidence)-1)){
      text <- paste0(text,proves[j],",")
    }
    text <- paste0(text,proves[length(nodesEvidence)],")")
    str2 <- text
    
    
    probname <- paste0("P(X ", valueEvent,"|")
    for (j in 1:(length(nodesEvidence)-1)){
      probname <- paste0(probname,proves[j],",")
    }
    probname <- paste0(probname,proves[length(nodesEvidence)],")")
  }
  
  # str2
  # i <- 2
  start.all <- Sys.time()
  print(paste0("Calculating inference for given evidence in node ", nodesEvidence))
  for(i in 1:length(nodesEvents)) {
    # l <- nodesEvents[i]
    # l
    l <- nodesEventsRef[i]
    # str <- paste0("(", names(baysnet)[l], ">=", valueEvent, ")")
    str <- paste0("(", names(baysnet)[l], valueEvent, ")")
    str
    nparams(baysnet)
    cmd1 = paste0("cpquery(baysnet, ", str, ", ", str2, ", method = ","'lw'",")")
    cmd3 = paste0("cpquery(baysnet, ", str, ", ", "TRUE", ", method = ","'lw'",")")
    cmd1
    cmd3
    
    start.with <- Sys.time()
    with[i] <- eval(parse(text = cmd1))
    end.with <- Sys.time()
    print(paste0("Elapsed time for with: ", difftime(end.with, start.with, units = "secs") %>% round(2)," seconds", " on node ", l))
    with[i]
    
    if(compute.without == TRUE){
      start.without <- Sys.time()
      without[i] <- eval(parse(text = cmd3))
      end.without <- Sys.time()
      print(paste0("Elapsed time for without: ", difftime(end.without, start.without, units = "secs") %>% round(2)," seconds", " on node ", l))
      without[i]
    }else{without[i] <- 0.158}
    
    # with[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = eval(parse(text = str2)))
    # withcomplement[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = eval(parse(text = str3)))
    # without[i] <- cpquery(baysnet, event = eval(parse(text = str)), evidence = TRUE)
    
  }
  end.all <- Sys.time()
  print(paste0("Elapsed time for all nodes: ", difftime(end.all, start.all, units = "hours") %>% round(2), " hours"))
  
  attr(with, "probability") <- probname
  attr(without, "probability") <- paste0("P(X ", valueEvent,")")
  df <- data.frame(names = names(baysnet)[nodesEventsRef], with = with, without = without)
  return(df)
  
}
