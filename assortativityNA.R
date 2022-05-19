library(igraph)
assortativityNA <- function(graph, types1, types2 = NULL, directed = TRUE, na.rm = TRUE){
  
  if(na.rm==TRUE){
    graph <- delete_vertices(graph, which(is.na(types1)))
    types1 <- types1[!is.na(types1)]
  }else{
    graph <- graph
  }
  
  output <- assortativity(graph, types1, types2, directed)
  return(output)
  
}