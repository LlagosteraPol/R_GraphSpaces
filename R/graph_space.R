rm(list = ls())

library(gtools)
require(igraph)
library(roxygen2)


# TODO: Improve efficiency by generating all the basic (non-isomorphic) graphs using 'nauty'
# TODO: Improve efficiency by storing only the dataframe of the possible edge configurations
# and generate the igraph objects on demand

#' Generate the graph space for 'n' nodes
#'
#' @param adj_mtx  equivalence class adjacency matrix
#' 
#' @return a list containing all possible graphs with 'n' nodes with all their isomorphisms
#' 
GraphSpace <- function(adj_mtx){
  g <- igraph::graph_from_adjacency_matrix(adj_mtx, 'undirected')
  #f_adj_mtx <- as.vector(t(adj_mtx)) # flattened matrix
  #up_adj_mtx <- upper.tri(adj_mtx, diag = FALSE) * adj_mtx
  graph_space <- list()
  
  node_per <- permutations(n=nrow(adj_mtx), r=nrow(adj_mtx), v=c(1:nrow(adj_mtx)))

  # For each edge combination, create a graph
  for(row in 1:nrow(node_per)){
    tmp_mtx <- list()
    g_tmp <- permute(g, node_per[row,])
    graph_space[[length(graph_space) + 1]] <- as.vector(t(as.matrix(as_adjacency_matrix(g_tmp))))
  }
  graph_space
}


IGraphSpace <- function(adj_mtx){
  g <- igraph::graph_from_adjacency_matrix(adj_mtx, 'undirected')
  #f_adj_mtx <- as.vector(t(adj_mtx)) # flattened matrix
  #up_adj_mtx <- upper.tri(adj_mtx, diag = FALSE) * adj_mtx
  graph_space <- list()
  
  node_per <- permutations(n=nrow(adj_mtx), r=nrow(adj_mtx), v=c(1:nrow(adj_mtx)))
  
  # For each edge combination, create a graph
  for(row in 1:nrow(node_per)){
    tmp_mtx <- list()
    graph_space[[length(graph_space) + 1]] <- permute(g, node_per[row,])
  }
  graph_space
}

GetGraphSpaceMatrices <- function(igraph_list){
  graph_space <- list()
  for(element in igraph_list){
    graph_space <- as.vector(t(as.matrix(as_adjacency_matrix(element))))
  }
  graph_space
}

#----------------------RUN------------------------
adj_mtx = matrix(c(0,1,0,1,0,1,0,1,0), nrow = 3)
graph_space <- GraphSpace(adj_mtx)
print(graph_space)

  igraph_space <- IGraphSpace(adj_mtx)
