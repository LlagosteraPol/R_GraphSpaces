#' Generate the graph space for 'n' nodes
#'
#' @param n size of the graph space (n*n matrix)
#' 
#' @return a list containing all possible graphs with 'n' nodes with all their isomorphisms
#' 
GraphSpace <- function(n){
  v_names <- c(1:n)
  all_possible_edges_df = combinations(n = n, r = 2, v = v_names)
  
  graph_space <- list()
  for(n_comb in 1:nrow(all_possible_edges_df)){
    tmp_graph_space <- list()
    # Get all combinations of n_comb edges
    edges_comb <- combinations(n=nrow(all_possible_edges_df), r=n_comb, v=c(1:nrow(all_possible_edges_df)))
    
    # For each edge combination, create a graph
    for(row in 1:nrow(edges_comb)){
      tmp_df <- as.data.frame(matrix(all_possible_edges_df[edges_comb[row,],], ncol=2))
      tmp_graph_space[[length(tmp_graph_space)+1]] <-  graph_from_data_frame(tmp_df, directed = FALSE, vertices = v_names)
    }
    graph_space[[paste0('nE', n_comb)]] <- tmp_graph_space
  }
  graph_space
}


GraphSpace <- function(adj_mtx){
  
  #f_adj_mtx <- as.vector(t(adj_mtx)) # flattened matrix
  up_adj_mtx <- upper.tri(adj_mtx, diag = FALSE) * adj_mtx
  graph_space <- list()
  
  tmp_graph_space <- list()
  # Get all combinations of n_comb edges
  edges_comb <- combinations(n=nrow(adj_mtx), r=n_comb, v=c(1:nrow(all_possible_edges_df)))
  
  # For each edge combination, create a graph
  for(row in 1:nrow(edges_comb)){
    tmp_df <- as.data.frame(matrix(all_possible_edges_df[edges_comb[row,],], ncol=2))
    tmp_graph_space[[length(tmp_graph_space)+1]] <-  graph_from_data_frame(tmp_df, directed = FALSE, vertices = v_names)
  }
  graph_space[[paste0('nE', n_comb)]] <- tmp_graph_space
  
  graph_space
}