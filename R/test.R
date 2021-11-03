

#------------------------------------GENERATE UNLABELLED GRAPHS WITH NAUTY----------------------------------
#geng num_nodes num_edges:num_edges
n_nodes <- 4
min_n_edges <- 3
max_n_edges <- 3
command = paste0("nauty-geng ", n_nodes, " ", min_n_edges, ":", max_n_edges, " > ", getwd(), "/Data/test.g6")
system(command)

#-----------------------------------CREATE ALL POSSIBLE GRAPH COMBINATIONS----------------------------------
node_names <- c(1,2,3)
all_possible_edges_df = combinations(n=3,r=2, v=node_names)
edges_df = data.frame(all_possible_edges_df[sample(1:nrow(all_possible_edges_df), 2),], 
                      stringsAsFactors = FALSE) #randomly select 'n_edges' of these rows

dfgraph = graph_from_data_frame(edges_df, directed = FALSE, vertices = node_names)
plot(dfgraph)

graph_space <- list()
for(n_comb in 1:nrow(all_possible_edges_df)){
  tmp_graph_space <- list()
  print(paste0("N comb: ", n_comb))
  edges_comb <- combinations(n=nrow(all_possible_edges_df), r=n_comb, v=c(1:nrow(all_possible_edges_df)))
  for(row in 1:nrow(edges_comb)){
    print(edges_comb[row,])
    print(all_possible_edges_df[edges_comb[row,],])
    tmp_graph <- as.data.frame(matrix(all_possible_edges_df[edges_comb[row,],], ncol=2))
    tmp_graph_space[[length(tmp_graph_space)+1]] <-  graph_from_data_frame(tmp_graph, directed = FALSE, vertices = node_names)
  }
  graph_space[[paste0('nE', n_comb)]] <- tmp_graph_space
}
plot(graph_space[[1]])

#--------------------------------CREATE ALL POSSIBLE ADJ_MTX ROW COMBINATIONS-------------------------------

adj_mtx = matrix(c(0,1,0,1,0,1,0,1,0), nrow = 3)
node_per <- permutations(n=nrow(adj_mtx), r=nrow(adj_mtx), v=c(1:nrow(adj_mtx)))
tmp_mtx <- adj_mtx[, node_per[1,]]
g <- igraph::graph_from_adjacency_matrix(adj_mtx, 'undirected')
plot(g)
g2 <- permute(g, node_per[5,])
plot(g2)
adj_mtx2 <- as.matrix(as_adjacency_matrix(g2))
as.vector(t(adj_mtx2))