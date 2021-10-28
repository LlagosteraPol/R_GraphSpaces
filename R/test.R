
# dyn.load("doubler.so")
# .C("double_me", x = as.integer(5))
# geng -bc num_nodes num_edges:num_edges
# command = "nauty-geng " + str(4) + " " + str(i) + ":" + str(
#   i) + " -c > " + getwd() + "/data/graph6/" + file_name
# system(command)

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