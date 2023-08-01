# Build a function to create a directed acyclic graph (DAG) tree using igraph
igraph_dag_tree=function(from_to_df,color='grey',shape='circle',label.cex=0.501,edge.arrow.size=0.62,vertex.size=12.5,seed_num=33){
  if(!is.null(from_to_df)){
    set.seed(seed_num)
    ontograph=from_to_df %>%
      graph_from_data_frame(directed=TRUE)
    
    V(ontograph)$color=color
    V(ontograph)$shape=shape
    V(ontograph)$label.cex=label.cex
    
    ontograph %>%
      plot.igraph(
        edge.arrow.size=edge.arrow.size,
        vertex.size=vertex.size,
        layout=layout_as_tree(.,mode='in')
      )
  }
}