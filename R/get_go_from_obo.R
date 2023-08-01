# Build a function to get GO from OBO file, as a reference for ontology alignment
get_go_from_obo=function(path){
  go1=get_OBO(path) %>%
    .$parents %>%
    lapply(
      X=seq(length(.)),
      Y=.,
      FUN=function(X,Y){
        if(X %in% seq(1,length(Y),100)) message(X/length(Y))
        
        if(length(Y[[X]])>0){
          data.frame(
            child=names(Y[X]),
            parents=Y[[X]],
            stringsAsFactors=FALSE
          )
        }
      }
    ) %>%
    do.call(rbind,.)
  
  data(gene_GO_terms)
  
  go2=c(go1$child,go1$parents) %>%
    .[!duplicated(.)] %>%
    lapply(
      X=seq(length(.)),
      Y=.,
      Z=gene_GO_terms %>%
        `names<-`(paste0(names(.),'_')) %>%
        unlist(),
      FUN=function(X,Y,Z){
        if(X %in% seq(1,length(Y),100)) message(X/length(Y))
        
        K=str_split_fixed(names(Z)[Z==Y[X]],'_',2)[,1]
        if(length(K)>0){
          data.frame(
            child=K,
            parents=Y[X],
            stringsAsFactors=FALSE
          )
        }
      }
    ) %>%
    do.call(rbind,.)
  
  rbind(
    go2 %>%
      mutate(relation='gene'),
    go1 %>%
      mutate(relation='is_a')
  )
}