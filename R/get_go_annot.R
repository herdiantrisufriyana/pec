# Build a function to get annotation of the GO
get_go_annot=function(the_go_id){
  data.frame(
    go_id=the_go_id,
    stringsAsFactors=FALSE
  ) %>%
    mutate(
      term=pbsapply(X=go_id,FUN=function(X){GOTERM[[X]]}),
      ontology=pbsapply(X=term,FUN=Ontology),
      definition=pbsapply(X=term,FUN=Definition),
      term=pbsapply(X=term,FUN=Term),
      parents=pbsapply(
        X=seq(nrow(.)),
        Y=go_id,
        Z=ontology,
        FUN=function(X,Y,Z){
          # if(X==1) message('Get the GO parents')
          # if(X %in% seq(1,length(Y),100)) message(X/length(Y))
          if(Z[X]=='BP'){
            K=GOBPPARENTS[[Y[X]]]
          }else if(Z[X]=='MF'){
            K=GOMFPARENTS[[Y[X]]]
          }else if(Z[X]=='CC'){
            K=GOCCPARENTS[[Y[X]]]
          }else{
            K=NA
          }
          K[K=='all']=rep('root',sum(K=='all'))
          paste(K,collapse=',')
        }
      ),
      ancestors=pbsapply(
        X=seq(nrow(.)),
        Y=go_id,
        Z=ontology,
        FUN=function(X,Y,Z){
          # if(X==1) message('Get the GO ancestors')
          # if(X %in% seq(1,length(Y),100)) message(X/length(Y))
          if(Z[X]=='BP'){
            K=GOBPANCESTOR[[Y[X]]]
          }else if(Z[X]=='MF'){
            K=GOMFANCESTOR[[Y[X]]]
          }else if(Z[X]=='CC'){
            K=GOCCANCESTOR[[Y[X]]]
          }else{
            K=NA
          }
          K[K=='all']=rep('root',sum(K=='all'))
          paste(K,collapse=',')
        }
      )
    )
}