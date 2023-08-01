# Build a function to bind expression sets
bind_eset=function(the_elist,the_non_event,non_event_name,the_event,event_name){
  
  the_gse=c(the_non_event$gse,the_event$gse)
  the_gsm=c(the_non_event$gsm,the_event$gsm)
  the_outcome=
    factor(
      c(rep(non_event_name,nrow(the_non_event)),rep(event_name,nrow(the_event))),
      c(non_event_name,event_name)
    )
  
  the_elist=
    the_elist[the_gse] %>%
    lapply(
      X=seq(length(.)),
      Y=.,
      Z=the_gsm,
      K=lapply(.,rownames) %>% Reduce(intersect,.),
      FUN=function(X,Y,Z,K){
        Y[[X]][K,Z[X]]
      }
    )
  
  the_eset=ExpressionSet(
    assayData=lapply(the_elist,exprs) %>% do.call(cbind,.),
    phenoData=lapply(the_elist,pData) %>% do.call(bind_rows,.) %>% AnnotatedDataFrame(),
    featureData=lapply(the_elist,fData) %>% .[[1]] %>% AnnotatedDataFrame()
  )
  
  pData(the_eset)=pData(the_eset) %>%
    rownames_to_column(var='id') %>%
    mutate(
      outcome=the_outcome,
      gse=the_gse
    ) %>%
    .[,c('outcome','gse',colnames(.) %>% .[!(. %in% c('outcome','gse'))])] %>%
    column_to_rownames(var='id')
  
  fData(the_eset)=fData(the_eset)[,-1]
  
  the_eset
  
}