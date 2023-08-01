# Build a function to bind the comparisons
bind_comparison=function(the_elist,the_grouping){
  the_grouping %>%
    lapply(
      X=seq(nrow(.$count_nrow)),
      Y=.$count_nrow,
      Z=.,
      K=the_elist,
      FUN=function(X,Y,Z,K){
        L=paste0(Y$group[X],'_',Y$non_event[X])
        M=paste0(Y$group[X],'_',Y$event[X])
        
        message(L,' and ',M)
        
        if(is.data.frame(Z[[L]])){
          O=Z[[L]]
        }else{
          O=Z[[L]] %>% do.call(bind_rows,.)
        }
        
        if(is.data.frame(Z[[M]])){
          P=Z[[M]]
        }else{
          P=Z[[M]] %>% do.call(bind_rows,.)
        }
        
        bind_eset(K,O,Y$non_event[X],P,Y$event[X])
      }
    ) %>%
    setNames(paste(
      the_grouping$count_nrow$group,
      the_grouping$count_nrow$non_event,
      the_grouping$count_nrow$event,
      sep='_'
    ))
}