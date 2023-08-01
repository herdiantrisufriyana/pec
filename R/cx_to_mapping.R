# Build a function to convert CliXO results to mapping dataframes
cx_to_mapping=function(the_cx_results){
  if(!is.null(the_cx_results)){
    the_cx_results %>%
      arrange(desc(relation),target,source) %>%
      lapply(
        X=seq(length(unique(.$target))),
        Y=unique(.$target),
        Z=.,
        FUN=function(X,Y,Z){
          K=filter(Z,target==Y[X])
          while(sum(K$relation=='is_a')>0){
            L=filter(K,relation=='is_a')
            M=filter(Z,target %in% L$source) %>%
              mutate(target=Y[X])
            K=filter(K,relation!='is_a') %>%
              rbind(M)
          }
          K
        }
      ) %>%
      do.call(rbind,.) %>%
      select(target,source) %>%
      setNames(c('oid','feature'))
  }
}