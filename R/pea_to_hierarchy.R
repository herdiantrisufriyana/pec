# Build a function to convert an PEA results to hierarchy dataframe
pea_to_hierarchy=function(the_pea_results){
  if(!is.null(the_pea_results)){
    the_pea_results %>%
      select(go_id,parents) %>%
      lapply(X=seq(nrow(.)),Y=.,FUN=function(X,Y){
        data.frame(
          go_id=Y$go_id[X],
          parents=str_split(Y$parents[X],',')[[1]],
          stringsAsFactors=FALSE
        )
      }) %>%
      do.call(rbind,.) %>%
      setNames(c('from','to'))
  }
}