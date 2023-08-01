# Build a function to convert an CliXO results to hierarchy dataframe
cx_to_hierarchy=function(the_cx_results){
  if(!is.null(the_cx_results)){
    the_cx_results=the_cx_results %>% 
      arrange(desc(relation),target,source) %>%
      filter(relation=='is_a') %>%
      select(source,target) %>%
      setNames(c('from','to'))
    if(nrow(the_cx_results)>0){
      the_cx_results
    }
  }
}