# Build a function to bulk-conduct overlapped DEG analysis by Fisher test
bulk_fisher_eset=function(the_comparison_list){
  list(
    the_comparison_list[!str_detect(names(the_comparison_list),'O._')],
    c(
      the_comparison_list[str_detect(names(the_comparison_list),'OD_')],
      the_comparison_list[!str_detect(names(the_comparison_list),'O._')]
    )
  ) %>%
    expand.grid() %>%
    lapply(X=seq(nrow(.)),Y=.,FUN=function(X,Y){
      message(names(Y[X,1]),' and ',names(Y[X,2]))
      fisher_eset(Y[X,])
    }) %>%
    do.call(rbind,.)
}