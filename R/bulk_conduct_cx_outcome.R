# Build a function to bulk-conduct PEA based on gene sets of associated factors that significantly overlap DEGs of the outcome
bulk_conduct_cx_outcome=function(the_pear_cor_mats){
  the_pear_cor_mats %>%
    pblapply(
      X=seq(length(.)),
      Y=.,
      FUN=function(X,Y){
        if(!is.null(Y[[X]])){
          Y[[X]] %>%
            clixo(os='windows')
        }
      }
    ) %>%
    setNames(names(the_pear_cor_mats))
}