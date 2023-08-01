# Do quantile normalization on validation sets by discovery controls before creating the arrays
qnorm_val_by_disc_control=function(the_comparison_list){
  new_validation_exprs=
    the_comparison_list[str_detect(names(the_comparison_list),'OV_')] %>%
    lapply(
      X=seq(length(.)),
      Y=.,
      Z=the_comparison_list[gsub('OV_','OD_',names(.))],
      FUN=function(X,Y,Z){
        Y[[X]] %>%
          exprs() %>%
          normalize.quantiles.use.target(
            target=fData(Z[[X]])$AveExpr
          ) %>%
          `dimnames<-`(dimnames(Y[[X]]))
      }
    ) %>%
    setNames(names(the_comparison_list)[str_detect(names(the_comparison_list),'OV_')])
  
  the_comparison_list[str_detect(names(the_comparison_list),'OV_')] %>%
    lapply(
      X=seq(length(.)),
      Y=.,
      Z=new_validation_exprs,
      FUN=function(X,Y,Z){
        exprs(Y[[X]])=Z[[X]]
        Y[[X]]
      }
    ) %>%
    setNames(names(the_comparison_list)[str_detect(names(the_comparison_list),'OV_')])
}