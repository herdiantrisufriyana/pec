# Build a function to integrate DEA results to the corresponding validation sets
integrate_dea_to_val=function(the_comparison_list,the_dea_results,the_disc_val_sets){
  the_comparison_list[the_disc_val_sets]=
    the_comparison_list[the_disc_val_sets] %>%
    lapply(
      X=seq(length(.)),
      Y=names(.),
      Z=.,
      K=the_dea_results,
      FUN=function(X,Y,Z,K){
        fData(Z[[X]])=fData(Z[[X]]) %>%
          rownames_to_column(var='feature') %>%
          left_join(
            K[[paste0('OD_',str_split_fixed(Y[X],'_',2)[,2])]] %>%
              .$result %>%
              rownames_to_column(var='feature'),
            by='feature'
          ) %>%
          column_to_rownames(var='feature')
        
        Z[[X]]
      }
    ) %>%
    setNames(the_disc_val_sets)
  
  the_comparison_list
}