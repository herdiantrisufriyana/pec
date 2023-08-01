# Build a function to integrate DEA results to feature data
integrate_dea_to_fData=function(the_comparison_list,the_dea_results){
  the_comparison_list %>%
    lapply(
      X=seq(length(.)),
      Y=.,
      Z=the_dea_results,
      FUN=function(X,Y,Z){
        message(names(Y[X]))
        ExpressionSet(
          assayData=exprs(Y[[X]]),
          phenoData=AnnotatedDataFrame(pData(Y[[X]])),
          featureData=fData(Y[[X]]) %>%
            rownames_to_column(var='feature') %>%
            left_join(
              Z[[X]]$result %>%
                rownames_to_column(var='feature'),
              by='feature'
            ) %>%
            column_to_rownames(var='feature') %>%
            AnnotatedDataFrame()
        )
      }) %>%
    setNames(names(the_comparison_list))
}