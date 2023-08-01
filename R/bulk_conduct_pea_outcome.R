# Build a function to bulk-conduct PEA based on gene sets of associated factors that significantly overlap DEGs of the outcome
bulk_conduct_pea_outcome=function(the_sum_oda_results,the_comparison_list,the_filt_go,the_pea_results_af,size_group=c('15-199','200-499','500-1999','2000+')){
  the_sum_oda_results %>%
    pblapply(
      X=seq(nrow(.)),
      Y=.,
      Z=the_comparison_list,
      K=the_filt_go,
      L=the_pea_results_af,
      FUN=function(X,Y,Z,K,L){
        message(Y$B[X],'.',Y$A[X])
        
        if(is.null(Z[[Y$B[X]]]) | is.null(L[[Y$A[X]]])){
          if(is.null(Z[[Y$B[X]]])) message(Y$B[X],' is null')
          if(is.null(L[[Y$A[X]]])) message(Y$A[X],' is null')
          message('Skipped')
          NULL
        }else{
          the_eset_aslist=Z[Y$B[X]]
          the_filt_go=K
          the_pea_results_af=L[[Y$A[X]]]
          
          the_filt_go=the_filt_go %>%
            filter(go_id %in% the_pea_results_af$go_id)
          
          the_eset_aslist %>%
            lapply(subset_by_filt_go,the_filt_go) %>%
            lapply(
              conduct_go_pea,
              the_filt_go,
              size_group=size_group %>% .[. %in% the_filt_go$size_group]
            ) %>%
            lapply(enr_go_ancestors_only) %>%
            .[[1]]
        }
      }
    ) %>%
    setNames(paste0(the_sum_oda_results$B,'.',the_sum_oda_results$A))
}