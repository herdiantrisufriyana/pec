# Build a function to center gene expression
center_gene_exp=function(the_sum_oda_results,the_comparison_list,the_mapping){
  the_sum_oda_results %>%
    lapply(
      X=seq(nrow(.)),
      Y=.,
      Z=the_comparison_list,
      K=the_mapping,
      FUN=function(X,Y,Z,K){
        message(Y$B[X],'.',Y$A[X],appendLF=FALSE)
        
        the_eset=Z[[Y$B[X]]] %>%
            .[fData(the_eset)$adj.P.Val<0.05,] %>%
            .[!is.na(fData(.)[,paste0('ovl_',Y$A[X])]),] %>%
            .[rownames(.) %>% 
                .[.
                  %in%K[[
                    str_replace_all(paste0(Y$B[X],'.',Y$A[X]),'OV_','OD_')
                  ]]$feature
                ]
              ,
            ]
        
        if(sum(fData(the_eset)$adj.P.Val<0.05)>2 & dim(the_eset)[2]>2){
          message('. Standardize gene expression.')
          
          the_eset %>%
            exprs() %>%
            sweep(
              MARGIN=1,
              STATS=fData(the_eset)$AveExpr,
              FUN='-'
            ) %>%
            matrix(ncol=ncol(the_eset),byrow=FALSE,dimnames=dimnames(the_eset)) %>%
            as.data.frame()
        }else{
          message(' is null.')
          NULL
        }
      }
    ) %>%
    setNames(paste0(the_sum_oda_results$B,'.',the_sum_oda_results$A))
}