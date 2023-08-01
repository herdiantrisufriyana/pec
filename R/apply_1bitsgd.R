# Build a function to apply 1-bit stochastic gradient descent (SGD)
apply_1bitsgd=function(the_sum_oda_results,the_comparison_list){
  the_sum_oda_results %>%
    lapply(
      X=seq(nrow(.)),
      Y=.,
      Z=the_comparison_list,
      FUN=function(X,Y,Z){
        message(Y$B[X],'.',Y$A[X],appendLF=FALSE)
        
        the_eset=Z[[Y$B[X]]] %>%
          .[!is.na(fData(.)[,paste0('ovl_',Y$A[X])]),]
        
        if(sum(fData(the_eset)$adj.P.Val<0.05)>2 & dim(the_eset)[2]>2){
          message('. Apply 1-bitSGD.')
          
          the_eset %>%
            exprs() %>%
            sweep(
              MARGIN=1,
              STATS=fData(the_eset)$AveExpr,
              FUN='-'
            ) %>%
            sweep(
              MARGIN=1,
              STATS=ifelse(fData(the_eset)$adj.P.Val<0.05,1,0),
              FUN='*'
            ) %>%
            sapply(function(x){ifelse(x==0,0,ifelse(x>0,1,-1))}) %>%
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