# Build a function to create Pearson correlation matrix
pear_cor_mat=function(the_sum_oda_results,the_comparison_list){
  the_sum_oda_results %>%
    lapply(
      X=seq(nrow(.)),
      Y=.,
      Z=the_comparison_list,
      FUN=function(X,Y,Z){
        message(Y$B[X],'.',Y$A[X],appendLF=FALSE)
        
        the_eset=Z[[Y$A[X]]] %>%
          .[!is.na(fData(.)[,paste0('ovl_',Y$B[X])]),]
        
        if(sum(fData(the_eset)$adj.P.Val<0.05)>2 & dim(the_eset)[2]>2){
          message('. Create the Pearson correlation matrix.')
          
          the_eset=the_eset[fData(the_eset)$adj.P.Val<0.05,]
          
          exprs_the_eset_centered=
            exprs(the_eset) %>%
            sweep(1,fData(the_eset)$AveExpr,'-')
          
          exprs(the_eset) %>%
            rownames() %>%
            lapply(X=seq(length(.)-1),Y=.,function(X,Y){
              data.frame(
                predictor1=Y[X]
                ,predictor2=Y[(X+1):length(.)]
              )
            }) %>%
            do.call(rbind,.) %>%
            mutate(
              pearson=
                pbsapply(X=seq(nrow(.)),Y=.,Z=exprs_the_eset_centered,function(X,Y,Z){
                  cor(
                    Z[Y$predictor1[X],]
                    ,Z[Y$predictor2[X],]
                    ,method='pearson'
                  )
                })
            ) %>%
            rbind(
              setNames(select(.,predictor2,predictor1,everything()),colnames(.))
              ,data.frame(
                predictor1=rownames(exprs(the_eset))
                ,predictor2=rownames(exprs(the_eset))
                ,pearson=1
              )
            ) %>%
            spread(predictor2,pearson) %>%
            column_to_rownames(var='predictor1') %>%
            as.matrix()
        }else{
          message(' is null.')
          NULL
        }
      }
    ) %>%
    setNames(paste0(the_sum_oda_results$B,'.',the_sum_oda_results$A))
}