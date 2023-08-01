# Build a function to merge expression set by bit transformation and ComBat
merge_eset=function(the_eset){
  the_eset0=the_eset
  
  denom=the_eset %>%
    sapply(
      X=seq(length(unique(pData(.)$gse))),
      Y=unique(pData(.)$gse),
      Z=.,
      FUN=function(X,Y,Z){
        exprs(Z) %>%
          .[,pData(Z)$gse==Y[X]] %>%
          .[,1] %>%
          max() %>%
          abs() %>%
          ceiling %>%
          sapply(function(x){seq(4,40,4)[findInterval(x,seq(4,40,4))]})
      }
    ) %>%
    setNames(unique(pData(the_eset)$gse))
  
  par(mfrow=c(1,3))
  
  exprs(the_eset) %>% boxplot(main='Original')
  
  exprs(the_eset)=
    exprs(the_eset) %>%
    sweep(2,rep(max(denom),ncol(the_eset))/denom[the_eset$gse],'*')
  
  exprs(the_eset) %>% boxplot(main='Expression Bit Transformation')
  
  exprs(the_eset)=suppressMessages(ComBat(
    dat=exprs(the_eset),
    batch=the_eset$gse,
    mod=model.matrix(~1,data=pData(the_eset)),
    par.prior=TRUE,
    prior.plots=FALSE
  ))
  
  exprs(the_eset) %>% boxplot(main='Adjust Batch Effect')
  
  compare_pca(exprs(the_eset0),exprs(the_eset),the_eset$gse)
  
  the_eset
}