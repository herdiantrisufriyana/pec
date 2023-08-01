# Build a function to compute minimum sample size for FDR on an expression set
FDR_min_size=function(the_eset_list){
  pointwise_FDR_min_size=function(x){
    pp=try(power.t.test.FDR(sd(x),delta=log2(2),FDR.level=0.05,pi0=1-0.05/2,power=0.8),silent=TRUE)
    if(inherits(pp,"try-error")){
      NA
    }else{
      pp$n
    }
  }
  
  min_size_df=function(the_eset){
    the_eset[,as.numeric(the_eset$outcome)==1] %>%
      exprs() %>%
      apply(1,pointwise_FDR_min_size) %>%
      as.data.frame() %>%
      setNames('FDR_min_size') %>%
      rownames_to_column(var='feature')
  }
  
  integrate_min_size=function(the_eset,the_min_size_df){
    fData(the_eset)=fData(the_eset) %>%
      rownames_to_column(var='feature') %>%
      left_join(the_min_size_df,by='feature') %>%
      column_to_rownames(var='feature')
    
    the_eset
  }
  
  message('Determine the controls')
  unique_controls=names(the_eset_list) %>%
    str_split_fixed('_',3) %>%
    .[,1:2] %>%
    sapply(X=seq(nrow(.)),Y=.,function(X,Y){paste(Y[X,],collapse='_')}) %>%
    unique()
  
  the_eset_list=the_eset_list %>%
    pblapply(
      X=seq(length(unique_controls)),
      Y=unique_controls,
      Z=.,
      FUN=function(X,Y,Z){
        message('Compute min. size of ',Y[X])
        
        the_df=
          Z[str_detect(names(Z),Y[X])] %>%
          .[[1]] %>%
          min_size_df()
        
        Z[str_detect(names(Z),Y[X])] %>%
          lapply(
            X=seq(length(.)),
            Y=.,
            Z=the_df,
            FUN=function(X,Y,Z){
              message('Integrate ',names(Y[X]))
              
              integrate_min_size(Y[[X]],Z)
            }
          ) %>%
          setNames(names(Z)[str_detect(names(Z),Y[X])])
      }
    ) %>%
    unlist()
  
  the_eset_list
}