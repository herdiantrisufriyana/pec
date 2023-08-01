# Build a function to conduct overlapped DEG analysis (no statistical test)
conduct_oda=function(the_comparison_list){
  the_comparison_list %>%
    lapply(
      X=seq(length(.)),
      Y=.,
      FUN=function(X,Y){
        message('Overlap ',names(Y[X]),' with:')
        
        Z=Y[[X]] %>%
          fData() %>%
          rownames_to_column(var='feature')
        
        K=list()
        K$u=filter(Z,adj.P.Val<0.05 & logFC>0)$feature
        K$d=filter(Z,adj.P.Val<0.05 & logFC<0)$feature
        
        L=Y %>%
          lapply(
            X=seq(length(.)),
            Y=.,
            Z=K,
            K=Z$feature,
            FUN=function(X,Y,Z,K){
              message(names(Y[X]))
              
              L=Y[[X]] %>%
                fData() %>%
                rownames_to_column(var='feature') %>%
                select(feature,logFC,adj.P.Val)
              
              M=list()
              M$u=filter(L,adj.P.Val<0.05 & logFC>0)$feature
              M$d=filter(L,adj.P.Val<0.05 & logFC<0)$feature
              
              N=ifelse(Z$u %in% M$u,'u_u',
                       ifelse(Z$u %in% M$d,'u_d',NA)) %>%
                as.data.frame() %>%
                setNames(paste0('ovl_',names(Y)[X])) %>%
                `rownames<-`(Z$u) %>%
                rownames_to_column(var='feature')
              
              O=ifelse(Z$d %in% M$u,'d_u',
                       ifelse(Z$d %in% M$d,'d_d',NA)) %>%
                as.data.frame() %>%
                setNames(paste0('ovl_',names(Y)[X])) %>%
                `rownames<-`(Z$d) %>%
                rownames_to_column(var='feature')
              
              P=K %>%
                as.data.frame() %>%
                setNames('feature') %>%
                left_join(rbind(N,O),by='feature') %>%
                select(-feature)
              
              P
            }) %>%
          do.call(cbind,.)
        
        fData(Y[[X]])=
          cbind(Z,L) %>%
          column_to_rownames(var='feature')
        
        Y[[X]]
      }
    ) %>%
    setNames(names(the_comparison_list))
}