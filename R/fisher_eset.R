# Build a function to conduct overlapped DEG analysis by Fisher test
fisher_eset=function(list_of_AB){
  A=list_of_AB[,1]
  B=list_of_AB[,2]
  
  AB=list(
    Ad=rownames(fData(A[[1]]) %>% .[.$adj.P.Val<0.05 & .$logFC<0,]),
    Az=rownames(fData(A[[1]]) %>% .[.$adj.P.Val>=0.05,]),
    Au=rownames(fData(A[[1]]) %>% .[.$adj.P.Val<0.05 & .$logFC>0,]),
    Bd=rownames(fData(B[[1]]) %>% .[.$adj.P.Val<0.05 & .$logFC<0,]),
    Bz=rownames(fData(B[[1]]) %>% .[.$adj.P.Val>=0.05,]),
    Bu=rownames(fData(B[[1]]) %>% .[.$adj.P.Val<0.05 & .$logFC>0,])
  )
  
  outcome_min_n=list(
    A=min(sum(as.numeric(A[[1]]$outcome)==1),sum(as.numeric(A[[1]]$outcome)==2)),
    B=min(sum(as.numeric(B[[1]]$outcome)==1),sum(as.numeric(B[[1]]$outcome)==2))
  )
  
  size_pass_prop=list(
    A=sum(fData(A[[1]])$FDR_min_size>=outcome_min_n$A,na.rm=TRUE)/nrow(A[[1]]),
    B=sum(fData(B[[1]])$FDR_min_size>=outcome_min_n$B,na.rm=TRUE)/nrow(B[[1]])
  )
  
  ovl_mat=matrix(
    c(
      length(intersect(c(AB$Au,AB$Ad),c(AB$Bu,AB$Bd))),
      length(setdiff(c(AB$Bu,AB$Bd),c(AB$Au,AB$Ad))),
      length(setdiff(c(AB$Au,AB$Ad),c(AB$Bu,AB$Bd))),
      length(setdiff(
        unique(c(AB$Az,AB$Bz)),
        unique(c(AB$Au,AB$Ad,AB$Bu,AB$Bd))
      ))
    ),
    ncol=2,byrow=TRUE,
    dimnames=list(c('Ai','Ao'),c('Bi','Bo'))
  )
  
  reg_mat=matrix(
    c(
      length(intersect(AB$Au,AB$Bu)),
      length(intersect(AB$Au,AB$Bd)),
      length(intersect(AB$Ad,AB$Bu)),
      length(intersect(AB$Ad,AB$Bd))
    ),
    ncol=2,byrow=TRUE,
    dimnames=list(c('Au','Ad'),c('Bu','Bd'))
  )
  
  jaccard=sum(c(ovl_mat)[1])/sum(c(ovl_mat)[1:3])
  
  rbind(
    ovl_mat %>%
      as.data.frame() %>%
      rownames_to_column(var='A') %>%
      gather(B,value,-A) %>%
      mutate_at('B',as.character) %>%
      mutate(variable=paste0(A,'_',B)) %>%
      select(variable,value),
    ovl_mat %>%
      fisher.test(alternative='greater') %>%
      .[c('p.value','conf.int','estimate')] %>%
      unlist() %>%
      as.data.frame() %>%
      rownames_to_column(var='variable') %>%
      mutate(variable=paste0('ovl.',variable)) %>%
      setNames(c('variable','value')),
    reg_mat %>%
      as.data.frame() %>%
      rownames_to_column(var='A') %>%
      gather(B,value,-A) %>%
      mutate_at('B',as.character) %>%
      mutate(variable=paste0(A,'_',B)) %>%
      select(variable,value),
    reg_mat %>%
      fisher.test() %>%
      .[c('p.value','conf.int','estimate')] %>%
      unlist() %>%
      as.data.frame() %>%
      rownames_to_column(var='variable') %>%
      mutate(variable=paste0('reg.',variable)) %>%
      setNames(c('variable','value'))
  ) %>%
    column_to_rownames(var='variable') %>%
    t() %>%
    as.data.frame() %>%
    setNames(c(
      'Ai_Bi','Ao_Bi','Ai_Bo','Ao_Bo','ovl.P.Value','ovl.OR.LB','ovl.OR.UB','ovl.OR',
      'Au_Bu','Ad_Bu','Au_Bd','Ad_Bd','reg.P.Value','reg.OR.LB','reg.OR.UB','reg.OR'
    )) %>%
    mutate(
      overlap=ifelse(ovl.P.Value<0.05,'Significant','Not significant'),
      regulation=ifelse(reg.P.Value>=0.05,'Undifferentiated',
                        ifelse(reg.OR>1,'Coexpression',
                               ifelse(reg.OR<1,'Counterexpression','Balance'))),
      A=names(A),
      B=names(B)
    ) %>%
    .[,c('A','B',colnames(.) %>% .[!(. %in% c('A','B'))])] %>%
    mutate(
      jaccard=ifelse(is.nan(jaccard),0,jaccard),
      size_pass_prop_A=size_pass_prop$A,
      size_pass_prop_B=size_pass_prop$B
    )
}