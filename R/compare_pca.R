# Build a function to compare PCA between two expression data
compare_pca=function(exprs_A,exprs_B,group){
  # PCA before Normalization
  A_pc=prcomp(exprs_A)
  A_pev=round(A_pc$sdev^2/sum(A_pc$sdev^2)*100,2)
  A_pc=A_pc$rotation[,1:2] %>%
    as.data.frame() %>%
    rownames_to_column(var='feature') %>%
    mutate(
      label=group,
      status='A'
    )
  
  # PCA after Normalization
  B_pc=prcomp(exprs_B)
  B_pev=round(B_pc$sdev^2/sum(B_pc$sdev^2)*100,2)
  B_pc=B_pc$rotation[,1:2] %>%
    as.data.frame() %>%
    rownames_to_column(var='feature') %>%
    mutate(
      label=group,
      status='B'
    )
  
  # Plot PCA side by side
  p=rbind(A_pc,B_pc) %>%
    ggplot(aes(x=PC1,y=PC2)) +
    geom_point(aes(color=label),alpha=0.5) +
    facet_wrap(.~status,ncol=2) +
    geom_label(
      aes(
        x=max(A_pc$PC1,B_pc$PC1),
        y=min(A_pc$PC2,B_pc$PC2),
        label=ifelse(status=='A',paste0('% Explained Variance:\n','PC1 (',A_pev[1],'%)\n','PC2 (',A_pev[2],'%)'),
                     ifelse(status=='B',paste0('% Explained Variance:\n','PC1 (',B_pev[1],'%)\n','PC2 (',B_pev[2],'%)'),NA))
      ),
      hjust=1,vjust=0,size=3,alpha=0.5,na.rm=TRUE
    ) +
    scale_x_continuous('PC1') +
    scale_y_continuous('PC2') +
    scale_color_discrete('') +
    theme_light()
  
  print(p)
}