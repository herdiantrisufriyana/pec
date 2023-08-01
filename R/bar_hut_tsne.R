# Build a function to conduct Barnes-Hut t-SNE
bar_hut_tsne=function(the_pear_cor_mat,seed_num=33){
  if(is.null(the_pear_cor_mat)){
    NULL
  }else if(floor((nrow(the_pear_cor_mat)-1)/3)<5){
    NULL
  }else{
    set.seed(seed_num)
    the_pear_cor_mat %>%
      Rtsne(
        dims=3,
        perplexity=max(5,min(50,floor((nrow(.)-1)/3))),
        theta=0.5,
        check_duplicates=FALSE,
        partial_pca=TRUE,
        is_distance=TRUE,
        max_iter=500,
        verbose=TRUE,
        pca_center=FALSE,
        pca_scale=FALSE,
        normalize=FALSE,
        stop_lying_iter=125,
        mom_switch_iter=125,
        momentum=0.5,
        final_momentum=0.8,
        eta=200,
        exaggeration_factor=12,
        num_threads=1
      ) %>%
      .$Y %>%
      `dimnames<-`(list(rownames(the_pear_cor_mat),c('tsne1','tsne2','tsne3')))
  }
}