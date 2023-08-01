# Build a function to create GO filtering dataframe
create_filt_go=function(the_comparison_list){
  the_comparison_list %>%
    pblapply(fData_ens) %>%
    unlist() %>%
    .[!duplicated(.)] %>%
    getgo('hg19','ensGene') %>%
    .[!is.na(names(.))] %>%
    pblapply(exc_go_iea) %>%
    pblapply(exc_go_igi) %>%
    .[sapply(.,length)>0] %>%
    `names<-`(paste0(names(.),'.')) %>%
    unlist() %>%
    as.data.frame() %>%
    rownames_to_column(var='ensembl_gene_id') %>%
    setNames(c('ensembl_gene_id','go_id')) %>%
    mutate(ensembl_gene_id=str_split_fixed(ensembl_gene_id,'[.]',2)[,1]) %>%
    left_join(
      group_by(.,go_id) %>%
        summarise(numInCat=n()),
      by='go_id'
    ) %>%
    mutate(
      size_group=ifelse(numInCat<15,'<15',
                        ifelse(numInCat<200,'15-199',
                               ifelse(numInCat<500,'200-499',
                                      ifelse(numInCat<2000,'500-1999','2000+'))))
    ) %>%
    left_join(get_go_annot(unique(.$go_id)),by='go_id')
}