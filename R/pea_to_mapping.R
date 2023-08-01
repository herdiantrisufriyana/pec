# Build a function to convert PEA results and expression sets to mapping dataframes
pea_to_mapping=function(the_pea_results,the_eset){
  the_pea_results %>%
    lapply(
      X=seq(length(.)),
      Y=.,
      Z=the_eset,
      FUN=function(X,Y,Z){
        if(!is.null(Y[[X]])){
          Y[[X]] %>%
            select(go_id) %>%
            left_join(filt_go,by='go_id') %>%
            select(go_id,ensembl_gene_id) %>%
            left_join(
              fData(Z[[str_split_fixed(names(Y[X]),'[.]',2)[,1]]]) %>%
                rownames_to_column(var='feature') %>%
                .[,c('feature','ensembl_gene_id',paste0('ovl_',str_split_fixed(names(Y[X]),'[.]',2)[,2]))] %>%
                setNames(c('feature','ensembl_gene_id','ovl')) %>%
                filter(!is.na(ovl)) %>%
                select(feature,ensembl_gene_id),
              by='ensembl_gene_id'
            ) %>%
            filter(!is.na(feature)) %>%
            select(go_id,feature) %>%
            setNames(c('oid','feature'))
        }
      }
    ) %>%
    setNames(names(the_pea_results))
}