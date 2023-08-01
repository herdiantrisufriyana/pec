# Build a function to subset an expression set by feature within the filter dataframe
subset_by_filt_go=function(the_eset,the_filt_go){
  the_eset %>%
    .[fData(.)$ensembl_gene_id %in% the_filt_go$ensembl_gene_id,]
}