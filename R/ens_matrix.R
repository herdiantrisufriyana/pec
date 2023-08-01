# Build a function to create 0/1 vector based on non-DEG/DEG named by Ensembl gene ID
ens_matrix=function(eset,sig_level=0.05){
  matrix(
    as.integer(rownames(eset) %in% rownames(eset %>% fData() %>% .[.$adj.P.Val<sig_level,])),
    dimnames=list(fData(eset)$ensembl_gene_id,NULL)
  ) %>%
    .[,1]
}