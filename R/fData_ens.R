# Build a function to take the Ensembl IDs from the feature data of an expression set
fData_ens=function(the_eset){
  fData(the_eset)$ensembl_gene_id
}