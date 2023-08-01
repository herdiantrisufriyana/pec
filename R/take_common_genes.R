# Build a function to use only the common genes among all sets
take_common_genes=function(the_eset,the_clist){
  the_eset[Reduce(intersect,lapply(the_clist,rownames)),]
}