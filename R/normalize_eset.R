# Build a function to normalize expression data in an expression set
normalize_eset=function(the_eset){
  par(mfrow=c(1,2))
  
  exprs(the_eset) %>% boxplot()
  
  the_eset=ExpressionSet(
    assayData=exprs(the_eset) %>%
      oligo::normalize() %>%
      `dimnames<-`(dimnames(the_eset)),
    phenoData=AnnotatedDataFrame(pData(the_eset)),
    featureData=AnnotatedDataFrame(fData(the_eset))
  )
  
  exprs(the_eset) %>% boxplot()
  
  the_eset
}