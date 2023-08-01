# Build a function to conduct pathway enrichment analysis
conduct_go_pea=function(the_eset,the_filt_go,size_groups=c('15-199','200-499','500-1999','2000+')){
  lapply(
    X=size_groups,
    Y=the_eset,
    Z=the_filt_go,
    FUN=function(X,Y,Z){
      ens_matrix(Y) %>%
        nullp(
          genome='hg19',
          id='ensGene',
          plot.fit=FALSE
        ) %>%
        goseq(
          genome='hg19',
          id='ensGene',
          method="Wallenius",
          gene2cat=filter(Z,size_group==X)[,c('ensembl_gene_id','go_id')]
        ) %>%
        # Do multiple testing by Benjamini-Hochberg FDR
        mutate(
          over_represented_padj=p.adjust(over_represented_pvalue,method="BH"),
          under_represented_padj=p.adjust(under_represented_pvalue,method="BH"),
          pct_rep=numDEInCat/numInCat
        ) %>%
        filter(over_represented_padj<0.05) %>%
        # Clean up everything
        arrange(
          factor(ontology,levels=c("BP","MF","CC")),
          desc(pct_rep),
          desc(numDEInCat)
        ) %>%
        select(
          category,term,ontology,numDEInCat,numInCat,pct_rep,
          over_represented_pvalue,over_represented_padj
        ) %>%
        rename(go_id=category) %>%
        left_join(
          Z %>%
            filter(size_group==X) %>%
            select(go_id,size_group,definition,parents,ancestors) %>%
            .[!duplicated(.),],
          by='go_id'
        )
    }) %>%
    do.call(rbind,.)
}