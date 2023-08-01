# Build a function to annotate the GPL probesets 
# using the downloaded annotation file
download_annotation_by_file=function(annot_file,skip_rows,mart){
  gpl=list()
  
  # Parse the annotation data of the GPL
  gpl$soft=
    suppressWarnings(suppressMessages(
      read_tsv(annot_file,skip=skip_rows)
    )) %>%
    .[,c('ID','Gene ID','Gene symbol')] %>%
    rename(
      probe_id=ID
      ,entrezgene_id=`Gene ID`
      ,hgnc_symbol=`Gene symbol`
    ) %>%
    filter(
      !(is.na(entrezgene_id))
      & !(is.na(hgnc_symbol))
    ) %>%
    filter(
      !(str_detect(entrezgene_id,'///'))
      & !(str_detect(hgnc_symbol,'///'))
    )
  
  # Download the annotation data of Ensembl, Entrez, and HGNC
  mart_ens_ent_hgnc=
    getBM(
      mart=mart
      ,attributes=
        c('ensembl_gene_id'
          ,'entrezgene_id'
          ,'hgnc_symbol'
        )
    ) %>%
    apply(2,as.character) %>%
    apply(2,trimws) %>%
    apply(2,\(x)gsub('^$|^ $',NA,x)) %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    filter(
      !(is.na(ensembl_gene_id))
      & !(is.na(entrezgene_id))
      & !(is.na(hgnc_symbol))
    ) %>%
    filter(
      !(str_detect(ensembl_gene_id,'///'))
      & !(str_detect(entrezgene_id,'///'))
      & !(str_detect(hgnc_symbol,'///'))
    )
  
  # Inner join and  make the probe id as row names, retain only single, non-NA annotation
  gpl$final=
    gpl$soft %>%
    mutate_all(as.character) %>%
    inner_join(
      mart_ens_ent_hgnc %>%
        mutate_all(as.character)
      ,by=c('entrezgene_id','hgnc_symbol')
    ) %>%
    .[c(1,4,2,3)] %>%
    group_by(probe_id) %>%
    summarize_all(\(x)
      ifelse(
        length(unique(x))>1
        ,paste(unique(x),collapse='///')
        ,x
      )
    ) %>%
    ungroup() %>%
    filter(
      !(is.na(ensembl_gene_id))
      & !(is.na(entrezgene_id))
      & !(is.na(hgnc_symbol))
    ) %>%
    filter(
      !(str_detect(ensembl_gene_id,'///'))
      & !(str_detect(entrezgene_id,'///'))
      & !(str_detect(hgnc_symbol,'///'))
    ) %>%
    column_to_rownames(var='probe_id')
  
  # Save the GPL
  gpl=gpl$final
  
  # Return
  gpl
}