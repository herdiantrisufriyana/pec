# Build a function to bind all phenotypes of interest
bind_poi=function(phenotype_input,conv_pcol_table,coi_list){
  pick_coi_and_rename_col=function(id){
    # From columns of interest, which one (the index) is not NA for this ID
    which_not_na=which(!(is.na(conv_pcol[coi,id])))
    
    # Find original names of the non-NA columns in the convertion table,
    # then make new table from the original one containing only columns of interest using standard names
    coi[which_not_na] %>%
      conv_pcol[.,id] %>%
      as.character() %>%
      phenotype_input[[id]][,.] %>%
      rownames_to_column(var='gsm') %>%
      setNames(c('gsm',rownames(conv_pcol)[coi[which_not_na]])) %>%
      lapply(as.character) %>%
      bind_rows() %>%
      mutate(gse=id)
  }
  
  df_list=list(
    # GSE4888
    GSE4888=pick_coi_and_rename_col('GSE4888') %>% mutate(set='Discovery set, Associated factor, Endometrium'),
    # GSE6364
    GSE6364=pick_coi_and_rename_col('GSE6364') %>% mutate(set='Discovery set, Associated factor, Endometrium'),
    # EMTAB680
    EMTAB680=pick_coi_and_rename_col('EMTAB680') %>% mutate(set='Discovery set, Associated factor, Decidua'),
    # GSE12767
    GSE12767=pick_coi_and_rename_col('GSE12767') %>% mutate(set='Discovery set, Associated factor, Placenta'),
    # GSE9984
    GSE9984=pick_coi_and_rename_col('GSE9984') %>% mutate(set='Discovery set, Associated factor, Placenta'),
    # GSE75010
    GSE75010=pick_coi_and_rename_col('GSE75010') %>% mutate(set='Discovery set, Outcome, Placenta'),
    # GSE98224
    GSE98224=pick_coi_and_rename_col('GSE98224') %>% mutate(set='Discovery set, Outcome, Placenta'),
    # GSE100415
    GSE100415=pick_coi_and_rename_col('GSE100415') %>% mutate(set='Discovery set, Outcome, Placenta'),
    # GSE30186
    GSE30186=pick_coi_and_rename_col('GSE30186') %>% mutate(set='Validation set, Outcome, Placenta'),
    # GSE10588
    GSE10588=pick_coi_and_rename_col('GSE10588') %>% mutate(set='Validation set, Outcome, Placenta'),
    # GSE24129
    GSE24129=pick_coi_and_rename_col('GSE24129') %>% mutate(set='Validation set, Outcome, Placenta'),
    # GSE25906
    GSE25906=pick_coi_and_rename_col('GSE25906') %>% mutate(set='Validation set, Outcome, Placenta'),
    # GSE4707
    GSE4707=pick_coi_and_rename_col('GSE4707') %>% mutate(set='Validation set, Outcome, Placenta'),
    # GSE44711
    GSE44711=pick_coi_and_rename_col('GSE44711') %>% mutate(set='Validation set, Outcome, Placenta'),
    # GSE128381
    GSE128381=pick_coi_and_rename_col('GSE128381') %>% mutate(set='Validation set, Outcome, Placenta')
  )
  
  bind_rows(df_list) %>%
    .[,c('set','gse',colnames(.)[!(colnames(.) %in% c('set','gse'))])]
}