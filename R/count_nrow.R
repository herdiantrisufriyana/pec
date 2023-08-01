# Build a function to count the row number of each dataset in the list
count_nrow=function(the_dataset_list){
  lapply(the_dataset_list,function(x){ifelse(is.data.frame(x),nrow(x),sum(unlist(lapply(x,nrow))))}) %>%
    as.data.frame() %>%
    gather(
      event
      ,event_value
      ,-E_PRE,-D_EPND,-P_TE,-C2_NC2,-C3_NC3,-H2_NH2,-H3_NH3,-OD_C,-OV_C
    ) %>%
    gather(
      non_event
      ,non_event_value
      ,-event,-event_value
    ) %>%
    filter(str_split_fixed(event,'_',2)[,1]==str_split_fixed(non_event,'_',2)[,1]) %>%
    mutate(
      group=str_split_fixed(event,'_',2)[,1],
      event=str_split_fixed(event,'_',2)[,2],
      non_event=str_split_fixed(non_event,'_',2)[,2]
    ) %>%
    .[,c('group',colnames(.) %>% .[.!='group'])] %>%
    filter(
      (group!='OV' & (event_value>=3 & non_event_value>=3))
      |
        (group=='OV' & (event_value>0 & non_event_value>0))
    )
}