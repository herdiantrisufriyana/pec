# Build a function to exclude GO IDs with IEA evidence code
exc_go_iea=function(the_go_list_by_ens){
  the_go_list_by_ens %>%
    .[!(. %in% go_evidence$go_id[go_evidence$go_linkage_type=='IEA'])]
}