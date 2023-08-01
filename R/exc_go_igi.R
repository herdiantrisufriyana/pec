# Build a function to exclude GO IDs with IGI evidence code
exc_go_igi=function(the_go_list_by_ens){
  the_go_list_by_ens %>%
    .[!(. %in% go_evidence$go_id[go_evidence$go_linkage_type=='IGI'])]
}