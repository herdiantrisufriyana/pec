# Build a function to read multiple alignOntology results by CliXO results (NULL kept)
read_ao_by_rc_results=function(the_cx_results,path,the_complete_go,the_go){
  the_cx_results %>%
    pblapply(
      X=seq(length(.)),
      Y=.,
      Z=the_complete_go,
      K=the_go,
      FUN=function(X,Y,Z,K){
        # message(names(Y[X]))
        
        if(!is.null(Y[[X]])){
          suppressMessages(suppressWarnings(
              read_tsv(paste0(path,names(Y[X]),'_FDRs.out'))
            )) %>%
            setNames('column') %>%
            .[!str_detect(.$column,'#'),1:5] %>%
            setNames(c('computed','reference','similarity','fdr','termin_nodes')) %>%
            mutate_at(1:2,as.character) %>%
            mutate_at(3:5,as.numeric) %>%
            left_join(
              data.frame(reference=Z$id) %>% rownames_to_column(var='id') %>%
                left_join(
                  data.frame(term=Z$name) %>% rownames_to_column(var='id'),
                  by='id'
                ) %>%
                left_join(
                  data.frame(definition=Z$def) %>% rownames_to_column(var='id'),
                  by='id'
                ) %>%
                select(-id),
              by='reference'
            ) %>%
            right_join(
              Y[[X]] %>%
                cx_to_mapping %>%
                group_by(oid) %>%
                summarise(comp_feature=paste(feature,collapse=',')) %>%
                ungroup() %>%
                rename(computed=oid),
              by='computed'
            ) %>%
            left_join(
              filter(K,parents %in% .$reference & relation=='gene') %>%
                group_by(parents) %>%
                summarise(ref_feature=paste(child,collapse=',')) %>%
                ungroup() %>%
                rename(reference=parents),
              by='reference'
            ) %>%
            mutate(
              common_termin=sapply(X=seq(nrow(.)),Y=comp_feature,Z=ref_feature,FUN=function(X,Y,Z){
                K=paste(intersect(str_split(Y[X],',')[[1]],str_split(Z[X],',')[[1]]),collapse=',')
                ifelse(K=='',NA,K)
              }),
              termin_nodes=sapply(X=seq(nrow(.)),Y=comp_feature,FUN=function(X,Y){
                length(str_split(Y[X],',')[[1]])
              })
            ) %>%
            arrange(computed)
        }
      }
    ) %>%
    setNames(names(the_cx_results))
}