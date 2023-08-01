# Build a function to get only the enriched parents and ancestors of PEA results
enr_go_ancestors_only=function(the_pea_results,size_groups=c('15-199','200-499','500-1999','2000+')){
  the_pea_results %>%
    lapply(
      X=size_groups,
      Y=.,
      FUN=function(X,Y){
        message('Size group: ',X)
        
        Z=filter(Y,size_group==X)
        if(nrow(Z)>0){
          K=filter(Y,size_group!=X)
          
          Z %>%
            lapply(
              X=seq(nrow(.)),
              Y=.,
              Z=K$go_id,
              FUN=function(X,Y,Z){
                message('GO ID:',Y$go_id[X] )
                message('- Original: ',Y$parents[X])
                
                # Check if parents/ancestors are enriched
                K=Y[X,] %>%
                  mutate(
                    ori_parents=parents,
                    ori_ancestors=ancestors,
                    parents=str_split(Y$parents[X],',')[[1]] %>%
                      .[. %in% c('root',Z)] %>%
                      paste(collapse=','),
                    ancestors=str_split(Y$ancestors[X],',')[[1]] %>%
                      .[. %in% c('root',Z)] %>%
                      paste(collapse=',')
                  ) %>%
                  # If none of the ancestors are enriched, then substitute to root
                  mutate(
                    ancestors=ifelse(ancestors=='','root',ancestors)
                  )
                
                # Are there any of the parents not enriched?
                L=Y$parents[X] %>%
                  str_split(',') %>%
                  .[[1]] %>%
                  .[!(. %in% c('root',Z))]
                
                # Start recording the parents that are not enriched
                M=L
                
                # While there are no parents or any of the parents are not enriched, seek the enriched parents as substitutes
                while(K$parents=='' | length(L)>0){
                  N=L %>%
                    lapply(function(x){
                      if(x=='root'){
                        'root'
                      }else if(Ontology(GOTERM[[x]])=='BP'){
                        GOBPPARENTS[[x]]
                      }else if(Ontology(GOTERM[[x]])=='MF'){
                        GOMFPARENTS[[x]]
                      }else if(Ontology(GOTERM[[x]])=='CC'){
                        GOCCPARENTS[[x]]
                      }else if(Ontology(GOTERM[[x]])=='BP'){
                        GOBPPARENTS[[x]]
                      }else{
                        NA
                      }
                    }) %>%
                    unlist()
                  
                  # Enriched parents and next parents are concatenated
                  N=Y$parents[X] %>%
                    str_split(',') %>%
                    .[[1]] %>%
                    .[. %in% c('root',Z)] %>%
                    c(N)
                  
                  N=ifelse(N=='all','root',N) %>%
                    .[!duplicated(.)]
                  
                  Y$parents[X]=
                    N %>%
                    paste(collapse=',')
                  
                  message('- Subtituted: ',Y$parents[X],'; ',appendLF=FALSE)
                  
                  # The concatenated parents are split if these are enriched or not
                  K$parents=
                    N %>%
                    .[. %in% c('root',Z)] %>%
                    paste(collapse=',')
                  
                  L=Y$parents[X] %>%
                    str_split(',') %>%
                    .[[1]] %>%
                    .[!(. %in% c('root',Z))]
                  
                  M=c(M,L) %>%
                    .[!duplicated(.)]
                  
                  message('(not enriched=',length(L),') from ',paste(c(unique(Z),'root'),collapse=', '))
                }
                
                # Remove all ancestors that are parts of the non-enriched parent records
                K$ancestors=
                  K$ancestors %>%
                  str_split(',') %>%
                  .[[1]] %>%
                  .[!(. %in% M)] %>%
                  paste(collapse=',')
                
                K %>%
                  mutate(rm_ancestors=paste(M,collapse=','))
              }
            ) %>%
            do.call(rbind,.)
        }
      }
    ) %>%
    do.call(rbind,.)
}