# Build a function to create a table of estimated fetal weight by WHO
create_who_efw=function(){
  suppressMessages(read_csv('data/PMID_29422204.csv')) %>%
    dplyr::select(-reference) %>%
    lapply(
      X=seq(unique(.$percentile)),
      Y=unique(.$percentile),
      Z=.,
      FUN=function(X,Y,Z){
        Z %>%
          filter(percentile==Y[X]) %>%
          lapply(
            X=seq(unique(.$variable)),
            Y=unique(.$variable),
            Z=.,
            K=Y[X],
            FUN=function(X,Y,Z,K){
              Z %>%
                filter(percentile==K & variable==Y[X]) %>%
                gather(ga,newborn_weight,-percentile,-variable) %>%
                mutate(ga=str_split_fixed(ga,'_',2)[,2]) %>%
                mutate_all(as.character) %>%
                right_join(
                  data.frame(
                    percentile=K,
                    variable=Y[X],
                    ga=as.character(20:36)
                  ) %>%
                    mutate_all(as.character),
                  by=c('percentile','variable','ga')
                ) %>%
                mutate_at(c('ga','newborn_weight'),as.numeric) %>%
                mutate(newborn_weight=na_interpolation(newborn_weight))
            }
          ) %>%
          do.call(rbind,.)
      }
    ) %>%
    do.call(rbind,.) %>%
    mutate(variable=gsub('[^[:alnum:] ]','',variable)) %>%
    mutate(variable=substr(variable,1,str_count(variable)-1)) %>%
    filter(variable=='World Health Organization' & str_detect(percentile,'10th'))
}