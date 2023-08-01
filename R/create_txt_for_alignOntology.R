# Build a function to write txt files containing the computed and reference ontologies for alignment
create_txt_for_alignOntology=function(the_cx_results,the_go_ref,save_dir){
  the_cx_results %>%
    pblapply(
      X=seq(length(.)),
      Y=.,
      FUN=function(X,Y){
        if(!is.null(Y[[X]])){
          Y[[X]] %>%
            select(target,source,relation) %>%
            write_tsv(paste0(save_dir,'/',names(Y[X]),'.txt'),col_names=FALSE)
        }
      }
    )
  
  the_go_ref %>%
    select(parents,child,relation) %>%
    write_tsv(paste0(save_dir,'/','go.txt'),col_names=FALSE)
}