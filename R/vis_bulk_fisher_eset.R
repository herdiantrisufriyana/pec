# Build a function to visualize ODA results
vis_bulk_fisher_eset=function(the_oda_results){
  the_oda_results %>%
    mutate(
      `Associated factor`=trimws(gsub('_',' ',substr(A,3,str_count(A)))) %>%
        factor(c(
          'PRE ESE','PRE MSE','PRE LSE',
          'EPND EPID','EPND IPID','EPND IPCD',
          'TE T1','TE T2',
          'NC2 CHOR2','NC3 CHOR3',
          'NH2 HELLP2','NH3 HELLP3'
        )),
      `Outcome`=trimws(gsub('_',' ',str_split_fixed(B,'_',3)[,3])) %>%
        factor(
          rev(c(
            c(
              'ECH','LCH','EGH','LGH',
              'EOMPE','LOMPE','EOSPE','LOSPE',
              'SIM EOMPE','SIM LOMPE','SIM EOSPE','SIM LOSPE'
            ),
            paste(
              c(
                'ECH','LCH','EGH','LGH',
                'EOMPE','LOMPE','EOSPE','LOSPE',
                'SIM EOMPE','SIM LOMPE','SIM EOSPE','SIM LOSPE'
              ),
              'FGR'
            ),
            c('EFGR','LFGR'),
            c('ESE','MSE','LSE'),
            c('EPID','IPID','IPCD'),
            c('T1','T2'),
            c('CHOR2','CHOR3'),
            c('HELLP2','HELLP3')
          ))
        ),
      `Regulation`=ifelse(overlap=='Not significant',NA,ifelse(reg.OR>2,2,reg.OR)),
      `Associated factor group`=factor(substr(A,1,1),c('E','D','P','C','H')),
      `Outcome group`=gsub(' FGR','',`Outcome`) %>%
        factor(c(
          'ECH','LCH','EGH','LGH',
          'EOMPE','LOMPE','EOSPE','LOSPE',
          'SIM EOMPE','SIM LOMPE','SIM EOSPE','SIM LOSPE',
          'EFGR','LFGR',
          'ESE','MSE','LSE',
          'EPID','IPID','IPCD',
          'T1','T2',
          'CHOR2','CHOR3',
          'HELLP2','HELLP3'
        ))
    ) %>%
    ggplot(aes(x=`Associated factor`,y=`Outcome`,fill=`Regulation`)) +
    geom_tile(color='white') +
    geom_text(
      aes(
        label=ifelse(overlap=='Not significant',round(size_pass_prop_A*size_pass_prop_B*100),paste0(round(jaccard*100),ifelse(regulation=='Undifferentiated','','*'))),
        color=ifelse(overlap=='Not significant','A','B'),
      ),
      size=2,na.rm=TRUE
    ) +
    facet_grid(`Outcome group`~`Associated factor group`,scale='free',space='free') +
    scale_color_manual(guide=FALSE,values=c('white','black')) +
    scale_fill_gradient2(
      '',
      low='red',mid='yellow',high='green',midpoint=1,na.value='black',
      breaks=c(0,0.33,0.66,1,1.33,1.66,2),labels=c('Counterexpression','','','Undifferentiated','','','Coexpression')
    ) +
    theme(
      axis.text.x=element_text(angle=90,hjust=1),
      strip.text.y=element_text(angle=0)
    )
}