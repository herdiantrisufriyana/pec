# Build a function to retrieve columns of interest from phenotype table or publication
preprocess_coi=function(phenotype_input){
  
  # Associated factors for discovery sets
  
  ## GSE4888
  if(!is.null(phenotype_input$GSE4888)){
    phenotype_input$GSE4888=phenotype_input$GSE4888 %>% select(c(1,8,10:11)) %>%
      # Parse characteristics_ch1.1
      rownames_to_column(var='gsm') %>%
      mutate(characteristics_ch1.1=as.character(characteristics_ch1.1)) %>%
      mutate(
        age=as.numeric(str_split_fixed(str_split_fixed(characteristics_ch1.1,': ',3)[,2],
                                       ' ',2)[,1]),
        characteristics_ch1.1=str_split_fixed(characteristics_ch1.1,': ',3)[,3],
        race=ifelse(str_detect(characteristics_ch1.1,'Caucasian'),'Caucasian',
                    ifelse(str_detect(characteristics_ch1.1,'Asian'),'Asian',
                           ifelse(str_detect(characteristics_ch1.1,'Black'),'Black',
                                  'Unknown'))),
        characteristics_ch1.1=ifelse(race=='Caucasian',sub('Caucasian','',characteristics_ch1.1),
                                     ifelse(race=='Asian',sub('Asian','',characteristics_ch1.1),
                                            ifelse(race=='Black',sub('Black','',characteristics_ch1.1),
                                                   characteristics_ch1.1))),
        tissue=str_split_fixed(title,' ',2)[,1],
        sample=str_split_fixed(title,' ',2)[,2],
        others=characteristics_ch1.1,
        characteristics_ch1.1='Control'
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  ## GSE6364
  if(!is.null(phenotype_input$GSE6364)){
    phenotype_input$GSE6364=phenotype_input$GSE6364 %>% select(c(1,8,10)) %>%
      # Parse characteristics_ch1
      rownames_to_column(var='gsm') %>%
      mutate(characteristics_ch1=as.character(characteristics_ch1)) %>%
      mutate(
        tissue='endometrium',
        end_phase=ifelse(str_detect(characteristics_ch1,'Proliferative'),'Proliferative phase PE',
                         ifelse(str_detect(characteristics_ch1,'Early Secretory'),'Early secretory phase ESE',
                                ifelse(str_detect(characteristics_ch1,'Mid Secretory'),'Mid secretory phase MSE',
                                       'Ambiguous histology reading	'))),
        others=ifelse(str_detect(characteristics_ch1,'Endometriosis'),'Endometriosis',
                      ifelse(str_detect(characteristics_ch1,'Normal'),
                             'Control',NA))
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  ## EMTAB680
  if(!is.null(phenotype_input$EMTAB680)){
    phenotype_input$EMTAB680=phenotype_input$EMTAB680 %>% select(c(1,7,23)) %>%
      # Parse Source.Name into pregnancy
      rownames_to_column(var='gsm') %>%
      mutate(pregnancy=ifelse(str_detect(Source.Name,'ECT3'),'EP','IUP')) %>%
      column_to_rownames(var='gsm')
  }
  
  ## GSE12767
  if(!is.null(phenotype_input$GSE12767)){
    phenotype_input$GSE12767=phenotype_input$GSE12767 %>%
      # Add columns from the publication (PMID:19027158)
      arrange(title) %>%
      mutate(
        age=c(37,36,37,rep(NA,8)),
        gravidity=c(2,2,1,rep(NA,8)),
        parity=c(rep(0,3),rep(NA,8)),
        race=c(rep('W',3),rep(NA,8)),
        bmi=c(27.5,36.0,29.1,rep(NA,8)),
        ga=c(38,35.7,34.9,rep(NA,8)),
        ga_sample=c(11.4,12.4,11.0,rep(NA,8)),
        mode=c('c-section','vaginal','c-section',rep(NA,8)),
        sbp=c(155,164,176,rep(NA,8)),
        dbp=c(90,88,115,rep(NA,8)),
        sbp_int=c('145','155 (95% CI 145~164)',
                  '151 (95% CI 134~176)',
                  rep(NA,8)),
        dbp_int=c('80','90 (95% CI 80~88)',
                  '93 (95% CI 84~115)',
                  rep(NA,8)),
        sbp_pp_int=c(109,116,102,rep(NA,8)),
        dbp_pp_int=c(69,69,69,rep(NA,8)),
        protein_urine=c(1.06,4.76,0.74,rep(NA,8)),
        uric_acid=c('10.2 (+)','8.1 (+)','5.2 (+)',rep(NA,8)),
        newborn_weight=c(3241,1965,2400,rep(NA,8)),
        iugr=c('NO','YES','NO',rep(NA,8)),
        creatinine=c(1.1,NA,NA,rep(NA,8)),
        platelet=c(87,NA,NA,rep(NA,8))
      ) %>%
      arrange(geo_accession) %>%
      `rownames<-`(NULL) %>%
      column_to_rownames(var='geo_accession') %>%
      select(c(1,7,36:55))
  }
  
  # GSE9984
  if(!is.null(phenotype_input$GSE9984)){
    phenotype_input$GSE9984=phenotype_input$GSE9984  %>% select(c(1,8,10))%>%
      rownames_to_column(var='gsm') %>%
      mutate(
        title=as.character(title),
        `source_name_ch1`='Placenta2', # To differentiate with the subtype placenta because of different GPL
        # Parse characteristics_ch1.1
        `characteristics_ch1`=as.character(`characteristics_ch1`),
        ga=ifelse(str_detect(str_to_lower(title),'first'),str_split_fixed(`characteristics_ch1`,'[(]|[)]',5)[,2],
                  ifelse(str_detect(str_to_lower(title),'second'),str_split_fixed(`characteristics_ch1`,'[(]|[)]',5)[,4],
                         ifelse(str_detect(str_to_lower(title),'term'),'259-287 days',NA))),
        ga=paste(
          as.numeric(str_split_fixed(ga,'-| days',3)[,1])/7,
          as.numeric(str_split_fixed(ga,'-| days',3)[,2])/7,
          sep='-'
        ),
        ga2=paste(
          round((as.numeric(str_split_fixed(ga,'-',2)[,1])-floor(as.numeric(str_split_fixed(ga,'-',2)[,1])))*7),
          round((as.numeric(str_split_fixed(ga,'-',2)[,2])-floor(as.numeric(str_split_fixed(ga,'-',2)[,2])))*7),
          sep='-'
        ),
        ga=paste(
          floor(as.numeric(str_split_fixed(ga,'-',2)[,1])),
          floor(as.numeric(str_split_fixed(ga,'-',2)[,2])),
          sep='-'
        ),
        # Based on manual check, taking maximum GA would determine sampling time period correctly
        ga=as.numeric(str_split_fixed(ga,'-',2)[,2]),
        ga2=as.numeric(str_split_fixed(ga2,'-',2)[,2]),
        # Add columns from the publication (PMID:19050320)
        pe='No',
        iugr='No',
        hellp='No',
        delivery='C-Section',
        prev_ht='No'
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  # Outcomes for discovery sets
  
  ## GSE75010
  if(!is.null(phenotype_input$GSE75010)) phenotype_input$GSE75010=phenotype_input$GSE75010 %>% select(c(1,66:93))
  
  ## GSE98224
  if(!is.null(phenotype_input$GSE98224)) phenotype_input$GSE98224=phenotype_input$GSE98224 %>% select(c(1,66:93))
  
  ## GSE100415
  if(!is.null(phenotype_input$GSE100415)) phenotype_input$GSE100415=phenotype_input$GSE100415 %>% select(c(1,57:78))
  
  # Outcomes for Validation sets
  seed_num=1
  
  ## GSE30186
  if(!is.null(phenotype_input$GSE30186)){
    set.seed(seed_num);phenotype_input$GSE30186=phenotype_input$GSE30186 %>% select(c(1,35:37)) %>%
      rownames_to_column(var='gsm') %>%
      mutate(
        `age:ch1`=as.numeric(str_split_fixed(`age:ch1`,' years',2)[,1]),
        `ethnic group:ch1`='Asian',
        `tissue:ch1`='Placenta',
        # Add columns from the publication (PMID:22702245), random numbers based on mean and SD
        ga2=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,255,6),rnorm(1,273,5)) %>% round()
        }),
        max_sbp=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,163,12.1),rnorm(1,111,11.1)) %>% round()
        }),
        max_dbp=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,108,4.1),rnorm(1,73,5.2)) %>% round()
        }),
        newborn_weight=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,2472,401),rnorm(1,3557,255)) %>% round()
        }),
        protein_urine=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,3.15,1.8),NA) %>% round(2)
        }),
        prev_ht='No'
      ) %>%
      mutate(
        max_sbp=ifelse((max_dbp+(max_sbp-max_dbp)/3)>=65 & (max_sbp-max_dbp)>=30,max_sbp,NA),
        max_dbp=ifelse((max_dbp+(max_sbp-max_dbp)/3)>=65 & (max_sbp-max_dbp)>=30,max_dbp,NA),
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  ## GSE10588
  if(!is.null(phenotype_input$GSE10588)){
    set.seed(seed_num); phenotype_input$GSE10588=phenotype_input$GSE10588 %>% select(c(1,10)) %>%
      rownames_to_column(var='gsm') %>%
      mutate(
        # Add columns from the publication (PMID:19249095), random numbers based on mean and SD
        age=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,30.5,5.2),rnorm(1,30.2,4.8)) %>% round(1)
        }),
        bmi=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,25.9,4.8),rnorm(1,24.8,5.3)) %>% round(1)
        }),
        protein_urine=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,3.93,2.5),NA) %>% round(2)
        }),
        mean_uta_pi=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,1.37,0.7),rnorm(1,0.74,0.30)) %>% round(2)
        }),
        mean_ua_pi=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,1.17,0.36),rnorm(1,0.78,0.18)) %>% round(2)
        }),
        ga2=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,238,25),rnorm(1,277,9)) %>% round()
        }),
        newborn_weight=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,2181,998),rnorm(1,3653,619)) %>% round()
        }),
        plac_weight=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'preeclampsia'),rnorm(1,445,183),rnorm(1,648,154)) %>% round()
        }),
        prev_ht='No'
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  ## GSE24129
  if(!is.null(phenotype_input$GSE24129)){
    set.seed(seed_num); phenotype_input$GSE24129=phenotype_input$GSE24129 %>% select(c(1,38,39)) %>%
      rownames_to_column(var='gsm') %>%
      mutate(
        # Add columns from the publication (PMID:21810232), random numbers based on mean and SD
        age=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'Pre-eclampsia'),rnorm(1,31.0,4.7),
                 ifelse(str_detect(X,'FGR'),rnorm(1,31.4,3.7),rnorm(1,31.5,6.5)
                 )) %>% round(1)
        }),
        bmi=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'Pre-eclampsia'),rnorm(1,21.7,3.7),
                 ifelse(str_detect(X,'FGR'),rnorm(1,19.9,1.9),rnorm(1,21.4,2.3)
                 )) %>% round(1)
        }),
        ga=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'Pre-eclampsia'),rnorm(1,34.4,1.8),
                 ifelse(str_detect(X,'FGR'),rnorm(1,37.3,1.0),rnorm(1,38.1,0.8)
                 )) %>% round(1)
        }),
        newborn_weight=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'Pre-eclampsia'),rnorm(1,1666.6,441.0),
                 ifelse(str_detect(X,'FGR'),rnorm(1,1765.4,483.9),rnorm(1,2891.5,309.6)
                 )) %>% round(1)
        }),
        plac_weight=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'Pre-eclampsia'),rnorm(1,341.3,59.4),
                 ifelse(str_detect(X,'FGR'),rnorm(1,329.4,61.3),rnorm(1,571.4,151.0)
                 )) %>% round(1)
        }),
        max_sbp=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'Pre-eclampsia'),rnorm(1,160.6,10.0),
                 ifelse(str_detect(X,'FGR'),rnorm(1,123.3,14.5),rnorm(1,111.3,11.1)
                 )) %>% round(1)
        }),
        max_dbp=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'Pre-eclampsia'),rnorm(1,105.3,9.6),
                 ifelse(str_detect(X,'FGR'),rnorm(1,77.0,11.9),rnorm(1,66.9,8.7)
                 )) %>% round(1)
        }),
        mean_ua_pi=sapply(X=title,FUN=function(X){
          ifelse(str_detect(X,'Pre-eclampsia'),rnorm(1,1.12,0.34),
                 ifelse(str_detect(X,'FGR'),rnorm(1,1.11,0.32),rnorm(1,0.76,0.10)
                 )) %>% round(1)
        })
      ) %>%
      mutate(
        max_sbp=ifelse((max_dbp+(max_sbp-max_dbp)/3)>=65 & (max_sbp-max_dbp)>=30,max_sbp,NA),
        max_dbp=ifelse((max_dbp+(max_sbp-max_dbp)/3)>=65 & (max_sbp-max_dbp)>=30,max_dbp,NA),
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  ## GSE25906
  if(!is.null(phenotype_input$GSE25906)){
    set.seed(seed_num); phenotype_input$GSE25906=phenotype_input$GSE25906 %>% select(c(1,35:40)) %>%
      rownames_to_column(var='gsm') %>%
      mutate(
        # Add columns from the publication (PMID:21810232), random numbers based on mean and SD
        plac_weight=sapply(X=`classification:ch1`,FUN=function(X){
          ifelse(str_detect(X,'preeclamptic'),rnorm(1,448.2,237.8),rnorm(1,491.7,140.3)) %>% round(1)
        }),
        newborn_weight=sapply(X=`classification:ch1`,FUN=function(X){
          ifelse(str_detect(X,'preeclamptic'),rnorm(1,2278.4,998.9),rnorm(1,3258.4,630.9)) %>% round(1)
        }),
        weight=sapply(X=`classification:ch1`,FUN=function(X){
          ifelse(str_detect(X,'preeclamptic'),rnorm(1,209.2,45.5),rnorm(1,213.9,51.8)) %>% round(1) %>%
            sapply(function(x){x*0.453592}) %>% round(1)
        }),
        delivery='vaginal'
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  ## GSE4707
  if(!is.null(phenotype_input$GSE4707)) phenotype_input$GSE4707=phenotype_input$GSE4707 %>% select(c(1,10,20))
  
  ## GSE44711
  if(!is.null(phenotype_input$GSE44711)){
    fig1_GSE44711_ovl_samples=phenotype_input$GSE44711 %>%
      .$description %>%
      as.character() %>%
      str_split_fixed(string=.,': ',2) %>%
      .[,2] %>%
      .[. %in% c(
        # Figure 1; three samples were unclear, but guessed number was unavailable in the phenotype table
        'PM51','PM67','PM49','PM86','PM6','PM39','PM21','PM129','PM80','PM99','PM15','PM64','PL131',
        'PM12','PL130','PM43','PM116','PM36','PM97','PM138','PL32','PL25','PL76','PL58','PL102','PL38',
        'PL11','PL96','PL26','PL64','PL113','PL112','PL87','PL104','PL56','PL45','PL65'
      )]
    
    phenotype_input$GSE44711=phenotype_input$GSE44711 %>% select(c(1,37:39,20)) %>%
      rownames_to_column(var='gsm') %>%
      mutate(
        # Add columns from the publication (PMID:21810232), matching Figure 1 and description in phenotype table
        description=str_split_fixed(as.character(description),': ',2)[,2],
        hellp=ifelse(description %in% c('PM129'),'Yes',
                     ifelse(description %in% c('PM12','PM21','PM39','PM43','PM51','PM86','PM116','PL113','PL64','PL65','PL56','PL11','PL76','PL96'),'No',NA)),
        fgr=ifelse(description %in% c('PM12','PM21','PM39','PM51','PM86','PM116'),'Yes',
                   ifelse(description %in% c('PM43','PM129','PL113','PL64','PL65','PL56','PL11','PL76','PL96'),'No',NA))
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  # GSE128381
  if(!is.null(phenotype_input$GSE128381)){
    phenotype_input$GSE128381=phenotype_input$GSE128381 %>% select(c(1,48,50:54,57:62)) %>%
      rownames_to_column(var='gsm') %>%
      mutate(
        `birth weight (gram):ch1`=as.numeric(`birth weight (gram):ch1`),
        `ethnicity:ch1`=str_to_sentence(gsub(')','',str_split_fixed(`ethnicity:ch1`,' [(]',2)[,2])),
        `gestational age (month):ch1`=as.numeric(`gestational age (month):ch1`),
        `gestational diabetes:ch1`=str_to_title(gsub(')','',str_split_fixed(`gestational diabetes:ch1`,' [(]',2)[,2])),
        `gestational hypertension:ch1`=
          str_to_title(gsub(')','',str_split_fixed(`gestational hypertension:ch1`,' [(]',2)[,2])),
        `gestational weight gain (kilogram):ch1`=as.numeric(`gestational weight gain (kilogram):ch1`),
        `maternal pre-pregnancy bmi:ch1`=as.numeric(`maternal pre-pregnancy bmi:ch1`),
        `maternal smoking:ch1`=str_to_sentence(gsub(')','',str_split_fixed(`maternal smoking:ch1`,' [(]',2)[,2])),
        `parity:ch1`=as.numeric(str_split_fixed(`parity:ch1`,' [(]',2)[,1]),
        `Sex:ch1`=ifelse(`Sex:ch1`=='Male','M','F'),
        `tissue:ch1`=`tissue:ch1`,
        # Add columns from the publication (PMID:31110514)
        pe='No'
      ) %>%
      column_to_rownames(var='gsm')
  }
  
  # Return
  phenotype_input
}