

# Session 3.
# Stack up both parts of the experiment, maintaining the stimulus lists. 
# Iterate over gender agreement files, attaching to each file the 
# stimuli for differential object marking.

combine_Session3_experiments = 
  
  function(study_site, language, experiment_path) {
    
    require(dplyr)    # data wrangling
    require(stringr)  # text processing
    
    gender_agreement_files = 
      list.files(pattern = paste0('^', study_site, '.*', language, 
                                  '.*Session3_.*gender_agreement.*'), 
                 path = experiment_path)
    
    for(i in seq_along(gender_agreement_files)) {
      
      gender_agreement_file = 
        list.files(pattern = paste0('^', study_site, '.*', language, 
                                    '.*Session3_.*gender_agreement.*List ', i, '.*'), 
                   path = experiment_path)
      
      gender_agreement = 
        read.csv(paste0(experiment_path, 
                        eval(parse(text = 'gender_agreement_file'))))
      
      differential_object_marking_file = 
        list.files(pattern = paste0('^', study_site, '.*', language, 
                                    '.*Session3_.*differential_object.*List ', i, '.*'), 
                   path = experiment_path)
      
      differential_object_marking = 
        read.csv(paste0(experiment_path, 
                        eval(parse(text = 'differential_object_marking_file'))))
      
      # Add any missing columns to allow merging of both dataframes
      
      gender_agreement[
        setdiff(names(differential_object_marking), 
                names(gender_agreement)) ] = ''
      
      differential_object_marking[
        setdiff(names(gender_agreement), 
                names(differential_object_marking)) ] = ''
      
      # Order columns equally
      
      gender_agreement = gender_agreement %>%
        select(order(colnames(.)))
      
      differential_object_marking = differential_object_marking %>%
        select(order(colnames(.)))
      
      # Stack up both dataframes
      rbind(gender_agreement, differential_object_marking) %>%
        
        # To save space in the data set, replace NAs with blanks
        mutate(across(names(.), ~replace(., is.na(.), ''))) %>%
        
        # Save as file
        write.csv(paste0(experiment_path, study_site, ' site, ', language, ', ',
                         'Session3_combined_Experiment, List ', i, '.csv'), 
                  row.names = FALSE, fileEncoding = 'UTF-8')
    }
    
    # Delete intermediary files, namely, those that don't have
    # the word 'combined' in their names.
    
    intermediary_files = 
      list.files(experiment_path) %>% 
      data.frame() %>% 
      filter(str_detect(., paste0(study_site, '.*', language)),
             !str_detect(., 'combined')) %>%
      pull(.)
    
    unlink(paste0(experiment_path, '/', intermediary_files))
    
  }

