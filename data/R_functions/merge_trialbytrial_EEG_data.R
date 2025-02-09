

merge_trialbytrial_EEG_data <- 
  
  # Default file pattern catches all data
  function(EEG_file_pattern = "^\\d+_trialbytrial_S[123]_S10[123]\\.",
           min_time = -100, max_time = 1100,
           include_baseline = TRUE,
           aggregate_electrodes = FALSE,
           aggregate_time_points = FALSE,
           selected_macroregion = NULL) {
    
    require(dplyr)
    require(readxl)
    
    IDs_session_progress <- 
      read.csv('data/Participant IDs and session progress.csv') %>%
      rename(mini_language = language) %>% # Rename for greater clarity
      # Remove session details to reduce data size
      select(-contains(c('inspector', 'conductor', 'supervision', 'date', 'time'))) %>%
      mutate(participant_lab_ID = as.factor(participant_lab_ID))
    
    source('data/preprocessing/preprocess digit span task.R')
    source('data/preprocessing/preprocess Stroop task.R')
    source('data/preprocessing/preprocess alternating serial reaction time task.R')
    
    source('data/preprocessing/preprocess Language History Questionnaire.R')
    
    source('data/R_functions/import_trialbytrial_EEG_data.R')
    trialbytrial_EEG_data = import_trialbytrial_EEG_data(EEG_file_pattern = EEG_file_pattern,
                                                         min_time = min_time, 
                                                         max_time = max_time,
                                                         aggregate_electrodes = aggregate_electrodes,
                                                         aggregate_time_points = aggregate_time_points,
                                                         include_baseline = include_baseline,
                                                         selected_macroregion = selected_macroregion)
    
    # Free unused memory
    gc()
    
    # Combine the data frames based on participants' IDs
    
    trialbytrial_EEG_data <-
      
      trialbytrial_EEG_data %>% 
      mutate(participant_lab_ID = as.factor(participant_lab_ID)) %>% 
      
      left_join(IDs_session_progress, by = "participant_lab_ID") %>%
      
      left_join(session1_digit_span, by = "participant_home_ID", relationship = "many-to-many") %>%
      
      left_join(session1_Stroop, by = "participant_home_ID", relationship = "many-to-many") %>%
      
      left_join(session1_ASRT, by = "participant_home_ID", relationship = "many-to-many") %>%
      
      left_join(LHQ3_aggregate_scores, by = "participant_LHQ3_ID", relationship = "many-to-many") %>%
      
      # Translate markers into labels and create time windows, brain regions, etc.
      mutate(
        
        # Recode dichotomous predictor (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159)
        recoded_mini_language = as.numeric(case_when(
          mini_language == 'Mini-Norwegian' ~ 0.5,
          mini_language == 'Mini-English' ~ -0.5,
          TRUE ~ NA_real_
        )),
        
        # Z-score between-participants predictors (Brauer & Curtin, 2018; 
        # https://doi.org/10.1037/met0000159).
        
        z_recoded_mini_language = ifelse(is.na(recoded_mini_language) | recoded_mini_language == '', NA, 
                                         as.numeric(scale(recoded_mini_language))),
        
        z_session1_digit_span = ifelse(is.na(session1_digit_span) | session1_digit_span == '', NA, 
                                       as.numeric(scale(session1_digit_span))),
        
        z_session1_Stroop = ifelse(is.na(session1_Stroop) | session1_Stroop == '', NA, 
                                   as.numeric(scale(session1_Stroop))),
        
        z_session1_ASRT = ifelse(is.na(session1_ASRT) | session1_ASRT == '', NA, 
                                 as.numeric(scale(session1_ASRT))),
        
        z_multilingual_language_diversity = ifelse(is.na(multilingual_language_diversity) | 
                                                     multilingual_language_diversity == '', NA, 
                                                   as.numeric(scale(multilingual_language_diversity))),
        
        # Convert character variables to factors
        across(where(is.character), as.factor)
      ) %>%
      
      # Order variables
      select(amplitude, participant_lab_ID, session, grammatical_property, 
             grammaticality, sentence_marker, brain_region, macroregion, 
             hemisphere, caudality, time_window, everything())
    
    # Replace empty cells with NAs and remove rows that have NAs in all columns
    trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
      mutate(across(where(is.factor), as.character),
             across(where(is.character), ~ na_if(.x, "")),
             # Convert character variables to factors
             across(where(is.character), as.factor)) %>%
      filter(if_any(everything(), ~ !is.na(.)))
    
    # Free unused memory
    gc()
    
    return(trialbytrial_EEG_data)
  }

