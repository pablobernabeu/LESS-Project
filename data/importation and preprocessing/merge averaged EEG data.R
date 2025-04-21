

# Merge EEG data aggregated across trials

library(dplyr)
library(readxl)


session_IDs_progress <- 
  read.csv('data/Participant IDs and session progress.csv') %>%
  # Name variable 'mini_language' for greater clarity
  rename(mini_language = language)

source('data/importation and preprocessing/preprocess digit span task.R')
source('data/importation and preprocessing/preprocess Stroop task.R')
source('data/importation and preprocessing/preprocess alternating serial reaction time task.R')

source('data/importation and preprocessing/preprocess Language History Questionnaire.R')

source('data/importation and preprocessing/import averaged EEG data.R')

# Free unused memory
gc()

# Combine data frames based on participants' IDs
averaged_EEG_data <- session_IDs_progress %>%
  
  mutate(participant_lab_ID = as.factor(participant_lab_ID)) %>%
  
  # Remove session details to reduce data size
  select(-contains(c('inspector', 'conductor', 'supervision'))) %>%
  
  full_join(session1_digit_span, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_Stroop, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_ASRT, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(LHQ3_aggregate_scores, by = "participant_LHQ3_ID", relationship = "many-to-many") %>%
  
  full_join(averaged_EEG_data %>%
              mutate(participant_lab_ID = as.factor(participant_lab_ID)), 
            by = "participant_lab_ID", relationship = "many-to-many") %>%
  
  mutate(
    
    # Z-score between-participants predictors, following Brauer and Curtin (2018; 
    # https://doi.org/10.1037/met0000159).
    
    z_session1_digit_span = scale(session1_digit_span),
    z_session1_Stroop = scale(session1_Stroop),
    z_session1_ASRT = scale(session1_ASRT),
    z_multilingual_language_diversity = scale(multilingual_language_diversity),
    
    # Convert character variables to factors
    across(where(is.character), as.factor)
  ) %>%
  
  # Z-score amplitude around each participant's own mean to preserve individual 
  # differences (Faust et al., 1999; https://doi.org/10.1037/0033-2909.125.6.777)
  # Similarly, z-score between-items predictors around each participant's own mean, 
  # following Brauer and Curtin (2018; https://doi.org/10.1037/met0000159).
  
  group_by(participant_lab_ID) %>%
  mutate(
    z_amplitude = scale(amplitude),
    z_recoded_grammaticality = scale(recoded_grammaticality),
    z_recoded_session = scale(recoded_session),
    z_recoded_hemisphere = scale(recoded_hemisphere),
    z_recoded_caudality = scale(recoded_caudality)
  ) %>%
  ungroup() %>%
  
  # Order variables
  select(amplitude, participant_lab_ID, session, grammatical_property, grammaticality, 
         electrode, brain_region, hemisphere, caudality, time, time_window, everything())

# Free unused memory
gc()

