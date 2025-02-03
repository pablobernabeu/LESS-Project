

# Merge data aggregated across trials

library(dplyr)
library(readxl)

IDs_session_progress <- 
  read.csv('data/Participant IDs and session progress.csv') %>%
  # Name variable 'mini_language' for greater clarity
  rename(mini_language = language)

source('data/preprocessing/preprocess digit span task.R')
source('data/preprocessing/preprocess Stroop task.R')
source('data/preprocessing/preprocess alternating serial reaction time task.R')

source('data/preprocessing/preprocess Language History Questionnaire.R')

source('data/preprocessing/import trial-by-trial EEG data.R')

# Free unused memory
gc()

# Combine the data frames based on participants' IDs

trialbytrial_EEG_data <-
  
  IDs_session_progress %>% 
  # Remove session details to reduce data size
  select(-contains(c('inspector', 'conductor', 'supervision'))) %>%
  mutate(participant_lab_ID = as.factor(participant_lab_ID)) %>%
  
  full_join(session1_digit_span, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_Stroop, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_ASRT, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(LHQ3_aggregate_scores, by = "participant_LHQ3_ID", relationship = "many-to-many") %>%
  
  full_join(trialbytrial_EEG_data %>% 
              mutate(participant_lab_ID = as.factor(participant_lab_ID)), 
            by = "participant_lab_ID", relationship = "many-to-many") %>%
  
  # Create time windows, brain regions, etc.
  
  mutate(
    time_window = case_when(
      time >= 200 & time <= 498 ~ '200_500',
      time >= 300 & time <= 598 ~ '300_600',
      time >= 400 & time <= 898 ~ '400_900',
      TRUE ~ NA_character_
    ),
    brain_region = case_when(
      electrode %in% c('Fp1', 'F3', 'F7', 'FT9', 'FC5') ~ 'left anterior',
      electrode %in% c('Fp2', 'F4', 'F8', 'FT10', 'FC6') ~ 'right anterior',
      electrode %in% c('T7', 'C3', 'CP5') ~ 'left medial',
      electrode %in% c('T8', 'C4', 'CP6') ~ 'right medial',
      electrode %in% c('P7', 'P3', 'O1') ~ 'left posterior',
      electrode %in% c('P8', 'P4', 'O2') ~ 'right posterior',
      electrode %in% c('Fz', 'FC1', 'FC2') ~ 'midline anterior',
      electrode %in% c('Cz', 'CP1', 'CP2') ~ 'midline medial',
      electrode %in% c('Pz', 'Oz') ~ 'midline posterior',
      TRUE ~ NA_character_
    ),
    hemisphere = case_when(
      str_detect(brain_region, 'left') ~ 'left',
      str_detect(brain_region, 'right') ~ 'right',
      TRUE ~ NA_character_
    ),
    caudality = case_when(
      str_detect(brain_region, 'anterior') ~ 'anterior',
      str_detect(brain_region, 'medial') ~ 'medial',
      str_detect(brain_region, 'posterior') ~ 'posterior',
      TRUE ~ NA_character_
    ),
    
    # Recode dichotomous predictors (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159).
    # Session is specifically recoded following Michael Clark's recommendation at 
    # https://m-clark.github.io/sem/growth-curves.html#numbering-the-time-points.
    
    recoded_grammaticality = case_when(
      grammaticality == 'ungrammatical' ~ -0.5,
      grammaticality == 'ancillary violation' ~ 0,
      grammaticality == 'grammatical' ~ 0.5,
      TRUE ~ NA_real_
    ),
    recoded_session = case_when(
      session == 2 ~ 0,
      session == 3 ~ 1,
      session == 4 ~ 2,
      session == 6 ~ 3,
      TRUE ~ NA_real_
    ),
    recoded_mini_language = case_when(
      mini_language == 'Mini-English' ~ -0.5,
      mini_language == 'Mini-Norwegian' ~ 0.5,
      TRUE ~ NA_real_
    ),
    recoded_hemisphere = case_when(
      hemisphere == 'left' ~ -0.5,
      hemisphere == 'right' ~ 0.5,
      TRUE ~ NA_real_
    ),
    recoded_caudality = case_when(
      caudality == 'anterior' ~ -0.5,
      caudality == 'medial' ~ 0,
      caudality == 'posterior' ~ 0.5,
      TRUE ~ NA_real_
    ),
    
    # Z-score between-participants predictors, following Brauer and Curtin (2018; 
    # https://doi.org/10.1037/met0000159).
    
    z_recoded_mini_language = scale(recoded_mini_language),
    z_session1_Stroop = scale(session1_Stroop),
    z_session1_ASRT = scale(session1_ASRT),
    z_session1_digit_span = scale(session1_digit_span),
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
         sentence_marker, electrode, brain_region, hemisphere, caudality, time, 
         time_window, everything())

# View(trialbytrial_EEG_data)

# Export to file (! TAKES A LOT OF TIME AND MEMORY !)
# write.csv(trialbytrial_EEG_data, 'data/final data/trialbytrial_EEG_data.csv')

# Read in data if necessary
# trialbytrial_EEG_data <- read.csv('data/final data/trialbytrial_EEG_data.csv')

# Free unused memory
gc()

