
# Merge data aggregated across trials

library(dplyr)
library(readxl)

session_IDs_progress <- read.csv('data/Participant IDs and session progress.csv')

source('data/preprocessing/preprocess digit span task.R')
source('data/preprocessing/preprocess Stroop task.R')
source('data/preprocessing/preprocess alternating serial reaction time task.R')

source('data/preprocessing/preprocess Language History Questionnaire.R')

source('data/preprocessing/import trial-by-trial EEG data.R')

# Combine the data frames based on participants' IDs

averaged_EEG_data <-
  
  session_IDs_progress %>%
  
  full_join(LHQ3_aggregate_scores, by = "participant_LHQ3_ID", relationship = "many-to-many") %>%
  
  full_join(session1_digit_span, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_Stroop, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_ASRT, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(trialbytrial_EEG_data, by = "participant_lab_ID", relationship = "many-to-many")

# View(averaged_EEG_data)

# Export to file (! TAKES A LOT OF TIME AND MEMORY !)
# write.csv(averaged_EEG_data, 'data/final data/averaged_EEG_data.csv')

# Read in data if necessary
# averaged_EEG_data <- read.csv('data/final data/averaged_EEG_data.csv')

# Free unused memory
gc()
