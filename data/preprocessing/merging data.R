

# Merge data

library(dplyr)
library(readxl)


session_IDs_progress = read.csv('data/Participant IDs and session progress.csv')

source('data/preprocessing/preprocessing digit span task.R')
source('data/preprocessing/preprocessing Stroop task.R')
source('data/preprocessing/preprocessing alternating serial reaction time task.R')

source('data/preprocessing/preprocessing Language History Questionnaire.R')

source('data/preprocessing/averaged EEG data importation.R')


# Combine the data frames based on participants' IDs

merged_data =
  
  session_IDs_progress %>%
  
  full_join(LHQ3_aggregate_scores, by = "participant_LHQ3_ID", relationship = "many-to-many") %>%
  
  full_join(session1_digit_span, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_Stroop, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_ASRT, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(ERP_data, by = "participant_lab_ID", relationship = "many-to-many")

# View(merged_data)

# Export to file (! TAKES A LOT OF TIME AND MEMORY !)
# write.csv(merged_data, 'data/final data/merged_data.csv')

# Read in data if necessary
# merged_data = read.csv('data/final data/merged_data.csv')

# Free up memory
gc()

