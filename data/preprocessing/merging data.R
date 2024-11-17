

# Merge data

library(dplyr)
library(readxl)


session_IDs_progress = read.csv('Participant IDs and session progress.csv')

LHQ3_aggregate_scores = 
  read_xlsx('data/raw data/language history/LHQ3 Aggregate Scores.xlsx') %>%
  rename()

source('data/preprocessing/preprocessing digit span task.R')
source('data/preprocessing/preprocessing Stroop task.R')
source('data/preprocessing/preprocessing alternating serial reaction time task.R')

source('data/preprocessing/averaged EEG data importation.R')


# Combine the data frames based on participants' IDs

merged_data =
  
  session_IDs_progress %>%
  
  # full_join(LHQ3_aggregate_scores, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_digit_span, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_Stroop, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_ASRT, by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(ERP_data, by = "participant_lab_ID", relationship = "many-to-many")

# View(merged_data)

# Export to file

write.csv(merged_data, 'data/final data/merged_data.csv')

merged_data = read.csv('data/final data/merged_data.csv')

