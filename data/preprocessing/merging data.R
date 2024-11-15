

# Merge data

library(dplyr)
library(readxl)

# Preprocess executive functions data from Session 1
source('data/preprocessing/preprocessing digit span task.R')
source('data/preprocessing/preprocessing Stroop task.R')
source('data/preprocessing/preprocessing alternating serial reaction time task.R')

# Read in language history data
LHQ3_aggregate_scores = read_xlsx('data/raw data/language history/LHQ3 Aggregate Scores.xlsx')

# Combine the data frames based on participants' home ID

merged_data =
  full_join(session1_digit_span, session1_Stroop, 
            by = "participant_home_ID", relationship = "many-to-many") %>%
  
  full_join(session1_ASRT, 
            by = "participant_home_ID", relationship = "many-to-many") %>%
  
  # Delete rows without participant IDs
  drop_na(participant_home_ID)

# View(merged_data)

# Export to file

write.csv(merged_data, 'data/final data/merged_data.csv')

