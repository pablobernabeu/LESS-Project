

# Preprocessing executive function tasks


library(plyr)      
library(tidyverse) 
library(readxl)     
library(janitor)  
library(rmarkdown)


source('data/preprocessing/preprocessing digit span task.R')
gc() # free up memory
source('data/preprocessing/preprocessing Stroop task.R')
gc() # free up memory
source('data/preprocessing/preprocessing alternating serial reaction time task.R')
gc() # free up memory

# Combine the data frames based on Participant Public ID
Session1_executive = 
  digit_span %>%
  full_join(stroop, by = "participant_home_ID", relationship = "many-to-many") %>%
  full_join(ASRT, by = "participant_home_ID", relationship = "many-to-many")

# View the combined data
# View(Session1_data)

# Merge the two data frames by 'Participant_ID'
Background_data = merge(LHQ3_final, Session1_data, by = "participant_home_ID")

# View the merged data
# View(Background_data)

# Save to a specific directory
write.csv(Background_data, "Background/Background_data.csv", row.names = FALSE)

