

# Preprocessing executive function tasks


library(plyr)      
library(tidyverse) 
library(readxl)     
library(janitor)  
library(rmarkdown)


# Loading the behavioural data from Session 1 
Stroop = read.csv("Raw data/executive functions/difference_reaction_time_stroop.csv")
TL = read.csv("Raw data/executive functions/TL_RT_wide_1.csv")
DGS = read.csv("Raw data/executive functions/analysis_table_DGS.csv")

# View(DGS)
# View(Stroop)
# View(TL)

# Combine the data frames based on Participant Public ID
Session1_data = DGS %>%
  full_join(Stroop, by = "Participant.Public.ID", relationship = "many-to-many") %>%
  full_join(TL, by = "Participant.Public.ID", relationship = "many-to-many")

# View the combined data
# View(Session1_data)

# Rename the column 'Participant.Public.ID' to 'Participant_ID'
names(Session1_data)[names(Session1_data) == "Participant.Public.ID"] = "Participant_ID"

# Merge the two data frames by 'Participant_ID'
Background_data = merge(LHQ3_final, Session1_data, by = "Participant_ID")

# View the merged data
# View(Background_data)

# Save to a specific directory
write.csv(Background_data, "Background/Background_data.csv", row.names = FALSE)

