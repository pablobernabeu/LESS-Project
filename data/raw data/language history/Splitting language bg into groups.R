library(readxl)
library(dplyr)
library(openxlsx)


# Load the two Excel files
norway_logbook <- read.csv("data/Participant IDs and session progress.csv")
LHQ3_Raw_Data <- read_excel("data/raw data/language history/LHQ3_Raw_Data.xlsx")
colnames(LHQ3_Raw_Data) <- LHQ3_Raw_Data[1, ]
#LHQ3_Raw_Data <- LHQ3_Raw_Data[-1, ]

View(LHQ3_Raw_Data)
View(norway_logbook)

# Select the 'language' column and 'participant_LHQ3_ID' from norway_logbook
norway_language <- norway_logbook[, c("participant_LHQ3_ID", "language")]




# Merge the 'language' column into LHQ3_Raw_Data based on the participant ID
LHQ3_Raw_Data <- merge(LHQ3_Raw_Data, norway_language, by.x = "	1.Participant ID number", by.y = "participant_LHQ3_ID", all.x = TRUE)

# View the updated LHQ3_Raw_Data
View(LHQ3_Raw_Data)


# Transfer the 'language' column from Norway site to LHQ3 Raw Data
updated_data <- LHQ3_Raw_Data %>%
  left_join(norway_logbook %>% select(participant_LHQ3_ID, language), 
            by = c("0.Participant ID" = "participant_LHQ3_ID"))


# Filter data for Mini-English and Mini-Norwegian
mini_english_data <- updated_data %>% filter(language == "Mini-English")
mini_norwegian_data <- updated_data %>% filter(language == "Mini-Norwegian")

# Write the filtered data to new Excel files
write.xlsx(mini_english_data, "/mnt/data/Mini_English_Data.xlsx")
write.xlsx(mini_norwegian_data, "/mnt/data/Mini_Norwegian_Data.xlsx")

