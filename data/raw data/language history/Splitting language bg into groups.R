library(readxl)
library(dplyr)
library(openxlsx)

norway_logbook <- read.csv("data/Participant IDs and session progress.csv")
LHQ3 Raw Data <- read_excel("data/raw data/language history/LHQ3 Raw Data.xlsx")
colnames(LHQ3 Raw Data) <- LHQ3 Raw Data[1, ]

norway_language <- norway_logbook[, c("participant_LHQ3_ID", "language")]
participant_data <- merge(LHQ3 Raw Data, norway_language, by.x = "\r\n0.Participant ID", by.y = "participant_LHQ3_ID", all = FALSE)

col_names <- colnames(participant_data)

# Loop through the column names to rename NA or empty headers
for (i in 2:length(col_names)) {
  if (is.na(col_names[i]) || col_names[i] == "") {
    col_names[i] <- paste0(col_names[i - 1], "_1") # Rename with the previous column's name +1
  }
}

# Assign the updated column names back to the data frame
colnames(participant_data) <- col_names

# Filter data for Mini-English and Mini-Norwegian
mini_english_data <- participant_data %>% filter(language == "Mini-English")
mini_norwegian_data <- participant_data %>% filter(language == "Mini-Norwegian")

# Write the filtered data to new Excel files
write.xlsx(mini_english_data, file = "data/raw data/language history/mini_english_LHQ3_data.xlsx")
write.xlsx(mini_norwegian_data, "data/raw data/language history/mini_Norwegian_LHQ3_Data.xlsx")
