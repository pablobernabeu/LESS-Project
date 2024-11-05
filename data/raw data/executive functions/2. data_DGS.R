

# title: "DGS_24.05"
# author: "Gabriella Silva"
# editor: "Christina Athanasiadi"
# date: "2024-09-10"
# output: html_document


library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(readxl)

# Define the path
path = "Raw data/executive functions/Session 1"

# Read the files
DGS_1 = read_csv(file.path(path, "DGS_DGS.csv"))
DGS_2 = read_csv(file.path(path, "DGS_Stroop.csv"))
DGS_3 = read_csv(file.path(path, "DGS_ASRT.csv"))

# Upload the additional CSV files
DGS_1_extra = read_csv(file.path(path, "qdvg4_DGS_DGS.csv"))
DGS_2_extra_1 = read_csv(file.path(path, "wbij5_DGS_Stroop.csv"))
DGS_2_extra_2 = read_csv(file.path(path, "xqls8_DGS_Stroop.csv"))

# Combine the data frames
DGS_1 = bind_rows(DGS_1, DGS_1_extra)
DGS_2 = bind_rows(DGS_2, DGS_2_extra_1, DGS_2_extra_2)

# View(DGS_1)
# View(DGS_2)

# Clear column names
colnames(DGS_1) = make.names(colnames(DGS_1))
colnames(DGS_2) = make.names(colnames(DGS_2))
colnames(DGS_3) = make.names(colnames(DGS_3))


# Select columns to combine
DGS_1 = DGS_1 %>%
  select(Attempt, Correct, display, listLength, Reaction.Time, display, Participant.Public.ID)

DGS_2 = DGS_2 %>%
  select(Attempt, Correct, display, listLength, Reaction.Time, display, Participant.Public.ID)

DGS_3 = DGS_3 %>%
  select(Attempt, Correct, display, listLength, Reaction.Time, display, Participant.Public.ID)

# Combine the dataframes
merged_DGS = bind_rows(DGS_1, DGS_2, DGS_3)

# Convert display column to a factor
merged_DGS = merged_DGS %>%
  mutate(display = as.factor(display))

# Group data by Participant.Private.ID
# grouped_DGS = merged_DGS %>%
 # group_by(Participant.Public.ID)

# To calculate the average Correct per participant:
# !Filter for rows where Attempt == 1 and calculate the average Correct for each 
# participant
average_correct = merged_DGS %>%
  filter(Attempt == 1) %>%          # Filter rows where Attempt is 1
  group_by(Participant.Public.ID) %>%
  summarise(average_correct = mean(Correct, na.rm = TRUE))  
  
print(average_correct)

# Calculate the total number of correct digits per participant
total_correct = merged_DGS %>%
  group_by(Participant.Public.ID) %>%
  summarize(total_correct = sum(Correct))

print(total_correct)

# Calculate the number of correct digits for each list length per participant
scores_by_length = merged_DGS %>%
  group_by(Participant.Public.ID, listLength) %>%
  summarize(total_correct = sum(Correct))

print(scores_by_length)

# Calculate the number of errors per type and per participant
errors = merged_DGS %>%
  group_by(Participant.Public.ID) %>%
  summarize(omissions = sum(ifelse(Correct == 0 & is.na(display), 1, 0)),
            intrusions = sum(ifelse(Correct == 0 & !is.na(display), 1, 0)),
            repetitions = sum(ifelse(Correct == 0 & display == "Repeat", 1, 0)))

print(errors)

# Combine the results into a single table
analysis_table_DGS = left_join(total_correct, scores_by_length, by = "Participant.Public.ID") %>%
  left_join(errors, by = "Participant.Public.ID")

# Find the highest score per participant
# Remove rows where Participant.Public.ID is NA, and then find the highest 
# total_correct.y for each participant
highest_total_correct_per_participant = analysis_table_DGS %>%
  filter(!is.na(Participant.Public.ID)) %>%
  group_by(Participant.Public.ID) %>%
  summarize(highest_total_correct = max(total_correct.y, na.rm = TRUE))

# Print the result
print(highest_total_correct_per_participant)

# Merge both highest_total_correct and average_correct with analysis_table_DGS
# analysis_table_DGS = analysis_table_DGS %>%
 # left_join(highest_total_correct_per_participant, by = "Participant.Public.ID") %>%
  # left_join(average_correct, by = "Participant.Public.ID") %>%
  # left_join(display, by = "Participant.Public.ID")

analysis_table_DGS = analysis_table_DGS %>%
  left_join(highest_total_correct_per_participant, by = "Participant.Public.ID") %>%
  left_join(average_correct, by = "Participant.Public.ID")

# View(analysis_table_DGS)
write_csv(analysis_table_DGS, "analysis_table_DGS.csv")
# A greater number of correctly remembered digits indicates a greater 
# working memory capacity.```
# total.correct.x refers to the total number of correct responses per participant
# across lists
# total.correct.y refers to the total number of correct responses per participant
# for each specific list

