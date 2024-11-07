

# Preprocessing stroop task


library(dplyr)
library(tidyr)
library(readr)
library(janitor)
# library(readxl)


# Path to files
path = "data/raw data/executive functions/Session 1"

# Read in and combine the files
stroop = rbind( 
  read_csv(file.path(path, "stroop 1.csv")),
  read_csv(file.path(path, "stroop 2.csv")),
  read_csv(file.path(path, "stroop 3.csv")),
  read_csv(file.path(path, "qdvg4 Stroop.csv")),
  read_csv(file.path(path, "wbij5 Stroop.csv")),
  read_csv(file.path(path, "xqls8 Stroop.csv")) 
) %>%
  
  # Rename columns
  rename(participant_home_ID = `Participant Public ID`,
         rt = `Reaction Time`, 
         trial_number = `Trial Number`) %>%
  
  # Convert string values to numeric where appropriate
  mutate(across(c(rt, trial_number), as.numeric)) %>%

# Clean column names
colnames(stroop) = make.names(colnames(stroop))
print(colnames(stroop))

# Select relevant columns
stroop = stroop %>%
  select(rt, participant_home_ID, Correct, Incorrect, Congruency, Zone.Type) %>%
  
  # Filter to remove rows where Zone.Type is not "response"
  filter(Zone.Type == "response_keyboard") %>%
  
  # Filter and clean the data by removing NA and ensuring Congruency is a factor
  drop_na(rt, Congruency) %>%
  mutate(Congruency = factor(Congruency, levels = c(0, 1), 
                             labels = c("incongruent", "congruent")))

# Calculate the median reaction time according to congruence for each participant
stroop = stroop %>%
  group_by(participant_home_ID, Congruency) %>%
  summarize(median_reaction_time_stroop = median(rt, na.rm = TRUE))

# Calculate the difference between the reaction times of each congruence 
# condition per participant.

stroop = stroop %>%
  group_by(participant_home_ID) %>%
  pivot_wider(names_from = Congruency, values_from = median_reaction_time_stroop) %>%
  mutate(stroop = incongruent - congruent) %>%
  select(participant_home_ID, stroop)

